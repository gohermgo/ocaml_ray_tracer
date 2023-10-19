open Ray
let abs_diff x y = Float.abs (Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

(* Ray instantiation tests *)
let%test "Scenario: Creating and querying a ray" = let r = init ~origin:(Tuple.point (1.0, 2.0, 3.0)) ~direction:(Tuple.vector (4.0, 5.0, 6.0)) in
  Tuple.equal (origin r) (Tuple.point (1.0, 2.0, 3.0)) &&
  Tuple.equal (direction r) (Tuple.vector (4.0, 5.0, 6.0))

(* Ray basic tests *)
let%test "Scenario: Computing a point from a distance" = let ro = Tuple.point (2.0, 3.0, 4.0) in
  let rd = Tuple.vector (1.0, 0.0, 0.0) in
  let r = init ~origin:ro ~direction:rd in 
  Tuple.equal (position r 0.0) (Tuple.point (2.0, 3.0, 4.0)) &&
  Tuple.equal (position r 1.0) (Tuple.point (3.0, 3.0, 4.0)) &&
  Tuple.equal (position r (-1.0)) (Tuple.point (1.0, 3.0, 4.0)) &&
  Tuple.equal (position r 2.5) (Tuple.point (4.5, 3.0, 4.0))

(* Ray transform tests *)
let%test "Scenario: Translating a ray" = let r = init ~origin:(Tuple.point(1.0, 2.0, 3.0)) ~direction:(Tuple.vector(0.0, 1.0, 0.0)) in
  let m = Transformation.translation 3.0 4.0 5.0 in
  let r2 = transform r m in
  Tuple.equal (origin r2) (Tuple.point(4.0, 6.0, 8.0)) &&
  Tuple.equal (direction r2) (Tuple.vector(0.0, 1.0, 0.0))

let%test "Scenario: Scaling a ray" = let r = init ~origin:(Tuple.point(1.0, 2.0, 3.0)) ~direction:(Tuple.vector(0.0, 1.0, 0.0)) in
  let m = Transformation.scaling 2.0 3.0 4.0 in
  let r2 = transform r m in
  Tuple.equal (origin r2) (Tuple.point(2.0, 6.0, 12.0)) &&
  Tuple.equal (direction r2) (Tuple.vector(0.0, 3.0, 0.0))

(* Ray intersection tests *)
let%test "Scenario: A ray intersects a sphere at two points" = let r = init ~origin:(Tuple.point (0.0, 0.0, -5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let s = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  let xs = check_intersection s r in
  Int.equal (Array.length xs) 2 &&
  f_equal (Intersection.t_value xs.(0)) 4.0 &&
  f_equal (Intersection.t_value xs.(1)) 6.0

let%test "Scenario: A ray intersects a sphere at a tangent" = let r = init ~origin:(Tuple.point (0.0, 1.0, -5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let s = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  let xs = check_intersection s r in
  (* SHOULD STILL RETURN TWO, MAKE SURE OF THIS *)
  Int.equal (Array.length xs) 2 &&
  f_equal (Intersection.t_value xs.(0)) 5.0 &&
  f_equal (Intersection.t_value xs.(1)) 5.0

let%test "Scenario: A ray misses a sphere" = let r = init ~origin:(Tuple.point (0.0, 2.0, -5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  (*let s = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let xs = intersects (Geometry.Sphr(s)) r in*)
  let s = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  let xs = check_intersection s r in
  Int.equal (Array.length xs) 0 

let%test "Scenario: A ray originates inside a sphere" = let r = init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  (*let s = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let xs = intersects (Geometry.Sphr(s)) r in*)
  let s = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  let xs = check_intersection s r in
  Int.equal (Array.length xs) 2 &&
  f_equal (Intersection.t_value xs.(0)) (-1.0) &&
  f_equal (Intersection.t_value xs.(1)) 1.0

let%test "Scenario: A sphere is behind a ray" = let r = init ~origin:(Tuple.point (0.0, 0.0, 5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  (*let s = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let xs = intersects (Geometry.Sphr(s)) r in*)
  let s = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  let xs = check_intersection s r in
  Int.equal (Array.length xs) 2 &&
  f_equal (Intersection.t_value xs.(0)) (-6.0) &&
  f_equal (Intersection.t_value xs.(1)) (-4.0)

let%test "Scenario: An intersection encapsulates t and object" = let sphere = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in 
  (*let sp = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in*)
  let sphere_ptr = ref sphere in
  let it = Intersection.init ~t:3.5 ~o:sphere_ptr in
  f_equal (Intersection.t_value it) (3.5) &&
  sphere_ptr == (Intersection.object_pointer it) &&
  sphere_ptr = (Intersection.object_pointer it)

(* Ray intersection array and equality tests *)
let%test "Scenario: Aggregating intersections" = let sphere = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  (*let sp = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in*)
  let sphere_ptr = ref sphere in
  let it1 = Intersection.init ~t:1.0 ~o:sphere_ptr in
  let it2 = Intersection.init ~t:2.0 ~o:sphere_ptr in
  let xs = [|it1; it2|] in
  Int.equal (Array.length xs) 2 &&
  f_equal (Intersection.t_value xs.(0)) (1.0) &&
  f_equal (Intersection.t_value xs.(1)) (2.0)

let%test "Scenario: Intersect sets the object on the intersection" =let sphere = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  (*let sp = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in*)
  let sphere_ptr = ref sphere in
  let it1 = Intersection.init ~t:1.0 ~o:sphere_ptr in
  let it2 = Intersection.init ~t:2.0 ~o:sphere_ptr in
  let xs = [|it1; it2|] in
  Int.equal (Array.length xs) 2 &&
  Intersection.object_pointer xs.(0) == sphere_ptr &&
  Intersection.object_pointer xs.(1) == sphere_ptr

(* Ray hit tests *)
let%test "Scenario: The hit, when all intersections have positive t" = let sphere = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  (*let sp = Sphere.init ~origin:Tuple.point_origin ~radius:1.0 in*)
  let sphere_ref = ref sphere in
  let i1 = Intersection.init ~t:1.0 ~o:sphere_ref in
  let i2 = Intersection.init ~t:2.0 ~o:sphere_ref in
  let xs = [|i1; i2|] in
  let i = Option.get (hit xs) in
  Intersection.equal i i1

let%test "Scenario: The hit, when some intersections have negative t" = let sphere = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  (*let sp = Sphere.init ~origin:Tuple.point_origin ~radius:1.0 in*)
  let sphere_ptr = ref sphere in
  let i1 = Intersection.init ~t:(-1.0) ~o:sphere_ptr in
  let i2 = Intersection.init ~t:1.0 ~o:sphere_ptr in
  let xs = [|i1; i2|] in
  let i = Option.get (hit xs) in
  Intersection.equal i i2

let%test "Scenario: The hit, when all intersections have negative t" = let sphere = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  (*let sp = Sphere.init ~origin:Tuple.point_origin ~radius:1.0 in*)
  let sphere_ptr = ref sphere in
  let i1 = Intersection.init ~t:(-2.0) ~o:sphere_ptr in
  let i2 = Intersection.init ~t:(-1.0) ~o:sphere_ptr in
  let xs = [|i1; i2|] in
  let i = hit xs in
  Option.is_none i

let%test "Scenario: The hit is always the lowest nonnegative intersection" = let sphere = Geometry.init_sphere ~o:Tuple.point_origin ~r:1.0 in
  (*let sp = Sphere.init ~origin:Tuple.point_origin ~radius:1.0 in*)
  let sphere_ptr = ref sphere in
  let i1 = Intersection.init ~t:5.0 ~o:sphere_ptr in
  let i2 = Intersection.init ~t:7.0 ~o:sphere_ptr in
  let i3 = Intersection.init ~t:(-3.0) ~o:sphere_ptr in
  let i4 = Intersection.init ~t:2.0 ~o:sphere_ptr in
  let xs = [|i1; i2; i3; i4|] in
  let i = Option.get (hit xs) in
  Intersection.equal i i4
