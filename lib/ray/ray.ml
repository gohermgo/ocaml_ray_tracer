type t = {origin: Tuple.t; direction: Tuple.t}
let init ~origin ~direction = let () = if not (Tuple.is_point origin) || not (Tuple.is_vector direction) then failwith "Bad ray init" in {origin = origin; direction = direction}
let origin ray = ray.origin
let direction ray = ray.direction

let%test "Scenario: Creating and querying a ray" = let r = init ~origin:(Tuple.point (1.0, 2.0, 3.0)) ~direction:(Tuple.vector (4.0, 5.0, 6.0)) in
  Tuple.equal (origin r) (Tuple.point (1.0, 2.0, 3.0)) &&
  Tuple.equal (direction r) (Tuple.vector (4.0, 5.0, 6.0))

let position ray t = Tuple.add (origin ray) (Tuple.mul (direction ray) t)

let%test "Scenario: Computing a point from a distance" = let r = init ~origin:(Tuple.point (2.0, 3.0, 4.0)) ~direction:(Tuple.vector (1.0, 0.0, 0.0)) in 
  Tuple.equal (position r 0.0) (Tuple.point (2.0, 3.0, 4.0)) &&
  Tuple.equal (position r 1.0) (Tuple.point (3.0, 3.0, 4.0)) &&
  Tuple.equal (position r (-1.0)) (Tuple.point (1.0, 3.0, 4.0)) &&
  Tuple.equal (position r 2.5) (Tuple.point (4.5, 3.0, 4.0))

let abs_diff x y = Float.abs (Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

let _discriminant _sp _ry = let sphere_to_ray = Tuple.sub (origin _ry) (Sphere.origin _sp) in
  let a = Tuple.dot (direction _ry) (sphere_to_ray) in
  let b = Float.mul 2.0 a in
  let c = Float.sub a 1.0 in
  Float.sub (Float.pow b 2.0) (Float.mul 4.0 (Float.mul a c))
type 'a intersection = {t_val: float; int_obj: 'a Geometry.shape ref}
type 'a intersection_col = 'a intersection array
let int_init t_val shp = {t_val = t_val; int_obj = shp}
let int_t_val i = i.t_val
let int_shape i = i.int_obj
(*let intersects_sphere sp ry = 
  let sp_to_ry = Tuple.sub (origin ry) (Sphere.origin sp) in
  let a = Tuple.dot (direction ry) (direction ry) in
  let b = Float.mul 2.0 (Tuple.dot (direction ry) sp_to_ry) in
  let c = Float.sub (Tuple.dot sp_to_ry sp_to_ry) 1.0 in
  let discriminant = Float.sub (Float.pow b 2.0) (Float.mul 4.0 (Float.mul a c)) in
  if discriminant < 0.0 then (Array.make 0 (int_init 0.0 (ref (Geometry.Formless))))
  else
    let sp_ref = ref (sp) in
    let _2a = Float.mul 2.0 a in
    let disc_sq = Float.sqrt discriminant in
    let t1 = Float.div (Float.sub (Float.neg b) disc_sq) _2a in
    let i1 = int_init t1 sp_ref in
    let t2 = Float.div (Float.add (Float.neg b) disc_sq) _2a in
    let i2 = int_init t2 sp_ref in
    [|i1; i2|]*)
let int_equal i1 i2 = f_equal (int_t_val i1) (int_t_val i2) &&
  (int_shape i1) == (int_shape i2)

let intersects (sp: 'a Geometry.shape) ry = let orig = match sp with
    | Geometry.Sphr s -> Sphere.origin s
    | Geometry.Formless -> Tuple.point (0.0, 0.0, 0.0)
  in
  let sp_to_ry = Tuple.sub (origin ry) orig in
  let a = Tuple.dot (direction ry) (direction ry) in
  let b = Float.mul 2.0 (Tuple.dot (direction ry) sp_to_ry) in
  let c = Float.sub (Tuple.dot sp_to_ry sp_to_ry) 1.0 in
  let discriminant = Float.sub (Float.pow b 2.0) (Float.mul 4.0 (Float.mul a c)) in
  if discriminant < 0.0 then (Array.make 0 (int_init 0.0 (ref (Geometry.Formless))))
  else
    let sp_ref = ref (sp) in
    let _2a = Float.mul 2.0 a in
    let disc_sq = Float.sqrt discriminant in
    let t1 = Float.div (Float.sub (Float.neg b) disc_sq) _2a in
    let i1 = int_init t1 sp_ref in
    let t2 = Float.div (Float.add (Float.neg b) disc_sq) _2a in
    let i2 = int_init t2 sp_ref in
    [|i1; i2|]

let%test "Scenario: A ray intersects a sphere at two points" = let r = init ~origin:(Tuple.point (0.0, 0.0, -5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let s = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let xs = intersects (Geometry.Sphr(s)) r in
  Int.equal (Array.length xs) 2 &&
  f_equal (int_t_val xs.(0)) 4.0 &&
  f_equal (int_t_val xs.(1)) 6.0

let%test "Scenario: A ray intersects a sphere at a tangent" = let r = init ~origin:(Tuple.point (0.0, 1.0, -5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let s = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let xs = intersects (Geometry.Sphr(s)) r in
  (* SHOULD STILL RETURN TWO, MAKE SURE OF THIS *)
  Int.equal (Array.length xs) 2 &&
  f_equal (int_t_val xs.(0)) 5.0 &&
  f_equal (int_t_val xs.(1)) 5.0

let%test "Scenario: A ray misses a sphere" = let r = init ~origin:(Tuple.point (0.0, 2.0, -5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let s = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let xs = intersects (Geometry.Sphr(s)) r in
  Int.equal (Array.length xs) 0 

let%test "Scenario: A ray originates inside a sphere" = let r = init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let s = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let xs = intersects (Geometry.Sphr(s)) r in
  Int.equal (Array.length xs) 2 &&
  f_equal (int_t_val xs.(0)) (-1.0) &&
  f_equal (int_t_val xs.(1)) 1.0

let%test "Scenario: A sphere is behind a ray" = let r = init ~origin:(Tuple.point (0.0, 0.0, 5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let s = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let xs = intersects (Geometry.Sphr(s)) r in
  Int.equal (Array.length xs) 2 &&
  f_equal (int_t_val xs.(0)) (-6.0) &&
  f_equal (int_t_val xs.(1)) (-4.0)

let%test "Scenario: An intersection encapsulates t and object" = let sp = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let sp = ref (Geometry.Sphr(sp)) in
  let it = int_init 3.5 sp in
  f_equal (int_t_val it) (3.5) &&
  sp == (int_shape it)

let intersections is = is
let%test "Scenario: Aggregating intersections" = let sp = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let sp = ref (Geometry.Sphr(sp)) in
  let it1 = int_init 1.0 sp in
  let it2 = int_init 2.0 sp in
  let xs = intersections [|it1; it2|] in
  Int.equal (Array.length xs) 2 &&
  f_equal (int_t_val xs.(0)) (1.0) &&
  f_equal (int_t_val xs.(1)) (2.0)

let%test "Scenario: Intersect sets the object on the intersection" = let sp = Sphere.init ~origin:(Tuple.point (0.0, 0.0, 0.0)) ~radius:1.0 in
  let sp = ref (Geometry.Sphr(sp)) in
  let it1 = int_init 1.0 sp in
  let it2 = int_init 2.0 sp in
  let xs = intersections [|it1; it2|] in
  Int.equal (Array.length xs) 2 &&
  int_shape xs.(0) == sp &&
  int_shape xs.(1) == sp

let hit int_arr = let () = Array.sort (fun a b -> Float.compare (int_t_val a) (int_t_val b)) int_arr in
  Array.find_opt (fun e -> int_t_val e > 0.0) int_arr

let%test "Scenario: The hit, when all intersections have positive t" = let sp = Sphere.init ~origin:Tuple.point_origin ~radius:1.0 in
  let sp = ref (Geometry.Sphr(sp)) in
  let i1 = int_init 1.0 sp in
  let i2 = int_init 2.0 sp in
  let xs = [|i1; i2|] in
  let i = Option.get (hit xs) in
  int_equal i i1

let%test "Scenario: The hit, when some intersections have negative t" = let sp = Sphere.init ~origin:Tuple.point_origin ~radius:1.0 in
  let sp = ref (Geometry.Sphr(sp)) in
  let i1 = int_init (-1.0) sp in
  let i2 = int_init 1.0 sp in
  let xs = [|i1; i2|] in
  let i = Option.get (hit xs) in
  int_equal i i2

let%test "Scenario: The hit, when all intersections have negative t" = let sp = Sphere.init ~origin:Tuple.point_origin ~radius:1.0 in
  let sp = ref (Geometry.Sphr(sp)) in
  let i1 = int_init (-2.0) sp in
  let i2 = int_init (-1.0) sp in
  let xs = [|i1; i2|] in
  let i = hit xs in
  Option.is_none i

let%test "Scenario: The hit is always the lowest nonnegative intersection" = let sp = Sphere.init ~origin:Tuple.point_origin ~radius:1.0 in
  let sp = ref (Geometry.Sphr(sp)) in
  let i1 = int_init 5.0 sp in
  let i2 = int_init 7.0 sp in
  let i3 = int_init (-3.0) sp in
  let i4 = int_init 2.0 sp in
  let xs = [|i1; i2; i3; i4|] in
  let i = Option.get (hit xs) in
  int_equal i i4
