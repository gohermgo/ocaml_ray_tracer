let abs_diff x y = Float.abs (Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

let%test "A sphere's default transformation" = let s = Geometry.init_unit_sphere () in
  Matrix.equal !(Geometry.get_transform s) (Matrix.ident 4)

let%test "Changing a sphere's transformation" = let s = Geometry.init_unit_sphere () in
  let t = Transformation.translation 2.0 3.0 4.0 in
  let () = Geometry.set_transform s t in
  Matrix.equal !(Geometry.get_transform s) t 

(* Improved intersection tests, involving inverse transform of ray *)
let%test "Intersecting a scaled sphere with a ray" = let r = Ray.init ~origin:(Tuple.point(0.0, 0.0, -5.0)) ~direction:(Tuple.vector(0.0, 0.0, 1.0)) in
  let s = Geometry.init_unit_sphere () in
  let () = Geometry.set_transform s (Transformation.scaling 2.0 2.0 2.0) in
  let xs = Ray.check_intersection s r in
  Int.equal (Array.length xs) 2 &&
  f_equal (Ray.Intersection.t_value xs.(0)) 3.0 &&  
  f_equal (Ray.Intersection.t_value xs.(1)) 7.0

let%test "Intersecting a translated sphere with a ray" = let r = Ray.init ~origin:(Tuple.point(0.0, 0.0, -5.0)) ~direction:(Tuple.vector(0.0, 0.0, 1.0)) in
  let s = Geometry.init_unit_sphere () in
  let () = Geometry.set_transform s (Transformation.translation 5.0 0.0 0.0) in
  let xs = Ray.check_intersection s r in
  Int.equal (Array.length xs) 0

(* Normal vector tests *)
let%test "The normal on a sphere at a point on the x axis" = let s = Geometry.init_unit_sphere () in
  let n = Geometry.normal_at s (Tuple.point (1.0, 0.0, 0.0)) in
  Tuple.equal n (Tuple.vector (1.0, 0.0, 0.0))
  
let%test "The normal on a sphere at a point on the y axis" = let s = Geometry.init_unit_sphere () in
  let n = Geometry.normal_at s (Tuple.point (0.0, 1.0, 0.0)) in
  Tuple.equal n (Tuple.vector (0.0, 1.0, 0.0))

let%test "The normal on a sphere at a point on the z axis" = let s = Geometry.init_unit_sphere () in
  let n = Geometry.normal_at s (Tuple.point (0.0, 0.0, 1.0)) in
  Tuple.equal n (Tuple.vector (0.0, 0.0, 1.0))

let (_test_point, _test_vector) = let vl = Float.div (Float.sqrt 3.0) 3.0 in 
  (Tuple.point(vl, vl, vl), Tuple.vector(vl, vl, vl))

let%test "The normal on a sphere at a nonaxial point" = let s = Geometry.init_unit_sphere () in
  let n = Geometry.normal_at s _test_point in
  Tuple.equal n (_test_vector)

let%test "The normal on a sphere at a nonaxial point" = let s = Geometry.init_unit_sphere () in
  let n = Geometry.normal_at s _test_point in
  Tuple.equal n (Tuple.norm n)

(* Normal vector for translated geometry tests *)
let%test "Computing the normal on a translated sphere" = let s = Geometry.init_unit_sphere () in
  let t = Transformation.translation 0.0 1.0 0.0 in
  let () = Geometry.set_transform s t in
  let n = Geometry.normal_at s (Tuple.point (0.0, 1.70711, -0.70711)) in
  Tuple.equal n (Tuple.vector (0.0, 0.70711, -0.70711))

let _test_value = Float.div (Float.sqrt 2.0) 2.0

let%test "Computing the normal on a transformed sphere" = let s = Geometry.init_unit_sphere () in
  let sc = Transformation.scaling 1.0 0.5 1.0 in
  let r = Transformation.rotation 0.0 0.0 (Float.div Float.pi 5.0) in
  let m = Matrix.mul sc r in
  let () = Geometry.set_transform s m in
  let n = Geometry.normal_at s (Tuple.point (0.0, _test_value, Float.neg _test_value)) in
  Tuple.equal n (Tuple.vector (0.0, 0.97014, -0.24254))

let%test "A sphere has a default material" = let s = Geometry.init_unit_sphere () in
  Material.equal !(Geometry.get_material s) (Material.init_def ())

let%test "A sphere may be assigned a material" = let s = Geometry.init_unit_sphere () in
  let m = Material.init_def() in
  let () = Material.set_ambient m 1.0 in
  let () = Geometry.set_material s m in
  Material.equal !(Geometry.get_material s) m

