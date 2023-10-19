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