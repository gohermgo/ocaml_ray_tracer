let%test "Creating a world" =
  let w = World.init [||] [||] in
  World.objects w == [||] &&
  World.lights w == [||]

let%test "The default world" =
  let w = World.init_def () in
  let light = Light.init_point (Tuple.point (-10.0, -10.0, -10.0)) (Color.init (1.0, 1.0, 1.0))

  and os = World.objects w 
  and ls = World.lights w in
  (* For mutable datatypes single = means physical equality, not pointing to the same reference *)
  os.(0) = World.def_obj1 () &&
  os.(1) = World.def_obj2 () &&
  ls.(0) = light 

let abs_diff x y = Float.abs(Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

let%test "Intersect a world with a ray" =
  let w = World.init_def ()
  and r = Ray.init ~origin:(Tuple.point (0.0, 0.0, -5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let xs = World.check_intersections w r in
  Array.length xs == 4 &&
  f_equal (Ray.Intersection.t_value xs.(0)) 4.0 &&
  f_equal (Ray.Intersection.t_value xs.(1)) 4.5 &&
  f_equal (Ray.Intersection.t_value xs.(2)) 5.5 &&
  f_equal (Ray.Intersection.t_value xs.(3)) 6.0 
