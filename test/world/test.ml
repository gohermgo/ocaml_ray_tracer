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

let%test "Shading an intersection" =
  let w = World.init_def ()
  and r = Ray.init ~origin:(Tuple.point (0.0, 0.0, -5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let s = ref (World.objects w).(0) in
  let i = Ray.Intersection.init ~t:4.0 ~o:s in
  let cs = Ray.precompute i r in
  let colr = World.shade_hit w cs in
  Color.equal_ colr (Color.init(0.38066, 0.47583, 0.2855))

let%test "Shading an intersection from the inside" =
  let w = World.init_def ()
  and light_pos = Tuple.point (0.0, 0.25, 0.0)
  and light_color = Color.init(1.0, 1.0, 1.0) in
  World.set_light w 0 (Light.init_point light_pos light_color);
  let r = Ray.init ~origin:(Tuple.point (0.0, 0.25, 0.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let s = ref (World.objects w).(1) in
  let i = Ray.Intersection.init ~t:0.5 ~o:s in
  let cs = Ray.precompute i r in
  let colr = World.shade_hit w cs in
  Color.equal_ colr (Color.init(0.90498, 0.90498, 0.90498))
  
let%test "The color when a ray misses" = 
  let w = World.init_def ()
  and r = Ray.init ~origin:(Tuple.point (0.0, 0.0, -5.0)) ~direction:(Tuple.vector (0.0, 1.0, 0.0)) in
  let c = World.color_at w r in
  (*print_float (Color.r c);
  print_newline ();
  print_float (Color.g c);
  print_newline ();
  print_float (Color.b c);
  print_newline ();*)
  Color.equal_ c (Color.init(0.0, 0.0, 0.0))

let%test "The color when a ray hits" = 
  let w = World.init_def ()
  and r = Ray.init ~origin:(Tuple.point (0.0, 0.0, -5.0)) ~direction:(Tuple.vector (0.0, 0.0, 1.0)) in
  let c = World.color_at w r in
  (*print_float (Color.r c);
  print_newline ();
  print_float (Color.g c);
  print_newline ();
  print_float (Color.b c);
  print_newline ();*)
  Color.equal_ c (Color.init(0.38066, 0.47583, 0.2855))

let%test "The color with an intersection behind the ray" = 
  let w = World.init_def ()
  and r = Ray.init ~origin:(Tuple.point (0.0, 0.0, 0.75)) ~direction:(Tuple.vector (0.0, 0.0, -1.0)) in
  let outer = (World.objects w).(0)
  and inner = (World.objects w).(1) in
  (*print_string "Pre - Ambient ";
  print_float (Material.ambient !(Geometry.get_material inner));
  print_newline();*)
  Material.set_ambient !(Geometry.get_material outer) 1.0;
  Material.set_ambient !(Geometry.get_material inner) 1.0;
  let c = World.color_at w r in
  (*print_float (Color.r c);
  print_newline ();
  print_float (Color.g c);
  print_newline ();
  print_float (Color.b c);
  print_newline ();
  print_float (Color.r (Material.color !(Geometry.get_material inner)));
  print_newline ();
  print_float (Color.g (Material.color !(Geometry.get_material inner)));
  print_newline ();
  print_float (Color.b (Material.color !(Geometry.get_material inner)));
  print_newline ();
  print_string "Ambient ";
  print_float (Material.ambient !(Geometry.get_material inner));
  print_newline();*)
  Color.equal_ c (Material.color !(Geometry.get_material inner))
 (* let s = ref (World.objects w).(0) in
  let i = Ray.Intersection.init ~t:4.0 ~o:s in
  let cs = Ray.precompute i r in
  let colr = World.shade_hit w cs in
  Color.equal colr (Color.init(0.38066, 0.47583, 0.2855))*)

