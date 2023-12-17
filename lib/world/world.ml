type 'a t = { objects: 'a Geometry.shape array; lights: 'a Light.t array}
let init objects lights = {objects = objects; lights = lights}

let def_obj1_mat () =
  let m = Material.init_def () in
  let c = Color.init (0.8, 1.0, 0.6) in Material.set_color m c;
  Material.set_diffuse m 0.7;
  Material.set_specular m 0.2;
  m
  
let def_obj1 () = 
  let s = Geometry.init_unit_sphere () in
  Geometry.set_material s (def_obj1_mat ());
  s

let def_obj2 () =
  let s = Geometry.init_unit_sphere ()
  and t = Transformation.scaling 0.5 0.5 0.5 in
  Geometry.set_transform s t;
  s
  
let init_def () = 
  let light = Light.init_point (Tuple.point (-10.0, -10.0, -10.0)) (Color.init (1.0, 1.0, 1.0)) in

  {objects = [|def_obj1 (); def_obj2 ()|]; lights = [|light|]}
  
let objects world = world.objects
let lights world = world.lights
let set_light world index light = world.lights.(index) <- light

let check_intersections world ray =
  (*let os = objects world in*)
  let a = Array.fold_left (fun acc e -> Array.append acc (Ray.check_intersection e ray)) [||] (objects world) in
  Array.sort Ray.Intersection.compare a;
  a

let check_intersections_pooled world ray =
  let a = Array.fold_left (fun acc e -> Array.append acc (Ray.check_intersection_pooled e ray)) [||] (objects world) in
  Array.sort Ray.Intersection.compare a;
  a

let check_intersections_parallel (pool: Domainslib.Task.pool) world ray =
  let a = Array.fold_left (fun acc e -> Array.append acc (Domainslib.Task.run pool (fun () -> Ray.check_intersection_parallel pool e ray))) [||] (objects world) in
  Array.sort Ray.Intersection.compare a;
  a
    
let shade_hit world comps =
  let hit_point = Ray.Comps.point comps
  and hit_eyev = Ray.Comps.eyev comps
  and hit_normalv = Ray.Comps.normalv comps
  and hit_object = Ray.Comps.object_pointer comps in
  let hit_material = Geometry.get_material !hit_object in
  Array.fold_left (fun color_acc light -> Color.add color_acc (Light.lighting !hit_material light hit_point hit_eyev hit_normalv)) (Color.init (0.0, 0.0, 0.0)) (lights world)
let color_at world ray =
  (*let is = check_intersections world ray in*)
  (*print_string "Intersection count ";
  print_int (Array.length is);
  print_newline ();
  Array.iter (fun i -> let comp = Ray.precompute i ray in
    let c = shade_hit world comp in
    print_endline "Color in map";
    print_float (Color.r c);
    print_newline ();
    print_float (Color.g c);
    print_newline ();
    print_float (Color.b c);
    print_newline ();
    print_newline ();
  ) is;*)
  match Ray.hit (check_intersections world ray) with
    | Some(hit) -> shade_hit world (Ray.precompute hit ray)
    | None -> Color.init (0.0, 0.0, 0.0)
  (* Seems the algorithm just takes the first hit???? *)
  (*if Array.length is > 0 then let c = shade_hit world (Ray.precompute is.(0) ray) in
    print_endline "Color on computation";
    print_float (Color.r c);
    print_newline ();
    print_float (Color.g c);
    print_newline ();
    print_float (Color.b c);
    print_newline ();
    print_newline ();
    c
  else Color.init (0.0, 0.0, 0.0)*)
  (*let shaded_hits = Array.map (fun i -> let c = shade_hit world (Ray.precompute i ray) in
    print_endline "Color in map";
  print_float (Color.r c);
  print_newline ();
  print_float (Color.g c);
  print_newline ();
  print_float (Color.b c);
  print_newline ();
    c
    ) is in
  Array.fold_left (fun acc col -> Color.add acc col) (Color.init (0.0, 0.0, 0.0)) shaded_hits *)
  (*let cs = Array.map (fun i -> Ray.precompute i ray) is in
  let fold_fun = fun color_acc c -> Color.add color_acc (shade_hit world c) in
  Array.fold_left fold_fun (Color.init (0.0, 0.0, 0.0)) cs*)
(*Array.map (fun o -> Ray.check_intersection 
  let comps = Ray.precompute*)
let color_at_pooled world ray =
  let intersections = check_intersections_pooled world ray in 
  (* let intersections = check_intersections_pooled pool world ray in *)
  match Ray.hit intersections with
    | Some(hit) -> shade_hit world (Ray.precompute hit ray)
    | None -> Color.init (0.0, 0.0, 0.0)

let color_at_parallel (pool: Domainslib.Task.pool) world ray =
  let intersections = Domainslib.Task.run pool (fun () -> check_intersections_parallel pool world ray) in
  match Ray.hit intersections with
    | Some(hit) -> shade_hit world (Ray.precompute hit ray)
    | None -> Color.init (0.0, 0.0, 0.0)
