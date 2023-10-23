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

let check_intersections world ray =
  (*let os = objects world in*)
  let a = Array.fold_left (fun acc e -> Array.append acc (Ray.check_intersection e ray)) [||] (objects world) in
  Array.sort Ray.Intersection.compare a;
  a
    