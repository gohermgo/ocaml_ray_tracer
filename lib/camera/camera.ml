type t = { hsize: int; vsize: int; fov: float; mutable transform: Matrix.t; mutable inverse_transform: Matrix.t; }
let hsize c = c.hsize
let vsize c = c.vsize
let fov c = c.fov
let transform c = c.transform
let set_transform c t = c.transform <- t; c.inverse_transform <- Matrix.invert t 
let inverse_transform c = c.inverse_transform

let half_view c = Float.tan (fov c /. 2.0)

let aspect_ratio c = (Float.of_int (hsize c) /. Float.of_int (vsize c))

let half_height c = 
  let aspect = aspect_ratio c 
  and half_view = half_view c in
  if aspect >= 1.0 then half_view /. aspect
  else half_view

let half_width c = 
  let aspect = aspect_ratio c
  and half_view = half_view c in
  if aspect >= 1.0 then half_view
  else half_view *. aspect

let pixel_size c = 
  (*let half_view = Float.tan (fov c /. 2.0) 
  and aspect = aspect_ratio c in*)
  (*let (half_width, _half_height) = if aspect >= 1.0 then 
    let w = half_view
    and h = (half_view /. aspect) in (w, h)
  else 
    let w = half_view *. aspect
    and h = half_view in (w, h)
  in*)
  (half_width c *. 2.0) /. Float.of_int (hsize c)
  (*let half_view = Float.tan (fov c /. 2.0)
  and aspect = aspect_ratio c in
  if aspect >= 1 then *)

let init hsize vsize fov = 
  let transform = Matrix.ident 4 in 
  { 
    hsize = hsize; vsize = vsize; 
    fov = fov; 
    transform = transform; inverse_transform = Matrix.invert transform; 
  }

let ray_for_pixel c x y =
  (* Offsets from canvas edge to pixel center *)
  let pixel_size = pixel_size c in
  let x_offset = ((Float.of_int x) +. 0.5) *. pixel_size
  and y_offset = ((Float.of_int y) +. 0.5) *. pixel_size in
  (* Camera looks toward -z, +x is left *)
  let world_x = (half_width c) -. x_offset
  and world_y = (half_height c) -. y_offset in
  (* Transform canvas point and origin, then compute ray *)
  let pixel = Matrix.mul_tuple (inverse_transform c) (Tuple.point (world_x, world_y, -1.0))
  and origin = Matrix.mul_tuple (inverse_transform c) (Tuple.point_origin) in
  let direction = Tuple.norm (Tuple.sub pixel origin) in
  Ray.init ~origin:origin ~direction:direction

(*
let ray_for_pixel_parallel pool c x y =
  let pixel_size = pixel_size c in
  let x_offset = ((Float.of_int x) +. 0.5) *. pixel_size
  and y_offset = ((Float.of_int y) +. 0.5) *. pixel_size in
  (* Camera looks toward -z, +x is left *)
  let world_x = (half_width c) -. x_offset
  and world_y = (half_height c) -. y_offset in
  (* Transform canvas point and origin, then compute ray *)
  let pixel = Matrix.mul_tuple (inverse_transform c) (Tuple.point (world_x, world_y, -1.0))
  and origin = Matrix.mul_tuple (inverse_transform c) (Tuple.point_origin) in
  let direction = Tuple.norm (Tuple.sub pixel origin) in
  Ray.init ~origin:origin ~direction:direction
*)

module T = Domainslib.Task

let parallel_render_horizontal pool y bound camera image world =
  print_endline "parallel_render_horizontal->";
  (*
  T.async pool (fun _ ->
    for x = 0 to bound do
      let ray = ray_for_pixel camera x y in
      let c = World.color_at world ray in
      Canvas.write_pixel image ~x_idx:x ~y_idx:y ~color:c;
    done;
  ) 
  *)
  T.parallel_for pool ~start:0 ~finish:bound ~body:(fun x ->
    let ray = ray_for_pixel camera x y in
    let c = T.run pool (fun () -> World.color_at_parallel pool world ray) in
    (*
    let c = T.run pool (fun () -> World.color_at world ray) in
    *)
    Canvas.write_pixel image ~x_idx:x ~y_idx:y ~color:c
  );
  ()

let render_horizontal_pooled y bound camera image world =
  print_endline "render_horizontal_pooled";
  (* let sub_pool = Option.get (T.lookup_pool "world") in *)
  for x = 0 to bound do
    let ray = ray_for_pixel camera x y in
    (* let c = World.color_at_pooled pool world ray in *)
    (* let c = T.run pool (fun () -> World.color_at_pooled pool world ray) in *)
    let c = World.color_at world ray in
    Canvas.write_pixel image ~x_idx:x ~y_idx:y ~color:c
  done

let _render_horizontal y bound camera image world = 
  for x = 0 to bound do
    let ray = ray_for_pixel camera x y in
    let c = World.color_at world ray in
    Canvas.write_pixel image ~x_idx:x ~y_idx:y ~color:c;
  done

let render_pooled c w =
  print_endline "render_pooled";
  let (hsize, vsize) = (hsize c, vsize c) in
  let image = Canvas.init ~width:hsize ~height:vsize in
  (*let main_pool = T.setup_pool ~num_domains:4 () in*)
  (*
  for y = 0 to vsize - 1 do
    render_horizontal_pooled pool y (hsize - 1) c image w
  done;
  *)
  (* let pool = Option.get (T.lookup_pool "render") in *)
  for y = 0 to vsize - 1 do
    render_horizontal_pooled y (hsize - 1) c image w
    (* T.run pool (fun () -> render_horizontal_pooled y (hsize - 1) c image w) *)
  done;
  print_newline ();
  image

let render_parallel c w =
  print_endline "render_parallel";
  let (hsize, vsize) = (hsize c, vsize c) in
  let image = Canvas.init ~width:hsize ~height:vsize in
  (*let main_pool = T.setup_pool ~num_domains:4 () in*)
  let pool = match T.lookup_pool "render" with
    | Some v -> v
    | _ -> raise (invalid_arg "no pool found in render_parallel") in
  (* let pool = Option.get (T.lookup_pool "render") in *)
  T.parallel_for pool ~start:0 ~finish:(vsize - 1) ~body:(fun y -> 
    (* T.run pool (fun () -> render_horizontal_pooled y (hsize - 1) c image w); *)
    render_horizontal_pooled y (hsize - 1) c image w;
  );
  print_newline ();
  image

let render_in_pool (pool: T.pool) (c: t) (w: 'a World.t) : Canvas.t =
  print_endline "render in pool";
  let (hsize, vsize) = (hsize c, vsize c) in
  let image = Canvas.init ~width:hsize ~height:vsize in
  T.parallel_for pool ~start:0 ~finish:(vsize - 1) ~body:(fun y ->
    render_horizontal_pooled y (hsize - 1) c image w;
  );
  print_endline "render in pool finished";
  image

let render pool c w =
  print_string "rendering->";
  let (hsize, vsize) = (hsize c, vsize c) in
  let image = Canvas.init ~width:hsize ~height:vsize in
  (*let main_pool = T.setup_pool ~num_domains:4 () in*)
  T.parallel_for pool ~start:0 ~finish:(vsize - 1) ~body:(fun y -> 
    T.run pool (fun () -> parallel_render_horizontal pool y (hsize - 1) c image w);
  );
  print_newline ();
  (*
  for y = 0 to (vsize - 1) do
    (*let pool = T.setup_pool ~num_domains:0 () in*)
    let promise = parallel_render_horizontal main_pool y (hsize - 1) c image w in
    T.await main_pool promise;
  done;
  *)
  
  (*let fun_y = (fun y -> for x = 0 to hsize - 1 do
    let ray = ray_for_pixel c x y in
    Canvas.write_pixel image ~x_idx:x ~y_idx:y ~color:(World.color_at w ray) 
    done) in
  T.parallel_for ~start:0 ~finish:(vsize - 1) ~body:fun_y pool;*)

  (*
  T.parallel_for ~start:0 ~finish:(vsize - 1) ~body:(fun y ->
    let sub_pool = T.setup_pool ~num_domains:2 () in
    parallel_render_horizontal sub_pool y (hsize - 1) c image w;
    T.teardown_pool sub_pool;
  ) main_pool;
  *)


  (*T.teardown_pool main_pool;*)
  (*for y = 0 to vsize - 1 do
    for x = 0 to hsize - 1 do
      let ray = ray_for_pixel c x y in
      Canvas.write_pixel image ~x_idx:x ~y_idx:y ~color:(World.color_at w ray)
    done;
  done;*)
  image
