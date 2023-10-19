let () = print_endline "Hello, World!";;

let p_pos_i = Tuple.point (0.0, 1.0, 0.0)
let p_vel_i = Tuple.mul (Tuple.norm (Tuple.vector (1.0, 1.8, 0.0))) 11.25
let p = Projectile.init ~position:p_pos_i ~velocity:p_vel_i

let e_g = Tuple.vector (0.0, -0.1, 0.0)
let e_w = Tuple.vector (-0.01, 0.0, 0.0)
let e = Environment.init ~gravity:e_g ~wind:e_w

let c = Canvas.init ~width:900 ~height:550

let rec aux canvas = function
  | projectile when (Projectile.p_y projectile) > 0.0 -> let x = Float.to_int (Projectile.p_x projectile) in
    let y = Int.sub (Canvas.height canvas) (Float.to_int (Projectile.p_y projectile)) in
    let () = Canvas.write_pixel canvas ~x_idx:x ~y_idx:y ~color:(Color.init (1.0, 1.0, 1.0)) in aux canvas (Ray_lib.tick ~environment:e ~projectile:projectile)
  | _ -> canvas;;
let save_canvas name c = 
  let filename = name ^ ".ppm" in
  let channel = open_out filename in
  Printf.fprintf channel "%s" (Ppm.canvas_to_ppm ~canvas:c);
  close_out channel;;

let c = aux c p in save_canvas "projectile" c
let clock = Canvas.init ~width:200 ~height:200

let analog_clock canv =
  let (x_mid, y_mid) = (Int.to_float (Canvas.width canv / 2), Int.to_float (Canvas.height canv / 2)) in
  let (_x_0, _y_0) = (x_mid, Float.add y_mid (Float.div y_mid 4.0)) in
  let origin = Tuple.point (0.0, 0.0, 0.0) in
  let v = ref (Tuple.vector (0.0, 1.0, 0.0)) in
  let rad_unit = Float.neg (Float.div Float.pi 6.0) in
  let tr_unit = Transformation.translation x_mid y_mid 0.0 in
  let rot_unit = Transformation.rotation 0.0 0.0 rad_unit in
  let scl_unit = Transformation.scaling 30.0 30.0 0.0 in 
  let unit_trans = Matrix.mul tr_unit scl_unit in
  let () = for i = 0 to 11 do
    let color = if (i mod 3) == 0 then Color.init (1.0, 0.0, 0.0) else Color.init (1.0, 1.0, 1.0) in 
    let v_curr = Tuple.add origin !v in
    let p_curr = Matrix.mul_tuple unit_trans v_curr in
    let () = print_endline (Tuple.to_string v_curr) in
    let () = print_endline (Tuple.to_string p_curr) in
    let (x_curr, y_curr) = (Tuple.x p_curr, Tuple.y p_curr) in
    let () = Canvas.write_pixel canv ~x_idx:(Float.to_int x_curr) ~y_idx:(Float.to_int y_curr)~color:color in
    (*let rad_z = Float.mul rad_unit (Int.to_float step) in*)
    (*let rot = Transformation.rotation 0.0 0.0 rad_z in*)
    (*let v_next = Matrix.mul_tuple rot v_curr in*)
    v := (Matrix.mul_tuple rot_unit !v)
  done in
  save_canvas "clock" canv;;
analog_clock clock
(*
let rec aux env = function
  | x when Projectile.y x > 0.0 -> print_float (Projectile.y x); print_newline (); aux env (Ray_lib.tick ~environment:env ~projectile:x)
  | _ -> print_endline "Hit the ground!";;
*)
(*aux e p*)
let _point_to_pixel (p:Tuple.t) = (Tuple.x p, Tuple.y p)
let _rays_from_view height_bound width_bound = let r_0 = Ray.init ~origin:Tuple.point_origin ~direction:(Tuple.vector(0.0, 0.0, 1.0)) in
  let r_arr = Array.make_matrix width_bound height_bound r_0 in
  let () = for w = 0 to width_bound - 1 do
    for h = 0 to height_bound - 1 do
      let (x_f, y_f) = (Int.to_float w, Int.to_float h) in
      let r_o = Tuple.point(x_f, y_f, 0.0) in
      let r_d = Tuple.vector(0.0, 0.0, 1.0) in
      let r = Ray.init ~origin:r_o ~direction:r_d in
      r_arr.(w).(h) <- r
    done
  done in
  r_arr;;
let () = let (w, h) = (100, 100) in
  let canv = Canvas.init ~width:w ~height:h in
  let col = Color.init(1.0, 0.0, 0.0) in
  let shape = Geometry.init_unit_sphere () in
  let ray_origin = Tuple.point(0.0, 0.0, -5.0) in
  let wall_z = 10.0 in
  let wall_size = 7.0 in
  let canvas_size = Int.to_float h in
  let pixel_size = Float.div wall_size canvas_size in
  let half = Float.div wall_size 2.0 in
  let () = for y = 0 to h - 1 do
    let world_y = Float.sub half (Float.mul pixel_size (Int.to_float y)) in
    for x = 0 to w - 1 do
      let world_x = Float.add (Float.neg half) (Float.mul pixel_size (Int.to_float x)) in
      let position = Tuple.point(world_x, world_y, wall_z) in
      let ray_direction = Tuple.sub position ray_origin in
      let r = Ray.init ~origin:ray_origin ~direction:ray_direction in
      let xs = Ray.check_intersection shape r in
      match Ray.hit xs with
        | Some(_) -> let () = print_int x in 
          let () = print_endline " hit" in Canvas.write_pixel canv ~x_idx:x ~y_idx:y ~color:col 
        | None -> print_endline "Miss"
    done
  done in 
  let () = save_canvas "sphere" canv in
  print_endline "Finished"
  (*let rays = rays_from_view h w in
  let sph = Geometry.init_unit_sphere () in
  let sph_t = Transformation.translation 0.0 0.0 2.5 in
  let () = Geometry.set_transform sph sph_t in
  let aux = Array.map (Ray.check_intersection sph) in
  let isects = Array.map aux rays in
  let aux = Array.map Ray.hit in
  let hits = Array.map aux isects in
  hits*)
  

(*let ray_to_pixel (r:Ray.t) = (Ray.)
let hit_to_pixel *)