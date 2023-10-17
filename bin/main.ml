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
let c = aux c p
let () = 
  let oc = open_out "canvas.ppm" in
  Printf.fprintf oc "%s" (Ppm.canvas_to_ppm ~canvas:c);
  close_out oc;

(*
let rec aux env = function
  | x when Projectile.y x > 0.0 -> print_float (Projectile.y x); print_newline (); aux env (Ray_lib.tick ~environment:env ~projectile:x)
  | _ -> print_endline "Hit the ground!";;
*)
(*aux e p*)
