let () = print_endline "Hello, World!";;

let p_pos_i = Tuple.point (0.0, 1.0, 0.0)
let p_vel_i = Tuple.norm (Tuple.vector (1.0, 1.0, 0.0))
let p = Projectile.init ~position:p_pos_i ~velocity:p_vel_i
let e_g = Tuple.vector (0.0, -0.1, 0.0)
let e_w = Tuple.vector (-0.01, 0.0, 0.0)
let e = Environment.init ~gravity:e_g ~wind:e_w

let rec aux = function
  | projectile when (Projectile.p_y projectile) > 0.0 -> print_endline (Projectile.to_string projectile); aux (Ray_lib.tick ~environment:e ~projectile:projectile)
  | _ -> print_string "Hit the ground!";;
aux p
(*
let rec aux env = function
  | x when Projectile.y x > 0.0 -> print_float (Projectile.y x); print_newline (); aux env (Ray_lib.tick ~environment:env ~projectile:x)
  | _ -> print_endline "Hit the ground!";;
*)
(*aux e p*)
