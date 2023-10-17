type t = { position: Tuple.t; velocity: Tuple.t }

let p_x projectile = Tuple.x projectile.position
let p_y projectile = Tuple.y projectile.position
let p_z projectile = Tuple.z projectile.position
let p_w projectile = Tuple.w projectile.position
let v_x projectile = Tuple.x projectile.velocity
let v_y projectile = Tuple.y projectile.velocity
let v_z projectile = Tuple.z projectile.velocity
let v_w projectile = Tuple.w projectile.velocity

let to_string projectile = "Position:\n\t" ^ Tuple.to_string (projectile.position) ^
  "\nVelocity:\n\t" ^ Tuple.to_string (projectile.velocity)

let init ~position:p ~velocity:v = { position = p; velocity = v }

let position projectile = projectile.position

let velocity projectile = projectile.velocity

