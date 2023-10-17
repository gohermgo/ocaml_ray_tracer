let tick ~environment:env ~projectile:proj = let old_pos = Projectile.position proj in
  let old_vel = Projectile.velocity proj in
  let new_pos = Tuple.add old_pos old_vel in
  let grav = Environment.gravity env in
  let wind = Environment.wind env in
  let env_vel = Tuple.add grav wind in
  let new_vel = Tuple.add old_vel env_vel in
  Projectile.init ~position:new_pos ~velocity:new_vel

