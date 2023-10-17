type t = { gravity: Tuple.t; wind: Tuple.t }
let init ~gravity:g ~wind:w = { gravity = g; wind = w }
let gravity environment = environment.gravity
let wind environment = environment.wind