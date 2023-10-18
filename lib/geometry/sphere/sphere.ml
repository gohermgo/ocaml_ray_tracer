type t = {origin: Tuple.t; radius: float}
let origin s = s.origin
let radius s = s.radius
let _sphere_resolution = 0.001
(* Simple implementation for now TODO: implement some form of sphere equation *)
let init ~origin ~radius = {origin = origin; radius = radius}
