type t = {origin: Tuple.t; radius: float; transform: Matrix.t ref}
let origin s = s.origin
let radius s = s.radius
let get_transform s = s.transform
let set_transform s t = s.transform := t
let _sphere_resolution = 0.001
(* Simple implementation for now TODO: implement some form of sphere equation *)
let init ~origin ~radius = {origin = origin; radius = radius; transform = ref (Matrix.ident 4)}
