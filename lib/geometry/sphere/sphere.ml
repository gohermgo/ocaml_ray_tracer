type t = {origin: Tuple.t; radius: float; transform: Matrix.t ref; material: Material.t ref}
let radius s = s.radius
let get_transform s = s.transform
let set_transform s t = s.transform := t
let get_material s = s.material
let set_material s m = s.material := m
let origin s = s.origin
  (*Matrix.mul_tuple !(get_transform s) s.origin OLD IMPLEMENTATION HERE FOR REFERENCE*)
let _sphere_resolution = 0.001
(* Simple implementation for now TODO: implement some form of sphere equation *)
let init ~origin ~radius = {origin = origin; radius = radius; transform = ref (Matrix.ident 4); material = ref (Material.init_def ())}

let normal_at s p = let m_i = Matrix.invert !(get_transform s) in
  let object_p = Matrix.mul_tuple m_i p in
  let object_n = Tuple.sub object_p (origin s) in
  let m_it = Matrix.transpose m_i in
  let world_n = Matrix.mul_tuple m_it object_n in
  let world_n = Tuple.vector (Tuple.x world_n, Tuple.y world_n, Tuple.z world_n) in
  Tuple.norm( world_n )
