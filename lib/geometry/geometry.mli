type _ shape =
  | Spherical : Sphere.t -> 'a shape
  | Formless
val init_sphere: o:Tuple.t -> r:float -> 'a shape
val init_unit_sphere: unit -> 'a shape
val centroid: s:'a shape -> Tuple.t
val get_transform: 'a shape -> Matrix.t ref
val set_transform: 'a shape -> Matrix.t -> unit
val get_material: 'a shape -> Material.t ref
val set_material: 'a shape -> Material.t -> unit
val normal_at: 'a shape -> Tuple.t -> Tuple.t
(*type shape = Sphere of Sphere.t*)