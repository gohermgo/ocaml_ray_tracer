type t = {origin: Tuple.t; radius: float; transform: Matrix.t ref; material: Material.t ref}
val radius: t -> float
val get_transform: t -> Matrix.t ref
val set_transform: t -> Matrix.t -> unit
val get_material: t -> Material.t ref
val set_material: t -> Material.t -> unit
val origin: t -> Tuple.t
val init: origin:Tuple.t -> radius:float -> t
val normal_at: t -> Tuple.t -> Tuple.t
