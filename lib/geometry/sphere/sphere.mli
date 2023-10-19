type t = {origin: Tuple.t; radius: float; transform: Matrix.t ref}
val origin: t -> Tuple.t
val radius: t -> float
val get_transform: t -> Matrix.t ref
val set_transform: t -> Matrix.t -> unit
val init: origin:Tuple.t -> radius:float -> t
