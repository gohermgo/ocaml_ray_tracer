type t = { position: Tuple.t; velocity: Tuple.t }

val p_x: t -> float
val p_y: t -> float
val p_z: t -> float
val p_w: t -> float

val v_x: t -> float
val v_y: t -> float
val v_z: t -> float
val v_w: t -> float

val to_string: t -> string

val init: position:Tuple.t -> velocity:Tuple.t -> t
val position: t -> Tuple.t
val velocity: t -> Tuple.t