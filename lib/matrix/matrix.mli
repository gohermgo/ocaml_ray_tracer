type t = float array array
val init: int -> int -> t
val print: t -> unit
val equal: t -> t -> bool
val get_col: t -> int -> float array
val mul: t -> t -> t option
val mul_tuple: Tuple.t -> t -> Tuple.t option