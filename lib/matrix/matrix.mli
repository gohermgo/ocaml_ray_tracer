type t = float array array
val init: int -> int -> t
val equal: t -> t -> bool
val ident: int -> t
val print: t -> unit
val get_col: t -> int -> float array
val mul: t -> t -> t option
val mul_tuple: t -> Tuple.t -> Tuple.t option
val transpose: t -> t