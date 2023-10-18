type t = float array array
val f_equal: float -> float -> bool
val init: int -> int -> t
val dim_of: t -> (int * int)
val equal: t -> t -> bool
val ident: int -> t
val print: t -> unit
val get_col: t -> int -> float array
val mul: t -> t -> t
val mul_tuple: t -> Tuple.t -> Tuple.t
val transpose: t -> t
val submat: t -> int -> int -> t
(* Depends on minor_3, not in interface *)
val minor: t -> int -> int -> float
(* Depends on cofactor_3, not in interface *)
val cofactor: t -> int -> int -> float
(* Depends on det_2, det_3, minor_3, not in interface *)
val det: t -> float
val is_invertible: t -> bool
val invert: t -> t