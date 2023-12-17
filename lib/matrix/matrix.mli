type t = float array array
val f_equal: float -> float -> bool
val init: int -> int -> t
val dim_of: t -> (int * int)

val row_equal: float array -> float array -> bool
val equal: t -> t -> bool

val ident: int -> t

val print: t -> unit
val get_col: t -> int -> float array

val mul: t -> t -> t
val mul_parallel: Domainslib.Task.pool -> t -> t -> t

val mul_tuple: t -> Tuple.t -> Tuple.t
val transpose: t -> t

val submat: t -> int -> int -> t
val submat_parallel: Domainslib.Task.pool -> t -> omit_row:int -> omit_col:int -> t

(* Depends on minor_3, not in interface *)
val minor: t -> int -> int -> float
val minor_parallel: Domainslib.Task.pool -> t -> row:int -> col:int -> float
(* Depends on cofactor_3, not in interface *)
val cofactor: t -> int -> int -> float
val cofactor_parallel: Domainslib.Task.pool -> t -> row:int -> col:int -> float
(* Depends on det_2, det_3, minor_3, not in interface *)
val det: t -> float
val det_parallel: Domainslib.Task.pool -> t -> float

val is_invertible: t -> bool
val is_invertible_parallel: Domainslib.Task.pool -> t -> bool

val invert: t -> t
val invert_parallel: t -> t
