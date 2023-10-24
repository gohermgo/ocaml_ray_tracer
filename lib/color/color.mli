type t = Tuple.t

val r: t -> float
val g: t -> float
val b: t -> float
val a: t -> float

val equal: t -> t -> bool

val init: float * float * float -> t
val of_array: float array -> t

val add: t -> t -> t

val sub: t -> t -> t

val mul_scalar: t -> float -> t

val mul: t -> t -> t