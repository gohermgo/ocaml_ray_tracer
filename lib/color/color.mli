type t = Tuple.t

val r: t -> float
val g: t -> float
val b: t -> float
val a: t -> float

val init: float * float * float -> t

val add: t -> t -> t

val sub: t -> t -> t

val mul_scalar: t -> float -> t

val mul: t -> t -> t