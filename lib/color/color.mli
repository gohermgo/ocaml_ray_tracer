type t' = {mutable r: float; mutable g: float; mutable b: float}

val r': t' -> float
val g': t' -> float
val b': t' -> float

val equal': t' -> t' -> bool

val init': r:float -> g:float -> b:float -> t'
val of_array': float array -> t'

val add': t' -> t' -> t'
val sub': t' -> t' -> t'
val mul_scalar': t' -> float -> t'
val mul': t' -> t' -> t'

type t = Tuple.t

type c =
  | C: t -> c
  | C': t' -> c

val r: t -> float
val g: t -> float
val b: t -> float
val a: t -> float

val equal_: t -> t -> bool

val equal: c -> c -> bool

val init: float * float * float -> t
val of_array: float array -> t

val add_: t -> t -> t
val sub_: t -> t -> t
val mul_scalar_: t -> float -> t
val mul_: t -> t -> t

val add: c -> c -> c
val sub: c -> c -> c
val mul_scalar: c -> float -> c
val mul: c -> c -> c
