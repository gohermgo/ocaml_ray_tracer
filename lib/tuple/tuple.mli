type t = float array
(*type t = { x: float; y: float; z: float; w: float }*)

val x: t -> float
val y: t -> float
val z: t -> float
val w: t -> float

val to_string: t -> string

val equal: t -> t -> bool

val init: (float * float * float * float) -> t
val of_array: float array -> t

val point: (float * float * float) -> t
val point_origin: t
val xp: unit -> t
val yp: unit -> t
val zp: unit -> t

val is_point: t -> bool

val vector: (float * float * float) -> t
val xv: unit -> t
val yv: unit -> t
val zv: unit -> t

val is_vector: t -> bool

val sum_of: t -> float

val squared_sum_of: t -> float

val add: t -> t -> t

(* Per element difference *)
val sub: t -> t -> t

val neg: t -> t

val mul: t -> float -> t

val div: t -> float -> t

val mag: t -> float

val norm: t -> t

val dot: t -> t -> float

val cross: t -> t -> t

val reflect: t -> t -> t