type t = {origin: Tuple.t; direction: Tuple.t}
val init: origin:Tuple.t -> direction:Tuple.t -> t
val origin: t -> Tuple.t
val direction: t -> Tuple.t
val position: t -> float -> Tuple.t
val _discriminant: Sphere.t -> t -> float
module Intersection : sig
  type 'a t = {t_value: float; o: 'a Geometry.shape ref}
  val init: t:float -> o:'a Geometry.shape ref -> 'a t
  val t_value: 'a t -> float
  val object_pointer: 'a t -> 'a Geometry.shape ref
  val equal: 'a t -> 'a t -> bool
  val compare: 'a t -> 'a t -> int
end
val transform: t -> Matrix.t -> t
val check_intersection: 'a Geometry.shape -> t -> 'a Intersection.t array
(*type 'a intersection = {t_val: float; int_obj: 'a Geometry.shape ref}
type 'a intersection_col = 'a intersection array
val int_init: float -> 'a Geometry.shape ref -> 'a intersection
val int_t_val: 'a intersection -> float
val int_shape: 'a intersection -> 'a Geometry.shape ref*)
(*val intersection_equal: 'a intersection -> 'a intersection -> bool*)
(*val init_intersection_arr: 'a Geometry.shape -> t -> 'a intersection array
val intersections: 'a intersection array -> 'a intersection array*)
val hit: 'a Intersection.t array -> 'a Intersection.t option
