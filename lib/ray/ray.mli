type t = {origin: Tuple.t; direction: Tuple.t}
val init: origin:Tuple.t -> direction:Tuple.t -> t
val origin: t -> Tuple.t
val direction: t -> Tuple.t
val position: t -> float -> Tuple.t
val _discriminant: Sphere.t -> t -> float
type 'a intersection = {t_val: float; int_obj: 'a Geometry.shape ref}
type 'a intersection_col = 'a intersection array
val int_init: float -> 'a Geometry.shape ref -> 'a intersection
val int_t_val: 'a intersection -> float
val int_shape: 'a intersection -> 'a Geometry.shape ref
val int_equal: 'a intersection -> 'a intersection -> bool
val intersects: 'a Geometry.shape -> t -> 'a intersection array
val intersections: 'a intersection array -> 'a intersection array
val hit: 'a intersection array -> 'a intersection option
