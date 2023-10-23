type 'a t = { objects: 'a Geometry.shape array; lights: 'a Light.t array}
val init: 'a Geometry.shape array -> 'a Light.t array -> 'a t
val def_obj1: unit -> 'a Geometry.shape
val def_obj2: unit -> 'a Geometry.shape
val init_def: unit -> 'a t
val objects: 'a t -> 'a Geometry.shape array
val lights: 'a t -> 'a Light.t array

val check_intersections: 'a t -> Ray.t -> 'a Ray.Intersection.t array