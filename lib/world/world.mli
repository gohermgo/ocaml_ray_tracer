type 'a t = { objects: 'a Geometry.shape array; lights: 'a Light.t array}
val init: 'a Geometry.shape array -> 'a Light.t array -> 'a t
val def_obj1: unit -> 'a Geometry.shape
val def_obj2: unit -> 'a Geometry.shape
val init_def: unit -> 'a t
val objects: 'a t -> 'a Geometry.shape array
val lights: 'a t -> 'a Light.t array
val set_light: 'a t -> int -> 'a Light.t -> unit

val check_intersections: 'a t -> Ray.t -> 'a Ray.Intersection.t array

val shade_hit: 'a t -> 'a Ray.Comps.t -> Color.t

val color_at: 'a t -> Ray.t -> Color.t