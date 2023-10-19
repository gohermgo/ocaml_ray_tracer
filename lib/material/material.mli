type t = {
  mutable color: Color.t; 
  mutable ambient: float; 
  mutable diffuse: float; 
  mutable specular: float; 
  mutable shininess: float
}

val color: t -> Color.t
val set_color: t -> Color.t -> unit

val ambient: t -> float
val set_ambient: t -> float -> unit

val diffuse: t -> float
val set_diffuse: t -> float -> unit

val specular: t -> float
val set_specular: t -> float -> unit

val shininess: t -> float
val set_shininess: t -> float -> unit

val equal: t -> t -> bool
val init_def: unit -> t
