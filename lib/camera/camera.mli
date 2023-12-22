type t = { hsize: int; vsize: int; fov: float; mutable transform: Matrix.t; mutable inverse_transform: Matrix.t; }
val hsize: t -> int
val vsize: t -> int
val fov: t -> float
val transform: t -> Matrix.t
val set_transform: t -> Matrix.t -> unit
val inverse_transform: t -> Matrix.t

val half_width: t -> float
val half_height: t -> float

val aspect_ratio: t -> float

val pixel_size: t -> float

val init: int -> int -> float -> t

val ray_for_pixel: t -> int -> int -> Ray.t

val render_parallel: t -> 'a World.t -> Canvas.t
val render_pooled: t -> 'a World.t -> Canvas.t
val render_in_pool: Domainslib.Task.pool -> t -> 'a World.t -> Canvas.t
val render: Domainslib.Task.pool -> t -> 'a World.t -> Canvas.t
