type t = Color.t list list
val height: t -> int
val width: t -> int
val init: width:int -> height:int -> t
val pixel_at: t -> x_idx:int -> y_idx:int -> Color.t option
val write_pixel: t -> x_idx:int -> y_idx:int -> color:Color.t -> t