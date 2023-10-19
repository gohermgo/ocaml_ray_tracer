(* Point light *)
module Point: sig
  type t = {position: Tuple.t; intensity: Color.t}
  val init: Tuple.t -> Color.t -> t
  val position: t -> Tuple.t
  val intensity: t -> Color.t
end


type 'a t = 
  | P: Point.t -> 'a t

val init_point: Tuple.t -> Color.t -> 'a t

val position: 'a t -> Tuple.t

val intensity: 'a t -> Color.t

val lighting: Material.t -> 'a t -> Tuple.t -> Tuple.t -> Tuple.t -> Color.t
