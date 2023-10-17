type t = { gravity: Tuple.t; wind: Tuple.t }
val init: gravity:Tuple.t -> wind:Tuple.t -> t
val gravity: t -> Tuple.t
val wind: t -> Tuple.t