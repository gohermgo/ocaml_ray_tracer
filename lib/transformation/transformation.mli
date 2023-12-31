val translation: float -> float -> float -> Matrix.t
val scaling: float -> float -> float -> Matrix.t
val rotation: float -> float -> float -> Matrix.t
val skew: x_y:float -> x_z:float -> y_x:float -> y_z:float -> z_x:float -> z_y:float -> Matrix.t
val def_from: unit -> Tuple.t
val def_to: unit -> Tuple.t
val def_up: unit -> Tuple.t
val view_transform: Tuple.t -> Tuple.t -> Tuple.t -> Matrix.t