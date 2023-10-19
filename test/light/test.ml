let%test "A point light has a position and intensity" = let lg = Light.init_point Tuple.point_origin (Color.init (1.0, 1.0, 1.0)) in
  Tuple.equal (Light.position lg) Tuple.point_origin
