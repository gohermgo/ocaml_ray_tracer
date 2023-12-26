let abs_diff x y = Float.abs (Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001
(* Tests equality function *)
let%test "The default material" = let m = Material.init_def () in 
  Material.equal m (Material.init_def ())
(*  Color.equal (Material.color m) (Color.init (1.0, 1.0, 1.0)) && 
  f_equal (Material.ambient m) 0.1 &&
  f_equal (Material.diffuse m) 0.9 &&
  f_equal (Material.specular m) 0.9 &&
  f_equal (Material.shininess m) 200.0*)
(* Tests equality manually *)
let%test "The default material" = let m = Material.init_def () in 
  Color.equal_ (Material.color m) (Color.init (1.0, 1.0, 1.0)) && 
  f_equal (Material.ambient m) 0.1 &&
  f_equal (Material.diffuse m) 0.9 &&
  f_equal (Material.specular m) 0.9 &&
  f_equal (Material.shininess m) 200.0
  
let m_scaffold = Material.init_def()
let p_scaffold = Tuple.point (0.0, 0.0, 0.0)

let%test "Lighting with the eye between the light and the surface" = let eyev = Tuple.vector (0.0, 0.0, -1.0) in
  let normalv = Tuple.vector (0.0, 0.0, -1.0) in
  let light = Light.init_point (Tuple.point (0.0, 0.0, -10.0)) (Color.init (1.0, 1.0, 1.0)) in
  let result = Light.lighting m_scaffold light p_scaffold eyev normalv in
  Color.equal_ result (Color.init (1.9, 1.9, 1.9))

let _sqrt2half = Float.div (Float.sqrt 2.0) 2.0

let%test "Lighting with the eye between light and surface, eye offset 45deg" = let eyev = Tuple.vector (0.0, _sqrt2half, Float.neg _sqrt2half) in
  let normv = Tuple.vector (0.0, 0.0, -1.0) in
  let light = Light.init_point (Tuple.point (0.0, 0.0, -10.0)) (Color.init (1.0, 1.0, 1.0)) in
  let result = Light.lighting m_scaffold light p_scaffold eyev normv in
  Color.equal_ result (Color.init (1.0, 1.0, 1.0))

let%test "Lighting with the eye opposite surface, light offset 45deg" = let eyev = Tuple.vector (0.0, 0.0, -1.0) in
  let normv = Tuple.vector (0.0, 0.0, -1.0) in
  let light = Light.init_point (Tuple.point (0.0, 10.0, -10.0)) (Color.init (1.0, 1.0, 1.0)) in
  let result = Light.lighting m_scaffold light p_scaffold eyev normv in
  Color.equal_ result (Color.init (0.7364, 0.7364, 0.7364))

let%test "Lighting with the eye in the path of the reflection vector" = let eyev = Tuple.vector (0.0, Float.neg _sqrt2half, Float.neg _sqrt2half) in
  let normv = Tuple.vector (0.0, 0.0, -1.0) in
  let light = Light.init_point (Tuple.point (0.0, 10.0, -10.0)) (Color.init (1.0, 1.0, 1.0)) in
  let result = Light.lighting m_scaffold light p_scaffold eyev normv in
  Color.equal_ result (Color.init (1.6364, 1.6364, 1.6364))

let%test "Lighting with the light behind the surface" = let eyev = Tuple.vector (0.0, 0.0, -1.0) in
  let normv = Tuple.vector (0.0, 0.0, -1.0) in
  let light = Light.init_point (Tuple.point (0.0, 0.0, 10.0)) (Color.init (1.0, 1.0, 1.0)) in
  let result = Light.lighting m_scaffold light p_scaffold eyev normv in
  Color.equal_ result (Color.init (0.1, 0.1, 0.1))
