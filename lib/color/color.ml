type t = Tuple.t

let r color = Tuple.x color
let g color = Tuple.y color
let b color = Tuple.z color
let a color = Tuple.w color

let abs_diff x y = Float.abs (Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

let equal c1 c2 = f_equal (r c1) (r c2) && f_equal (g c1) (g c2) && f_equal (b c1) (b c2)
  (*Tuple.equal c1 c2*)
  (*let aux n1 n2 = (Float.sub n1 n2) < Float.epsilon in
  aux (r c1) (r c2) &&
  aux (g c1) (g c2) &&
  aux (b c1) (b c2)*)

let init (r, g, b) = Tuple.init (r, g, b, 1.0)

let%test "Scenario: Colors are (red, green, blue) tuples" = let color = init (-0.5, 0.4, 1.7) in
  equal color (Tuple.init (-0.5, 0.4, 1.7, 1.0))

let add c1 c2 = let c = Tuple.add c1 c2 in init (r c, g c, b c)

let%test "Scenario: Adding colors" = let c1 = init (0.9, 0.6, 0.75) in
  let c2 = init (0.7, 0.1, 0.25) in
  equal (add c1 c2) (Tuple.init (1.6, 0.7, 1.0, 1.0))

let sub c1 c2 = let c = Tuple.sub c1 c2 in init (r c, g c, b c)

let%test "Scenario: Subtracting colors" = let c1 = init (0.9, 0.6, 0.75) in
  let c2 = init (0.7, 0.1, 0.25) in
  equal (sub c1 c2) (Tuple.init (0.2, 0.5, 0.5, 1.0));;

let mul_scalar c scalar = let c = Tuple.mul c scalar in init (r c, g c, b c)

let%test "Scenario: Multiplying a color by a scalar" = let c = init (0.2, 0.3, 0.4) in
  Tuple.equal (mul_scalar c 2.0) (Tuple.init (0.4, 0.6, 0.8, 1.0))

let mul c1 c2 = init(r c1 *. r c2, g c1 *. g c2, b c1 *. b c2)

let%test "Scenario: Multiplying colors" = let c1 = init (1.0, 0.2, 0.4) in
  let c2 = init (0.9, 1.0, 0.1) in
  equal (mul c1 c2) (init (0.9, 0.2, 0.04))