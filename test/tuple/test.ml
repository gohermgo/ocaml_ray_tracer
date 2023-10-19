open Tuple
let%test "Scenario: A tuple with w=1.0 is a point" = let tuple = init (4.3, -4.2, 3.1, 1.0) in
  (is_point tuple) && not (is_vector tuple)
(* Scenario: A tuple with w=0.0 is a vector *)
let%test "A tuple with w=0.0 is a vector" = let tuple = init (4.3, -4.2, 3.1, 0.0) in
  (is_vector tuple) && not (is_point tuple)

(* Scenario: point creates tuples with w=1.0 *)
let%test "Point creates tuples with w=1.0" = let (x, y, z) = (4.3, -4.2, 3.1) in
  equal (point (x, y, z)) (init (x, y, z, 1.0))
(* Scenario: vector creates tuples with w=0.0 *)
let%test "Vector creates tuples with w=0.0" = let (x, y, z) = (4.3, -4.2, 3.1) in
  equal (vector (x, y, z)) (init (x, y, z, 0.0))
let%test "Scenario: Adding two tuples" = let a1 = init (3.0, -2.0, 5.0, 1.0) in
  let a2 = init (-2.0, 3.0, 1.0, 0.0) in
  equal (add a1 a2) (init (1.0, 1.0, 6.0, 1.0))
(* Demonstrates how the difference of points gives w=0.0, thus a vector *)
let%test "Scenario: Subtracting two points" = let p1 = point (3.0, 2.0, 1.0) in
  let p2 = point (5.0, 6.0, 7.0) in
  equal (sub p1 p2) (vector (-2.0, -4.0, -6.0))
(* Demonstrates how the difference of a point and a vector gives w=1.0, thus another point *)
let%test "Scenario: Subtracting a vector from a point" = let p = point (3.0, 2.0, 1.0) in
  let v = vector (5.0, 6.0, 7.0) in
  equal (sub p v) (point (-2.0, -4.0, -6.0))

(* Demonstrates negating prior to implementing *)
let%test "Scenario: Subtracting a vector from the zero vector" = let v = vector (1.0, -2.0, 3.0) in 
  equal (sub (vector (0.0, 0.0, 0.0)) v) (vector (-1.0, 2.0, -3.0))
let%test "Scenario: Negating a tuple" = let a = init (1.0, -2.0, 3.0, -4.0) in
  equal (neg a) (init (-1.0, 2.0, -3.0, 4.0))

let%test "Scenario: Multiplying a tuple by a scalar" = let a = init (1.0, -2.0, 3.0, -4.0) in
  equal (mul a 3.5) (init (3.5, -7.0, 10.5, -14.0))
let%test "Scenario: Multiplying a tuple by a fraction" = let a = init (1.0, -2.0, 3.0, -4.0) in
  equal (mul a 0.5) (init (0.5, -1.0, 1.5, -2.0))

let%test "Scenario: Dividing a tuple by a scalar" = let a = init (1.0, -2.0, 3.0, -4.0) in
  equal (div a 2.0) (init (0.5, -1.0, 1.5, -2.0))

let%test "Scenario: Computing the magnitude of vector(1.0, 0.0, 0.0)" = let v = vector (1.0, 0.0, 0.0) in
  Float.equal (mag v) 1.0
let%test "Scenario: Computing the magnitude of vector(0.0, 1.0, 0.0)" = let v = vector (0.0, 1.0, 0.0) in
  Float.equal (mag v) 1.0
let%test "Scenario: Computing the magnitude of vector(0.0, 0.0, 1.0)" = let v = vector (0.0, 0.0, 1.0) in
  Float.equal (mag v) 1.0
let%test "Scenario: Computing the magnitude of vector(1.0, 2.0, 3.0)" = let v = vector (1.0, 2.0, 3.0) in
  Float.equal (mag v) (Float.sqrt 14.0)
let%test "Scenario: Computing the magnitude of vector(-1.0,-2.0,-3.0)" = let v = vector (-1.0, -2.0, -3.0) in
  Float.equal (mag v) (Float.sqrt 14.0)

let%test "Scenario: Normalizing a vector(4.0, 0.0, 0.0) gives (1.0, 0.0, 0.0)" = let v = vector (4.0, 0.0, 0.0) in
  equal (norm v) (vector (1.0, 0.0, 0.0))
let%test "Scenario: Normalizing a vector(1.0, 2.0, 3.0)" = let v = vector (1.0, 2.0, 3.0) in
  equal (norm v) (vector (Float.div 1.0 (Float.sqrt 14.0), Float.div 2.0 (Float.sqrt 14.0), Float.div 3.0 (Float.sqrt 14.0)))
let%test "Scenario: The magnitude of a normalized vector" = let v = vector (1.0, 2.0, 3.0) in
  Float.equal (mag (norm v)) 1.0

let%test "Scenario: The dot product of two tuples" = let a = vector (1.0, 2.0, 3.0) in
  let b = vector (2.0, 3.0, 4.0) in
  Float.equal (dot a b) 20.0

let%test "Scenario: The cross product of two vectors" = let a = vector (1.0, 2.0, 3.0) in
  let b = vector (2.0, 3.0, 4.0) in
  equal (cross a b) (vector (-1.0, 2.0, -1.0)) && equal (cross b a) (vector (1.0, -2.0, 1.0))

(* Reflecting tests *)
let%test "Reflecting a vector approaching at 45deg" = let v = vector (1.0, -1.0, 0.0) in
  let n = vector (0.0, 1.0, 0.0) in
  let r = reflect v n in
  equal r (vector(1.0, 1.0, 0.0))
let%test "Reflecting a vector off a slanted surface" = let v = vector (0.0, -1.0, 0.0) in
  let x_ = Float.div (Float.sqrt 2.0) 2.0 in
  let n = vector (x_, x_, 0.0) in
  let r = reflect v n in
  equal r (vector(1.0, 0.0, 0.0))