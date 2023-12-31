(*type t = { x: float; y: float; z: float; w: float }*)
type t = float array

let x t = t.(0)
let y t = t.(1)
let z t = t.(2)
let w t = t.(3)

let to_string t = "X: " ^ Float.to_string (x t) ^ 
  ", Y: " ^ Float.to_string (y t) ^ 
  ", Z: " ^ Float.to_string (z t) ^
  ", W: " ^ Float.to_string (w t)

let abs_diff e1 e2 = Float.abs (e1 -. e2)
let f_equal e1 e2 = (abs_diff e1 e2) < 0.0001
let equal t1 t2 = Array.for_all2 f_equal t1 t2
  (*Array.map2 (fun e1 e2 -> f_equal e1 e2) t1 t2 in Array.
  f_equal (x t1) (x t2) && f_equal (y t1) (y t2) && f_equal (z t1) (z t2) && f_equal (w t1) (w t2)*)

let init (x, y, z, w) = [|x; y; z; w|] (*{ x = x; y = y; z = z; w = w}*)
let of_array (a: float array) = a

let point (x, y, z) = init (x, y, z, 1.0)
let xp () = point (1.0, 0.0, 0.0)
let yp () = point (0.0, 1.0, 0.0)
let zp () = point (0.0, 0.0, 1.0)
let point_origin = point (0.0, 0.0, 0.0)

let is_point p = Float.equal (w p) 1.0


let vector (x, y, z) = init (x, y, z, 0.0)
let xv () = vector (1.0, 0.0, 0.0)
let yv () = vector (0.0, 1.0, 0.0)
let zv () = vector (0.0, 0.0, 1.0)

let is_vector v = Float.equal (w v) 0.0

let sum_of t = Array.fold_left (+.) 0.0 t

let squared_sum_of t = Array.fold_left (fun acc e -> acc +. (e ** 2.0)) 0.0 t

let add t1 t2 = let t = Array.map2 Float.add t1 t2 in of_array t
  (*init (x t, y t, z t, w t)*)

(*let%test "Scenario: Adding two tuples" = let a1 = init (3.0, -2.0, 5.0, 1.0) in
  let a2 = init (-2.0, 3.0, 1.0, 0.0) in
  equal (add a1 a2) (init (1.0, 1.0, 6.0, 1.0))*)

(* sub t1 t2 is the per element difference of t1 and t2 *)
let sub t1 t2 = let t = Array.map2 Float.sub t1 t2 in of_array t
  (*init (x t, y t, z t, w t)*)

(* Demonstrates how the difference of points gives w=0.0, thus a vector *)
(*let%test "Scenario: Subtracting two points" = let p1 = point (3.0, 2.0, 1.0) in
  let p2 = point (5.0, 6.0, 7.0) in
  equal (sub p1 p2) (vector (-2.0, -4.0, -6.0))*)
(* Demonstrates how the difference of a point and a vector gives w=1.0, thus another point *)
(*let%test "Scenario: Subtracting a vector from a point" = let p = point (3.0, 2.0, 1.0) in
  let v = vector (5.0, 6.0, 7.0) in
  equal (sub p v) (point (-2.0, -4.0, -6.0))*)

(* Demonstrates negating prior to implementing *)
(*let%test "Scenario: Subtracting a vector from the zero vector" = let v = vector (1.0, -2.0, 3.0) in 
  equal (sub (vector (0.0, 0.0, 0.0)) v) (vector (-1.0, 2.0, -3.0))*)
  

let neg t = Array.map Float.neg t
  (*init (Float.neg t.x, Float.neg t.y, Float.neg t.z, Float.neg t.w)*)
(*let%test "Scenario: Negating a tuple" = let a = init (1.0, -2.0, 3.0, -4.0) in
  equal (neg a) (init (-1.0, 2.0, -3.0, 4.0))*)

let mul t scalar = let f = Float.mul scalar in Array.map f t
  (*init (Float.mul scalar t.x, Float.mul scalar t.y, Float.mul scalar t.z, Float.mul scalar t.w)*)
(*let%test "Scenario: Multiplying a tuple by a scalar" = let a = init (1.0, -2.0, 3.0, -4.0) in
  equal (mul a 3.5) (init (3.5, -7.0, 10.5, -14.0))
let%test "Scenario: Multiplying a tuple by a fraction" = let a = init (1.0, -2.0, 3.0, -4.0) in
  equal (mul a 0.5) (init (0.5, -1.0, 1.5, -2.0))*)

let div t scalar = let f = (fun x -> Float.div x scalar) in Array.map f t
  (*init (Float.div t.x scalar, Float.div t.y scalar, Float.div t.z scalar, Float.div t.w scalar)*)
(*let%test "Scenario: Dividing a tuple by a scalar" = let a = init (1.0, -2.0, 3.0, -4.0) in
  equal (div a 2.0) (init (0.5, -1.0, 1.5, -2.0))*)

let mag t = 
  (*let squared_sum_of = Array.fold_left (fun acc e -> acc +. (Float.pow e 2.0)) 0.0 in*)
  Float.sqrt (squared_sum_of t)
  (*let squared = (fun x -> Float.pow x 2.0) in 
  let g = Array.fold_left (fun acc e -> Float.add acc (squared e)) 0.0 in
  Float.sqrt (g t)*)
  (*let x = Array.fold_left (fun acc e -> Float.add acc (squared e)) 0.0 t in Float.sqrt x*)
  (*let aux value = Float.pow value 2.0 in
  Float.sqrt ((aux t.x) +. (aux t.y) +. (aux t.z) +. (aux t.w))*)
(*let%test "Scenario: Computing the magnitude of vector(1.0, 0.0, 0.0)" = let v = vector (1.0, 0.0, 0.0) in
  Float.equal (mag v) 1.0
let%test "Scenario: Computing the magnitude of vector(0.0, 1.0, 0.0)" = let v = vector (0.0, 1.0, 0.0) in
  Float.equal (mag v) 1.0
let%test "Scenario: Computing the magnitude of vector(0.0, 0.0, 1.0)" = let v = vector (0.0, 0.0, 1.0) in
  Float.equal (mag v) 1.0
let%test "Scenario: Computing the magnitude of vector(1.0, 2.0, 3.0)" = let v = vector (1.0, 2.0, 3.0) in
  Float.equal (mag v) (Float.sqrt 14.0)
let%test "Scenario: Computing the magnitude of vector(-1.0,-2.0,-3.0)" = let v = vector (-1.0, -2.0, -3.0) in
  Float.equal (mag v) (Float.sqrt 14.0)*)

let norm t = div t (mag t)

(*let%test "Scenario: Normalizing a vector(4.0, 0.0, 0.0) gives (1.0, 0.0, 0.0)" = let v = vector (4.0, 0.0, 0.0) in
  equal (norm v) (vector (1.0, 0.0, 0.0))
let%test "Scenario: Normalizing a vector(1.0, 2.0, 3.0)" = let v = vector (1.0, 2.0, 3.0) in
  equal (norm v) (vector (Float.div 1.0 (Float.sqrt 14.0), Float.div 2.0 (Float.sqrt 14.0), Float.div 3.0 (Float.sqrt 14.0)))
let%test "Scenario: The magnitude of a normalized vector" = let v = vector (1.0, 2.0, 3.0) in
  Float.equal (mag (norm v)) 1.0*)

let dot t1 t2 : float = let t = Array.map2 Float.mul t1 t2 in sum_of t
  (*(Float.mul t1.x t2.x +. Float.mul t1.y t2.y +. Float.mul t1.z t2.z +. Float.mul t1.w t2.w)*)
(*let%test "Scenario: The dot product of two tuples" = let a = vector (1.0, 2.0, 3.0) in
  let b = vector (2.0, 3.0, 4.0) in
  Float.equal (dot a b) 20.0*)

let cross t1 t2 = (*let aux a b c d = Float.sub (Float.mul a b) (Float.mul c d) in*)
  let x = (t1.(1) *. t2.(2)) -. (t1.(2) *. t2.(1))
  and y = (t1.(2) *. t2.(0)) -. (t1.(0) *. t2.(2))
  and z = (t1.(0) *. t2.(1)) -. (t1.(1) *. t2.(0)) in vector (x, y, z)
(*let%test "Scenario: The cross product of two vectors" = let a = vector (1.0, 2.0, 3.0) in
  let b = vector (2.0, 3.0, 4.0) in
  equal (cross a b) (vector (-1.0, 2.0, -1.0)) && equal (cross b a) (vector (1.0, -2.0, 1.0))*)
let reflect v_in v_norm = sub v_in (mul v_norm (2.0 *. (dot v_in v_norm)))
  (*let v_out = sub v_in v_norm in
  let temp = Float.mul 2.0 (dot v_in v_norm) in
  mul v_out temp*)
