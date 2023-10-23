(* Basic tests *)

let%test "Scenario: Constructing and inspecting a 4x4 matrix" = 
  let matrix = Matrix.init 4 4 in
  let () = matrix.(0) <- [|1.0; 2.0; 3.0; 4.0|] 
  and () = matrix.(1) <- [|5.5; 6.5; 7.5; 8.5|] 
  and () = matrix.(2) <- [|9.0; 10.0; 11.0; 12.0|] 
  and () = matrix.(3) <- [|13.5; 14.5; 15.5; 16.5|] in
  Float.equal (matrix.(0).(0)) (1.0) &&
  Float.equal (matrix.(0).(3)) (4.0) && 
  Float.equal (matrix.(1).(0)) (5.5) && 
  Float.equal (matrix.(1).(2)) (7.5) && 
  Float.equal (matrix.(2).(2)) (11.0) && 
  Float.equal (matrix.(3).(0)) (13.5) && 
  Float.equal (matrix.(3).(2)) (15.5) 

let%test "Scenario: A 2x2 matrix ought to be representable" = 
  let matrix = Matrix.init 2 2 in
  let () = matrix.(0) <- [|-3.0; 5.0|]
  and () = matrix.(1) <- [|1.0; -2.0|] in
  Float.equal (matrix.(0).(0)) (-3.0) &&
  Float.equal (matrix.(0).(1)) (5.0) &&
  Float.equal (matrix.(1).(0)) (1.0) &&
  Float.equal (matrix.(1).(1)) (-2.0)

let%test "Scenario: A 3x3 matrix ought to be representable" = 
  let matrix = Matrix.init 3 3 in
  let () = matrix.(0) <- [|-3.0; 5.0; 0.0|]
  and () = matrix.(1) <- [|1.0; -2.0; -7.0|]
  and () = matrix.(2) <- [|0.0; 1.0; 1.0|] in
  Float.equal (matrix.(0).(0)) (-3.0) &&
  Float.equal (matrix.(1).(1)) (-2.0) &&
  Float.equal (matrix.(2).(2)) (1.0)

(* Equality tests *)

let%test "Scenario: Matrix equality with identical matrices" = 
  let m1 = Matrix.init 4 4 in
  let () = m1.(0) <- [|1.0; 2.0; 3.0; 4.0|]
  and () = m1.(1) <- [|5.0; 6.0; 7.0; 8.0|]
  and () = m1.(2) <- [|9.0; 8.0; 7.0; 6.0|]
  and () = m1.(3) <- [|5.0; 4.0; 3.0; 2.0|]
  and m2 = Matrix.init 4 4 in
  let () = m2.(0) <- [|1.0; 2.0; 3.0; 4.0|]
  and () = m2.(1) <- [|5.0; 6.0; 7.0; 8.0|]
  and () = m2.(2) <- [|9.0; 8.0; 7.0; 6.0|]
  and () = m2.(3) <- [|5.0; 4.0; 3.0; 2.0|] in
  Matrix.equal m1 m2

let%test "Scenario: Matrix equality with different matrices" = 
  let m1 = Matrix.init 4 4 in
  let () = m1.(0) <- [|1.0; 2.0; 3.0; 4.0|]
  and () = m1.(1) <- [|5.0; 6.0; 7.0; 8.0|]
  and () = m1.(2) <- [|9.0; 8.0; 7.0; 6.0|]
  and () = m1.(3) <- [|5.0; 4.0; 3.0; 2.0|]
  and m2 = Matrix.init 4 4 in
  let () = m2.(0) <- [|2.0; 3.0; 4.0; 5.0|]
  and () = m2.(1) <- [|6.0; 7.0; 8.0; 9.0|]
  and () = m2.(2) <- [|8.0; 7.0; 6.0; 5.0|]
  and () = m2.(3) <- [|4.0; 3.0; 2.0; 1.0|] in
  not (Matrix.equal m1 m2)

let%test "Scenario: Row equality can be done" =
  let r1 = [|4.0; 0.0; -16.0; 0.0|] in
  Matrix.row_equal r1 [|4.0; 0.0; -16.0; 0.0|]

(* Identity matrix tests *)

let%test "Scenario: Constructing and inspecting a 4x4 identity matrix" = 
  let m = Matrix.ident 4 in
  Matrix.row_equal m.(0) [|1.0; 0.0; 0.0; 0.0;|] &&
  Matrix.row_equal m.(1) [|0.0; 1.0; 0.0; 0.0;|] &&
  Matrix.row_equal m.(2) [|0.0; 0.0; 1.0; 0.0;|] &&
  Matrix.row_equal m.(3) [|0.0; 0.0; 0.0; 1.0;|] 

(* Column vector construction test *)

let%test "Scenario: Constructing and inspecting a column vector" = 
  let matrix = Matrix.init 4 4 in
  let () = matrix.(0) <- [|1.0; 2.0; 3.0; 4.0|]
  and () = matrix.(1) <- [|5.5; 6.5; 7.5; 8.5|]
  and () = matrix.(2) <- [|9.0; 10.0; 11.0; 12.0|]
  and () = matrix.(3) <- [|13.5; 14.5; 15.5; 16.5|] in
  let column = Matrix.get_col matrix 0 in
  Float.equal (column.(0)) (1.0) &&
  Float.equal (column.(1)) (5.5) &&
  Float.equal (column.(2)) (9.0) &&
  Float.equal (column.(3)) (13.5)

(* Manipulation tests *)


let%test "Scenario: Multiplying two matrices" = 
  let m1 = Matrix.init 4 4 in
  let () = m1.(0) <- [|1.0; 2.0; 3.0; 4.0|]
  and () = m1.(1) <- [|5.0; 6.0; 7.0; 8.0|]
  and () = m1.(2) <- [|9.0; 8.0; 7.0; 6.0|]
  and () = m1.(3) <- [|5.0; 4.0; 3.0; 2.0|]
  and m2 = Matrix.init 4 4 in
  let () = m2.(0) <- [|-2.0; 1.0; 2.0; 3.0|]
  and () = m2.(1) <- [|3.0; 2.0; 1.0; -1.0|]
  and () = m2.(2) <- [|4.0; 3.0; 6.0; 5.0|]
  and () = m2.(3) <- [|1.0; 2.0; 7.0; 8.0|] in
  let m = Matrix.mul m1 m2
  and expected = Matrix.init 4 4 in
  let () = expected.(0) <- [|20.0; 22.0; 50.0; 48.0|]
  and () = expected.(1) <- [|44.0; 54.0; 114.0; 108.0|]
  and () = expected.(2) <- [|40.0; 58.0; 110.0; 102.0|]
  and () = expected.(3) <- [|16.0; 26.0; 46.0; 42.0|] in
  Matrix.equal m expected

let%test "Scenario: Multiplying a matrix by the identity matrix" = 
  let m = Matrix.init 4 4 in
  let () = m.(0) <- [|0.0; 1.0; 2.0; 4.0|]
  and () = m.(1) <- [|1.0; 2.0; 4.0; 8.0|]
  and () = m.(2) <- [|2.0; 4.0; 8.0; 16.0|]
  and () = m.(3) <- [|4.0; 8.0; 16.0; 32.0|] in
  let product = Matrix.mul m (Matrix.ident 4) in
  Matrix.equal m product

let%test "Scenario: A matrix multiplied by a tuple" = 
  let m = Matrix.init 4 4 in
  let () = m.(0) <- [|1.0; 2.0; 3.0; 4.0|]
  and () = m.(1) <- [|2.0; 4.0; 4.0; 2.0|]
  and () = m.(2) <- [|8.0; 6.0; 4.0; 1.0|]
  and () = m.(3) <- [|0.0; 0.0; 0.0; 1.0|]
  and t = Tuple.init (1.0, 2.0, 3.0, 1.0) in
  let product = Matrix.mul_tuple m t  in
  Tuple.equal product (Tuple.init (18.0, 24.0, 33.0, 1.0))

let%test "Scenario: Multiplying the identity matrix by a tuple" = 
  let t = Tuple.init (1.0, 2.0, 3.0, 4.0) in
  let product = Matrix.mul_tuple (Matrix.ident 4) t in
  Tuple.equal product t

let%test "Scenario: Transposing a matrix" = 
  let m = Matrix.init 4 4 in
  let () = m.(0) <- [|0.0; 9.0; 3.0; 0.0|]
  and () = m.(1) <- [|9.0; 8.0; 0.0; 8.0|]
  and () = m.(2) <- [|1.0; 8.0; 5.0; 3.0|]
  and () = m.(3) <- [|0.0; 0.0; 5.0; 8.0|] in
  let m_t = Matrix.transpose m in
  Matrix.row_equal m_t.(0) [|0.0; 9.0; 1.0; 0.0|] &&
  Matrix.row_equal m_t.(1) [|9.0; 8.0; 8.0; 0.0|] &&
  Matrix.row_equal m_t.(2) [|3.0; 0.0; 5.0; 5.0|] &&
  Matrix.row_equal m_t.(3) [|0.0; 8.0; 3.0; 8.0|] 

let%test "Scenario: Transposing the identity matrix" = 
  let m = Matrix.ident 4 in
  let m_t = Matrix.transpose m in
  Matrix.equal m_t m

(* Submatrices, determinants, cofactors, minors tests *)

let%test "Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix" = 
  let m = Matrix.init 3 3 in
  let () = m.(0) <- [|1.0; 5.0; 0.0|] 
  and () = m.(1) <- [|-3.0; 2.0; 7.0|]
  and () = m.(2) <- [|0.0; 6.0; -3.0|] in
  let m_sub = Matrix.submat m 0 2 in
  let (nrows, ncols) = Matrix.dim_of m_sub in (*(Array.length m_sub, Array.length m_sub.(0)) in*)
  nrows == 2 && ncols == 2 &&
  Matrix.row_equal m_sub.(0) [|-3.0; 2.0|] &&
  Matrix.row_equal m_sub.(1) [|0.0; 6.0|]

let%test "Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix" = 
  let m = Matrix.init 4 4 in
  let () = m.(0) <- [|-6.0; 1.0; 1.0; 6.0|]  
  and () = m.(1) <- [|-8.0; 5.0; 8.0; 6.0|] 
  and () = m.(2) <- [|-1.0; 0.0; 8.0; 2.0|] 
  and () = m.(3) <- [|-7.0; 1.0; -1.0; 1.0|] in
  let m_sub = Matrix.submat m 2 1 in
  let (nrows, ncols) = Matrix.dim_of m_sub in (*(Array.length m_s, Array.length m_s.(0)) in*)
  nrows == 3 && ncols == 3 &&
  Matrix.row_equal m_sub.(0) [|-6.0; 1.0; 6.0|] &&
  Matrix.row_equal m_sub.(1) [|-8.0; 8.0; 6.0|] &&
  Matrix.row_equal m_sub.(2) [|-7.0; -1.0; 1.0|]

let abs_diff x y = Float.abs(Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

let%test "Calculating the determinant of a 4x4 matrix" = 
  let m = Matrix.init 4 4 in
  let () = m.(0) <- [|-2.0; -8.0; 3.0; 5.0|] 
  and () = m.(1) <- [|-3.0; 1.0; 7.0; 3.0|] 
  and () = m.(2) <- [|1.0; 2.0; -9.0; 6.0|] 
  and () = m.(3) <- [|-6.0; 7.0; 7.0; -9.0|] in
  f_equal (Matrix.cofactor m 0 0) 690.0 &&
  f_equal (Matrix.cofactor m 0 1) 447.0 &&
  f_equal (Matrix.cofactor m 0 2) 210.0 &&
  f_equal (Matrix.cofactor m 0 3) 51.0 &&
  f_equal (Matrix.det m) (-4071.0)

(* Intersion tests *)

let%test "Scenario: Testing an invertible matrix for invertibility" = 
  let m = Matrix.init 4 4 in
  let () = m.(0) <- [|6.0; 4.0; 4.0; 4.0|] 
  and () = m.(1) <- [|5.0; 5.0; 7.0; 6.0|] 
  and () = m.(2) <- [|4.0; -9.0; 3.0; -7.0|] 
  and () = m.(3) <- [|9.0; 1.0; 7.0; -6.0|] in
  f_equal (Matrix.det m) (-2120.0) && 
  Matrix.is_invertible m 

let%test "Scenario: Testing a noninvertible matrix for invertibility" = 
  let m = Matrix.init 4 4 in
  let () = m.(0) <- [|-4.0; 2.0; -2.0; -3.0|] 
  and () = m.(1) <- [|9.0; 6.0; 2.0; 6.0|] 
  and () = m.(2) <- [|0.0; -5.0; 1.0; -5.0|] 
  and () = m.(3) <- [|0.0; 0.0; 0.0; 0.0|] in
  f_equal (Matrix.det m) 0.0 && 
  not (Matrix.is_invertible m)

let%test "Scenario: Calculating the inverse of a matrix" = 
  let m = Matrix.init 4 4 in

  let () = m.(0) <- [|-5.0; 2.0; 6.0; -8.0|]
  and () = m.(1) <- [|1.0; -5.0; 1.0; 8.0|]
  and () = m.(2) <- [|7.0; 7.0; -6.0; -7.0|]
  and () = m.(3) <- [|1.0; -3.0; 7.0; 4.0|] in

  f_equal (Matrix.det m) 532.0 &&
  f_equal (Matrix.cofactor m 2 3) (-160.0) &&
  f_equal (Matrix.cofactor m 3 2) 105.0 &&

  let m_i = Matrix.invert m in

  f_equal m_i.(3).(2) (Float.div (-160.0) 532.0) &&
  f_equal m_i.(2).(3) (Float.div 105.0 532.0) &&

  Matrix.row_equal m_i.(0) [|0.21805; 0.45113;  0.24060; -0.04511|] &&
  Matrix.row_equal m_i.(1) [|-0.80827; -1.45677;  -0.44361; 0.52068|] &&
  Matrix.row_equal m_i.(2) [|-0.07895; -0.22368;  -0.05263; 0.19737|] &&
  Matrix.row_equal m_i.(3) [|-0.52256; -0.81391;  -0.30075; 0.30639|]

let%test "Scenario: Calculating the inverse of another matrix" = 
  let m = Matrix.init 4 4 in

  let () = m.(0) <- [|8.0; -5.0; 9.0; 2.0|]
  and () = m.(1) <- [|7.0; 5.0; 6.0; 1.0|]
  and () = m.(2) <- [|-6.0; 0.0; 9.0; 6.0|]
  and () = m.(3) <- [|-3.0; 0.0; -9.0; -4.0|] in

  let m_i = Matrix.invert m in

  Matrix.row_equal m_i.(0) [|-0.15385; -0.15385; -0.28205; -0.53846|] &&
  Matrix.row_equal m_i.(1) [|-0.07692;  0.12308;  0.02564;  0.03077|] &&
  Matrix.row_equal m_i.(2) [|0.35897; 0.35897; 0.43590; 0.92308|] &&
  Matrix.row_equal m_i.(3) [|-0.69231; -0.69231; -0.76923; -1.92308|]

let%test "Scenario: Calculating the inverse of a third matrix" = 
  let m = Matrix.init 4 4 in

  let () = m.(0) <- [|9.0; 3.0; 0.0; 9.0|] 
  and () = m.(1) <- [|-5.0; -2.0; -6.0; -3.0|] 
  and () = m.(2) <- [|-4.0; 9.0; 6.0; 4.0|] 
  and () = m.(3) <- [|-7.0; 6.0; 6.0; 2.0|] in

  let m_i = Matrix.invert m in

  Matrix.row_equal m_i.(0) [|-0.04074; -0.07778; 0.14444; -0.22222|] &&
  Matrix.row_equal m_i.(1) [|-0.07778; 0.03333; 0.36667; -0.33333|] &&
  Matrix.row_equal m_i.(2) [|-0.02901; -0.14630; -0.10926; 0.12963|] &&
  Matrix.row_equal m_i.(3) [|0.17778; 0.06667; -0.26667; 0.33333|]

let%test "Scenario: Calculating the inverse of a third matrix" = 
  let m1 = Matrix.init 4 4 in
  let () = m1.(0) <- [|3.0; -9.0; 7.0; 3.0|]
  and () = m1.(1) <- [|3.0; -8.0; 2.0; -9.0|]
  and () = m1.(2) <- [|-4.0; 4.0; 4.0; 1.0|]
  and () = m1.(3) <- [|-6.0; 5.0; -1.0; 1.0|]
  and m2 = Matrix.init 4 4 in
  let () = m2.(0) <- [|8.0; 2.0; 2.0; 2.0|]
  and () = m2.(1) <- [|3.0; -1.0; 7.0; 0.0|]
  and () = m2.(2) <- [|7.0; 0.0; 5.0; 4.0|]
  and () = m2.(3) <- [|6.0; -2.0; 0.0; 5.0|] in
  let m_prod = Matrix.mul m1 m2
  and m2_i = Matrix.invert m2 in
  Matrix.equal (Matrix.mul m_prod m2_i) m1
