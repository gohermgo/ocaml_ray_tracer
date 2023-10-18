type t = float array array
let f_equal x y = let abs_diff = Float.abs(Float.sub x y) in
  abs_diff < Float.epsilon

let init row_count column_count = Array.make_matrix column_count row_count 0.0

let%test "Scenario: Constructing and inspecting a 4x4 matrix" = let matrix = init 4 4 in
  let () = matrix.(0) <- [|1.0; 2.0; 3.0; 4.0|] in
  let () = matrix.(1) <- [|5.5; 6.5; 7.5; 8.5|] in
  let () = matrix.(2) <- [|9.0; 10.0; 11.0; 12.0|] in
  let () = matrix.(3) <- [|13.5; 14.5; 15.5; 16.5|] in
  Float.equal (matrix.(0).(0)) (1.0) &&
  Float.equal (matrix.(0).(3)) (4.0) && 
  Float.equal (matrix.(1).(0)) (5.5) && 
  Float.equal (matrix.(1).(2)) (7.5) && 
  Float.equal (matrix.(2).(2)) (11.0) && 
  Float.equal (matrix.(3).(0)) (13.5) && 
  Float.equal (matrix.(3).(2)) (15.5) 

let%test "Scenario: A 2x2 matrix ought to be representable" = let matrix = init 2 2 in
  let () = matrix.(0) <- [|-3.0; 5.0|] in
  let () = matrix.(1) <- [|1.0; -2.0|] in
  Float.equal (matrix.(0).(0)) (-3.0) &&
  Float.equal (matrix.(0).(1)) (5.0) &&
  Float.equal (matrix.(1).(0)) (1.0) &&
  Float.equal (matrix.(1).(1)) (-2.0)

let%test "Scenario: A 3x3 matrix ought to be representable" = let matrix = init 3 3 in
  let () = matrix.( 0) <- [|-3.0; 5.0; 0.0|] in
  let () = matrix.( 1) <- [|1.0; -2.0; -7.0|] in
  let () = matrix.( 2) <- [|0.0; 1.0; 1.0|] in
  Float.equal (matrix.(0).(0)) (-3.0) &&
  Float.equal (matrix.(1).(1)) (-2.0) &&
  Float.equal (matrix.(2).(2)) (1.0)

let dim_of m = (Array.length m, Array.length m.(0))

let row_equal = Array.for_all2 (fun e1 e2 -> Float.equal e1 e2)

let equal m1 m2 = if (Array.length m1) != (Array.length m2)
  then false
  else 
    let row_length = (Array.length m1) in
    (*let row_equal r1 r2 = Array.for_all2 (fun e1 e2 -> Float.equal e1 e2) r1 r2 in*)
    let rec aux = function
      | x when x < row_length -> let not_equal = not (row_equal m1.(x) m2.(x)) in
        if not_equal
        then false 
        else aux (x + 1)
      | _ -> true in
    aux 0

let%test "Scenario: Matrix equality with identical matrices" = let m1 = init 4 4 in
  let () = m1.(0) <- [|1.0; 2.0; 3.0; 4.0|] in
  let () = m1.(1) <- [|5.0; 6.0; 7.0; 8.0|] in
  let () = m1.(2) <- [|9.0; 8.0; 7.0; 6.0|] in
  let () = m1.(3) <- [|5.0; 4.0; 3.0; 2.0|] in
  let m2 = init 4 4 in
  let () = m2.(0) <- [|1.0; 2.0; 3.0; 4.0|] in
  let () = m2.(1) <- [|5.0; 6.0; 7.0; 8.0|] in
  let () = m2.(2) <- [|9.0; 8.0; 7.0; 6.0|] in
  let () = m2.(3) <- [|5.0; 4.0; 3.0; 2.0|] in
  equal m1 m2

let%test "Scenario: Matrix equality with different matrices" = let m1 = init 4 4 in
  let () = m1.(0) <- [|1.0; 2.0; 3.0; 4.0|] in
  let () = m1.(1) <- [|5.0; 6.0; 7.0; 8.0|] in
  let () = m1.(2) <- [|9.0; 8.0; 7.0; 6.0|] in
  let () = m1.(3) <- [|5.0; 4.0; 3.0; 2.0|] in
  let m2 = init 4 4 in
  let () = m2.(0) <- [|2.0; 3.0; 4.0; 5.0|] in
  let () = m2.(1) <- [|6.0; 7.0; 8.0; 9.0|] in
  let () = m2.(2) <- [|8.0; 7.0; 6.0; 5.0|] in
  let () = m2.(3) <- [|4.0; 3.0; 2.0; 1.0|] in
  not (equal m1 m2)

let ident dim = let m = init dim dim in
  let () = for i=0 to dim - 1 do
    m.(i).(i) <- 1.0
  done in
  m

let%test "Scenario: Constructing and inspecting a 4x4 identity matrix" = let m = ident 4 in
  row_equal m.(0) [|1.0; 0.0; 0.0; 0.0;|] &&
  row_equal m.(1) [|0.0; 1.0; 0.0; 0.0;|] &&
  row_equal m.(2) [|0.0; 0.0; 1.0; 0.0;|] &&
  row_equal m.(3) [|0.0; 0.0; 0.0; 1.0;|] 

let print m = let nrows = Array.length m in
  let ncols = Array.length m.(0) in
  for i=0 to nrows - 1 do
    for j=0 to ncols - 1 do
      print_float m.(i).(j)
    done
  done  


let get_col matrix col_idx = let row_length = Array.length matrix in
  Array.init row_length (fun row_idx -> matrix.(row_idx).(col_idx))

let%test "Scenario: Constructing and inspecting a column array" = let matrix = init 4 4 in
  let () = matrix.(0) <- [|1.0; 2.0; 3.0; 4.0|] in
  let () = matrix.(1) <- [|5.5; 6.5; 7.5; 8.5|] in
  let () = matrix.(2) <- [|9.0; 10.0; 11.0; 12.0|] in
  let () = matrix.(3) <- [|13.5; 14.5; 15.5; 16.5|] in
  let column = get_col matrix 0 in
  Float.equal (column.(0)) (1.0) &&
  Float.equal (column.(1)) (5.5) &&
  Float.equal (column.(2)) (9.0) &&
  Float.equal (column.(3)) (13.5)

let a_sum = Array.fold_left (+.) 0.0;;

let a_mul = Array.map2 (fun row_e col_e -> Float.mul row_e col_e);;
  
let mul m1 m2 = let col_length1 = Array.length (m1.(0)) in
  let row_length2 = Array.length m2 in
  if col_length1 != row_length2
  then None
  else let col_length2 = Array.length (m2.(0)) in
    let row_length1 = Array.length m1 in
    let m = init row_length1 col_length2 in
    let aux row_array col_array = a_sum (a_mul row_array col_array) in
    (* Row 1 in m1 * Col 1 in m2, Row 1 in m1 * Col 2 in m2 ... *)
    let () = for i=0 to col_length1 - 1 do
      for j=0 to row_length2 - 1 do
        m.(i).(j) <- aux m1.(i) (get_col m2 j)
      done
    done in Some(m)
 
let%test "Scenario: Multiplying two matrices" = let m1 = init 4 4 in
  let () = m1.(0) <- [|1.0; 2.0; 3.0; 4.0|] in
  let () = m1.(1) <- [|5.0; 6.0; 7.0; 8.0|] in
  let () = m1.(2) <- [|9.0; 8.0; 7.0; 6.0|] in
  let () = m1.(3) <- [|5.0; 4.0; 3.0; 2.0|] in
  let m2 = init 4 4 in
  let () = m2.(0) <- [|-2.0; 1.0; 2.0; 3.0|] in
  let () = m2.(1) <- [|3.0; 2.0; 1.0; -1.0|] in
  let () = m2.(2) <- [|4.0; 3.0; 6.0; 5.0|] in
  let () = m2.(3) <- [|1.0; 2.0; 7.0; 8.0|] in
  let m = mul m1 m2 in
  if Option.is_none(m)
  then false
  else 
    let m = Option.get m in
    let expected = init 4 4 in
    let () = expected.(0) <- [|20.0; 22.0; 50.0; 48.0|] in
    let () = expected.(1) <- [|44.0; 54.0; 114.0; 108.0|] in
    let () = expected.(2) <- [|40.0; 58.0; 110.0; 102.0|] in
    let () = expected.(3) <- [|16.0; 26.0; 46.0; 42.0|] in
    equal m expected

let%test "Scenario: Multiplying a matrix by the identity matrix" = let m = init 4 4 in
  let () = m.(0) <- [|0.0; 1.0; 2.0; 4.0|] in
  let () = m.(1) <- [|1.0; 2.0; 4.0; 8.0|] in
  let () = m.(2) <- [|2.0; 4.0; 8.0; 16.0|] in
  let () = m.(3) <- [|4.0; 8.0; 16.0; 32.0|] in
  let res = mul m (ident 4) in
  if Option.is_none res
  then false
  else
    let res = Option.get res in
    equal m res

let mul_tuple m t = let nrows = Array.length m in
  let ncols = Array.length m.(0) in
  if (ncols != 4) || (nrows != 4)
  then None
  else
    let a = [|Tuple.x t; Tuple.y t; Tuple.z t; Tuple.w t|] in
    let a_res = Array.map (fun m_arr -> a_sum(a_mul m_arr a)) m in
    let t_res = Tuple.init (a_res.(0), a_res.(1), a_res.(2), a_res.(3)) in
    Some(t_res)
    

let%test "Scenario: A matrix multiplied by a tuple" = let m = init 4 4 in
  let () = m.(0) <- [|1.0; 2.0; 3.0; 4.0|] in
  let () = m.(1) <- [|2.0; 4.0; 4.0; 2.0|] in
  let () = m.(2) <- [|8.0; 6.0; 4.0; 1.0|] in
  let () = m.(3) <- [|0.0; 0.0; 0.0; 1.0|] in
  let t = Tuple.init (1.0, 2.0, 3.0, 1.0) in
  let res = mul_tuple m t  in
  if Option.is_none res
  then false
  else
    let res = Option.get res in
    Tuple.equal res (Tuple.init (18.0, 24.0, 33.0, 1.0))

let%test "Scenario: Multiplying the identity matrix by a tuple" = let t = Tuple.init (1.0, 2.0, 3.0, 4.0) in
  let res = mul_tuple (ident 4) t in
  if Option.is_none res
  then false
  else
    let res = Option.get res in
    Tuple.equal res t
  
let transpose m = let nrows = Array.length m in
  let ncols = Array.length m.(0) in
  (* Note the inversion in the next statement *)
  let res = init ncols nrows in
  let () = for i=0 to ncols - 1 do
    res.(i) <- get_col m i
  done in
  res

let%test "Scenario: Transposing a matrix" = let m = init 4 4 in
  let () = m.(0) <- [|0.0; 9.0; 3.0; 0.0|] in
  let () = m.(1) <- [|9.0; 8.0; 0.0; 8.0|] in
  let () = m.(2) <- [|1.0; 8.0; 5.0; 3.0|] in
  let () = m.(3) <- [|0.0; 0.0; 5.0; 8.0|] in
  let m_t = transpose m in
  row_equal m_t.(0) [|0.0; 9.0; 1.0; 0.0|] &&
  row_equal m_t.(1) [|9.0; 8.0; 8.0; 0.0|] &&
  row_equal m_t.(2) [|3.0; 0.0; 5.0; 5.0|] &&
  row_equal m_t.(3) [|0.0; 8.0; 3.0; 8.0|] 
let%test "Scenario: Transposing the identity matrix" = let m = ident 4 in
  let m_t = transpose m in
  equal m_t m

let det_2 m = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == 2 && ncols == 2) in
  let prod1 = Float.mul m.(0).(0) m.(1).(1) in
  let prod2 = Float.mul m.(0).(1) m.(1).(0) in
  Float.sub prod1 prod2
let%test "Scenario: Calculating the determinant of a 2x2 matrix" = let m = init 2 2 in
  let () = m.(0) <- [|1.0; 5.0|] in
  let () = m.(1) <- [|-3.0; 2.0|] in
  f_equal (det_2 m) 17.0

let submat m omit_row omit_col = let (nrows, ncols) = dim_of m in
  let m_r = init (nrows - 1) (ncols - 1) in
  let col_aux row sub_row = for col = 0 to ncols - 1 do
    (* Do nothing on omitted col *)
    if col == omit_col then ()
    else
      (* Shift sub col index back, after omitted col *)
      let sub_col = if col < omit_col then col else col - 1 in
      m_r.(sub_row).(sub_col) <- m.(row).(col)
  done in
  let () = for row=0 to nrows - 1 do
    (* Do nothing on omitted row *)
    if row == omit_row then ()
    else
      (* Shift sub row index back, after the omitted row *)
      let sub_row = if row < omit_row then row else row - 1 in
      col_aux row sub_row
  done in
  m_r

let%test "Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix" = let m = init 3 3 in
  let () = m.(0) <- [|1.0; 5.0; 0.0|] in    
  let () = m.(1) <- [|-3.0; 2.0; 7.0|] in
  let () = m.(2) <- [|0.0; 6.0; -3.0|] in
  let m_s = submat m 0 2 in
  let (nrows, ncols) = (Array.length m_s, Array.length m_s.(0)) in
  nrows == 2 && ncols == 2 &&
  row_equal m_s.(0) [|-3.0; 2.0|] &&
  row_equal m_s.(1) [|0.0; 6.0|]
let%test "Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix" = let m = init 4 4 in
  let () = m.(0) <- [|-6.0; 1.0; 1.0; 6.0|] in    
  let () = m.(1) <- [|-8.0; 5.0; 8.0; 6.0|] in
  let () = m.(2) <- [|-1.0; 0.0; 8.0; 2.0|] in
  let () = m.(3) <- [|-7.0; 1.0; -1.0; 1.0|] in
  let m_s = submat m 2 1 in
  let (nrows, ncols) = (Array.length m_s, Array.length m_s.(0)) in
  nrows == 3 && ncols == 3 &&
  row_equal m_s.(0) [|-6.0; 1.0; 6.0|] &&
  row_equal m_s.(1) [|-8.0; 8.0; 6.0|] &&
  row_equal m_s.(2) [|-7.0; -1.0; 1.0|]

let minor_3 m row_idx col_idx = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == 3 && ncols == 3) in 
  let m_s = submat m row_idx col_idx in
  det_2 m_s

let%test "Scenario: Calculating a minor of a 3x3 matrix" = let m = init 3 3 in
  let () = m.(0) <- [|3.0; 5.0; 0.0|] in
  let () = m.(1) <- [|2.0; -1.0; -7.0|] in
  let () = m.(2) <- [|6.0; -1.0; 5.0|] in
  let m_s = submat m 1 0 in
  f_equal (det_2 m_s) 25.0 && f_equal (minor_3 m 1 0) 25.0

let cofactor_3 m row_idx col_idx = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == 3 && ncols == 3) in
  let should_negate = (row_idx + col_idx) mod 2 != 0 in
  let m_minor = minor_3 m row_idx col_idx in
  let res = if should_negate
  then Float.neg m_minor
  else m_minor in
  res

let%test "Scenario: Calculating a cofactor of a 3x3 matrix" = let m = init 3 3 in
  let () = m.(0) <- [|3.0; 5.0; 0.0|] in
  let () = m.(1) <- [|2.0; -1.0; -7.0|] in
  let () = m.(2) <- [|6.0; -1.0; 5.0|] in
  f_equal (minor_3 m 0 0) (-12.0) &&
  f_equal (cofactor_3 m 0 0)(-12.0) &&
  f_equal (minor_3 m 1 0) (25.0) &&
  f_equal (cofactor_3 m 1 0)(-25.0)

let det_3 m = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == 3 && ncols == 3) in
  let det_res = ref 0.0 in
  let () = for col = 0 to ncols - 1 do
    (*let cofactor = cofactor_3 m 0 col in*)
    let new_det = !det_res +. m.(0).(col) *. (cofactor_3 m 0 col) in
    det_res := new_det
  done in !det_res

let%test "Calculating the determinant of a 3x3 matrix" = let m = init 3 3 in
  let () = m.(0) <- [|1.0; 2.0; 6.0|] in
  let () = m.(1) <- [|-5.0; 8.0; -4.0|] in
  let () = m.(2) <- [|2.0; 6.0; 4.0|] in
  f_equal (cofactor_3 m 0 0) 56.0 &&
  f_equal (cofactor_3 m 0 1) 12.0 &&
  f_equal (cofactor_3 m 0 2) (-46.0) &&
  f_equal (det_3 m) (-196.0)

let minor m row_idx col_idx = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == ncols) in
  if nrows == 3
  then minor_3 m row_idx col_idx
  else
    let m_s = submat m row_idx col_idx in
    det_3 m_s

let cofactor m row_idx col_idx = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == ncols && nrows > 2) in
  if nrows == 3 
  then cofactor_3 m row_idx col_idx
  else
    let should_negate = (row_idx + col_idx) mod 2 != 0 in
    let m_minor = (minor m row_idx col_idx) in
    if should_negate
    then Float.neg m_minor
    else m_minor

let det m = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == ncols) in
  (* We can assume nrows == ncols here *)
  if nrows == 2  
  then det_2 m
  else if nrows == 3
  then det_3 m
  else
    let det_res = ref 0.0 in
    let () = for col = 0 to ncols - 1 do
      let cofactor = cofactor m 0 col in
      let new_det = !det_res +. m.(0).(col) *. cofactor in
      det_res := new_det
    done in
    !det_res

let%test "Calculating the determinant of a 4x4 matrix" = let m = init 4 4 in
  let () = m.(0) <- [|-2.0; -8.0; 3.0; 5.0|] in
  let () = m.(1) <- [|-3.0; 1.0; 7.0; 3.0|] in
  let () = m.(2) <- [|1.0; 2.0; -9.0; 6.0|] in
  let () = m.(3) <- [|-6.0; 7.0; 7.0; -9.0|] in
  f_equal (cofactor m 0 0) 690.0 &&
  f_equal (cofactor m 0 1) 447.0 &&
  f_equal (cofactor m 0 2) 210.0 &&
  f_equal (cofactor m 0 3) 51.0 &&
  f_equal (det m) (-4071.0)
