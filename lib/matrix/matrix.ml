type t = float array array
let abs_diff x y = Float.abs(Float.sub x y)
let _f_equal_precise x y = (abs_diff x y) < Float.epsilon
let f_equal x y = (abs_diff x y) < 0.0001

let init row_count column_count = Array.make_matrix column_count row_count 0.0


let dim_of m = (Array.length m, Array.length m.(0))

let row_equal = Array.for_all2 (fun e1 e2 -> f_equal e1 e2)

let equal m1 m2 = 
  if (Array.length m1) != (Array.length m2) then false else 
  let row_length = (Array.length m1) in
  (*let row_equal r1 r2 = Array.for_all2 (fun e1 e2 -> Float.equal e1 e2) r1 r2 in*)
  let rec aux = function
    | row_idx when row_idx < row_length -> if not (row_equal m1.(row_idx) m2.(row_idx)) then false else aux (row_idx + 1)     
    | _ -> true in
  aux 0


let ident dim = 
  let m = init dim dim in
  let rec aux = function
    | idx when idx < dim -> m.(idx).(idx) <- 1.0; aux (idx + 1)
    | _ -> ()
  in
  aux 0;
  m
  (*let () = for i=0 to dim - 1 do
    m.(i).(i) <- 1.0
  done in
  m*)


let print m = let nrows = Array.length m in
  let ncols = Array.length m.(0) in
  for i=0 to nrows - 1 do
    let () = for j=0 to ncols - 1 do
      let () = print_float m.(i).(j) in
      if j != (ncols - 1) then print_string " | "
    done in
    print_newline ()
  done  


let get_col matrix col_idx = let row_length = Array.length matrix in
  Array.init row_length (fun row_idx -> matrix.(row_idx).(col_idx))

let a_sum = Array.fold_left (+.) 0.0;;

let a_mul = Array.map2 (fun row_e col_e -> Float.mul row_e col_e);;
  
let mul m1 m2 = 
  let (nrows1, ncols1), (nrows2, ncols2) = (dim_of m1), (dim_of m2) in
  (*let (nrows2, ncols2) = dim_of m2 in*)
  assert (ncols1 == nrows2);
  let m = init nrows1 ncols2
  and aux row_array col_array = Tuple.sum_of (a_mul row_array col_array) in
  (* Row 1 in m1 * Col 1 in m2, Row 1 in m1 * Col 2 in m2 ... *)
  let () = for row=0 to ncols1 - 1 do
    for col=0 to nrows2 - 1 do
      m.(row).(col) <- aux m1.(row) (get_col m2 col)
    done
  done in m
 

let mul_tuple m t = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == 4 && ncols == 4) in
  let a = [|Tuple.x t; Tuple.y t; Tuple.z t; Tuple.w t|] in
  let a_res = Array.map (fun m_arr -> a_sum(a_mul m_arr a)) m in
  let t_res = Tuple.init (a_res.(0), a_res.(1), a_res.(2), a_res.(3)) in
  t_res
    


  
let transpose m = let nrows = Array.length m in
  let ncols = Array.length m.(0) in
  (* Note the inversion in the next statement *)
  let res = init ncols nrows in
  let () = for i=0 to ncols - 1 do
    res.(i) <- get_col m i
  done in
  res


let det_2 m = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == 2 && ncols == 2) in
  let prod1 = Float.mul m.(0).(0) m.(1).(1) in
  let prod2 = Float.mul m.(0).(1) m.(1).(0) in
  Float.sub prod1 prod2

(* Test kept here to keep det_2 from interface *)
let%test "Scenario: Calculating the determinant of a 2x2 matrix" = let m = init 2 2 in
  let () = m.(0) <- [|1.0; 5.0|] in
  let () = m.(1) <- [|-3.0; 2.0|] in
  f_equal (det_2 m) 17.0

let submat m omit_row omit_col = let (nrows, ncols) = dim_of m in
  let m_r = init (nrows - 1) (ncols - 1) in
  let col_aux row sub_row = for col = 0 to ncols - 1 do
    (* Do nothing on omitted col *)
    if col == omit_col then () else
    (* Shift sub col index back, after omitted col *)
    let sub_col = if col < omit_col then col else col - 1 in m_r.(sub_row).(sub_col) <- m.(row).(col)
  done in
  let () = for row=0 to nrows - 1 do
    (* Do nothing on omitted row *)
    if row == omit_row then () else
    (* Shift sub row index back, after the omitted row *)
    let sub_row = if row < omit_row then row else row - 1 in col_aux row sub_row
  done in
  m_r


let minor_3 m row_idx col_idx = let (nrows, ncols) = dim_of m in assert (nrows == 3 && ncols == 3);
  (*let m_s = submat m row_idx col_idx in*)
  det_2 (submat m row_idx col_idx)

(* Test kept here to keep minor_3 from interface *)
let%test "Scenario: Calculating a minor of a 3x3 matrix" = let m = init 3 3 in
  let () = m.(0) <- [|3.0; 5.0; 0.0|] in
  let () = m.(1) <- [|2.0; -1.0; -7.0|] in
  let () = m.(2) <- [|6.0; -1.0; 5.0|] in
  let m_s = submat m 1 0 in
  f_equal (det_2 m_s) 25.0 && f_equal (minor_3 m 1 0) 25.0

let is_odd n = n mod 2 != 0

let cofactor_3 m row_idx col_idx = let (nrows, ncols) = dim_of m in assert (nrows == 3 && ncols == 3);
  (* let should_negate = (row_idx + col_idx) mod 2 != 0 in *)
  let m_minor = minor_3 m row_idx col_idx in if is_odd (row_idx + col_idx) then Float.neg m_minor else m_minor
  (*let res = if should_negate
  then Float.neg m_minor
  else m_minor in
  res*)

(* Test kept here to keep cofactor_3 from interface *)
let%test "Scenario: Calculating a cofactor of a 3x3 matrix" = 
  let m = init 3 3 in
  let () = m.(0) <- [|3.0; 5.0; 0.0|]
  and () = m.(1) <- [|2.0; -1.0; -7.0|]
  and () = m.(2) <- [|6.0; -1.0; 5.0|] in
  f_equal (minor_3 m 0 0) (-12.0) &&
  f_equal (cofactor_3 m 0 0)(-12.0) &&
  f_equal (minor_3 m 1 0) (25.0) &&
  f_equal (cofactor_3 m 1 0)(-25.0)

let det_n m n f =
  let (nrows, ncols) = dim_of m in assert (nrows == n && ncols == n);
  let rec aux = function
    | col when col < ncols -> m.(0).(col) *. (f m 0 col) +. aux (col + 1)
    | _ -> 0.0 in aux 0

let det_3 m = det_n m 3 cofactor_3
  (*let (nrows, ncols) = dim_of m in assert (nrows == 3 && ncols == 3);
  let rec aux = function
    | col when col < ncols -> m.(0).(col) *. (cofactor_3 m 0 col) +. aux (col + 1)
    | _ -> 0.0 in aux 0 *)
  (*let det_res = ref 0.0 in
  let () = for col = 0 to ncols - 1 do
    (*let cofactor = cofactor_3 m 0 col in*)
    let new_det = !det_res +. m.(0).(col) *. (cofactor_3 m 0 col) in
    det_res := new_det
  done in !det_res*)

(* Test kept here to keep det_3 from interface *)
let%test "Calculating the determinant of a 3x3 matrix" = 
  let m = init 3 3 in
  let () = m.(0) <- [|1.0; 2.0; 6.0|]
  and () = m.(1) <- [|-5.0; 8.0; -4.0|]
  and () = m.(2) <- [|2.0; 6.0; 4.0|] in
  f_equal (cofactor_3 m 0 0) 56.0 &&
  f_equal (cofactor_3 m 0 1) 12.0 &&
  f_equal (cofactor_3 m 0 2) (-46.0) &&
  f_equal (det_3 m) (-196.0)

let minor m row_idx col_idx = let (nrows, ncols) = dim_of m in assert (nrows == ncols);
  if nrows == 3 then minor_3 m row_idx col_idx else det_3 (submat m row_idx col_idx)


let cofactor m row_idx col_idx = 
  let (nrows, ncols) = dim_of m in assert (nrows == ncols && nrows > 2);
  if nrows == 3 then cofactor_3 m row_idx col_idx else
  (*let dim_sum = row_idx + col_idx in*)
  (*let should_negate = (row_idx + col_idx) mod 2 != 0 in*)
  let m_minor = (minor m row_idx col_idx) in
  if is_odd (row_idx + col_idx) then Float.neg m_minor else m_minor
  (*if should_negate
  then Float.neg m_minor
  else m_minor*)

let det m = 
  let (nrows, ncols) = dim_of m in assert (nrows == ncols);
  (* We can assume nrows == ncols here *)
  if nrows == 2 then det_2 m else 
  if nrows == 3 then det_3 m else
  let rec aux = function
    | col when col < ncols -> m.(0).(col) *. (cofactor m 0 col) +. aux (col + 1)
    | _ -> 0.0 in
  aux 0
    (*let det_res = ref 0.0 in
    let () = for col = 0 to ncols - 1 do
      let cofactor = cofactor m 0 col in
      let new_det = !det_res +. m.(0).(col) *. cofactor in
      det_res := new_det
    done in
    !det_res*)

let is_invertible m = not (f_equal (det m) 0.0)

let invert m = let () = assert (is_invertible m) in
  let d = det m in
  let (nrows, ncols) = dim_of m in
  let m_i = init nrows ncols in
  let () = for row = 0 to nrows - 1 do
    for col = 0 to ncols - 1 do
      m_i.(col).(row) <- Float.div (cofactor m row col) d
    done
  done in
  m_i
