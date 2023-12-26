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

module D = Domain
module DL = Domainslib
module T = DL.Task

let ident dim = 
  let m = init dim dim in
  let rec ident' = function
    | idx when idx < dim -> m.(idx).(idx) <- 1.0; ident' (idx + 1)
    | _ -> ()
  in
  ident' 0;
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

let get_col (m : float array array) (col_idx: int) = 
  let row_length = Array.length m in
  Array.init row_length (fun row_idx -> m.(row_idx).(col_idx))

let a_sum : float array -> float = 
  Array.fold_left (+.) 0.0;;

let a_mul : float array -> float array -> float array  = 
  Array.map2 (fun row_e col_e -> Float.mul row_e col_e);;

let aux_mul (row: float array) (col: float array) : float = 
  Tuple.sum_of (a_mul row col)
  
let mul m1 m2 = 
  let (nrows1, ncols1), (nrows2, ncols2) = (dim_of m1), (dim_of m2) in assert (ncols1 == nrows2);
  (*let (nrows2, ncols2) = dim_of m2 in*)
  let m = init nrows1 ncols2 and aux row_array col_array = Tuple.sum_of (a_mul row_array col_array) in
  (* Row 1 in m1 * Col 1 in m2, Row 1 in m1 * Col 2 in m2 ... *)
  let () = for row=0 to ncols1 - 1 do
    for col = 0 to nrows2 - 1 do
      m.(row).(col) <- aux m1.(row) (get_col m2 col)
    done
  done in m

let mul_parallel (pool: T.pool) (m1: float array array) (m2: float array array) =
  let (nrows1, ncols1), (nrows2, ncols2) = (dim_of m1), (dim_of m2) in assert (ncols1 == nrows2);
  let m = init nrows1 ncols2 in
  (*
  let body = fun row ->
    for col = 0 to nrows2 - 1 do
      m.(row).(col) <- aux_mul m1.(row) (get_col m2 col)
    done
  in
  *)
  T.parallel_for pool ~start:0 ~finish:(ncols1 - 1) ~body:(fun row ->
    for col = 0 to nrows2 - 1 do
      m.(row).(col) <- aux_mul m1.(row) (get_col m2 col)
    done );
  m
 

let mul_tuple m t = let (nrows, ncols) = dim_of m in
  let () = assert (nrows == 4 && ncols == 4) in
  let a = [|Tuple.x t; Tuple.y t; Tuple.z t; Tuple.w t|] in
  let a_res = Array.map (fun m_arr -> a_sum ( a_mul m_arr a ) ) m in
  let t_res = Tuple.init (a_res.(0), a_res.(1), a_res.(2), a_res.(3)) in
  t_res
    


  
let transpose m = let (nrows, ncols) = dim_of m in
  (*
  let nrows = Array.length m in
  let ncols = Array.length m.(0) in
  *)
  (* Note the inversion in the next statement *)
  let res = init ncols nrows in
  let () = for i=0 to ncols - 1 do
    res.(i) <- get_col m i
  done in
  res


let det_2 m = 
  let (nrows, ncols) = dim_of m in assert (nrows == 2 && ncols == 2);
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
  for row = 0 to nrows - 1 do
    if row == omit_row then ()
    else
      let sub_row = if row > omit_row then row - 1 else row in
      for col = 0 to ncols - 1 do
        if col == omit_col then ()
        else
          let sub_col = if col > omit_col then col - 1 else col in
            let _x = Array.get m_r sub_row in
            let _x = Array.get _x sub_col in
            m_r.(sub_row).(sub_col) <- m.(row).(col)
      done;
  done;
  m_r

let submat_parallel (pool: T.pool) (m: t) ~omit_row:(omit_row: int) ~omit_col:(omit_col: int) : float array array =
  let (nrows, ncols) = dim_of m in
  let m_r = init (nrows - 1) (ncols - 1) in
  (*
  let body_inner = fun row col ->
    let sub_row =
      if row > omit_row then row - 1 else row in
    for col = 0 to ncols - 1 do
      if col == omit_col then ()
      else let sub_col = if col > omit_col then col - 1 else col in
      m_r.(sub_row).(sub_col) <- m.(row).(col)
    done
  in
  
  let body = fun row ->
    if row == omit_row then ()
    else let sub_row = if row > omit_row then row - 1 else row in
    for col = 0 to ncols - 1 do
      if col == omit_col then ()
      else 
      let sub_col = if col > omit_col then col - 1 else col in
      m_r.(sub_row).(sub_col) <- m.(row).(col)
    done
  in
  *)
  T.parallel_for pool ~start:0 ~finish:(nrows - 1) ~body:(fun row ->
    if row == omit_row then ()
    else let sub_row = if row > omit_row then row - 1 else row in
    for col = 0 to ncols - 1 do
      if col == omit_col then ()
      else 
      let sub_col = if col > omit_col then col - 1 else col in
      m_r.(sub_row).(sub_col) <- m.(row).(col)
    done
  );
  (*
  T.run pool (fun () -> T.parallel_for pool ~start:0 ~finish:(nrows - 1) ~body:(fun row ->
    if row == omit_row then ()
    else let sub_row = if row > omit_row then row - 1 else row in
    for col = 0 to ncols - 1 do
      if col == omit_col then ()
      else 
      let sub_col = if col > omit_col then col - 1 else col in
      m_r.(sub_row).(sub_col) <- m.(row).(col)
    done)
  );
  *)
  m_r

(* This is somehow slower still * 
 * More research is needed      *)
let _submat m omit_row omit_col = let (nrows, ncols) = dim_of m in
  (*let m_r = init (nrows - 1) (ncols - 1) in*)
  Array.init (nrows - 1) (fun row_idx -> let srow = if row_idx >= omit_row then row_idx + 1 else row_idx in
    Array.init (ncols - 1) (fun col_idx -> let scol = if col_idx >= omit_col then col_idx + 1 else col_idx in
      m.(srow).(scol)))
  (*Array.mapi_inplace (fun row_idx row -> 
    let srow = if row_idx >= omit_row then row_idx + 1 else row_idx in
    Array.mapi
    (fun col_idx _ -> 
      let scol = if col_idx >= omit_col then col_idx + 1 else col_idx in
      m.(srow).(scol)) row;
  ) m_r;
  m_r*)

  (*let initf = fun i -> if i > omit_col then 
Array.init
  for row = 0 to nrows - 1 do
    if row == omit_row ()
    else
      let srow = if row > omit_row then row - 1 else row in
      let f = fun col_idx col -> 
  done;
  let iter_fun = fun row_idx row -> Array.iteri 
    (fun col_idx col -> 
    if col_idx > omit_col then
      m_r.(row_idx).(col_idx - 1) <- col.(col_idx)
    else if col_idx < omit_col then
      m_r.(row_idx).(col_idx) <- col.(col_idx)
    ) m in
  Array.iteri iter_fun *)
(*  let col_aux row sub_row = for col = 0 to ncols - 1 do *)
    (* Do nothing on omitted col *)
    (*if col == omit_col then print_endline "SKIP" else*)
    (* Shift sub col index back, after omitted col *)
    (*let sub_col = if col < omit_col then col else col - 1 in 
      print_newline ();
      print_string ("Col " ^ (Int.to_string col));
      print_endline (" Row " ^ (Int.to_string row));
      print_string ("SCol " ^ (Int.to_string sub_col));
      print_endline (" SRow " ^ (Int.to_string sub_row));
      print_newline ();
      print m_r;
      print_newline ();
      print m;
      m_r.(sub_row).(sub_col) <- m.(row).(col)
  done in
  let () = for row = 0 to nrows - 1 do*)
    (* Do nothing on omitted row *)
    (*if row == omit_row then () else*)
    (* Shift sub row index back, after the omitted row *)
    (*let sub_row = if row < omit_row then row else row - 1 in col_aux row sub_row*)
  (*done in
  m_r*)


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

let minor_3_parallel (pool: T.pool) (m: t) ~row:(row_idx: int) ~col:(col_idx:int) =
  let (nrows, ncols) = dim_of m in assert (nrows == 3 && ncols == 3);
  (* let m_sub = T.run pool (fun () -> submat_parallel pool m ~omit_row:row_idx ~omit_col:col_idx) in *)
  let m_sub = submat_parallel pool m ~omit_row:row_idx ~omit_col:col_idx in
  det_2 m_sub
  (*
  det_2 (submat_parallel pool m ~omit_row:row_idx ~omit_col:col_idx)
  *)

let%test "Scenario: Calculating a minor of a 3x3 matrix (parallel)" = let m = init 3 3 in
  let () = m.(0) <- [|3.0; 5.0; 0.0|]   
  and () = m.(1) <- [|2.0; -1.0; -7.0|]
  and () = m.(2) <- [|6.0; -1.0; 5.0|]
  and pool = T.setup_pool ~num_domains:4 () in
  let m_sub = T.run pool (fun () -> submat_parallel pool m ~omit_row:1 ~omit_col:0)
  (*
  T.teardown_pool pool;
  let pool = T.setup_pool ~num_domains:4 () in
  *)
  and m_minor = T.run pool (fun () -> minor_3_parallel pool m ~row:1 ~col:0) in

  (*
  let m_minor = minor_3_parallel pool m ~row:1 ~col:0 in
  *)
  T.teardown_pool pool;
  f_equal (det_2 m_sub) 25.0 && f_equal m_minor 25.0

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


let cofactor_3_parallel (pool: T.pool) (m: t) ~row:(row_idx: int) ~col:(col_idx: int) : float =
  let (nrows, ncols) = dim_of m in assert (nrows == 3 && ncols == 3);
  (* let m_minor = T.run pool (fun () -> minor_3_parallel pool m ~row:row_idx ~col:col_idx) in *)
  let m_minor = minor_3_parallel pool m ~row:row_idx ~col:col_idx in
  if is_odd (row_idx + col_idx) then Float.neg m_minor else m_minor

let%test "Scenario: Calculating a cofactor of a 3x3 matrix (parallel)" = 
  let m = init 3 3 in

  let () = m.(0) <- [|3.0; 5.0; 0.0|]
  and () = m.(1) <- [|2.0; -1.0; -7.0|]
  and () = m.(2) <- [|6.0; -1.0; 5.0|]
  and pool = T.setup_pool ~num_domains:4 () in

  let m_1 = T.run pool (fun () -> minor_3_parallel pool m ~row:0 ~col:0)
  and m_2 = T.run pool (fun () -> minor_3_parallel pool m ~row:1 ~col:0)
  and c_1 = T.run pool (fun () -> cofactor_3_parallel pool m ~row:0 ~col:0)
  and c_2 = T.run pool (fun () -> cofactor_3_parallel pool m ~row:1 ~col:0) in
  
  f_equal m_1 (-12.0) &&
  f_equal c_1 (-12.0) &&
  f_equal m_2 (25.0) &&
  f_equal c_2 (-25.0)

let det_n m n f =
  let (nrows, ncols) = dim_of m in assert (nrows == n && ncols == n);
  let rec det' = function
    | col when col < ncols -> m.(0).(col) *. (f m 0 col) +. det' (col + 1)
    | _ -> 0.0 in det' 0

let det_n_parallel (pool: T.pool) (m: t) (n: int) (f: T.pool -> t -> row:int -> col:int -> float) =
  let (nrows, ncols) = dim_of m in assert (nrows == n && ncols == n);
  let rec det' = function
    | col when col < ncols -> let f_res = T.run pool (fun () -> f pool m ~row:0 ~col:col) in
      m.(0).(col) *. f_res +. det' (col + 1)
    | _ -> 0.0 in
  det' 0

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
let det_3_parallel (pool: T.pool) (m: t) = det_n_parallel pool m 3 cofactor_3_parallel

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

let minor_parallel (pool: T.pool) (m: t) ~row:(row_idx: int) ~col:(col_idx: int) =
  let (nrows, ncols) = dim_of m in assert (nrows == ncols);
  (* if nrows == 3 then T.run pool (fun () -> minor_3_parallel pool m ~row:row_idx ~col:col_idx) *)
  if nrows == 3 then T.run pool (fun () -> minor_3_parallel pool m) ~row:row_idx ~col:col_idx
  else
  let m_sub = T.run pool (fun () -> submat_parallel pool m ~omit_row:row_idx ~omit_col:col_idx) in
  (* let m_sub = submat_parallel pool m ~omit_row:row_idx ~omit_col:col_idx in *)
  (* T.run pool (fun () -> det_3_parallel pool m_sub) *)
  det_3_parallel pool m_sub

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

let cofactor_parallel (pool: T.pool) (m: t) ~row:(row_idx: int) ~col:(col_idx: int) : float = 
  let (nrows, ncols) = dim_of m in assert (nrows == ncols && nrows > 2);
  (* if nrows == 3 then T.run pool (fun () -> cofactor_3_parallel pool m ~row:row_idx ~col:col_idx) else *)
  if nrows == 3 then cofactor_3_parallel pool m ~row:row_idx ~col:col_idx else
  (*let dim_sum = row_idx + col_idx in*)
  (*let should_negate = (row_idx + col_idx) mod 2 != 0 in*)

  (* let m_minor = T.run pool (fun () -> minor_parallel pool m ~row:row_idx ~col:col_idx) in *)
  let m_minor = minor_parallel pool m ~row:row_idx ~col:col_idx in
  if is_odd (row_idx + col_idx) then Float.neg m_minor else m_minor

let _unused_det m = 
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

(*let det_n m n f =
  let (nrows, ncols) = dim_of m in assert (nrows == n && ncols == n);
  let rec det' = function
    | col when col < ncols -> m.(0).(col) *. (f m 0 col) +. det' (col + 1)
    | _ -> 0.0 in det' 0

let det_3 m = det_n m 3 cofactor_3

let minor m row_idx col_idx = let (nrows, ncols) = dim_of m in assert (nrows == ncols);
  if nrows == 3 then minor_3 m row_idx col_idx else det_3 (submat m row_idx col_idx)
*)
let det_parallel (pool: T.pool) (m: t) = 
  let (nrows, ncols) = dim_of m in assert (nrows == ncols);
  (* We can assume nrows == ncols here *)
  if nrows == 2 then det_2 m else 
  if nrows == 3 then det_3_parallel pool m else
  let rec aux = function
    | col when col < ncols -> 
      (*let c = T.run pool (fun () -> cofactor_parallel pool m ~row:0 ~col:col) in *)
      let c = cofactor_parallel pool m ~row:0 ~col:col in
      m.(0).(col) *. c +. aux (col + 1)
    | _ -> 0.0 in
  aux 0
let rec _minor m row col =
  let (nrows, ncols) = dim_of m in assert (nrows == ncols);
  if (nrows == 3) then let m' = _submat m row col in 
    (m'.(0).(0) *. m'.(1).(1)) -. (m'.(0).(1) *. m'.(1).(0))
  else 
    let det' = ref 0.0
    and m = _submat m row col in 
    let (_, ncols) = dim_of m in
    for i = 0 to ncols - 1 do
      let minor' = _minor m 0 i in 
      let cofactor' = if is_odd i then Float.neg minor' else minor' in
      det' := !det' +. (m.(0).(i) *. cofactor');
    done;
    !det'

let _cofactor m row col =
  let (nrows, ncols) = dim_of m in assert (nrows == ncols);
  let minor' = _minor m row col in
  if is_odd (row + col) then Float.neg minor' else minor'
  (*if (nrows == 3) then let m' = _minor m row col in 
    if is_odd (row + col) then Float.neg m' else m'
  else*)

let det m =
  let (nrows, ncols) = dim_of m in assert (nrows == ncols);
  let det' = ref 0.0 in
  if nrows == 2 then det' := (m.(0).(0) *. m.(1).(1)) -. (m.(0).(1) *. m.(1).(0))
  else for col = 0 to ncols - 1 do
    det' := !det' +. (m.(0).(col) *. (_cofactor m 0 col));
  done; 
  !det'

let is_invertible m = not (f_equal (det m) 0.0)
let is_invertible_parallel (pool: T.pool) (m: t) =
  let d = T.run pool (fun () -> det_parallel pool m) in
  not (f_equal d 0.0)

let _is_invertible m = let d = det m in
  (not (f_equal d 0.0), d)

let invert m = (*let () = assert (is_invertible m) in
  let d = _det m in*)
  (*let (invertible, d) = _is_invertible m in assert invertible;*)
  let d = det m in
  (* Assert matrix is invertible *)
  assert (not (f_equal d 0.0));
  let (nrows, ncols) = dim_of m in
  let m_i = init nrows ncols in
  let () = for row = 0 to nrows - 1 do
    for col = 0 to ncols - 1 do
      m_i.(col).(row) <- Float.div (_cofactor m row col) d
    done
  done in
  m_i

let invert_parallel (m: t) =
  let pool = match T.lookup_pool "matrix" with
  | Some v -> v
  | None -> raise (invalid_arg "no pool found in invert_parallel") in
  (* let pool = Option.get (T.lookup_pool "matrix") in *)
  (* let d = T.run pool (fun () -> det_parallel pool m) in assert (not (f_equal d 0.0)); *)
  let d = det_parallel pool m in 
  assert (not (f_equal d 0.0));
  let (nrows, ncols) = dim_of m in
  let m_i = init nrows ncols in
  (*
  T.run pool (fun () -> T.parallel_for pool ~start:0 ~finish:(nrows - 1) ~body:(fun row ->
    for col = 0 to ncols - 1 do
      (* let c = T.run pool (fun () -> cofactor_parallel pool m ~row:row ~col:col) in *)
      let c = cofactor_parallel pool m ~row:row ~col:col in
      m_i.(col).(row) <- Float.div c d
    done )); 
  *)
  (*
  T.parallel_for pool ~start:0 ~finish:(nrows - 1) ~body:(fun row ->
    for col = 0 to ncols - 1 do
      (* let c = T.run pool (fun () -> cofactor_parallel pool m ~row:row ~col:col) in *)
      let c = cofactor_parallel pool m ~row:row ~col:col in
      m_i.(col).(row) <- Float.div c d
    done );
  *)
  T.run pool (fun () -> T.parallel_for pool ~start:0 ~finish:(nrows - 1) ~body:(fun row ->
    for col = 0 to ncols - 1 do
      (* let c = T.run pool (fun () -> cofactor_parallel pool m ~row:row ~col:col) in *)
      let c = cofactor_parallel pool m ~row:row ~col:col in
      m_i.(col).(row) <- Float.div c d
    done)
  );
  m_i

let%test "Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix" = 
  let m = init 3 3 in
  let () = m.(0) <- [|1.0; 5.0; 0.0|] 
  and () = m.(1) <- [|-3.0; 2.0; 7.0|]
  and () = m.(2) <- [|0.0; 6.0; -3.0|] in
  let m_sub = submat m 0 2 in
  let (nrows, ncols) = dim_of m_sub in (*(Array.length m_sub, Array.length m_sub.(0)) in*)
  nrows == 2 && ncols == 2 &&
  row_equal m_sub.(0) [|-3.0; 2.0|] &&
  row_equal m_sub.(1) [|0.0; 6.0|]

let%test "Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix (parallel)" = 
  let m = init 3 3 in

  let () = m.(0) <- [|1.0; 5.0; 0.0|] 
  and () = m.(1) <- [|-3.0; 2.0; 7.0|]
  and () = m.(2) <- [|0.0; 6.0; -3.0|]
  and pool = T.setup_pool ~num_domains:4 () in

  let m_sub = T.run pool (fun () -> submat_parallel pool m ~omit_row:0 ~omit_col:2) in
  T.teardown_pool pool;

  let (nrows, ncols) = dim_of m_sub in (*(Array.length m_sub, Array.length m_sub.(0)) in*)
  nrows == 2 && ncols == 2 &&
  row_equal m_sub.(0) [|-3.0; 2.0|] &&
  row_equal m_sub.(1) [|0.0; 6.0|]

let%test "Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix" = 
  let m = init 4 4 in
  let () = m.(0) <- [|-6.0; 1.0; 1.0; 6.0|]  
  and () = m.(1) <- [|-8.0; 5.0; 8.0; 6.0|] 
  and () = m.(2) <- [|-1.0; 0.0; 8.0; 2.0|] 
  and () = m.(3) <- [|-7.0; 1.0; -1.0; 1.0|] in
  let m_sub = submat m 2 1 in
  let (nrows, ncols) = dim_of m_sub in (*(Array.length m_s, Array.length m_s.(0)) in*)
  nrows == 3 && ncols == 3 &&
  row_equal m_sub.(0) [|-6.0; 1.0; 6.0|] &&
  row_equal m_sub.(1) [|-8.0; 8.0; 6.0|] &&
  row_equal m_sub.(2) [|-7.0; -1.0; 1.0|]

let abs_diff x y = Float.abs(Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

let%test "Calculating the determinant of a 4x4 matrix" = 
  let m = init 4 4 in
  let () = m.(0) <- [|-2.0; -8.0; 3.0; 5.0|] 
  and () = m.(1) <- [|-3.0; 1.0; 7.0; 3.0|] 
  and () = m.(2) <- [|1.0; 2.0; -9.0; 6.0|] 
  and () = m.(3) <- [|-6.0; 7.0; 7.0; -9.0|] in
  f_equal (_cofactor m 0 0) 690.0 &&
  f_equal (_cofactor m 0 1) 447.0 &&
  f_equal (_cofactor m 0 2) 210.0 &&
  f_equal (_cofactor m 0 3) 51.0 &&
  f_equal (det m) (-4071.0)

(* Intersion tests *)

let%test "Scenario: Testing an invertible matrix for invertibility" = 
  let m = init 4 4 in
  let () = m.(0) <- [|6.0; 4.0; 4.0; 4.0|] 
  and () = m.(1) <- [|5.0; 5.0; 7.0; 6.0|] 
  and () = m.(2) <- [|4.0; -9.0; 3.0; -7.0|] 
  and () = m.(3) <- [|9.0; 1.0; 7.0; -6.0|] in
  f_equal (det m) (-2120.0) && 
  is_invertible m 

let%test "Scenario: Testing a noninvertible matrix for invertibility" = 
  let m = init 4 4 in
  let () = m.(0) <- [|-4.0; 2.0; -2.0; -3.0|] 
  and () = m.(1) <- [|9.0; 6.0; 2.0; 6.0|] 
  and () = m.(2) <- [|0.0; -5.0; 1.0; -5.0|] 
  and () = m.(3) <- [|0.0; 0.0; 0.0; 0.0|] in
  f_equal (det m) 0.0 && 
  not (is_invertible m)

let%test "Scenario: Calculating the inverse of a matrix" = 
  let m = init 4 4 in

  let () = m.(0) <- [|-5.0; 2.0; 6.0; -8.0|]
  and () = m.(1) <- [|1.0; -5.0; 1.0; 8.0|]
  and () = m.(2) <- [|7.0; 7.0; -6.0; -7.0|]
  and () = m.(3) <- [|1.0; -3.0; 7.0; 4.0|] in

  f_equal (det m) 532.0 &&
  f_equal (_cofactor m 2 3) (-160.0) &&
  f_equal (_cofactor m 3 2) 105.0 &&

  let m_i = invert m in

  f_equal m_i.(3).(2) (Float.div (-160.0) 532.0) &&
  f_equal m_i.(2).(3) (Float.div 105.0 532.0) &&

  row_equal m_i.(0) [|0.21805; 0.45113;  0.24060; -0.04511|] &&
  row_equal m_i.(1) [|-0.80827; -1.45677;  -0.44361; 0.52068|] &&
  row_equal m_i.(2) [|-0.07895; -0.22368;  -0.05263; 0.19737|] &&
  row_equal m_i.(3) [|-0.52256; -0.81391;  -0.30075; 0.30639|]

let%test "Scenario: Calculating the inverse of another matrix" = 
  let m = init 4 4 in

  let () = m.(0) <- [|8.0; -5.0; 9.0; 2.0|]
  and () = m.(1) <- [|7.0; 5.0; 6.0; 1.0|]
  and () = m.(2) <- [|-6.0; 0.0; 9.0; 6.0|]
  and () = m.(3) <- [|-3.0; 0.0; -9.0; -4.0|] in

  let m_i = invert m in

  row_equal m_i.(0) [|-0.15385; -0.15385; -0.28205; -0.53846|] &&
  row_equal m_i.(1) [|-0.07692;  0.12308;  0.02564;  0.03077|] &&
  row_equal m_i.(2) [|0.35897; 0.35897; 0.43590; 0.92308|] &&
  row_equal m_i.(3) [|-0.69231; -0.69231; -0.76923; -1.92308|]

let%test "Scenario: Calculating the inverse of a third matrix" = 
  let m = init 4 4 in

  let () = m.(0) <- [|9.0; 3.0; 0.0; 9.0|] 
  and () = m.(1) <- [|-5.0; -2.0; -6.0; -3.0|] 
  and () = m.(2) <- [|-4.0; 9.0; 6.0; 4.0|] 
  and () = m.(3) <- [|-7.0; 6.0; 6.0; 2.0|] in

  let m_i = invert m in

  row_equal m_i.(0) [|-0.04074; -0.07778; 0.14444; -0.22222|] &&
  row_equal m_i.(1) [|-0.07778; 0.03333; 0.36667; -0.33333|] &&
  row_equal m_i.(2) [|-0.02901; -0.14630; -0.10926; 0.12963|] &&
  row_equal m_i.(3) [|0.17778; 0.06667; -0.26667; 0.33333|]

let%test "Scenario: Calculating the inverse of a third matrix" = 
  let m1 = init 4 4 in
  let () = m1.(0) <- [|3.0; -9.0; 7.0; 3.0|]
  and () = m1.(1) <- [|3.0; -8.0; 2.0; -9.0|]
  and () = m1.(2) <- [|-4.0; 4.0; 4.0; 1.0|]
  and () = m1.(3) <- [|-6.0; 5.0; -1.0; 1.0|]
  and m2 = init 4 4 in
  let () = m2.(0) <- [|8.0; 2.0; 2.0; 2.0|]
  and () = m2.(1) <- [|3.0; -1.0; 7.0; 0.0|]
  and () = m2.(2) <- [|7.0; 0.0; 5.0; 4.0|]
  and () = m2.(3) <- [|6.0; -2.0; 0.0; 5.0|] in
  let m_prod = mul m1 m2
  and m2_i = invert m2 in
  equal (mul m_prod m2_i) m1

(*let row_reduce (mat: t) : t =
  let rec row_reduce_aux (row: int) (col: int) (mat2: t) : unit =
    let (nrows, ncols) = dim_of mat2 in
    if (col == nrows + 1) then ()
    else
      let col = get_col mat2 col in
      match 
  in
  mat

let set_col (mat: t) (idx: int) (col: float array) =
  let (nrows, ncols) = dim_of mat in
  assert (Array.length col == nrows);
  assert (idx < ncols);
  for row = 0 to nrows do
    mat.(row).(idx) <- col.(row)
  done

let inverse (mat: t) = 
  let (nrows, ncols) = dim_of mat in
  if nrows == ncols then
    let augmented = init nrows (2 * nrows) in
    for row = 0 to nrows do
      let col = get_col mat row in
      let arr = Array.make nrows 0.0 in
      begin
        assert (Array.length col == nrows);
        arr.(row) <- 1.0;
        set_col augmented row col;
        set_col augmented (row + nrows) arr;
      end
    done;
    let augmented' = *)
