(* ROW COLUMN indexing *)
type t = Color.t ref list list

let height canvas = List.length canvas

let width canvas = match List.nth_opt canvas 0 with
  | None -> 0
  | Some(col) -> List.length col 

let init ~width:w ~height:h = List.init h (fun _ -> List.init w (fun _ -> ref (Color.init (0.0, 0.0, 0.0))))

let%test "Scenario: Creating a canvas" = let c = init ~width:20 ~height:10 in
  Int.equal (width c) 20 &&
  Int.equal (height c) 10 &&
  (*                                                    DEREF*)
  List.for_all (List.for_all (fun color -> Tuple.equal (!color) (Color.init (0.0, 0.0, 0.0)))) c

let pixel_at canvas ~x_idx:x ~y_idx:y = match List.nth_opt canvas y with
  | None -> None
  | Some(row_ref) -> List.nth_opt row_ref x 
(*
let map_pixel_row pixel_row ~x_idx:x ~color:new_color = let aux column_index current_color = 
  if Int.equal column_index x then new_color else current_color in List.mapi aux pixel_row

let write_pixel pixel_canvas ~y_idx:y ~x_idx:x ~color:new_color = let aux row_index current_row = 
  if Int.equal row_index y then map_pixel_row current_row ~x_idx:x ~color:new_color else current_row in List.mapi aux pixel_canvas
*)

let write_pixel canvas ~x_idx:x ~y_idx:y ~color:new_color = match List.nth_opt canvas y with
  | None -> ()
  | Some(row_ref) -> match List.nth_opt row_ref x with
    | None -> ()
    | Some(pixel_ref) -> pixel_ref := new_color

(*let write_pixel canvas ~x_idx:x ~y_idx:y ~color:new_color = *)
  (* This works on a row, moving across each column *)
  (*let mapi_func i current_color = if Int.equal i x then new_color else current_color in*)
  (*let aux ~canvas_row:row = List.mapi mapi_func row in*)
  (* This works on the canvas, moving down the rows *)
  (*let mapi_func i current_row = if Int.equal i y then aux ~canvas_row:current_row else current_row in*)
  (*List.mapi mapi_func canvas*)

let%test "Scenario: Writing pixels to a canvas" = let c = init ~height:10 ~width:20 in
  let red = Color.init (1.0, 0.0, 0.0) in write_pixel c ~x_idx:2 ~y_idx:3 ~color:red;
  match pixel_at c ~x_idx:2 ~y_idx:3 with
    | Some(color) -> Tuple.equal (!color) red
    | None -> false
  (*List.mapi (fun idx curr_row -> if Int.equal idx y_idx then write_pixel_aux curr_row ~x_idx:x_idx ~color:color else curr_row) canvas
  
  let aux idx row = if Int.equal y_idx idx then List.mapi (fun col_idx pixel_color -> if Int.equal col_idx x_idx then color else pixel_color) row else row in
  aux canvas;;
  List.mapi (fun idx row -> if Int.equal idx y_idx then row else ()) in 
  let canvas_height = List.length canvas in
  if y_idx > (canvas_height - 1) then Error ( () )
  else let row = List.nth canvas y_idx in 
  let canvas_width = List.length row in
  if x_idx > (canvas_width - 1) then Error ( () )
  else let List.nth row x_idx*)