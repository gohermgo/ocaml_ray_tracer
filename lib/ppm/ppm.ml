let clamp_float value = match Float.ceil (value *. 255.0) with
  | x when x > 255.0 -> 255
  | x when x < 0.0 -> 0
  | x -> Float.to_int x
let clamp_to_str value = Int.to_string (clamp_float value)

let pixel_to_ppm ~pixel_ref:p_r = let pixel = !p_r in
  clamp_to_str (Color.r pixel) ^ " " ^ clamp_to_str (Color.g pixel) ^ " " ^ clamp_to_str (Color.b pixel) ^ " "

let canvas_row_to_ppm ~row_ref:r_r = let rec aux = function
  | [] -> []
  | pixel_ref :: t -> pixel_to_ppm ~pixel_ref:pixel_ref :: aux t in
  let row_str = String.trim (List.fold_left (^) "" (aux r_r)) in
  let row_str_len = String.length row_str in
  if row_str_len >= 70 then
    let new_line_idx = String.rindex_from row_str 70 ' ' in
    let str_p1 = String.sub row_str 0 new_line_idx in
    let str_p2 = String.sub row_str (new_line_idx + 1) ((row_str_len - 1) - new_line_idx) in
    str_p1 ^ "\n" ^ str_p2
  else row_str

let ppm_header c = "P3\n" ^ Int.to_string (Canvas.width c) ^ " " ^ Int.to_string (Canvas.height c) ^ "\n" ^ "255" ^ "\n"

let canvas_to_ppm ~canvas:c = let header = ppm_header c in
  let rec aux = function
    | [] -> []
    | row_ref :: t -> canvas_row_to_ppm ~row_ref:row_ref :: aux t in
  let lis = List.map (fun s -> s ^ "\n") (aux c) in
  List.fold_left (^) header lis;;
  
let%test "Scenario: Constructing the PPM header" = let c = Canvas.init ~width:5 ~height:3 in
  let str_list = String.split_on_char '\n' (canvas_to_ppm ~canvas:c) in
  (*print_endline (List.nth str_list 0);
  print_endline (List.nth str_list 1);
  print_endline (List.nth str_list 2);*)
  String.equal (List.nth str_list 0) ("P3") &&
  String.equal (List.nth str_list 1) ("5 3") &&
  String.equal (List.nth str_list 2) ("255") 

let%test "Scenario: Constructing the PPM pixel data" = let c = Canvas.init ~width:5 ~height:3 in
  (*let c1 = Color.init (1.5, 0.0, 0.0) in
  let c2 = Color.init (0.0, 0.5, 0.0) in
  let c3 = Color.init (-0.5, 0.0, 1.0) in*)
  Canvas.write_pixel c ~x_idx:0 ~y_idx:0 ~color:(Color.init (1.5, 0.0, 0.0));
  Canvas.write_pixel c ~x_idx:2 ~y_idx:1 ~color:(Color.init (0.0, 0.5, 0.0));
  Canvas.write_pixel c ~x_idx:4 ~y_idx:2 ~color:(Color.init (-0.5, 0.0, 1.0));
  let ppm = String.split_on_char '\n' (canvas_to_ppm ~canvas:c) in
  (*print_endline (List.nth ppm 3);
  print_endline (List.nth ppm 4);
  print_endline (List.nth ppm 5);*)
  String.equal (List.nth ppm 3) "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0" &&
  String.equal (List.nth ppm 4) "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0" &&
  String.equal (List.nth ppm 5) "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"

let%test "Scenario: Splitting long lines in PPM files" = let c = Canvas.init ~width:10 ~height:2 in 
  let rec write_pixel_row y = function
    | x when x < (Canvas.width c) -> Canvas.write_pixel c ~x_idx:x ~y_idx:y ~color:(Color.init (1.0, 0.8, 0.6)); write_pixel_row y (x + 1)
    | _ -> () in
  let rec aux = function
    | y when y < (Canvas.height c) -> write_pixel_row y 0; aux (y + 1)
    | _ -> () in
  let () = aux 0 in
  let ppm = String.split_on_char '\n' (canvas_to_ppm ~canvas:c) in
  String.equal (List.nth ppm 3) "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204" &&
  String.equal (List.nth ppm 4) "153 255 204 153 255 204 153 255 204 153 255 204 153" &&
  String.equal (List.nth ppm 5) "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204" &&
  String.equal (List.nth ppm 6) "153 255 204 153 255 204 153 255 204 153 255 204 153" 
  
let%test "Scenario: PPM file are terminated by a newline character" = let c = Canvas.init ~width:5 ~height:3 in
  let ppm = canvas_to_ppm ~canvas: c in
  let char_list = Base.String.to_list ppm in
  Char.equal '\n' (List.hd (List.rev char_list))
