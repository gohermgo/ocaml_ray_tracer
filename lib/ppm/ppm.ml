let canvas_to_ppm ~canvas:c = let base_header = "P3\n" ^ Int.to_string (Canvas.width c) ^ " " ^ Int.to_string (Canvas.height c) ^ "\n" ^ "255" in
  let aux value = Float.ceil (value *. 255.0) in
  let clamp value = let num = aux value in if num > 255.0 then 255 else if num < 0.0 then 0 else Float.to_int num in
  let aux value = Int.to_string (clamp value) in
  (*let aux value = Float.to_int (Float.ceil value*.255.0) in
  let aux value = Int.max (aux value) 0 in
  let aux value = Int.min (aux value) 255 in
  let aux value = Int.to_string (aux value) in*)
  let row_to_ppx_string ~row:r = List.map (fun color -> aux (Color.r color) ^ " " ^ aux (Color.g color) ^ " " ^ aux (Color.b color) ^ " ") r in 
  let rec aux = function
    | [] -> []
    | row :: t -> row_to_ppx_string ~row:row :: aux t in
  let string_list = aux c in
  let rec aux = function
    | [] -> []
    | row_strings :: t -> (List.fold_left (String.cat) ("") row_strings) :: aux t in
  let strs = List.map String.trim (aux string_list) in
  let color_string = List.fold_left (fun s1 s2 -> (s1 ^ "\n" ^ s2)) "" (strs) in
  base_header ^ color_string ^ "\n" ;;
  
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
  let c = Canvas.write_pixel c ~x_idx:0 ~y_idx:0 ~color:(Color.init (1.5, 0.0, 0.0)) in
  let c = Canvas.write_pixel c ~x_idx:2 ~y_idx:1 ~color:(Color.init (0.0, 0.5, 0.0)) in
  let c = Canvas.write_pixel c ~x_idx:4 ~y_idx:2 ~color:(Color.init (-0.5, 0.0, 1.0)) in
  let ppm = String.split_on_char '\n' (canvas_to_ppm ~canvas:c) in
  (*print_endline (List.nth ppm 3);
  print_endline (List.nth ppm 4);
  print_endline (List.nth ppm 5);*)
  String.equal (List.nth ppm 3) "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0" &&
  String.equal (List.nth ppm 4) "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0" &&
  String.equal (List.nth ppm 5) "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"

let%test "Scenario: PPM file are terminated by a newline character" = let c = Canvas.init ~width:5 ~height:3 in
  let ppm = canvas_to_ppm ~canvas: c in
  let char_list = Base.String.to_list ppm in
  Char.equal '\n' (List.hd (List.rev char_list))

