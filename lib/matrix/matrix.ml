type t = float array array

let init row_count column_count = Array.init row_count (fun _ -> Array.init column_count 
  (fun _ -> 0.0))

let set m row_index column_index = if row_index >= (Array.length m)
  then ()
  else 
    let row = (Array.get m row_index) in
    let row_length = Array.length (row) in
  if column_index >= row_length 
  then ()
  else 
    let cell = (Array.get row column_index) in
    