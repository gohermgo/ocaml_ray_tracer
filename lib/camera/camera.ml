type t = { hsize: int; vsize: int; fov: float; mutable transform: Matrix.t; mutable inverse_transform: Matrix.t; }
let hsize c = c.hsize
let vsize c = c.vsize
let fov c = c.fov
let transform c = c.transform
let set_transform c t = c.transform <- t; c.inverse_transform <- Matrix.invert t 
let inverse_transform c = c.inverse_transform

let half_view c = Float.tan (fov c /. 2.0)

let aspect_ratio c = (Float.of_int (hsize c) /. Float.of_int (vsize c))

let half_height c = 
  let aspect = aspect_ratio c 
  and half_view = half_view c in
  if aspect >= 1.0 then half_view /. aspect
  else half_view

let half_width c = 
  let aspect = aspect_ratio c
  and half_view = half_view c in
  if aspect >= 1.0 then half_view
  else half_view *. aspect

let pixel_size c = 
  (*let half_view = Float.tan (fov c /. 2.0) 
  and aspect = aspect_ratio c in*)
  (*let (half_width, _half_height) = if aspect >= 1.0 then 
    let w = half_view
    and h = (half_view /. aspect) in (w, h)
  else 
    let w = half_view *. aspect
    and h = half_view in (w, h)
  in*)
  (half_width c *. 2.0) /. Float.of_int (hsize c)
  (*let half_view = Float.tan (fov c /. 2.0)
  and aspect = aspect_ratio c in
  if aspect >= 1 then *)

let init hsize vsize fov = 
  let transform = Matrix.ident 4 in 
  { 
    hsize = hsize; vsize = vsize; 
    fov = fov; 
    transform = transform; inverse_transform = Matrix.invert transform; 
  }

let ray_for_pixel c x y =
  (* Offsets from canvas edge to pixel center *)
  let pixel_size = pixel_size c in
  let x_offset = ((Float.of_int x) +. 0.5) *. pixel_size
  and y_offset = ((Float.of_int y) +. 0.5) *. pixel_size in
  (* Camera looks toward -z, +x is left *)
  let world_x = (half_width c) -. x_offset
  and world_y = (half_height c) -. y_offset in
  (* Transform canvas point and origin, then compute ray *)
  let pixel = Matrix.mul_tuple (inverse_transform c) (Tuple.point (world_x, world_y, -1.0))
  and origin = Matrix.mul_tuple (inverse_transform c) (Tuple.point_origin) in
  let direction = Tuple.norm (Tuple.sub pixel origin) in
  Ray.init ~origin:origin ~direction:direction

let render c w =
  let (hsize, vsize) = (hsize c, vsize c) in
  let image = Canvas.init ~width:hsize ~height:vsize in
  for y = 0 to vsize - 1 do
    for x = 0 to hsize - 1 do
      let ray = ray_for_pixel c x y in
      Canvas.write_pixel image ~x_idx:x ~y_idx:y ~color:(World.color_at w ray)
    done;
  done;
  image