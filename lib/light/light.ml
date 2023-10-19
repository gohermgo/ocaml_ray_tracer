(* Point light *) 
module Point = struct
  type t = {position: Tuple.t; intensity: Color.t}
  let init p c = {position = p; intensity = c}
  let position l = l.position
  let intensity l = l.intensity
end

type _ t = 
  | P: Point.t -> 'a t

let init_point p c = P (Point.init p c)

let position (lg: _ t) = match lg with
  | P p -> Point.position p

let intensity (lg: _ t) = match lg with
  | P p -> Point.intensity p


let lighting m lg pos eyev normv = 
  (*combine the surface color with the light's color/intensity *)
  let eff_color = Color.mul (Material.color m) (intensity lg) in
  (*find the direction to the light source *)
  let lightv = Tuple.norm (Tuple.sub (position lg) pos) in
  (*compute the ambient contribution *)
  let ambient = Color.mul_scalar eff_color (Material.ambient m) in
  (*light_dot_normal represents the cosine of the angle between the *)
  (*light vector and the normal vector. A negative number means the *)
  (*light is on the other side of the surface.*)
  let light_dot_n = Tuple.dot lightv normv in
  let (diffuse, specular) = if light_dot_n < 0.0 then
    (Color.init (0.0, 0.0, 0.0), Color.init (0.0, 0.0, 0.0))
  else
    (*compute diffuse contribution *) 
    let diffuse = Tuple.mul eff_color (Float.mul (Material.diffuse m) light_dot_n) in
    let reflectv = Tuple.reflect (Tuple.neg lightv) normv in
    let reflect_dot_eye = Tuple.dot reflectv eyev in
    let specular = if reflect_dot_eye <= 0.0 then
      Color.init (0.0, 0.0, 0.0)
    else 
      let factor = Float.pow reflect_dot_eye (Material.shininess m) in
      Color.mul_scalar (Color.mul_scalar (intensity lg) (Material.specular m)) factor
    in
    (diffuse, specular)
  in
  (*let () = print_endline (Tuple.to_string ambient) in
  let () = print_endline (Tuple.to_string diffuse) in
  let () = print_endline (Tuple.to_string specular) in
  let final_value = ambient in*)
  Tuple.add ambient (Tuple.add diffuse specular)
