let abs_diff x y = Float.abs (Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

type t = {
  mutable color: Color.t; 
  mutable ambient: float; 
  mutable diffuse: float; 
  mutable specular: float; 
  mutable shininess: float
}

let color m = m.color
let set_color m c = m.color <- c

let ambient m = m.ambient
let set_ambient m a = m.ambient <- a

let diffuse m = m.diffuse
let set_diffuse m d = m.diffuse <- d

let specular m = m.specular
let set_specular m s = m.specular <- s

let shininess m = m.shininess
let set_shininess m s = m.shininess <- s

let equal m1 m2 = Color.equal (color m1) (color m2) &&
  f_equal (ambient m1) (ambient m2) &&
  f_equal (diffuse m1) (diffuse m2) &&
  f_equal (specular m1) (specular m2) &&
  f_equal (shininess m1) (shininess m2)

let init_def () = {
  color = Color.init (1.0, 1.0, 1.0); 
  ambient = 0.1; 
  diffuse = 0.9; 
  specular = 0.9; 
  shininess = 200.0
}