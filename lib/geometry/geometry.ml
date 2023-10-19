type _ shape =
  | Spherical : Sphere.t -> 'a shape
  | Formless
let init_sphere ~o ~r = let s = Sphere.init ~origin:o ~radius:r in
  Spherical s
let init_unit_sphere () = init_sphere ~o:Tuple.point_origin ~r:1.0
let centroid ~s = match s with
  | Spherical s -> Sphere.origin s
  | Formless -> Tuple.point_origin
let get_transform s = match s with
  | Spherical s -> Sphere.get_transform s
  | Formless -> ref (Matrix.init 4 4)
let set_transform s m = match s with
  | Spherical s -> Sphere.set_transform s m
  | Formless -> ()
(*type shp = Sphere.t value
type shape = Sphere of Sphere.t*)