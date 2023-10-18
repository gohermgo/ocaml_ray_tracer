type _ shape =
  | Sphr : Sphere.t -> 'a shape
  | Formless
(*type shp = Sphere.t value
type shape = Sphere of Sphere.t*)