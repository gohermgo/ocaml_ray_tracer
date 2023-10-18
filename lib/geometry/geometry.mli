type _ shape =
  | Sphr : Sphere.t -> 'a shape
  | Formless
(*type shape = Sphere of Sphere.t*)