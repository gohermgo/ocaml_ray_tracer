type t = {origin: Tuple.t; direction: Tuple.t}
let init ~origin ~direction = let () = if not (Tuple.is_point origin) || not (Tuple.is_vector direction) then failwith "Bad ray init" in {origin = origin; direction = direction}
let origin ray = ray.origin

let direction ray = ray.direction

let position ray t = Tuple.add (origin ray) (Tuple.mul (direction ray) t)

let abs_diff x y = Float.abs (Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

let _discriminant _sp _ry = let sphere_to_ray = Tuple.sub (origin _ry) (Sphere.origin _sp) in
  let a = Tuple.dot (direction _ry) (sphere_to_ray) in
  let b = Float.mul 2.0 a in
  let c = Float.sub a 1.0 in
  Float.sub (Float.pow b 2.0) (Float.mul 4.0 (Float.mul a c))
module Intersection = struct
  type 'a t = {t_value: float; o: 'a Geometry.shape ref}
  let init ~(t:float) ~(o:'a Geometry.shape ref) = {t_value = t; o = o}
  let t_value i = i.t_value
  let object_pointer i = i.o
  let equal i1 i2 = f_equal (t_value i1) (t_value i2) &&
    (* Check pointing to same object *)
    (object_pointer i1) == (object_pointer i2) &&
    (* Check objects are structurally equal *)
    (object_pointer i1) = (object_pointer i2)
  let compare i1 i2 = let t1 = t_value i1 in
    let t2 = t_value i2 in
    Float.compare t1 t2
end

module Comps = struct
  type 'a t = { t_value: float; o: 'a Geometry.shape ref; point: Tuple.t; eyev: Tuple.t; normalv: Tuple.t; inside: bool }
  let init t o p eyev normalv i = {t_value = t; o = o; point = p; eyev = eyev; normalv = normalv; inside = i}
  let t_value c = c.t_value
  let object_pointer c = c.o
  let point c = c.point
  let eyev c = c.eyev
  let normalv c = c.normalv
  let inside c = c.inside
end

let transform (r_i: t) (m: Matrix.t) : t = let o_new = Matrix.mul_tuple m (origin r_i) in
  let d_new = Matrix.mul_tuple m (direction r_i) in
  init ~origin:o_new ~direction:d_new

let check_intersection (shape: 'a Geometry.shape) (ry) : 'a Intersection.t array = 
  let centroid = Geometry.centroid ~s:shape in
  let shape_trans = Geometry.get_transform shape in
  let ray_trans = Matrix.invert !shape_trans in
  let ry = transform ry ray_trans in
  let (ray_origin, ray_dir) = (origin ry, direction ry) in
  let vec_to_centroid = Tuple.sub ray_origin centroid in
  let a = Tuple.dot ray_dir ray_dir in
  let b = Float.mul 2.0 (Tuple.dot (direction ry) vec_to_centroid) in
  let c = Float.sub (Tuple.dot vec_to_centroid vec_to_centroid) 1.0 in
  let discriminant = Float.sub (Float.pow b 2.0) (Float.mul 4.0 (Float.mul a c)) in
  if discriminant < 0.0 then (Array.make 0 (Intersection.init ~t:0.0 ~o:(ref (Geometry.Formless))))
  else
    let sp_ref = ref shape in
    let twice_a = Float.mul 2.0 a in
    let disc_sq = Float.sqrt discriminant in
    let t1 = Float.div (Float.sub (Float.neg b) disc_sq) twice_a in
    let i1 = Intersection.init ~t:t1 ~o:sp_ref in
    let t2 = Float.div (Float.add (Float.neg b) disc_sq) twice_a in
    let i2 = Intersection.init ~t:t2 ~o:sp_ref in
    [|i1; i2|]

let hit (iarr: 'a Intersection.t array) = let () = Array.sort Intersection.compare iarr in
  Array.find_opt (fun e -> Intersection.t_value e > 0.0) iarr

let precompute i ray =
  let t = Intersection.t_value i
  and o = Intersection.object_pointer i in
  let p = position ray t
  and eyev = Tuple.neg (direction ray) in
  let normalv = Geometry.normal_at !o p in
  let (inside, normalv) = if Tuple.dot normalv eyev < 0.0 
    then (true, Tuple.neg normalv) 
    else (false, normalv) in
  Comps.init t o p eyev normalv inside