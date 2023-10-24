let translation x y z = let m = Matrix.ident 4 in
  let () = m.(0).(3) <- x in
  let () = m.(1).(3) <- y in
  let () = m.(2).(3) <- z in
  m

let%test "Scenario: Multiplying by a translation matrix" = let tr = translation 5.0 (-3.0) 2.0 in
  let p = Tuple.point (-3.0, 4.0, 5.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (2.0, 1.0, 7.0))

let%test "Scenario: Multiplying by the inverse of a translation matrix" = let tr = Matrix.invert(translation 5.0 (-3.0) 2.0) in
  let p = Tuple.point (-3.0, 4.0, 5.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (-8.0, 7.0, 3.0))

let abs_diff x y = Float.abs (Float.sub x y)
let _f_equal x y = (abs_diff x y) < 0.0001
let%test "Scenario: Translation does not affect vectors" = let tr = translation 5.0 (-3.0) 2.0 in
  let v = Tuple.vector (-3.0, 4.0, 5.0) in
  Tuple.equal (Matrix.mul_tuple tr v) v

let scaling x y z = let m = Matrix.ident 4 in
  let () = m.(0).(0) <- x in
  let () = m.(1).(1) <- y in
  let () = m.(2).(2) <- z in
  m

let%test "Scenario: A scaling matrix applied to a point" = let tr = scaling 2.0 3.0 4.0 in
  let p = Tuple.point (-4.0, 6.0, 8.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (-8.0, 18.0, 32.0))

let%test "Scenario: A scaling matrix applied to a vector" = let tr = scaling 2.0 3.0 4.0 in
  let v = Tuple.vector (-4.0, 6.0, 8.0) in
  Tuple.equal (Matrix.mul_tuple tr v) (Tuple.vector (-8.0, 18.0, 32.0))

let%test "Scenario: Multiplying by the inverse of a scaling matrix" = let tr = Matrix.invert (scaling 2.0 3.0 4.0) in
  let v = Tuple.vector (-4.0, 6.0, 8.0) in
  Tuple.equal (Matrix.mul_tuple tr v) (Tuple.vector (-2.0, 2.0, 2.0))

let%test "Scenario: Reflection is scaling by a negative value" = let tr = scaling (-1.0) 1.0 1.0 in
  let p = Tuple.point (2.0, 3.0, 4.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (-2.0, 3.0, 4.0))

let rotation_x rad = let tr = Matrix.ident 4 in
  let () = tr.(1).(1) <- Float.cos rad in  
  let () = tr.(1).(2) <- Float.neg (Float.sin rad) in
  let () = tr.(2).(1) <- Float.sin rad in
  let () = tr.(2).(2) <- Float.cos rad in
  tr

let%test "Scenario: Rotating a point around the x axis" = let p = Tuple.point (0.0, 1.0, 0.0) in
  let rot_eigth = rotation_x (Float.div Float.pi 4.0) in
  let rot_quarter = rotation_x (Float.div Float.pi 2.0) in
  let half_rt_2 = Float.div (Float.sqrt 2.0) 2.0 in
  Tuple.equal (Matrix.mul_tuple rot_eigth p) (Tuple.point (0.0, half_rt_2, half_rt_2)) &&
  Tuple.equal (Matrix.mul_tuple rot_quarter p) (Tuple.point (0.0, 0.0, 1.0))

let%test "Scenario: The inverse of an x-rotation rotates in the opposite direction" = let p = Tuple.point (0.0, 0.0, 1.0) in
  let rot_eigth = Matrix.invert (rotation_x (Float.div Float.pi 4.0)) in
  let half_rt_2 = Float.div (Float.sqrt 2.0) 2.0 in
  Tuple.equal (Matrix.mul_tuple rot_eigth p) (Tuple.point (0.0, half_rt_2, half_rt_2))

let rotation_y rad = let tr = Matrix.ident 4 in
  let () = tr.(0).(0) <- Float.cos rad in  
  let () = tr.(0).(2) <- Float.sin rad in
  let () = tr.(2).(0) <- Float.neg (Float.sin rad) in
  let () = tr.(2).(2) <- Float.cos rad in
  tr

let%test "Scenario: Rotating a point around the y axis" = let p = Tuple.point (0.0, 0.0, 1.0) in
  let rot_eigth = rotation_y (Float.div Float.pi 4.0) in
  let rot_quarter = rotation_y (Float.div Float.pi 2.0) in
  let half_rt_2 = Float.div (Float.sqrt 2.0) 2.0 in
  Tuple.equal (Matrix.mul_tuple rot_eigth p) (Tuple.point (half_rt_2, 0.0, half_rt_2)) &&
  Tuple.equal (Matrix.mul_tuple rot_quarter p) (Tuple.point (1.0, 0.0, 0.0))

let rotation_z rad = let tr = Matrix.ident 4 in
  let () = tr.(0).(0) <- Float.cos rad in  
  let () = tr.(0).(1) <- Float.neg (Float.sin rad) in
  let () = tr.(1).(0) <- Float.sin rad in
  let () = tr.(1).(1) <- Float.cos rad in
  tr

let%test "Scenario: Rotating a point around the z axis" = let p = Tuple.point (0.0, 1.0, 0.0) in
  let rot_eigth = rotation_z (Float.div Float.pi 4.0) in
  let rot_quarter = rotation_z (Float.div Float.pi 2.0) in
  let half_rt_2 = Float.div (Float.sqrt 2.0) 2.0 in
  Tuple.equal (Matrix.mul_tuple rot_eigth p) (Tuple.point (Float.neg (half_rt_2), half_rt_2, 0.0)) &&
  Tuple.equal (Matrix.mul_tuple rot_quarter p) (Tuple.point (-1.0, 0.0, 0.0))

let rotation rad_x rad_y rad_z = Matrix.mul (Matrix.mul (rotation_x rad_x) (rotation_y rad_y)) (rotation_z rad_z)
let%test "Scenario: Rotating a point around the x axis COMBINED" = let p = Tuple.point (0.0, 1.0, 0.0) in
  let rot_eigth = rotation (Float.div Float.pi 4.0) 0.0 0.0 in
  let rot_quarter = rotation (Float.div Float.pi 2.0) 0.0 0.0 in
  let half_rt_2 = Float.div (Float.sqrt 2.0) 2.0 in
  Tuple.equal (Matrix.mul_tuple rot_eigth p) (Tuple.point (0.0, half_rt_2, half_rt_2)) &&
  Tuple.equal (Matrix.mul_tuple rot_quarter p) (Tuple.point (0.0, 0.0, 1.0))
let%test "Scenario: Rotating a point around the y axis COMBINED" = let p = Tuple.point (0.0, 0.0, 1.0) in
  let rot_eigth = rotation 0.0 (Float.div Float.pi 4.0) 0.0 in
  let rot_quarter = rotation 0.0 (Float.div Float.pi 2.0) 0.0 in
  let half_rt_2 = Float.div (Float.sqrt 2.0) 2.0 in
  Tuple.equal (Matrix.mul_tuple rot_eigth p) (Tuple.point (half_rt_2, 0.0, half_rt_2)) &&
  Tuple.equal (Matrix.mul_tuple rot_quarter p) (Tuple.point (1.0, 0.0, 0.0))
let%test "Scenario: Rotating a point around the z axis COMBINED" = let p = Tuple.point (0.0, 1.0, 0.0) in
  let rot_eigth = rotation 0.0 0.0 (Float.div Float.pi 4.0) in
  let rot_quarter = rotation 0.0 0.0 (Float.div Float.pi 2.0) in
  let half_rt_2 = Float.div (Float.sqrt 2.0) 2.0 in
  Tuple.equal (Matrix.mul_tuple rot_eigth p) (Tuple.point (Float.neg (half_rt_2), half_rt_2, 0.0)) &&
  Tuple.equal (Matrix.mul_tuple rot_quarter p) (Tuple.point (-1.0, 0.0, 0.0))

let skew ~x_y ~x_z ~y_x ~y_z ~z_x ~z_y = let tr = Matrix.ident 4 in
  let () = tr.(0).(1) <- x_y in
  let () = tr.(0).(2) <- x_z in
  let () = tr.(1).(0) <- y_x in
  let () = tr.(1).(2) <- y_z in
  let () = tr.(2).(0) <- z_x in
  let () = tr.(2).(1) <- z_y in
  tr

let def_from () = Tuple.point_origin

let def_to () = Tuple.neg (Tuple.zp ())

let def_up () = Tuple.zv ()

let view_transform from_p to_p up_v =
  let forward = Tuple.norm (Tuple.sub to_p from_p) in
  let left = Tuple.cross forward (Tuple.norm up_v) in
  let true_up = Tuple.cross left forward in
  let orientation = Matrix.ident 4 in
  orientation.(0) <- [|Tuple.x left; Tuple.y left; Tuple.z left; 0.0|];
  orientation.(1) <- [|Tuple.x true_up; Tuple.y true_up; Tuple.z true_up; 0.0|];
  orientation.(2) <- [|~-.(Tuple.x forward); ~-.(Tuple.y forward); ~-.(Tuple.z forward); 0.0|];
  Matrix.mul orientation (translation ~-.(Tuple.x from_p) ~-.(Tuple.y from_p) ~-.(Tuple.z from_p))
  

let%test "Scenario: A skew moves x in proportion to y" = let tr = skew ~x_y:1.0 ~x_z:0.0 ~y_x:0.0 ~y_z:0.0 ~z_x:0.0 ~z_y:0.0 in 
  let p = Tuple.point (2.0, 3.0, 4.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (5.0, 3.0, 4.0))

let%test "Scenario: A skew moves x in proportion to z" = let tr = skew ~x_y:0.0 ~x_z:1.0 ~y_x:0.0 ~y_z:0.0 ~z_x:0.0 ~z_y:0.0 in 
  let p = Tuple.point (2.0, 3.0, 4.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (6.0, 3.0, 4.0))

let%test "Scenario: A skew moves y in proportion to x" = let tr = skew ~x_y:0.0 ~x_z:0.0 ~y_x:1.0 ~y_z:0.0 ~z_x:0.0 ~z_y:0.0 in 
  let p = Tuple.point (2.0, 3.0, 4.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (2.0, 5.0, 4.0))

let%test "Scenario: A skew moves y in proportion to z" = let tr = skew ~x_y:0.0 ~x_z:0.0 ~y_x:0.0 ~y_z:1.0 ~z_x:0.0 ~z_y:0.0 in 
  let p = Tuple.point (2.0, 3.0, 4.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (2.0, 7.0, 4.0))

let%test "Scenario: A skew moves z in proportion to x" = let tr = skew ~x_y:0.0 ~x_z:0.0 ~y_x:0.0 ~y_z:0.0 ~z_x:1.0 ~z_y:0.0 in 
  let p = Tuple.point (2.0, 3.0, 4.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (2.0, 3.0, 6.0))

let%test "Scenario: A skew moves z in proportion to y" = let tr = skew ~x_y:0.0 ~x_z:0.0 ~y_x:0.0 ~y_z:0.0 ~z_x:0.0 ~z_y:1.0 in 
  let p = Tuple.point (2.0, 3.0, 4.0) in
  Tuple.equal (Matrix.mul_tuple tr p) (Tuple.point (2.0, 3.0, 7.0))

let%test "Scenario: Individual transformations are applied in sequence" = let p = Tuple.point (1.0, 0.0, 1.0) in
  let a = rotation (Float.div (Float.pi) 2.0) 0.0 0.0 in
  let b = scaling 5.0 5.0 5.0 in
  let c = translation 10.0 5.0 7.0 in
  (* Apply rotation first *)
  let p2 = Matrix.mul_tuple a p in
  Tuple.equal p2 (Tuple.point (1.0, -1.0, 0.0)) &&
  (* Then apply scaling *)
  let p3 = Matrix.mul_tuple b p2 in
  Tuple.equal p3 (Tuple.point (5.0, -5.0, 0.0)) &&
  (* Then apply translation *)
  let p4 = Matrix.mul_tuple c p3 in
  Tuple.equal p4 (Tuple.point (15.0, 0.0, 7.0))

let%test "Scenario: Chained transformations must be applied in reverse order" = let p = Tuple.point (1.0, 0.0, 1.0) in
  let a = rotation (Float.div (Float.pi) 2.0) 0.0 0.0 in
  let b = scaling 5.0 5.0 5.0 in
  let c = translation 10.0 5.0 7.0 in
  let t = Matrix.mul c (Matrix.mul a b) in
  Tuple.equal (Matrix.mul_tuple t p) (Tuple.point (15.0, 0.0, 7.0))

(* View transformation tests *)

let%test "The transformation matrix for the default orientation" =
  let from_p = Tuple.point (0.0, 0.0, 0.0)
  and to_p = Tuple.point (0.0, 0.0, -1.0)
  and up_v = Tuple.vector (0.0, 1.0, 0.0) in
  let orientation = view_transform from_p to_p up_v in  
  Matrix.equal orientation (Matrix.ident 4)

let%test "A view transformation matrix looking in positive z direction" =
  let from_p = Tuple.point (0.0, 0.0, 0.0)
  and to_p = Tuple.point (0.0, 0.0, 1.0)
  and up_v = Tuple.vector (0.0, 1.0, 0.0) in
  let orientation = view_transform from_p to_p up_v in  
  Matrix.equal orientation (scaling (-1.0) 1.0 (-1.0))

let%test "The view transformation moves the world" =
  let from_p = Tuple.point (0.0, 0.0, 8.0)
  and to_p = Tuple.point (0.0, 0.0, 0.0)
  and up_v = Tuple.vector (0.0, 1.0, 0.0) in
  let orientation = view_transform from_p to_p up_v in  
  Matrix.equal orientation (translation 0.0 0.0 (-8.0))

let%test "An arbitrary view transform" =
  let from_p = Tuple.point (1.0, 3.0, 2.0)
  and to_p = Tuple.point (4.0, -2.0, 8.0)
  and up_v = Tuple.vector (1.0, 1.0, 0.0) in
  let orientation = view_transform from_p to_p up_v
  and expected = Matrix.ident 4 in
  expected.(0) <- [| -0.50709; 0.50709; 0.67612; -2.36643|];
  expected.(1) <- [| 0.76772; 0.60609; 0.12122; -2.82843|];
  expected.(2) <- [| -0.35857; 0.59761; -0.71714; 0.0|];
  Matrix.equal orientation expected


