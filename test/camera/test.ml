let abs_diff x y = Float.abs (Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

let%test "Constructing a camera" =
  let hsize = 160
  and vsize = 120
  and fov = Float.pi /. 2.0 in
  let c = Camera.init hsize vsize fov in
  Camera.hsize c == 160 &&
  Camera.vsize c == 120 &&
  f_equal (Camera.fov c) fov &&
  Matrix.equal (Camera.transform c) (Matrix.ident 4)
let%test "Pixel size for a horizontal canvas" =
  let c = Camera.init 200 125 (Float.pi /. 2.0) in
  f_equal (Camera.pixel_size c) 0.01
let%test "Pixel size for a vertical canvas" =
  let c = Camera.init 125 200 (Float.pi /. 2.0) in
  f_equal (Camera.pixel_size c) 0.01

let%test "Constructing a ray through the center of the canvas" =
  let c = Camera.init 201 101 (Float.pi /. 2.0) in
  let r = Camera.ray_for_pixel c 100 50 in
  Tuple.equal (Ray.origin r) Tuple.point_origin &&
  Tuple.equal (Ray.direction r) (Tuple.vector (0.0, 0.0, -1.0))
let%test "Constructing a ray through the corner of the canvas" =
  let c = Camera.init 201 101 (Float.pi /. 2.0) in
  let r = Camera.ray_for_pixel c 0 0 in
  Tuple.equal (Ray.origin r) (Tuple.point (0.0, 0.0, 0.0)) &&
  Tuple.equal (Ray.direction r) (Tuple.vector (0.66519, 0.33259, -0.66851))

let%test "Constructing a ray when the camera is transformed" =
  let c = Camera.init 201 101 (Float.pi /. 2.0) in
  Camera.set_transform c (Matrix.mul (Transformation.rotation 0.0 (Float.pi /. 4.0) 0.0) (Transformation.translation 0.0 (-2.0) 5.0));
  let r = Camera.ray_for_pixel c 100 50 in
  Tuple.equal (Ray.origin r) (Tuple.point (0.0, 2.0, (-5.0))) &&
  Tuple.equal (Ray.direction r) (Tuple.vector (Float.sqrt 2.0 /. 2.0, 0.0, Float.neg (Float.sqrt 2.0 /. 2.0)))

let%test "Rendering a world with a camera" =
  let w = World.init_def ()
  and c = Camera.init 11 11 (Float.pi /. 2.0)
  and from_p = Tuple.point (0.0, 0.0, -5.0)
  and to_p = Tuple.point_origin
  and up_v = Tuple.yv () in
  Camera.set_transform c (Transformation.view_transform from_p to_p up_v);
  let pool = Domainslib.Task.setup_pool ~num_domains:4 () in
  let image = Domainslib.Task.run pool (fun () -> Camera.render pool c w) in
  Domainslib.Task.teardown_pool pool;
  Color.equal !(Option.get (Canvas.pixel_at image ~x_idx:5 ~y_idx:5)) (Color.init (0.38066, 0.47583, 0.2855))
