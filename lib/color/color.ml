let abs_diff x y = Float.abs (Float.sub x y)
let f_equal x y = (abs_diff x y) < 0.0001

type t' = {mutable r: float; mutable g: float; mutable b: float}

let r' (color: t') : float = color.r
let g' (color: t') : float = color.g
let b' (color: t') : float = color.b

let equal' (c1: t') (c2: t') : bool =
  f_equal (r' c1) (r' c2) &&
  f_equal (g' c1) (g' c2)&&
  f_equal (b' c1) (b' c2) 

let init' ~r:(r: float) ~g:(g: float) ~b:(b: float) : t' =
  {r; g; b}

let of_array' (a: float array) : t' =
  let dim = Array.length a in assert (dim = 3);
  { r = a.(0); g = a.(1); b = a.(2) }

let add' (c1: t') (c2: t') : t' = 
  {
    r = c1.r +. c2.r; 
    g = c1.g +. c2.g; 
    b = c1.b +. c2.b
  }
let sub' (c1: t') (c2: t') : t' = 
  {
    r = c1.r -. c2.r; 
    g = c1.g -. c2.g; 
    b = c1.b -. c2.b
  }
let mul_scalar' (c: t') (s: float) : t' = 
  {
    r = s *. c.r;
    g = s *. c.g;
    b = s *. c.b
  }
let mul' (c1: t') (c2: t') : t' = 
  {
    r = c1.r *. c2.r; 
    g = c1.g *. c2.g; 
    b = c1.b *. c2.b
  }

type t = Tuple.t

type c =
  | C: t -> c
  | C': t' -> c

let r color = Tuple.x color
let g color = Tuple.y color
let b color = Tuple.z color
let a color = Tuple.w color


let equal_ c1 c2 = f_equal (r c1) (r c2) && f_equal (g c1) (g c2) && f_equal (b c1) (b c2)
let equal (c1: c) (c2: c) = match (c1, c2) with
  | C c1, C c2 -> equal_ c1 c2
  | C' c1, C c2 -> f_equal (c1.r) (r c2) && f_equal (c1.g) (g c2) && f_equal (c1.b) (b c2)
  | C c1, C' c2 -> f_equal (r c1) (c2.r) && f_equal (g c1) (c2.g) && f_equal (b c1) (c2.b)
  | C' c1, C' c2 -> equal' c1 c2
  (*Tuple.equal c1 c2*)
  (*let aux n1 n2 = (Float.sub n1 n2) < Float.epsilon in
  aux (r c1) (r c2) &&
  aux (g c1) (g c2) &&
  aux (b c1) (b c2)*)

let init (r, g, b) = Tuple.init (r, g, b, 1.0)
let of_array c_arr = Tuple.of_array c_arr 


let%test "Scenario: Colors are (red, green, blue) tuples" = 
  let color = init (-0.5, 0.4, 1.7) in
  equal_ color (Tuple.init (-0.5, 0.4, 1.7, 1.0))

let add_ (c1: t) (c2: t) = Tuple.add c1 c2 (*let c = Tuple.add c1 c2 in init (r c, g c, b c)*)

let%test "Scenario: Adding colors" = 
  let c1 = init (0.9, 0.6, 0.75)
  and c2 = init (0.7, 0.1, 0.25) in
  equal_ (add_ c1 c2) (Tuple.init (1.6, 0.7, 1.0, 1.0))

let sub_ c1 c2 = Tuple.sub c1 c2 (*let c = Tuple.sub c1 c2 in init (r c, g c, b c)*)

let%test "Scenario: Subtracting colors" = 
  let c1 = init (0.9, 0.6, 0.75)
  and c2 = init (0.7, 0.1, 0.25) in
  equal_ (sub_ c1 c2) (Tuple.init (0.2, 0.5, 0.5, 1.0))

let mul_scalar_ c scalar = let c = Tuple.mul c scalar in init (r c, g c, b c)

let%test "Scenario: Multiplying a color by a scalar" = 
  let c = init (0.2, 0.3, 0.4) in
  Tuple.equal (mul_scalar_ c 2.0) (Tuple.init (0.4, 0.6, 0.8, 1.0))

let mul_ c1 c2 = Array.map2 ( *. ) c1 c2
  (*init(r c1 *. r c2, g c1 *. g c2, b c1 *. b c2)*)

let%test "Scenario: Multiplying colors" = 
  let c1 = init (1.0, 0.2, 0.4)
  and c2 = init (0.9, 1.0, 0.1) in
  equal_ (mul_ c1 c2) (init (0.9, 0.2, 0.04))

let add (c1: c) (c2: c): c = match (c1, c2) with
  | C c1, C c2 -> C (add_ c1 c2)
  | C' c1, C c2 -> C (init ((c1.r +. r c2), (c1.g +. g c2), (c1.b +. b c2)))
  | C c1, C' c2 -> C (init ((r c1 +. c2.r), (g c1 +. c2.g), (b c1 +. c2.b)))
  | C' c1, C' c2 -> C' (add' c1 c2)

let sub (c1: c) (c2: c): c = match (c1, c2) with
  | C c1, C c2 -> C (sub_ c1 c2)
  | C' c1, C c2 -> C (init ((c1.r -. r c2), (c1.g -. g c2), (c1.b -. b c2)))
  | C c1, C' c2 -> C (init ((r c1 -. c2.r), (g c1 -. c2.g), (b c1 -. c2.b)))
  | C' c1, C' c2 -> C' (sub' c1 c2)

let mul_scalar (c: c) (v: float): c = match c with
  | C c -> C (mul_scalar_ c v)
  | C' c -> C' (mul_scalar' c v)

let mul (c1: c) (c2: c): c = match (c1, c2) with
  | C c1, C c2 -> C (mul_ c1 c2)
  | C' c1, C c2 -> C (init ((c1.r *. r c2), (c1.g *. g c2), (c1.b *. b c2)))
  | C c1, C' c2 -> C (init ((r c1 *. c2.r), (g c1 *. c2.g), (b c1 *. c2.b)))
  | C' c1, C' c2 -> C' (mul' c1 c2)
