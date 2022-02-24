type t = float * float

type line =
  { a : t
  ; b : t
  }

let zero = 0., 0.
let horizontal_op op (x1, y1) (x2, y2) = op x1 x2, op y1 y2
let add = horizontal_op ( +. )
let sub = horizontal_op ( -. )
let mul = horizontal_op ( *. )
let div = horizontal_op ( /. )
let negate = horizontal_op ( *. ) (-1., -1.)
let map f (x, y) = f x, f y
let add_scalar t s = map (( +. ) s) t
let sub_scalar t s = sub t (s, s)
let mul_scalar t s = map (( *. ) s) t
let div_scalar t s = div t (s, s)

let equal a b =
  match horizontal_op ( = ) a b with
  | true, true -> true
  | _          -> false

let norm (x, y) = Float.sqrt ((x *. x) +. (y *. y))
let distance a b = norm (sub a b)
let approx ?(eps = Util.epsilon) a b = Float.(compare (distance a b) eps) < 1

let normalize ((x, y) as t) =
  let n = norm t in
  if n > 0. then x /. n, y /. n else t

let dot (x1, y1) (x2, y2) = (x1 *. x2) +. (y1 *. y2)
let cross (x1, y1) (x2, y2) = 0., 0., (x1 *. y2) -. (y1 *. x2)

let mean l =
  let n, sum = List.fold_left (fun (i, s) t -> i + 1, add t s) (0, zero) l in
  map (fun a -> a /. Float.of_int n) sum

let lerp a b u = add (mul_scalar a (1. -. u)) (mul_scalar b u)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then n - 1 else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )

let angle a b = Float.acos (Math.clamp ~min:(-1.) ~max:1. (dot a b /. (norm a *. norm b)))
let angle_points a b c = angle (sub a b) (sub c b)

let clockwise_sign ?(eps = Util.epsilon) a b c =
  let ba = sub b a
  and cb = sub c b in
  let _, _, z = cross ba cb in
  if Float.abs z <= eps *. norm ba *. norm cb then 0. else Math.sign z

let collinear p1 p2 p3 =
  let a = distance p1 p2
  and b = distance p2 p3
  and c = distance p3 p1 in
  a +. b < c || b +. c < a || c +. a < b

let distance_to_vector p v = norm (sub p (mul_scalar v (dot p v)))
let left_of_line ?eps ~line t = clockwise_sign ?eps t line.b line.a

let closest_simplex1 ?(eps = Util.epsilon) p1 p2 =
  if norm (sub p2 p1) <= eps *. (norm p1 +. norm p2) /. 2.
  then p1, [ p1 ]
  else (
    let c = sub p2 p1 in
    let t = -1. *. dot p1 c /. dot c c in
    if t < 0.
    then p1, [ p1 ]
    else if t > 1.
    then p2, [ p2 ]
    else add p1 (mul_scalar c t), [ p1; p2 ] )

let line_closest_point ?(bounds = false, false) ~line t =
  match bounds with
  | false, false ->
    let n = normalize (sub line.a line.b) in
    add line.b (mul_scalar n (dot (sub t line.b) n))
  | true, true   -> add t (fst @@ closest_simplex1 (sub line.a t) (sub line.b t))
  | b1, b2       ->
    let line = if b1 && not b2 then line else { a = line.b; b = line.a } in
    let seg_vec = normalize (sub line.b line.a) in
    let projection = dot (sub t line.a) seg_vec in
    if projection <= 0. then line.a else add line.a (mul_scalar seg_vec projection)

let distance_to_line ?(bounds = false, false) ~line t =
  match bounds with
  | false, false -> distance_to_vector (sub t line.a) (normalize (sub line.b line.a))
  | bounds       -> norm (sub t (line_closest_point ~bounds ~line t))

let line_intersection
    ?(eps = Util.epsilon)
    ?(bounds1 = false, false)
    ?(bounds2 = false, false)
    l1
    l2
  =
  let d1 = sub l1.a l1.b
  and d2 = sub l2.a l2.b in
  let _, _, denominator = cross d1 d2 in
  if Math.approx ~eps denominator 0.
  then None
  else (
    let v = sub l1.a l2.a in
    let a_frac =
      let _, _, num = cross v d2 in
      num /. denominator
    and b_frac =
      let _, _, num = cross v d1 in
      num /. denominator
    in
    let good =
      let bn_a1, bn_a2 = bounds1
      and bn_b1, bn_b2 = bounds2 in
      ((not bn_a1) || a_frac >= 0. -. eps)
      && ((not bn_a2) || a_frac <= 1. +. eps)
      && ((not bn_b1) || b_frac >= 0. -. eps)
      && ((not bn_b2) || b_frac <= 1. +. eps)
    in
    if good then Some (add l1.a (mul_scalar (sub l1.b l1.a) a_frac)) else None )

let line_normal (x1, y1) (x2, y2) = normalize (y1 -. y2, x2 -. x1)
let get_x (x, _) = x
let get_y (_, y) = y
let get_z _ = 0.
let get_xy = Fun.id
let to_string (x, y) = Printf.sprintf "[%f, %f]" x y
let deg_of_rad t = map Math.deg_of_rad t
let rad_of_deg t = map Math.rad_of_deg t
let ( <+> ) = add
let ( <-> ) = sub
let ( <*> ) = mul
let ( </> ) = div
let of_vec3 (x, y, _) = x, y
let to_vec3 ?(z = 0.) (x, y) = x, y, z

let rotate theta (x, y) =
  let s = Float.sin theta
  and c = Float.cos theta in
  let x' = (x *. c) -. (y *. s)
  and y' = (y *. c) +. (x *. s) in
  x', y'

let rotate_about_pt r pivot t = add t pivot |> rotate r |> add (negate pivot)
let translate = add
let scale = mul
let mirror ax t = sub t (mul_scalar ax (2. *. (dot t ax /. dot ax ax)))
