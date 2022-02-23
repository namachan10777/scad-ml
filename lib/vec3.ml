type t = float * float * float

let zero = 0., 0., 0.
let horizontal_op op (x1, y1, z1) (x2, y2, z2) = op x1 x2, op y1 y2, op z1 z2
let add = horizontal_op ( +. )
let sub = horizontal_op ( -. )
let mul = horizontal_op ( *. )
let div = horizontal_op ( /. )
let negate = horizontal_op ( *. ) (-1., -1., -1.)
let map f (x, y, z) = f x, f y, f z
let add_scalar t s = map (( +. ) s) t
let sub_scalar t s = sub t (s, s, s)
let mul_scalar t s = map (( *. ) s) t
let div_scalar t s = div t (s, s, s)

let equal a b =
  match horizontal_op ( = ) a b with
  | true, true, true -> true
  | _                -> false

let norm (x, y, z) = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z))
let distance a b = norm (sub a b)
let approx ?(eps = Util.epsilon) a b = Float.(compare (distance a b) eps) < 1

let normalize ((x, y, z) as t) =
  let n = norm t in
  if n > 0. then x /. n, y /. n, z /. n else t

let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let cross (x1, y1, z1) (x2, y2, z2) =
  (y1 *. z2) -. (z1 *. y2), (z1 *. x2) -. (x1 *. z2), (x1 *. y2) -. (y1 *. x2)

let mean l =
  let n, sum = List.fold_left (fun (i, s) t -> i + 1, add t s) (0, zero) l in
  map (fun a -> a /. Float.of_int n) sum

let angle a b = Float.acos (Math.clamp ~min:(-1.) ~max:1. (dot a b /. (norm a *. norm b)))
let angle_points a b c = angle (sub a b) (sub c b)

let clockwise_sign ?(eps = Util.epsilon) a b c =
  let ba = sub b a
  and cb = sub c b in
  let crx = dot a (cross ba cb) in
  if Float.abs crx <= eps *. norm ba *. norm cb then 0. else Math.sign crx

let collinear p1 p2 p3 =
  let a = distance p1 p2
  and b = distance p2 p3
  and c = distance p3 p1 in
  a +. b < c || b +. c < a || c +. a < b

let lerp a b u = add (mul_scalar a (1. -. u)) (mul_scalar b u)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then n - 1 else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )

let distance_to_vector p v = norm (sub p (mul_scalar v (dot p v)))

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

let line_closest_point ?(bounds = false, false) (p1, p2) t =
  match bounds with
  | false, false ->
    let n = normalize (sub p1 p2) in
    add p2 (mul_scalar n (dot (sub t p2) n))
  | true, true   -> add t (fst @@ closest_simplex1 (sub p1 t) (sub p2 t))
  | b1, b2       ->
    let p1, p2 = if b1 && not b2 then p1, p2 else p2, p1 in
    let seg_vec = normalize (sub p2 p1) in
    let projection = dot (sub t p1) seg_vec in
    if projection <= 0. then p1 else add p1 (mul_scalar seg_vec projection)

let distance_to_line ?(bounds = false, false) ((p1, p2) as line) t =
  match bounds with
  | false, false -> distance_to_vector (sub t p1) (normalize (sub p2 p1))
  | bounds       -> norm (sub t (line_closest_point ~bounds line t))

let get_x (x, _, _) = x
let get_y (_, y, _) = y
let get_z (_, _, z) = z
let get_xy (x, y, _) = x, y
let to_string (x, y, z) = Printf.sprintf "[%f, %f, %f]" x y z
let deg_of_rad t = map (fun r -> 180.0 *. r /. Float.pi) t
let rad_of_deg t = map (fun d -> d *. Float.pi /. 180.) t
let ( <+> ) = add
let ( <-> ) = sub
let ( <*> ) = mul
let ( </> ) = div
let to_vec2 = get_xy
let of_vec2 ?(z = 0.) (x, y) = x, y, z

let rotate_x theta (x, y, z) =
  let s = Float.sin theta
  and c = Float.cos theta in
  let y' = (y *. c) -. (z *. s)
  and z' = (z *. c) +. (y *. s) in
  x, y', z'

let rotate_y theta (x, y, z) =
  let s = Float.sin theta
  and c = Float.cos theta in
  let x' = (x *. c) +. (z *. s)
  and z' = (z *. c) -. (x *. s) in
  x', y, z'

let rotate_z theta (x, y, z) =
  let s = Float.sin theta
  and c = Float.cos theta in
  let x' = (x *. c) -. (y *. s)
  and y' = (y *. c) +. (x *. s) in
  x', y', z

let rotate (rx, ry, rz) t = rotate_x rx t |> rotate_y ry |> rotate_z rz
let rotate_about_pt r pivot t = add t pivot |> rotate r |> add (negate pivot)
let translate = add
let scale = mul
let mirror ax t = sub t (mul_scalar ax (2. *. (dot t ax /. dot ax ax)))
let projection (x, y, _) = x, y, 0.
