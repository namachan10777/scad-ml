type t = Vec.V2t.t =
  { x : float
  ; y : float
  }

type line =
  { a : t
  ; b : t
  }

let zero = { x = 0.; y = 0. }
let v = Vec.v2
let of_tup (x, y) = { x; y }
let to_tup { x; y } = x, y
let horizontal_op op a b = v (op a.x b.x) (op a.y b.y)
let add = horizontal_op ( +. )
let sub = horizontal_op ( -. )
let mul = horizontal_op ( *. )
let div = horizontal_op ( /. )
let negate t = v (t.x *. -1.) (t.y *. -1.)
let map f { x; y } = v (f x) (f y)
let sadd t s = v (t.x +. s) (t.y +. s)
let ssub t s = v (t.x -. s) (t.y -. s)
let smul t s = v (t.x *. s) (t.y *. s)
let sdiv t s = v (t.x /. s) (t.y /. s)
let equal a b = Float.equal a.x b.x && Float.equal a.y b.y
let norm { x; y } = Float.sqrt ((x *. x) +. (y *. y))
let distance a b = norm (sub a b)
let approx ?(eps = Util.epsilon) a b = Float.(compare (distance a b) eps) < 1

let normalize t =
  let n = norm t in
  if n > 0. then sdiv t n else t

let dot a b = (a.x *. b.x) +. (a.y *. b.y)
let cross a b = Vec.v3 0. 0. ((a.x *. b.y) -. (a.y *. b.x))

let mean l =
  let n, sum = List.fold_left (fun (i, s) t -> i + 1, add t s) (0, zero) l in
  sdiv sum (Int.to_float n)

let lerp a b u = add (smul a (1. -. u)) (smul b u)

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
  let Vec.V3t.{ z; _ } = cross ba cb in
  if Float.abs z <= eps *. norm ba *. norm cb then 0. else Math.sign z

let collinear p1 p2 p3 =
  let a = distance p1 p2
  and b = distance p2 p3
  and c = distance p3 p1 in
  a +. b < c || b +. c < a || c +. a < b

let distance_to_vector p v = norm (sub p (smul v (dot p v)))
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
    else add p1 (smul c t), [ p1; p2 ] )

let line_closest_point ?(bounds = false, false) ~line t =
  match bounds with
  | false, false ->
    let n = normalize (sub line.a line.b) in
    add line.b (smul n (dot (sub t line.b) n))
  | true, true   -> add t (fst @@ closest_simplex1 (sub line.a t) (sub line.b t))
  | b1, b2       ->
    let line = if b1 && not b2 then line else { a = line.b; b = line.a } in
    let seg_vec = normalize (sub line.b line.a) in
    let projection = dot (sub t line.a) seg_vec in
    if projection <= 0. then line.a else add line.a (smul seg_vec projection)

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
  let Vec.V3t.{ z = denominator; _ } = cross d1 d2 in
  if Math.approx ~eps denominator 0.
  then None
  else (
    let v = sub l1.a l2.a in
    let a_frac =
      let Vec.V3t.{ z = num; _ } = cross v d2 in
      num /. denominator
    and b_frac =
      let Vec.V3t.{ z = num; _ } = cross v d1 in
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
    if good then Some (add l1.a (smul (sub l1.b l1.a) a_frac)) else None )

let line_normal a b = normalize (v (a.y -. b.y) (b.x -. a.x))
let get_x { x; _ } = x
let get_y { y; _ } = y
let get_z _ = 0.
let to_vec2 t = t
let to_string { x; y } = Printf.sprintf "[%f, %f]" x y
let deg_of_rad t = map Math.deg_of_rad t
let rad_of_deg t = map Math.rad_of_deg t
let ( +@ ) = add
let ( -@ ) = sub
let ( *@ ) = mul
let ( /@ ) = div
let ( +$ ) = sadd
let ( -$ ) = ssub
let ( *$ ) = smul
let ( /$ ) = sdiv
let of_vec3 Vec.V3t.{ x; y; _ } = { x; y }
let to_vec3 ?(z = 0.) { x; y } = Vec.v3 x y z

let rotate theta { x; y } =
  let s = Float.sin theta
  and c = Float.cos theta in
  let x' = (x *. c) -. (y *. s)
  and y' = (y *. c) +. (x *. s) in
  v x' y'

let rotate_about_pt r pivot t = add t pivot |> rotate r |> add (negate pivot)
let translate = add
let scale = mul
let mirror ax t = sub t (smul ax (2. *. (dot t ax /. dot ax ax)))
