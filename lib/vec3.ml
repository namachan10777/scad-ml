type t = Vec.v3 =
  { x : float
  ; y : float
  ; z : float
  }

type line =
  { a : t
  ; b : t
  }

type bbox =
  { min : t
  ; max : t
  }

let zero = { x = 0.; y = 0.; z = 0. }
let v = Vec.v3
let of_tup (x, y, z) = { x; y; z }
let to_tup { x; y; z } = x, y, z
let horizontal_op op a b = v (op a.x b.x) (op a.y b.y) (op a.z b.z)
let add = horizontal_op ( +. )
let sub = horizontal_op ( -. )
let mul = horizontal_op ( *. )
let div = horizontal_op ( /. )
let negate t = v (t.x *. -1.) (t.y *. -1.) (t.z *. -1.)
let map f { x; y; z } = v (f x) (f y) (f z)
let sadd t s = v (t.x +. s) (t.y +. s) (t.z +. s)
let ssub t s = v (t.x -. s) (t.y -. s) (t.z -. s)
let smul t s = v (t.x *. s) (t.y *. s) (t.z *. s)
let sdiv t s = v (t.x /. s) (t.y /. s) (t.z /. s)
let equal a b = Float.equal a.x b.x && Float.equal a.y b.y && Float.equal a.z b.z
let norm { x; y; z } = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z))
let distance a b = norm (sub a b)

let approx ?(eps = Util.epsilon) a b =
  not (Int.equal Float.(compare (distance a b) eps) 1)

let normalize t =
  let n = norm t in
  if n > 0. then sdiv t n else t

let dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z)

let cross a b =
  let x = (a.y *. b.z) -. (a.z *. b.y)
  and y = (a.z *. b.x) -. (a.x *. b.z)
  and z = (a.x *. b.y) -. (a.y *. b.x) in
  { x; y; z }

let mean l =
  let n, sum = List.fold_left (fun (i, s) t -> i + 1, add t s) (0, zero) l in
  sdiv sum (Int.to_float n)

let mean' a =
  let sum = ref zero
  and len = Array.length a in
  for i = 0 to len - 1 do
    sum := add !sum a.(i)
  done;
  sdiv !sum (Int.to_float len)

let lerp a b u = add (smul a (1. -. u)) (smul b u)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then Int.max 1 (n - 1) else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )

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

let distance_to_vector p v = norm (sub p (smul v (dot p v)))

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

let point_on_line ?(eps = Util.epsilon) ?bounds ~line t =
  distance_to_line ?bounds ~line t < eps

let get_x { x; _ } = x
let get_y { y; _ } = y
let get_z { z; _ } = z
let to_string { x; y; z } = Printf.sprintf "[%f, %f, %f]" x y z
let deg_of_rad t = map (fun r -> 180.0 *. r /. Float.pi) t
let rad_of_deg t = map (fun d -> d *. Float.pi /. 180.) t
let ( +@ ) = add
let ( -@ ) = sub
let ( *@ ) = mul
let ( /@ ) = div
let ( +$ ) = sadd
let ( -$ ) = ssub
let ( *$ ) = smul
let ( /$ ) = sdiv
let to_vec2 { x; y; _ } = Vec.v2 x y
let of_vec2 ?(z = 0.) ({ x; y } : Vec.v2) = { x; y; z }

let rotate_x theta { x; y; z } =
  let s = Float.sin theta
  and c = Float.cos theta in
  let y' = (y *. c) -. (z *. s)
  and z' = (z *. c) +. (y *. s) in
  v x y' z'

let rotate_y theta { x; y; z } =
  let s = Float.sin theta
  and c = Float.cos theta in
  let x' = (x *. c) +. (z *. s)
  and z' = (z *. c) -. (x *. s) in
  v x' y z'

let rotate_z theta { x; y; z } =
  let s = Float.sin theta
  and c = Float.cos theta in
  let x' = (x *. c) -. (y *. s)
  and y' = (y *. c) +. (x *. s) in
  v x' y' z

let rotate { x; y; z } t = rotate_x x t |> rotate_y y |> rotate_z z
let rotate_about_pt r pivot t = sub t pivot |> rotate r |> add pivot
let translate = add
let scale = mul
let mirror ax t = sub t (smul ax (2. *. (dot t ax /. dot ax ax)))
let projection { x; y; _ } = { x; y; z = 0. }
