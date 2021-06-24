type t = float * float * float

let horizontal_op op (x1, y1, z1) (x2, y2, z2) = op x1 x2, op y1 y2, op z1 z2
let add = horizontal_op ( +. )
let sub = horizontal_op ( -. )
let mul = horizontal_op ( *. )
let div = horizontal_op ( /. )
let negate = horizontal_op ( *. ) (-1., -1., -1.)
let map f (x, y, z) = f x, f y, f z

let equal p1 p2 =
  match horizontal_op ( = ) p1 p2 with
  | true, true, true -> true
  | _                -> false

let rotate_x theta (x, y, z) =
  let s = Float.sin theta in
  let c = Float.cos theta in
  let y' = (y *. c) -. (z *. s) in
  let z' = (z *. c) +. (y *. s) in
  x, y', z'

let rotate_y theta (x, y, z) =
  let s = Float.sin theta in
  let c = Float.cos theta in
  let x' = (x *. c) +. (z *. s) in
  let z' = (z *. c) -. (x *. s) in
  x', y, z'

let rotate_z theta (x, y, z) =
  let s = Float.sin theta in
  let c = Float.cos theta in
  let x' = (x *. c) -. (y *. s) in
  let y' = (y *. c) +. (x *. s) in
  x', y', z

let rotate (tx, ty, tz) p = rotate_x tx p |> rotate_y ty |> rotate_z tz
let rotate_about_pt r pivot p = add p pivot |> rotate r |> add (negate pivot)
let norm (x, y, z) = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z))

let normalize ((x, y, z) as p) =
  let n = norm p in
  if n > 0. then x /. n, y /. n, z /. n else p

let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let cross (x1, y1, z1) (x2, y2, z2) =
  (y1 *. z2) -. (z1 *. y2), (z1 *. x2) -. (x1 *. z2), (x1 *. y2) -. (y1 *. x2)

let get_x (x, _, _) = x
let get_y (_, y, _) = y
let get_z (_, _, z) = z
let to_string (x, y, z) = Printf.sprintf "[%f, %f, %f]" x y z
let deg_of_rad t = map (fun r -> 180.0 *. r /. Float.pi) t
let rad_of_deg t = map (fun d -> d *. Float.pi /. 180.) t
let ( <+> ) = add
let ( <-> ) = sub
let ( <*> ) = mul
let ( </> ) = div
let to_vec2 (x, y, _) = x, y
