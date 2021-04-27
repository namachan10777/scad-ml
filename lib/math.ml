let pi = Core.pi

type t = float * float * float

let horizontal_op op p1 p2 =
  match p1, p2 with
  | (x1, y1, z1), (x2, y2, z2) -> op x1 x2, op y1 y2, op z1 z2

let add = horizontal_op ( +. )
let sub = horizontal_op ( -. )
let mul = horizontal_op ( *. )
let div = horizontal_op ( /. )
let negate = horizontal_op ( *. ) (-1., -1., -1.)

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
let rotate_about_pt r o p = sub p o |> rotate r |> add o
