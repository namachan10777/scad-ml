type t = float * float * float * float

let id = 0., 0., 0., 1.

let make ax angle =
  let x, y, z = Vec3.normalize ax in
  let s = Float.sin (angle /. 2.) in
  x *. s, y *. s, z *. s, Float.cos (angle /. 2.)

let basic_op op (x1, y1, z1, w1) (x2, y2, z2, w2) = op x1 x2, op y1 y2, op z1 z2, op w1 w2
let add = basic_op ( +. )
let sub = basic_op ( -. )
let add_scalar (x, y, z, w) s = x, y, z, w +. s
let sub_scalar (x, y, z, w) s = x, y, z, w -. s
let scalar_sub_quat (x, y, z, w) s = -.x, -.y, -.z, s -. w

let mul (x1, y1, z1, w1) (x2, y2, z2, w2) =
  let x = (y1 *. z2) -. (z1 *. y2) +. (w2 *. x1) +. (w1 *. x2)
  and y = (z1 *. x2) -. (x1 *. z2) +. (w2 *. y1) +. (w1 *. y2)
  and z = (x1 *. y2) -. (y1 *. x2) +. (w2 *. z1) +. (z2 *. w1)
  and w = (w1 *. w2) -. (x1 *. x2) -. (y1 *. y2) -. (z1 *. z2) in
  x, y, z, w

let mul_scalar (x, y, z, w) s = x *. s, y *. s, z *. s, w *. s
let div_scalar (x, y, z, w) s = x /. s, y /. s, z /. s, w /. s
let negate q = mul_scalar q (-1.)
let norm (x, y, z, w) = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z) +. (w *. w))

let normalize t =
  let mag = norm t in
  if mag > 0. then div_scalar t mag else t

let dot (x1, y1, z1, w1) (x2, y2, z2, w2) =
  (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2) +. (w1 *. w2)

let conj (x, y, z, w) = -.x, -.y, -.z, w
let distance a b = norm (sub a b)

let of_rotmatrix m =
  let g = RotMatrix.get m in
  match RotMatrix.trace m with
  | tr when tr > 0. ->
    let s = Float.sqrt (tr +. 1.) *. 2. in
    let w = 0.25 *. s
    and x = (g 2 1 -. g 1 2) /. s
    and y = (g 0 2 -. g 2 0) /. s
    and z = (g 1 0 -. g 0 1) /. s in
    x, y, z, w
  | _ when g 0 0 > g 1 1 && g 0 0 > g 2 2 ->
    let s = Float.sqrt (1. +. g 0 0 -. g 1 1 -. g 2 2) *. 2. in
    let w = (g 2 1 -. g 1 2) /. s
    and x = 0.25 *. s
    and y = (g 0 1 +. g 1 0) /. s
    and z = (g 0 2 +. g 2 0) /. s in
    x, y, z, w
  | _ when g 1 1 > g 2 2 ->
    let s = Float.sqrt (1. +. g 1 1 -. g 0 0 -. g 2 2) *. 2. in
    let w = (g 0 2 -. g 2 0) /. s
    and x = (g 0 1 +. g 1 0) /. s
    and y = 0.25 *. s
    and z = (g 1 2 +. g 2 1) /. s in
    x, y, z, w
  | _ ->
    let s = Float.sqrt (1. +. g 2 2 -. g 0 0 -. g 1 1) *. 2. in
    let w = (g 1 0 -. g 0 1) /. s
    and x = (g 0 2 +. g 2 0) /. s
    and y = (g 1 2 +. g 2 1) /. s
    and z = 0.25 *. s in
    x, y, z, w

let to_multmatrix (x, y, z, w) =
  let s =
    let len_sqr = (x *. x) +. (y *. y) +. (z *. z) +. (w *. w) in
    if len_sqr != 0. then 2. /. len_sqr else 0.
  in
  let ((_, _, zs) as xyzs) = Vec3.map (( *. ) s) (x, y, z) in
  let xsw, ysw, zsw = Vec3.map (( *. ) w) xyzs in
  let xsx, ysx, zsx = Vec3.map (( *. ) x) xyzs
  and _, ysy, zsy = Vec3.map (( *. ) y) xyzs
  and zsz = z *. zs in
  MultMatrix.of_row_list_exn
    [ 1. -. ysy -. zsz, ysx -. zsw, zsx +. ysw, 0.
    ; ysx +. zsw, 1. -. xsx -. zsz, zsy -. xsw, 0.
    ; zsx -. ysw, zsy +. xsw, 1. -. xsx -. ysy, 0.
    ; 0., 0., 0., 1.
    ]

let to_string (x, y, z, w) = Printf.sprintf "[%f, %f, %f, %f]" x y z w
let get_x (x, _, _, _) = x
let get_y (_, y, _, _) = y
let get_z (_, _, z, _) = z
let get_w (_, _, _, w) = w
