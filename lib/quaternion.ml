type t =
  { x : float
  ; y : float
  ; z : float
  ; w : float
  }

let id = { x = 0.; y = 0.; z = 0.; w = 1. }
let coefficients { x; y; z; w } = x, y, z, w

let make ax angle =
  let x, y, z = Vec3.normalize ax in
  let s = Float.sin (angle /. 2.) in
  { x = x *. s; y = y *. s; z = z *. s; w = Float.cos (angle /. 2.) }

let basic_op op a b = { x = op a.x b.x; y = op a.y b.y; z = op a.z b.z; w = op a.w b.w }
let add = basic_op ( +. )
let sub = basic_op ( -. )
let add_scalar t s = { t with w = t.w +. s }
let sub_scalar t s = { t with w = t.w -. s }
let scalar_sub_quat t s = { x = -.t.x; y = -.t.y; z = -.t.z; w = s -. t.w }

let mul a b =
  let x = (a.y *. b.z) -. (a.z *. b.y) +. (b.w *. a.x) +. (a.w *. b.x)
  and y = (a.z *. b.x) -. (a.x *. b.z) +. (b.w *. a.y) +. (a.w *. b.y)
  and z = (a.x *. b.y) -. (a.y *. b.x) +. (b.w *. a.z) +. (b.z *. a.w)
  and w = (a.w *. b.w) -. (a.x *. b.x) -. (a.y *. b.y) -. (a.z *. b.z) in
  { x; y; z; w }

let mul_scalar t s = { x = t.x *. s; y = t.y *. s; z = t.z *. s; w = t.w *. s }
let div_scalar t s = { x = t.x /. s; y = t.y /. s; z = t.z /. s; w = t.w /. s }
let negate q = mul_scalar q (-1.)
let norm { x; y; z; w } = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z) +. (w *. w))

let normalize t =
  let n = norm t in
  if n > 0. then div_scalar t n else t

let dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z) +. (a.w *. b.w)
let conj t = { x = -.t.x; y = -.t.y; z = -.t.z; w = t.w }
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
    { x; y; z; w }
  | _ when g 0 0 > g 1 1 && g 0 0 > g 2 2 ->
    let s = Float.sqrt (1. +. g 0 0 -. g 1 1 -. g 2 2) *. 2. in
    let w = (g 2 1 -. g 1 2) /. s
    and x = 0.25 *. s
    and y = (g 0 1 +. g 1 0) /. s
    and z = (g 0 2 +. g 2 0) /. s in
    { x; y; z; w }
  | _ when g 1 1 > g 2 2 ->
    let s = Float.sqrt (1. +. g 1 1 -. g 0 0 -. g 2 2) *. 2. in
    let w = (g 0 2 -. g 2 0) /. s
    and x = (g 0 1 +. g 1 0) /. s
    and y = 0.25 *. s
    and z = (g 1 2 +. g 2 1) /. s in
    { x; y; z; w }
  | _ ->
    let s = Float.sqrt (1. +. g 2 2 -. g 0 0 -. g 1 1) *. 2. in
    let w = (g 1 0 -. g 0 1) /. s
    and x = (g 0 2 +. g 2 0) /. s
    and y = (g 1 2 +. g 2 1) /. s
    and z = 0.25 *. s in
    { x; y; z; w }

let of_euler (x_roll, y_pitch, z_yaw) =
  let open Float in
  let cy = cos (z_yaw *. 0.5)
  and sy = sin (z_yaw *. 0.5)
  and cp = cos (y_pitch *. 0.5)
  and sp = sin (y_pitch *. 0.5)
  and cr = cos (x_roll *. 0.5)
  and sr = sin (x_roll *. 0.5) in
  let w = (cr *. cp *. cy) +. (sr *. sp *. sy)
  and x = (sr *. cp *. cy) -. (cr *. sp *. sy)
  and y = (cr *. sp *. cy) +. (sr *. cp *. sy)
  and z = (cr *. cp *. sy) -. (sr *. sp *. cy) in
  { x; y; z; w }

let to_rotmatrix { x; y; z; w } =
  let s =
    let len_sqr = (x *. x) +. (y *. y) +. (z *. z) +. (w *. w) in
    if len_sqr != 0. then 2. /. len_sqr else 0.
  in
  let ((_, _, zs) as xyzs) = Vec3.map (( *. ) s) (x, y, z) in
  let xsw, ysw, zsw = Vec3.map (( *. ) w) xyzs in
  let xsx, ysx, zsx = Vec3.map (( *. ) x) xyzs
  and _, ysy, zsy = Vec3.map (( *. ) y) xyzs
  and zsz = z *. zs in
  RotMatrix.of_row_list_exn
    [ 1. -. ysy -. zsz, ysx -. zsw, zsx +. ysw
    ; ysx +. zsw, 1. -. xsx -. zsz, zsy -. xsw
    ; zsx -. ysw, zsy +. xsw, 1. -. xsx -. ysy
    ]

let to_euler t = RotMatrix.to_euler (to_rotmatrix t)
let to_multmatrix ?(trans = Vec3.zero) t = MultMatrix.of_rotmatrix (to_rotmatrix t) trans
let to_string { x; y; z; w } = Printf.sprintf "[%f, %f, %f, %f]" x y z w
let get_x t = t.x
let get_y t = t.y
let get_z t = t.z
let get_w t = t.w

let slerp a b =
  let a = normalize a
  and b = normalize b in
  fun v ->
    let v = if v < 0. then 0. else if v > 1. then 1. else v in
    let compute a' b' d =
      let theta_0 = Float.acos d in
      let sin_theta_0 = Float.sin theta_0 in
      let theta = theta_0 *. v in
      let sin_theta = Float.sin theta in
      let s0 = Float.cos theta -. (d *. sin_theta /. sin_theta_0)
      and s1 = sin_theta /. sin_theta_0 in
      add (mul_scalar a' s0) (mul_scalar b' s1) |> normalize
    in
    (* If dot is negative, slerp won't take shorter path. Fix by reversing one quat.
     *  Dot is constrained for cases using compute, so acos is safe. *)
    match dot a b with
    | d when d < 0. -> compute (negate a) b (-.d)
    | d when d > 0.9995 -> add a (mul_scalar (sub b a) v) |> normalize
    | d -> compute a b d

let rotate_vec3 t (x, y, z) =
  let r = { x; y; z; w = 0. } in
  mul (mul t r) (conj t) |> fun { x; y; z; _ } -> x, y, z

let rotate_vec3_about_pt t p v = Vec3.(rotate_vec3 t (v <+> p) <-> p)

let alignment v1 v2 =
  let dp = Vec3.dot v1 v2 in
  if dp > 0.999999 (* already parallel *)
  then id
  else if dp < -0.999999 (* opposite *)
  then (
    let x_cross = Vec3.cross (1., 0., 0.) v1 in
    let axis =
      Vec3.normalize
      @@ if Vec3.norm x_cross < 0.000001 then Vec3.cross (0., 1., 0.) v1 else x_cross
    in
    make axis Float.pi )
  else (
    let x, y, z = Vec3.(cross v1 v2) in
    let w = Vec3.((norm v1 *. norm v2) +. dot v1 v2) in
    normalize { x; y; z; w } )
