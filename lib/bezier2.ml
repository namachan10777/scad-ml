open Vec
include Bezier.Make (Vec2)

let line_intersection ~(line : Vec2.line) ps =
  let ps = Array.of_list ps in
  let n = Array.length ps - 1 in
  let bez_coefs = coefs' ps
  and normal = v2 (line.a.y -. line.b.y) (line.b.x -. line.a.x) in
  let f i =
    if i = n
    then Vec2.(dot (sub bez_coefs.(0) line.a) normal)
    else Vec2.dot bez_coefs.(n - i) normal
  in
  let roots = Math.real_roots @@ Array.init (n + 1) f in
  let f i acc = if roots.(i) >= 0. && roots.(i) <= 1. then roots.(i) :: acc else acc in
  Util.fold_init (Array.length roots) f []

let translate p t u = Vec2.translate p (t u)
let rotate ?about r t u = Vec2.rotate ?about r (t u)
let[@inline] zrot ?about r t u = rotate ?about r t u
let affine m t u = Affine2.transform m (t u)
let affine3 m t u = Affine3.transform m (Vec3.of_vec2 (t u))
let quaternion ?about q t u = Quaternion.transform ?about q (Vec3.of_vec2 (t u))
let axis_rotate ?about ax a = quaternion ?about (Quaternion.make ax a)
let scale s t u = Vec2.scale s (t u)
let mirror ax t u = Vec2.mirror ax (t u)
