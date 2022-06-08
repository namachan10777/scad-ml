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
let rotate r t u = Vec2.rotate r (t u)
let rotate_about_pt r p t u = Vec2.rotate_about_pt r p (t u)
let multmatrix m t u = MultMatrix.transform m (Vec3.of_vec2 (t u))
let quaternion q t u = Quaternion.rotate_vec3 q (Vec3.of_vec2 (t u))
let quaternion_about_pt q p t u = Quaternion.rotate_vec3_about_pt q p (Vec3.of_vec2 (t u))
let vector_rotate ax a = quaternion (Quaternion.make ax a)
let vector_rotate_about_pt ax a p = quaternion_about_pt (Quaternion.make ax a) p
let scale s t u = Vec2.scale s (t u)
let mirror ax t u = Vec2.mirror ax (t u)
