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
  let roots = Math.real_roots' @@ Array.init (n + 1) f in
  let f i acc = if roots.(i) >= 0. && roots.(i) <= 1. then roots.(i) :: acc else acc in
  Util.fold_init (Array.length roots) f []
