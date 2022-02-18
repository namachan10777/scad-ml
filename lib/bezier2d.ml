include Bezier.Make (Vec2)

let line_intersection ps (x1, y1) (x2, y2) =
  let ps = Array.of_list ps in
  let n = Array.length ps - 1 in
  let bez_coefs = coefs' ps
  and normal = y1 -. y2, x2 -. x1 in
  let f i =
    if i = n
    then Vec2.(dot (sub bez_coefs.(0) (x1, y1)) normal)
    else Vec2.dot bez_coefs.(n - i) normal
  in
  let roots = Math.real_roots' @@ Array.init (n + 1) f in
  let f i acc = if roots.(i) >= 0. && roots.(i) <= 1. then roots.(i) :: acc else acc in
  Util.fold_init (Array.length roots) f []
