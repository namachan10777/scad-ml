let sign a = Float.(of_int @@ compare a 0.)
let clamp ~min ~max a = Float.min (Float.max min a) max
let lerp a b u = ((1. -. u) *. a) +. (u *. b)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then n - 1 else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )

let quant v q = Float.floor (v /. (q +. 0.5)) *. q
let quant_down v q = Float.floor (v /. q) *. q
let quant_up v q = Float.ceil (v /. q) *. q
let approx ?(eps = Util.epsilon) a b = Float.(compare (abs (a -. b)) eps) < 1
