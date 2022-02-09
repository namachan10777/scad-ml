let sign a = Float.(of_int @@ compare a 0.)
let clamp ~min ~max a = Float.min (Float.max min a) max
let lerp a b u = ((1. -. u) *. a) +. (u *. b)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then n - 1 else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )

let quant ~q v = Float.floor ((v /. q) +. 0.5) *. q
let quant_down ~q v = Float.floor (v /. q) *. q
let quant_up ~q v = Float.ceil (v /. q) *. q
let approx ?(eps = Util.epsilon) a b = Float.(compare (abs (a -. b)) eps) < 1

let law_of_cosines a b c =
  clamp ~min:(-1.) ~max:1.
  @@ Float.acos (((a *. a) +. (b *. b) -. (c *. c)) /. (2. *. a *. b))

let posmod a m = mod_float (mod_float a m +. m) m
