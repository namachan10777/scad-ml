let sign a = Float.(of_int @@ compare a 0.)
let clamp ~min ~max a = Float.min (Float.max min a) max
let lerp a b u = ((1. -. u) *. a) +. (u *. b)

let lerpn ?(endpoint = true) a b n =
  let d = Float.of_int @@ if endpoint then n - 1 else n in
  List.init n (fun i ->
      let u = Float.of_int i /. d in
      lerp a b u )
