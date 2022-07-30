open Vec
module Bez = Bezier.Make (Vec2)

let n_samples = 11
let step = 1. /. Float.of_int (n_samples - 1)

let newton_raphson drv bez target_x guess_u =
  let rec aux attempts guess =
    let slope = (drv guess).x in
    let guess = guess -. (((bez guess).x -. target_x) /. slope) in
    if Math.approx slope 0. || attempts = 3 then guess else aux (attempts + 1) guess
  in
  aux 0 guess_u

let make p1 p2 =
  let ps = [ v2 0. 0.; p1; p2; v2 1. 1. ] in
  let bez = Bez.make ps
  and drv = Bez.deriv ps in
  let samples = Array.init n_samples (fun i -> (bez (Float.of_int i *. step)).x) in
  let u_of_x x =
    let idx, start =
      let i = ref 1
      and start = ref 0.
      and continue = ref true in
      while !i < n_samples && !continue do
        if samples.(!i) <= x
        then (
          incr i;
          start := !start +. step )
        else continue := false
      done;
      !i - 1, !start
    in
    let guess_u =
      let dist = (x -. samples.(idx)) /. (samples.(idx + 1) -. samples.(idx)) in
      start +. (dist *. step)
    in
    let initial_slope = (drv guess_u).x in
    if initial_slope >= 0.001
    then newton_raphson bez drv x guess_u
    else if initial_slope = 0.
    then guess_u
    else
      Util.bisection
        ~max_iter:10
        ~tolerance:1e-7
        ~lower:start
        ~upper:(start +. step)
        (fun u -> (bez u).x -. x)
  in
  fun x -> if x = 0. then 0. else if x = 1. then 1. else (bez (u_of_x x)).y
