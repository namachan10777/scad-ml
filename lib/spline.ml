(* https://github.com/Simsso/Online-Tools/blob/master/src/page/adapter/cubic-spline-interpolation.js#L252 *)

type boundary =
  [ `Quadratic
  | `NotAKnot
  | `Periodic
  | `Natural
  ]

(* Reduced row echelon form
   Taken from https://rosettacode.org/wiki/Reduced_row_echelon_form *)
let rref m =
  let nr, nc = Array.length m, Array.length m.(0) in
  let add r s k =
    for i = 0 to nc - 1 do
      m.(r).(i) <- m.(r).(i) +. (m.(s).(i) *. k)
    done
  in
  for c = 0 to min (nc - 1) (nr - 1) do
    for r = c + 1 to nr - 1 do
      if abs_float m.(c).(c) < abs_float m.(r).(c)
      then (
        let v = m.(r) in
        m.(r) <- m.(c);
        m.(c) <- v )
    done;
    let t = m.(c).(c) in
    if t <> 0.0
    then begin
      for r = 0 to nr - 1 do
        if r <> c then add r c (-.m.(r).(c) /. t)
      done;
      for i = 0 to nc - 1 do
        m.(c).(i) <- m.(c).(i) /. t
      done
    end
  done

(* https://github.com/Simsso/Online-Tools/blob/master/src/page/logic/cubic-spline-interpolation.js *)
let interpolate ?(_boundary = `Natural) ps =
  let ps = Array.of_list ps
  and row = ref 0 in
  let len = Array.length ps in
  let solution_idx = (len - 1) * 4 in
  let m = Array.make_matrix solution_idx solution_idx 0. in
  (* splines through p equations *)
  for n = 0 to len - 1 do
    let r = m.(!row)
    and n4 = n * 4
    and x0, y0 = ps.(n)
    and x1, y1 = ps.(n + 1) in
    let () =
      r.(n4) <- Float.pow x0 3.;
      r.(n4 + 1) <- Float.pow x0 2.;
      r.(n4 + 2) <- x0;
      r.(n4 + 3) <- 1.;
      r.(solution_idx) <- y0;
      incr row
    in
    let r = m.(!row) in
    r.(n4) <- Float.pow x1 3.;
    r.(n4 + 1) <- Float.pow x1 2.;
    r.(n4 + 2) <- x1;
    r.(n4 + 3) <- 1.;
    r.(solution_idx) <- y1;
    incr row
  done;
  (* second derivative *)
  (* boundary conditions *)
  ()
