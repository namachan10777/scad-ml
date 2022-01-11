(* https://github.com/Simsso/Online-Tools/blob/master/src/page/adapter/cubic-spline-interpolation.js#L252 *)

type boundary =
  [ `Quadratic
  | `NotAKnot
  | `Periodic
  | `Natural
  ]

type coefs =
  { a : float
  ; b : float
  ; c : float
  ; d : float
  }

module RangeMap = Map.Make (struct
  type t = float * float

  let compare (x1, _) (x2, _) = Float.compare x1 x2
end)

type t = coefs RangeMap.t

(* Reduced row echelon form
   Taken from https://rosettacode.org/wiki/Reduced_row_echelon_form *)
let rref m =
  let n_row, n_col = Array.length m, Array.length m.(0) in
  let add r s k =
    for i = 0 to n_col - 1 do
      m.(r).(i) <- m.(r).(i) +. (m.(s).(i) *. k)
    done
  in
  for c = 0 to min (n_col - 1) (n_row - 1) do
    for r = c + 1 to n_row - 1 do
      if Float.abs m.(c).(c) < Float.abs m.(r).(c)
      then (
        let v = m.(r) in
        m.(r) <- m.(c);
        m.(c) <- v )
    done;
    let t = m.(c).(c) in
    if t <> 0.0
    then (
      for r = 0 to n_row - 1 do
        if r <> c then add r c (-.m.(r).(c) /. t)
      done;
      for i = 0 to n_col - 1 do
        m.(c).(i) <- m.(c).(i) /. t
      done )
  done

(* https://github.com/Simsso/Online-Tools/blob/master/src/page/logic/cubic-spline-interpolation.js *)
let fit ?(boundary = `Natural) ps =
  let ps = List.sort_uniq (fun (x1, _) (x2, _) -> Float.compare x1 x2) ps |> Array.of_list
  and row = ref 0 in
  let len = Array.length ps in
  let solution_idx = (len - 1) * 4 in
  let nth = solution_idx - 1
  and m = Array.make_matrix solution_idx solution_idx 0. in
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
  (* first derivative *)
  for n = 0 to len - 2 do
    let x1, _ = ps.(n + 1)
    and r = m.(!row)
    and n4 = n * 4 in
    r.(n4) <- 3. *. Float.pow x1 2.;
    r.(n4 + 1) <- 2. *. x1;
    r.(n4 + 2) <- 1.;
    r.(n4 + 4) <- -3. *. Float.pow x1 2.;
    r.(n4 + 5) <- -2. *. x1;
    r.(n4 + 6) <- -1.;
    incr row
  done;
  (* second derivative *)
  for n = 0 to len - 2 do
    let x1, _ = ps.(n + 1)
    and r = m.(!row)
    and n4 = n * 4 in
    r.(n4) <- 6. *. x1;
    r.(n4 + 1) <- 2.;
    r.(n4 + 4) <- -6. *. x1;
    r.(n4 + 5) <- -2.;
    incr row
  done;
  (* boundary conditions *)
  let () =
    match boundary with
    | `Quadratic ->
      (* first and last spline quadratic *)
      m.(!row).(0) <- 1.;
      incr row;
      m.(!row).(solution_idx - 4) <- 1.
    | `NotAKnot  ->
      let r = m.(!row) in
      let () =
        r.(0) <- 1.;
        r.(4) <- -1.;
        incr row
      in
      let r = m.(!row) in
      r.(solution_idx - 8) <- 1.;
      r.(solution_idx - 4) <- -1.
    | `Periodic  ->
      (* first derivative of first and last point equal *)
      let r = m.(!row)
      and x0, _ = ps.(0)
      and xn, _ = ps.(len - 1) in
      let () =
        r.(0) <- 3. *. Float.pow x0 2.;
        r.(1) <- 2. *. x0;
        r.(2) <- 1.;
        r.(solution_idx - 4) <- -3. *. Float.pow xn 2.;
        r.(solution_idx - 3) <- -2. *. xn;
        r.(solution_idx - 2) <- -1.;
        incr row
      in
      (* second derivative of first and last point equal *)
      let r = m.(!row) in
      r.(0) <- 6. *. x0;
      r.(1) <- 2.;
      r.(solution_idx - 4) <- -6. *. xn;
      r.(solution_idx - 3) <- -2.
    | `Natural   ->
      let r = m.(!row)
      and x0, _ = ps.(0)
      and xn, _ = ps.(len - 1) in
      let () =
        r.(0) <- 6. *. x0;
        r.(1) <- 2.;
        incr row
      in
      let r = m.(!row) in
      r.(solution_idx - 4) <- 6. *. xn;
      r.(solution_idx - 3) <- 2.
  in
  let f (i, t) (x, _) =
    let idx = i * 4
    and next = i + 1 in
    let coefs =
      { a = m.(idx).(nth)
      ; b = m.(idx + 1).(nth)
      ; c = m.(idx + 2).(nth)
      ; d = m.(idx + 3).(nth)
      }
    in
    next, RangeMap.add (x, fst ps.(next)) coefs t
  in
  snd @@ Array.fold_left f (0, RangeMap.empty) ps

let extrapolate t xs =
  let f x =
    RangeMap.find_first_opt (fun (xmin, xmax) -> x >= xmin && x <= xmax) t
    |> Option.map (fun _ { a; b; c; d } ->
           (a *. x *. x *. x *. +.b *. x *. x) +. (c *. x) +. d )
  in
  List.filter_map f xs

let interpolate t n =
  let (xmin, _), _ = RangeMap.min_binding t
  and (_, xmax), _ = RangeMap.max_binding t in
  let step = (xmax -. xmin) /. Float.of_int (n - 1) in
  extrapolate t (List.init n (fun i -> xmin +. (Float.of_int i *. step)))
