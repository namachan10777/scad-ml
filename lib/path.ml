let total_travel = function
  | [] | [ _ ] -> 0.
  | hd :: tl   ->
    let f (sum, last) p = sum +. Vec3.distance p last, p in
    fst @@ List.fold_left f (0., hd) tl

let cummulative_travel = function
  | []       -> []
  | hd :: tl ->
    let f (acc, sum, last) p =
      let sum = sum +. Vec3.distance p last in
      sum :: acc, sum, p
    in
    let travels, _, _ = List.fold_left f ([ 0. ], 0., hd) tl in
    List.rev travels

let to_continuous path =
  let travels = Array.of_list (cummulative_travel path)
  and path = Array.of_list path in
  let len = Array.length travels in
  let total = travels.(len - 1) in
  let extrapolate s =
    let d = Float.(min (max s 0.) 1.) *. total in
    let i = ref 0
    and p = ref None in
    while Option.is_none !p && !i < len - 1 do
      let idx = !i in
      let d0 = Array.unsafe_get travels idx
      and d1 = Array.unsafe_get travels (idx + 1) in
      if d >= d0 && d <= d1
      then (
        let frac = (d -. d0) /. (d1 -. d0)
        and p0 = Array.unsafe_get path idx
        and p1 = Array.unsafe_get path (idx + 1) in
        p := Some Vec3.(mul_scalar (p1 <-> p0) frac <+> p0) )
      else incr i
    done;
    Option.get !p
  in
  extrapolate

let of_continuous = Bezier.curve

let resample ~freq path =
  let n =
    match freq with
    | `N n       -> n
    | `Spacing s -> Int.of_float @@ (total_travel path /. s)
  in
  let step = 1. /. Float.of_int (n - 1)
  and f = to_continuous path in
  List.init n (fun i -> f @@ (Float.of_int i *. step))

let to_transforms ?scale ?twist path =
  let p = Array.of_list path in
  let len = Array.length p in
  let scale =
    match scale with
    | Some (x, y) ->
      let step = Vec3.map (fun a -> (a -. 1.) /. Float.of_int len) (x, y, 1.) in
      fun i -> MultMatrix.scaling @@ Vec3.map (fun a -> (a *. Float.of_int i) +. 1.) step
    | None        -> fun _ -> MultMatrix.id
  and twist =
    match twist with
    | Some r ->
      let step = r /. Float.of_int len in
      fun i -> Quaternion.(to_multmatrix @@ make (0., 0., 1.) (step *. Float.of_int i))
    | None   -> fun _ -> MultMatrix.id
  in
  let f i =
    let tangent =
      let open Vec3 in
      ( if i = 0
      then p.(1) <-> p.(0)
      else if i = len - 1
      then p.(i) <-> p.(i - 1)
      else p.(i + 1) <-> p.(i - 1) )
      |> normalize
    in
    scale i
    |> MultMatrix.mul (twist i)
    |> MultMatrix.mul
         Quaternion.(to_multmatrix ~trans:p.(i) @@ alignment (0., 0., 1.) tangent)
  in
  List.init len f
