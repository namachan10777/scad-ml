type t = Vec3.t list

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

let helix ?fn ?fa ?fs ?(left = true) ~n_turns ~pitch ?r2 r1 =
  let r2 = Option.value ~default:r1 r2 in
  let n_frags = Util.helical_fragments ?fn ?fa ?fs (Float.max r1 r2) in
  let r_step = (r2 -. r1) /. Float.of_int (n_turns * n_frags)
  and h_step = pitch /. Float.of_int n_frags
  and a_step = 2. *. Float.pi /. Float.of_int n_frags *. if left then -1. else 1. in
  let f i =
    let i = Float.of_int i in
    let r = r1 +. (r_step *. i)
    and a = a_step *. i in
    Float.(r *. cos a, r *. sin a, h_step *. i)
  in
  List.init ((n_frags * n_turns) + 1) f

let scaler ~len (x, y) =
  let step = Vec3.map (fun a -> (a -. 1.) /. Float.of_int len) (x, y, 1.) in
  fun i -> MultMatrix.scaling @@ Vec3.map (fun a -> (a *. Float.of_int i) +. 1.) step

let twister ~len r =
  let step = r /. Float.of_int len in
  fun i -> Quaternion.(to_multmatrix @@ make (0., 0., 1.) (step *. Float.of_int i))

let to_transforms ?(euler = false) ?scale ?twist path =
  let p = Array.of_list path in
  let len = Array.length p
  and id _ = MultMatrix.id in
  let scale = Util.value_map_opt ~default:id (scaler ~len) scale
  and twist = Util.value_map_opt ~default:id (twister ~len) twist
  and transform =
    let diff i =
      let sub a b = Vec3.(p.(a) <-> p.(b)) in
      if i = 0 then sub 1 0 else if i = len - 1 then sub i (i - 1) else sub (i + 1) (i - 1)
    in
    if euler
    then (
      let adjust =
        let a = Float.pi /. 2. in
        Quaternion.(to_multmatrix @@ of_euler (a, 0., a))
      in
      fun i ->
        let dx, dy, dz = diff i in
        let ay = Float.atan2 dz (Float.sqrt ((dx *. dx) +. (dy *. dy)))
        and az = Float.atan2 dy dx in
        MultMatrix.mul
          Quaternion.(to_multmatrix ~trans:p.(i) (of_euler (0., ay, az)))
          adjust )
    else
      fun i ->
      let tangent = Vec3.normalize (diff i) in
      Quaternion.(to_multmatrix ~trans:p.(i) @@ alignment (0., 0., 1.) tangent)
  in
  let f i = scale i |> MultMatrix.mul (twist i) |> MultMatrix.mul (transform i) in
  List.init len f
