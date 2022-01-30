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
  if len < 2 then failwith "Invalid path (too few points).";
  let scale = Util.value_map_opt ~default:id (scaler ~len) scale
  and twist = Util.value_map_opt ~default:id (twister ~len) twist
  and transform =
    if euler
    then (
      let m = Quaternion.(to_multmatrix @@ of_euler Float.(pi /. 2., 0., pi /. 2.)) in
      fun i ->
        let dx, dy, dz =
          if i = 0
          then Vec3.(p.(1) <-> p.(0))
          else if i = len - 1
          then Vec3.(p.(i) <-> p.(i - 1))
          else Vec3.(p.(i + 1) <-> p.(i - 1))
        in
        let ay = Float.atan2 dz (Float.sqrt ((dx *. dx) +. (dy *. dy)))
        and az = Float.atan2 dy dx in
        MultMatrix.mul Quaternion.(to_multmatrix ~trans:p.(i) (of_euler (0., -.ay, az))) m
      )
    else (
      let accum_qs =
        let local i =
          let p1 = p.(i)
          and p2 = p.(i + 1)
          and p3 = p.(i + 2) in
          Quaternion.alignment Vec3.(normalize (p2 <-> p1)) Vec3.(normalize (p3 <-> p2))
        in
        match List.init (len - 2) local with
        | []       -> [| Quaternion.id |]
        | [ q ]    -> [| q; Quaternion.id |]
        | hd :: tl ->
          let f (acc, qs) m =
            let q = Quaternion.mul m acc in
            q, q :: qs
          in
          let _, qs = List.fold_left f (hd, [ hd; Quaternion.id ]) tl in
          Util.array_of_list_rev qs
      in
      let init =
        let cardinal =
          (* Determine an appropriate axis to pre-align the 2d shape with
                 (from normal of (0., 0., 1.)), BEFORE alignment with the initial
                 tangent of the path. Adjust for sign of major axes to prevent
                 inconsistent flipping. *)
          let similarity a b = Vec3.dot a b /. Vec3.(norm a *. norm b)
          and n = Vec3.(normalize (p.(1) <-> p.(0))) in
          let z = similarity n (0., 0., 1.)
          and x = similarity n (1., 0., 0.)
          and y = similarity n (0., 1., 0.) in
          let abs_x = Float.abs x
          and abs_y = Float.abs y
          and abs_z = Float.abs z
          and sgn_x = Util.sign x
          and sgn_y = Util.sign y
          and sgn_z = Util.sign z in
          let comp a b =
            if Float.compare (Float.abs (a -. b)) 0.01 = 1 then Float.compare a b else 0
          in
          match comp abs_x abs_y, comp abs_x abs_z, comp abs_y abs_z with
          | 1, 1, _   -> sgn_x, 0., 0. (* x-axis *)
          | -1, _, 1  -> 0., sgn_y, 0. (* y-axis *)
          | 0, -1, -1 -> 0., 0., sgn_z (* xy equal, but less than z *)
          | 0, _, _   -> 0., sgn_y, 0. (* xy equal, roughly following plane *)
          | _         -> 0., 0., sgn_z
        in
        let d = Vec3.normalize Vec3.(p.(1) <-> p.(0)) in
        MultMatrix.mul
          Quaternion.(to_multmatrix @@ alignment cardinal d)
          Quaternion.(to_multmatrix @@ alignment (0., 0., 1.) cardinal)
      in
      fun i ->
        if i = 0
        then MultMatrix.(mul (translation p.(0)) init)
        else MultMatrix.mul Quaternion.(to_multmatrix ~trans:p.(i) accum_qs.(i - 1)) init
      )
  in
  let f i = scale i |> MultMatrix.mul (twist i) |> MultMatrix.mul (transform i) in
  List.init len f
