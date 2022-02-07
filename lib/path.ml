module type S = sig
  type vec
  type t = vec list

  val total_travel' : vec array -> float
  val total_travel : vec list -> float
  val cummulative_travel : vec list -> float list
  val to_continuous : vec list -> float -> vec
  val resample : freq:[< `N of int | `Spacing of float ] -> vec list -> vec list
  val prune_colinear' : vec array -> vec array
  val prune_colinear : vec list -> vec list
end

module Make (V : Sigs.Vec) : S with type vec := V.t = struct
  type vec = V.t
  type t = vec list

  let total_travel' path =
    let len = Array.length path
    and p = Array.unsafe_get path in
    if len < 2
    then 0.
    else (
      let sum = ref 0. in
      for i = 0 to len - 2 do
        sum := !sum +. V.distance (p i) (p (i + 1))
      done;
      !sum )

  let total_travel = function
    | [] | [ _ ] -> 0.
    | hd :: tl   ->
      let f (sum, last) p = sum +. V.distance p last, p in
      fst @@ List.fold_left f (0., hd) tl

  let cummulative_travel = function
    | []       -> []
    | hd :: tl ->
      let f (acc, sum, last) p =
        let sum = sum +. V.distance p last in
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
          p := Some V.(mul_scalar (p1 <-> p0) frac <+> p0) )
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

  let prune_colinear_rev' path =
    let len = Array.length path in
    let w = Util.index_wrap ~len in
    let f i acc =
      let p = path.(i) in
      if not (V.colinear path.(w (i - 1)) p path.(w (i + 1))) then p :: acc else acc
    in
    Util.fold_init len f []

  let prune_colinear' path = Util.array_of_list_rev (prune_colinear_rev' path)
  let prune_colinear path = List.rev @@ prune_colinear_rev' (Array.of_list path)
end
