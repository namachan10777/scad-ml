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
  val deriv : ?closed:bool -> ?h:float -> vec list -> vec list
  val deriv_nonuniform : ?closed:bool -> h:float list -> vec list -> vec list
  val tangents : ?uniform:bool -> ?closed:bool -> vec list -> vec list
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

  let segment_lengths ?(closed = false) = function
    | []       -> []
    | hd :: tl ->
      let f (acc, last) p = V.distance p last :: acc, p in
      let lengths, last = List.fold_left f ([], hd) tl in
      List.rev (if closed then V.distance last hd :: lengths else lengths)

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

  let deriv ?(closed = false) ?(h = 1.) path =
    let path = Array.of_list path in
    let len = Array.length path in
    let f =
      let g i = Array.unsafe_get path (Util.index_wrap ~len i) in
      let calc i = V.(sub (g (i + 1)) (g (i - 1))) in
      if closed
      then calc
      else
        function
        | 0 when len < 3 -> V.sub (g 1) (g 0)
        | 0 -> V.(sub (mul_scalar (sub (g 1) (g 0)) 3.) (sub (g 2) (g 1)))
        | i when i = len - 1 && len < 3 -> V.sub (g (-1)) (g (-2))
        | i when i = len - 1 ->
          V.(sub (sub (g (-3)) (g (-2))) (mul_scalar (sub (g (-2)) (g (-1))) 3.))
        | i -> calc i
    in
    List.init len (fun i -> V.div_scalar (f i) (2. *. h))

  let deriv_nonuniform ?(closed = false) ~h path =
    let path = Array.of_list path
    and h = Array.of_list h in
    let len = Array.length path in
    let valid_h_len = len - Bool.to_int (not closed) in
    if valid_h_len <> Array.length h
    then (
      let msg =
        Printf.sprintf
          "Invalid length of non-uniform sampling parameter `h`. Should be %i."
          valid_h_len
      in
      raise (Invalid_argument msg) );
    let f =
      let w = Util.index_wrap ~len in
      let calc i =
        let h1 = h.(w (i - 1))
        and h2 = h.(i)
        and vc = path.(i)
        and v1 = path.(w (i - 1))
        and v2 = path.(w (i + 1)) in
        let v1 = if h2 < h1 then V.lerp vc v1 (h2 /. h1) else v1
        and v2 = if h1 < h2 then V.lerp vc v2 (h1 /. h2) else v2 in
        V.(div_scalar (sub v2 v1) (2. *. Float.min h1 h2))
      in
      if closed
      then calc
      else
        function
        | 0 -> V.(div_scalar (sub path.(1) path.(0)) h.(0))
        | i when i = len - 1 -> V.(div_scalar (sub path.(i) path.(i - 1)) h.(i - 1))
        | i -> calc i
    in
    List.init len f

  let tangents ?(uniform = true) ?(closed = false) path =
    ( if uniform
    then deriv ~closed path
    else deriv_nonuniform ~closed ~h:(segment_lengths ~closed path) path )
    |> List.map V.normalize
end
