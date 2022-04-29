module type S = sig
  type vec
  type line
  type t = vec list

  (** [length path]

      Calculate the length (total travel distance) of the [path]. *)
  val length : t -> float

  (** [length' path]

      Calculate the length (total travel distance) of the [path]. *)
  val length' : vec array -> float

  (** [cummulative_length path]

      Calculate the cummulative length (distance travelled by each point) along
    the [path].  *)
  val cummulative_length : t -> float list

  (** [to_continuous path]

      Return a continuous function from values over the range of [0.] to [1.]
    to positions along [path] (like a bezier function). *)
  val to_continuous : t -> float -> vec

  (** [resample ~freq path]

      Resample [path] with the given [freq]uency (either a flat number of
    points, or a target point spacing). *)
  val resample : freq:[< `N of int | `Spacing of float ] -> t -> t

  (** [noncollinear_triple ?eps path]

      Returns a triple of non-collinear points from [path] (if the path is not
    completely collinear). Two well separated points are selected, and the third
    point is the furthest off the line drawn by the first two points.*)
  val noncollinear_triple : ?eps:float -> t -> (vec * vec * vec) option

  (** [is_collinear ?eps path]

      Returns [true] if all points in [path] are collinear (fall within [eps]
    distance of the same line). *)
  val is_collinear : ?eps:float -> t -> bool

  (** [prune_collinear path]

      Remove collinear points from [path]. *)
  val prune_collinear : t -> t

  (** [prune_collinear' path]

      Remove collinear points from [path]. *)
  val prune_collinear' : vec array -> vec array

  (** [deriv ?closed ?h path]

      Computes a numerical derivative of [path], with [h] (default [1.]) giving
    the step size of the sampling of [path], so that the derivative can be
    scaled correctly. Setting [closed] to [true] will include computation of
    the derivative between the last and first point of the [path] (default
    [false]). *)
  val deriv : ?closed:bool -> ?h:float -> t -> t

  (** [deriv_nonuniform ?closed ?h path]

      Computes a numerical derivative of [path], with [h] giving
    the non-uniform step sizes of the sampling of [path], so that the derivative can be
    scaled correctly. Setting [closed] to [true] will include computation of
    the derivative between the last and first point of the [path] (default
    [false]). As [h] provides scaling factors for each segment of the path, it
    must have a length of one less than [path] if it's unclosed, and the same
    length if [closed] is [true]. *)
  val deriv_nonuniform : ?closed:bool -> h:float list -> t -> t

  (** [tangents ?uniform ?closed path]

      Compute tangent unit vectors of [path]. Set [closed] to [true] to indicate
   that tangents should include between the end and beginning of the path
   (default = [false]). Sampling of [path] is assumed to be [uniform] unless the
   parameter is set to [false], in which case the derivatives will be adjusted
   to correct for non-uniform sampling of points. *)
  val tangents : ?uniform:bool -> ?closed:bool -> t -> t

  (** [continuous_closest_point ?n_steps ?max_err f p]

      Find the closest position (from [0.] to [1.]) along the path function [f]
      to the point [p].

      - [n_steps] sets the granularity of search at each stage.
      - [max_err] the maximum distance the solution can be from the target [p] *)
  val continuous_closest_point
    :  ?n_steps:int
    -> ?max_err:float
    -> (float -> vec)
    -> vec
    -> float

  (** [segment ?closed path]

      Break [path] into line segments. If [closed] is [true], include a segment
    between the last and first points of [path] (default [false]). *)
  val segment : ?closed:bool -> t -> line list
end

module Make (V : Vec.S) : S with type vec := V.t and type line = V.line = struct
  type vec = V.t
  type line = V.line
  type t = vec list

  let length' path =
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

  let length = function
    | [] | [ _ ] -> 0.
    | hd :: tl   ->
      let f (sum, last) p = sum +. V.distance p last, p in
      fst @@ List.fold_left f (0., hd) tl

  let cummulative_length = function
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
    let travels = Array.of_list (cummulative_length path)
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
          p := Some (V.lerp p0 p1 frac) )
        else incr i
      done;
      Option.get !p
    in
    extrapolate

  let resample ~freq path =
    let n =
      match freq with
      | `N n       -> n
      | `Spacing s -> Int.of_float @@ (length path /. s)
    in
    let step = 1. /. Float.of_int (n - 1)
    and f = to_continuous path in
    List.init n (fun i -> f @@ (Float.of_int i *. step))

  let noncollinear_triple ?(eps = Util.epsilon) = function
    | [] | [ _ ] | [ _; _ ]   -> None
    | hd :: (p :: tl as rest) ->
      let furthest_point, dist =
        let f (fp, dist) p =
          let d = V.distance hd p in
          if d > dist then p, d else fp, dist
        in
        List.fold_left f (p, V.distance hd p) tl
      in
      if dist <= eps
      then None
      else (
        let n = V.(sdiv (sub hd furthest_point) dist)
        and threshold = dist *. eps in
        let offline, _ =
          let f (op, offset) p =
            let off = V.distance_to_vector (V.sub p hd) n in
            if off > offset then Some p, off else op, offset
          in
          List.fold_left f (None, threshold) rest
        in
        Option.map (fun op -> hd, furthest_point, op) offline )

  let is_collinear ?eps path = Option.is_none (noncollinear_triple ?eps path)

  let prune_collinear_rev' path =
    let len = Array.length path in
    let w = Util.index_wrap ~len in
    let f i acc =
      let p = path.(i) in
      if not (V.collinear path.(w (i - 1)) p path.(w (i + 1))) then p :: acc else acc
    in
    Util.fold_init len f []

  let prune_collinear' path = Util.array_of_list_rev (prune_collinear_rev' path)
  let prune_collinear path = List.rev @@ prune_collinear_rev' (Array.of_list path)

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
        | 0 -> V.(sub (smul (sub (g 1) (g 0)) 3.) (sub (g 2) (g 1)))
        | i when i = len - 1 && len < 3 -> V.sub (g (-1)) (g (-2))
        | i when i = len - 1 ->
          V.(sub (sub (g (-3)) (g (-2))) (smul (sub (g (-2)) (g (-1))) 3.))
        | i -> calc i
    in
    List.init (len - Bool.to_int (not closed)) (fun i -> V.sdiv (f i) (2. *. h))

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
      invalid_arg msg );
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
        V.(sdiv (sub v2 v1) (2. *. Float.min h1 h2))
      in
      if closed
      then calc
      else
        function
        | 0 -> V.(sdiv (sub path.(1) path.(0)) h.(0))
        | i when i = len - 1 -> V.(sdiv (sub path.(i) path.(i - 1)) h.(i - 1))
        | i -> calc i
    in
    List.init (len - Bool.to_int (not closed)) f

  let tangents ?(uniform = true) ?(closed = false) path =
    ( if uniform
    then deriv ~closed path
    else deriv_nonuniform ~closed ~h:(segment_lengths ~closed path) path )
    |> List.map V.normalize

  let continuous_closest_point ?(n_steps = 15) ?(max_err = 0.01) path_f p =
    let step = 1. /. Float.of_int n_steps in
    let rec aux start_u end_u =
      let minima_ranges =
        let us =
          let f i = Math.lerp start_u end_u (step *. Float.of_int i) in
          Array.init (n_steps + 1) f
        in
        let ps = Array.map path_f us in
        let len = Array.length ps in
        let f i acc =
          let i = len - 1 - i in
          let d1 = V.distance ps.(i - 2) p
          and d2 = V.distance ps.(i - 1) p
          and d3 = V.distance ps.(i) p in
          if d2 <= d1 && d2 <= d3 then (us.(i - 2), us.(i)) :: acc else acc
        in
        Util.fold_init (len - 2) f []
      in
      match minima_ranges with
      | [ (a, b) ] when V.distance (path_f a) (path_f b) < max_err -> (a +. b) /. 2.
      | [ (a, b) ] -> aux a b
      | (a1, b1) :: tl ->
        let f (min_u, min_dist) (a, b) =
          let u = aux a b in
          let d = V.distance (path_f u) p in
          if d < min_dist then u, d else min_u, min_dist
        in
        fst @@ List.fold_left f (f (0., Float.max_float) (a1, b1)) tl
      | [] -> failwith "Failure to find minima."
    in
    aux 0. 1.

  let segment ?(closed = false) = function
    | [] | [ _ ] -> invalid_arg "Cannot segment path with fewer than 2 points."
    | [ _; _ ] when closed -> invalid_arg "Path of length 2 cannot be closed."
    | hd :: tl ->
      let f (a, segs) b = b, V.{ a; b } :: segs in
      let last, segs = List.fold_left f (hd, []) tl in
      List.rev @@ if closed then V.{ a = last; b = hd } :: segs else segs
end
