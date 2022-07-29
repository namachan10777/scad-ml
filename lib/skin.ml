module Bez = Bezier.Make (Vec3)

type resampler =
  [ `Direct of [ `ByLen | `BySeg ]
  | `Reindex of [ `ByLen | `BySeg ]
  ]

type duplicator =
  [ `Distance
  | `FastDistance
  | `Tangent
  ]

type mapping =
  [ `Flat of [ resampler | duplicator ]
  | `Mix of [ resampler | duplicator ] list
  ]

type slices =
  [ `Flat of int
  | `Mix of int list
  ]

let is_direct = function
  | `Direct _ -> true
  | _         -> false

let is_resampler : [ resampler | duplicator ] -> bool = function
  | `Direct _ | `Reindex _ -> true
  | _                      -> false

let is_duplicator : [ resampler | duplicator ] -> bool = function
  | `Distance | `FastDistance | `Tangent -> true
  | _ -> false

let bezier_transition ?(k = 0.5) ~fn ~init a b =
  let step = 1. /. Float.of_int fn
  and bezs =
    try List.map2 (fun a b -> Bez.make [ a; Vec3.lerp a b k; b ]) a b with
    | Invalid_argument _ -> invalid_arg "Profiles must have equal length."
  in
  let f j acc =
    let u = Float.of_int j *. step in
    List.map (fun bez -> bez u) bezs :: acc
  in
  Util.fold_init fn f init

let linear_transition ~fn ~init a b =
  let step = 1. /. Float.of_int fn
  and lerps =
    try List.map2 Vec3.lerp a b with
    | Invalid_argument _ -> invalid_arg "Profiles must have equal length."
  in
  let f j acc =
    let u = Float.of_int j *. step in
    List.map (fun lerp -> lerp u) lerps :: acc
  in
  Util.fold_init fn f init

let getter ~len ~name = function
  | `Flat n -> Fun.const n
  | `Mix l  ->
    let a = Array.of_list l in
    if Array.length a <> len
    then
      invalid_arg
      @@ Printf.sprintf
           "`Mix %s entries (%i) do not match the number of transitions (%i)"
           name
           (Array.length a)
           len;
    Array.get a

let slice_profiles ?(looped = false) ~slices = function
  | [] | [ _ ]        -> invalid_arg "Too few profiles to slice."
  | hd :: tl as profs ->
    let len = List.length profs in
    let get_slices = getter ~len:(len - if looped then 0 else 1) ~name:"slice" slices in
    let f (init, last, i) next =
      let acc = linear_transition ~fn:(get_slices i + 1) ~init last next in
      acc, next, i + 1
    in
    let profiles, last, i = List.fold_left f ([], hd, 0) tl in
    if looped
    then (
      let profiles, _, _ = f (profiles, last, i) hd in
      List.rev profiles )
    else List.rev (last :: profiles)

type dp_map_dir =
  | Diag (* map the next vertex of [big] to the next vertex of [small] *)
  | Left (* map the next vertex of [big] to the current vertex of [small] *)
  | Up (* map the next vertex of [small] to the current vertex of [big] *)

(* Construct a matrix of shape ([len_small] x [len_big]) with mappings between the
   points of the polygons [small] and [big]. *)
let dp_distance_array ?(abort_thresh = Float.infinity) small big =
  let len_small = Array.length small
  and len_big = Array.length big in
  let small_idx = ref 1
  and total_dist =
    let a = Array.make (len_big + 1) 0. in
    for i = 1 to len_big do
      a.(i) <- a.(i - 1) +. Vec3.distance big.(i mod len_big) small.(0)
    done;
    a
  and dist_row = Array.make (len_big + 1) 0. (* current row, reused each iter *)
  and min_cost = ref 0. (* minimum cost of current dist_row, break above threshold *)
  and dir_map = Array.init (len_small + 1) (fun _ -> Array.make (len_big + 1) Left) in
  while !small_idx < len_small + 1 do
    min_cost := Vec3.distance big.(0) small.(!small_idx mod len_small) +. total_dist.(0);
    dist_row.(0) <- !min_cost;
    dir_map.(!small_idx).(0) <- Up;
    for big_idx = 1 to len_big do
      let cost, dir =
        let diag = total_dist.(big_idx - 1)
        and left = dist_row.(big_idx - 1)
        and up = total_dist.(big_idx) in
        if up < diag && up < left
        then up, Up
        else if left < diag && left <= up
        then left, Left (* favoured in tie with up *)
        else diag, Diag (* smallest, tied with left, or three-way *)
      and d = Vec3.distance big.(big_idx mod len_big) small.(!small_idx mod len_small) in
      dist_row.(big_idx) <- cost +. d;
      dir_map.(!small_idx).(big_idx) <- dir;
      if dist_row.(big_idx) < !min_cost then min_cost := dist_row.(big_idx)
    done;
    (* dump current row of distances as new totals *)
    Array.blit dist_row 0 total_dist 0 (len_big + 1);
    (* Break out early if minimum cost for this combination of small/big is
         above the threshold. The map matrix is incomplete, but it will not be
         used anyway. *)
    small_idx := if !min_cost > abort_thresh then len_small + 1 else !small_idx + 1
  done;
  total_dist.(len_big), dir_map

(* Produce ascending lists of indices (with repeats) for the small and big
   polygons that should be used to reconstruct the polygons with the point
   duplications required to associate the vertices between them.  *)
let dp_extract_map m =
  let len_small = Array.length m - 1
  and len_big = Array.length m.(0) - 1 in
  let rec loop i j small_map big_map =
    let i, j =
      match m.(i).(j) with
      | Diag -> i - 1, j - 1
      | Left -> i, j - 1
      | Up   -> i - 1, j
    in
    let small_map = (i mod len_small) :: small_map
    and big_map = (j mod len_big) :: big_map in
    if i = 0 && j = 0 then small_map, big_map else loop i j small_map big_map
  in
  loop len_small len_big [] []

(* Duplicate points according to mappings and shift the new polygon (rotating
   its the point list) to handle the case when points from both ends of one
   curve map to a single point on the other. *)
let dp_apply_map_and_shift map poly =
  let shift =
    (* the mapping lists are already sorted in ascending order *)
    let last_max_idx l =
      let f (i, max, idx) v = if v >= max then i + 1, v, i else i + 1, max, idx in
      List.fold_left f (0, 0, 0) l
    in
    let len, _, idx = last_max_idx map in
    len - idx - 1
  and len = Array.length poly in
  let f i acc = poly.((i + shift) mod len) :: acc in
  List.fold_right f map []

let distance_match a b =
  let a = Array.of_list a
  and b = Array.of_list b in
  let swap = Array.length a > Array.length b in
  let small, big = if swap then b, a else a, b in
  let map, shifted_big =
    let len_big = Array.length big in
    let rec find_best cost map poly i =
      let shifted =
        Array.init len_big (fun j -> big.(Util.index_wrap ~len:len_big (i + j)))
      in
      let cost', map' = dp_distance_array ~abort_thresh:cost small shifted in
      let cost, map, poly =
        if cost' < cost then cost', map', shifted else cost, map, poly
      in
      if i < len_big then find_best cost map poly (i + 1) else map, poly
    in
    let cost, map = dp_distance_array small big in
    find_best cost map big 1
  in
  let small_map, big_map = dp_extract_map map in
  let small' = dp_apply_map_and_shift small_map small
  and big' = dp_apply_map_and_shift big_map shifted_big in
  if swap then big', small' else small', big'

let aligned_distance_match a b =
  let a = Array.of_list a
  and b = Array.of_list b in
  let a_map, b_map = dp_extract_map @@ snd @@ dp_distance_array a b in
  let a' = dp_apply_map_and_shift a_map a
  and b' = dp_apply_map_and_shift b_map b in
  a', b'

let tangent_match a b =
  let a' = Array.of_list a
  and b' = Array.of_list b in
  let swap = Array.length a' > Array.length b' in
  let small, big = if swap then b', a' else a', b' in
  let len_small = Array.length small
  and len_big = Array.length big in
  let cut_pts =
    let sm, bg = if swap then b, a else a, b in
    Array.init len_small (fun i ->
        fst
        @@ Path3.closest_tangent
             ~offset:Path3.(Vec3.sub (centroid sm) (centroid bg))
             ~line:Vec3.{ a = small.(i); b = small.((i + 1) mod len_small) }
             bg )
  in
  let len_duped, duped_small =
    let f i (len, pts) =
      let count =
        let a = cut_pts.(i)
        and b = cut_pts.(Util.index_wrap ~len:len_small (i - 1)) in
        Math.posmod (a - b) len_big
      in
      ( len + count
      , Util.fold_init count (fun _ pts -> small.(len_small - 1 - i) :: pts) pts )
    in
    Util.fold_init len_small f (0, [])
  and shifted_big =
    let shift = cut_pts.(len_small - 1) + 1 in
    List.init len_big (fun i -> big.((shift + i) mod len_big))
  in
  if len_duped <> len_big
  then
    failwith
      "Tangent alignment failed, likely due to insufficient points or a concave curve.";
  if swap then shifted_big, duped_small else duped_small, shifted_big

let skin
    ?(style = `MinEdge)
    ?refine
    ?(mapping = `Flat (`Direct `ByLen))
    ?(endcaps = `Both)
    ~slices
  = function
  | [] | [ _ ] -> invalid_arg "At least two profiles are required to skin."
  | profs      ->
    let refine = Option.bind refine (fun n -> if n > 1 then Some n else None)
    and looped, bot_cap, top_cap =
      match endcaps with
      | `Both -> false, true, true
      | `Loop -> true, false, false
      | `Bot  -> false, true, false
      | `Top  -> false, false, true
      | `None -> false, false, false
    and resample n s = Path3.subdivide ~closed:true ~freq:(`N (n, s))
    and profs = Array.of_list profs in
    let n_profs = Array.length profs in
    let n_transitions = n_profs - if looped then 0 else 1 in
    let get_mapping = getter ~len:n_transitions ~name:"mapping" mapping
    and n =
      let max = Array.fold_left (fun mx l -> Int.max (List.length l) mx) 0 profs in
      Util.value_map_opt ~default:max (fun r -> r * max) refine
    and all_resamplers =
      match mapping with
      | `Flat (`Direct _ | `Reindex _) -> true
      | `Mix l -> List.for_all is_resampler l
      | _ -> false
    in
    let len_sliced, sliced =
      if all_resamplers
      then (
        (* there are no duplicators, so all profiles can be handled together. *)
        let unpack_resampler i =
          match get_mapping i with
          | `Direct sampling  -> true, sampling
          | `Reindex sampling -> false, sampling
          | _                 -> failwith "impossible"
        in
        let f i (acc, last_p) =
          let direct, sampling = unpack_resampler i in
          let resampled = resample n sampling profs.(i + 1) in
          if direct
          then resampled :: acc, resampled
          else Path3.reindex_polygon last_p resampled :: acc, resampled
        and resampled_hd = resample n (snd @@ unpack_resampler 0) profs.(0) in
        let fixed_hd =
          if looped
          then (
            let direct, samp = unpack_resampler (n_profs - 1) in
            if not direct
            then Path3.reindex_polygon (resample n samp profs.(n_profs - 1)) resampled_hd
            else resampled_hd )
          else resampled_hd
        in
        let fixed =
          let l, _ = Util.fold_init (n_profs - 1) f ([ resampled_hd ], resampled_hd) in
          List.rev @@ if looped then fixed_hd :: l else l
        in
        1, [ slice_profiles ~looped:false ~slices fixed ] )
      else (
        let get_slices = getter ~len:n_transitions ~name:"slices" slices in
        (* This is likely to change, but it is my attempt to support having
              sampling method on a per resampler basis, while also navigating the
              transitions between resamplers and duplicators. It may not really be possible
              to achieve something flexible and intuitive though, so transitioning to
              something more similar to BOSL2s solution is still in the cards. *)
        let up =
          let fallback i p =
            match get_mapping (Util.index_wrap ~len:n_profs (i - 1)) with
            | `Direct sampling | `Reindex sampling -> resample n sampling p
            | _ -> resample n `BySeg p
          in
          let f i p =
            if i < n_transitions || looped
            then (
              match get_mapping i with
              | `Direct sampling | `Reindex sampling -> resample n sampling p
              | _ -> if i > 0 || looped then fallback i p else resample n `BySeg p )
            else fallback i p
          in
          Array.mapi f profs
        and upsample_dups (a, b) = [ resample n `BySeg a; resample n `BySeg b ] in
        let f i acc =
          let j = (i + 1) mod n_profs in
          let pair =
            (* resamplers are upsampled before alignment, duplicators are upsampled after *)
            match get_mapping i with
            | `Direct _     -> [ up.(i); up.(j) ]
            | `Reindex _    -> [ up.(i); Path3.reindex_polygon up.(i) up.(j) ]
            | `Distance     -> upsample_dups @@ distance_match profs.(i) profs.(j)
            | `FastDistance -> upsample_dups @@ aligned_distance_match profs.(i) profs.(j)
            | `Tangent      -> upsample_dups @@ tangent_match profs.(i) profs.(j)
          in
          slice_profiles ~slices:(`Flat (get_slices i)) pair :: acc
        in
        n_transitions, List.rev @@ Util.fold_init n_transitions f [] )
    in
    let f (i, acc) rows =
      let endcaps =
        match bot_cap, top_cap with
        | true, true when i = 0 && i = len_sliced - 1 -> `Both
        | true, _ when i = 0 -> `Bot
        | _, true when i = len_sliced - 1 -> `Top
        | _ -> `None
      in
      i + 1, Mesh0.of_rows ~style ~endcaps rows :: acc
    in
    Mesh0.join @@ snd @@ List.fold_left f (0, []) sliced
