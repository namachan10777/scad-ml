(* TODO:
   - come up with a design that will allow composing morphing sweep functions
    with relative ease (For those, will need to morph the correct amount between
    steps along the path. If going from path, rather than from transforms, then
    using relative distance along the path would enable a linear morph through
    the sweep)
   - morphing sweep/extrusions will be starting from Poly2 as the others do, and
    support rounded caps
   - I think I probably can't get around exposing a "low level" version that
    deals in lists of profiles and parameters, but the simpler bases should all
    be covered by the functions that only take two profiles as parameters.
   - need to watch out for the impact that duplicated vertices will have on
    integration with sweeping / polyholes since I have checks in some places
    that would catch it as an incorrect polygon.
   - I think I have decided to leave `refine` out or only allow a flat value
    (defaulting to 1), so that is one less complication. Still need to find the
    longest profile and subdivide up to that amount for resampling methods, and
    refine the duplicator profiles post duplication. Though, since generated
    curves should be generated at desired quality from the start, I think maybe
    the caller should just take care of refinining their discrete profiles
    themselves (this discrepancy is what one would want non-flat refine values
    for in the first place I think).
   - sampling and refine should be non-default optionals with custom handling
   ~ sampling default depends on whether duplicator methods are included
   ~ refine must be Some > 0 (invalid_arg if not)
    *)

module Bez = Bezier.Make (Vec3)

type sampling =
  [ `ByLen
  | `BySeg
  ]

type resampler =
  [ `Direct of sampling
  | `Reindex of sampling
  ]

type duplicator =
  [ `Distance
  | `FastDistance
  | `Tangent
  ]

type spec =
  [ `Flat of [ resampler | duplicator ]
  | `Mix of [ resampler | duplicator ] list
  ]

type slices =
  [ `Flat of int
  | `Mix of int list
  ]

let is_direct : resampler -> bool = function
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
             ~offset:Poly3.(Vec3.sub (centroid (make sm)) (centroid (make bg)))
             ~line:Vec3.{ a = small.(i); b = small.((i + 1) mod len_small) }
             bg )
  in
  let len_duped, duped_small =
    let f i (len, pts) =
      let count =
        (cut_pts.(len_small - 1 - i) - cut_pts.(len_small - 2 - i)) mod len_big
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
    ?refine (* ?(spec = `Flat (`Direct `ByLen)) *)
    ~spec
    ?(endcaps = `Both)
    ~slices
  = function
  | [] | [ _ ] -> invalid_arg "At least two profiles are required to skin."
  | profiles   ->
    let refine = Option.bind refine (fun n -> if n > 1 then Some n else None)
    and looped, bot_cap, top_cap =
      match endcaps with
      | `Both -> false, true, true
      | `Loop -> true, false, false
      | `Bot  -> false, true, false
      | `Top  -> false, false, true
      | `None -> false, false, false
    and resample n s = Path3.subdivide ~closed:true ~freq:(`N (n, s))
    and profiles = Array.of_list profiles in
    let n_profiles = Array.length profiles in
    let n_transitions = n_profiles - if looped then 0 else 1 in
    let get_spec = getter ~len:n_transitions ~name:"spec" spec in
    let all_resamplers =
      match spec with
      | `Flat (`Direct _ | `Reindex _) -> true
      | `Mix l -> List.for_all is_resampler l
      | _ -> false
    in
    let sliced =
      if all_resamplers
      then (
        let unpack_resampler i =
          match get_spec i with
          | `Direct sampling  -> true, sampling
          | `Reindex sampling -> false, sampling
          | _                 -> failwith "impossible"
        and n =
          let max_len =
            Array.fold_left (fun mx l -> Int.max (List.length l) mx) 0 profiles
          in
          Util.value_map_opt ~default:max_len (fun r -> r * max_len) refine
        in
        let f i (acc, last_p) =
          let direct, sampling = unpack_resampler i in
          let resampled = resample n sampling profiles.(i + 1) in
          if direct
          then resampled :: acc, resampled
          else Path3.reindex_polygon last_p resampled :: acc, resampled
        and resampled_hd = resample n (snd @@ unpack_resampler 0) profiles.(0) in
        let fixed_hd =
          let direct, sampling = unpack_resampler (n_profiles - 1) in
          if looped && not direct
          then
            Path3.reindex_polygon
              (resample n sampling profiles.(n_profiles - 1))
              resampled_hd
          else resampled_hd
        in
        let fixed =
          let l, _ = Util.fold_init (n_profiles - 1) f ([ resampled_hd ], resampled_hd) in
          List.rev @@ if looped then fixed_hd :: l else l
        in
        [ slice_profiles ~looped:false ~slices fixed ] )
      else (
        (* let n = *)

        (*   let max_lens (\* = *\) *)
        (*     let f i (mx, acc) l = *)
        (*             if is_duplicator @@ get_spec i then , mx::acc, *)
        (*     List.fold_left (fun mx l -> Int.max (List.length l) mx) 0 profiles *)
        (*   in *)
        (*   Util.value_map_opt ~default:max_len (fun r -> r * max_len) refine *)
        (* in *)
        (* let profile_lens = Array.map List.length profiles in *)
        let unpack_spec i =
          match get_spec i with
          | `Direct sampling  -> true, sampling
          | `Reindex sampling -> false, sampling
          | _                 -> failwith "impossible"
        and n =
          let max_len =
            Array.fold_left (fun mx l -> Int.max (List.length l) mx) 0 profiles
          in
          Util.value_map_opt ~default:max_len (fun r -> r * max_len) refine
        in
        let f i (acc, last_p) =
          let direct, sampling = unpack_spec i in
          let resampled = resample n sampling profiles.(i) in
          if direct
          then resampled :: acc, resampled
          else Path3.reindex_polygon last_p resampled :: acc, resampled
        in
        let first_fixed = resample n (snd @@ unpack_spec 0) profiles.(0) in
        let fixed, _ = Util.fold_init n_profiles f ([ first_fixed ], first_fixed) in
        let fixed = List.rev @@ if looped then first_fixed :: fixed else fixed in
        [ slice_profiles ~looped:false ~slices fixed ] )
    in
    let len = List.length sliced in
    let f (i, acc) rows =
      let endcaps =
        match bot_cap, top_cap with
        | true, true when i = 0 && i = len - 1 -> `Both
        | true, _ when i = 0 -> `Bot
        | _, true when i = len - 1 -> `Top
        | _ -> `None
      in
      i + 1, Mesh.of_rows ~style ~endcaps rows :: acc
    in
    Mesh.join @@ snd @@ List.fold_left f (0, []) sliced
