(* TODO:
   - come up with a design that will allow composing morphing sweep functions
    with relative ease (For those, will need to morph the correct amount between
    steps along the path. If going from path, rather than from transforms, then
    using relative distance along the path would enable a linear morph through
    the sweep)
   - morphing sweep/extrusions will be starting from Poly2 as the others do, and
    support rounded caps
   - I think a ~k curvature hardness parameter would be
    nice to have, so that morphing doesn't have to be strictly linear between
    the profiles (bezier with middle control point set between 0 and 1)
   - I think I probably can't get around exposing a "low level" version that
    deals in lists of profiles and parameters, but the simpler bases should all
    be covered by the functions that only take two profiles as parameters.
   - read through and understand how I should implement the types such that
    efficient upsampling timing like in BOSL2 can be achieved without too much
    headache or dynamism
   - need to watch out for the impact that duplicated vertices will have on
    integration with sweeping / polyholes since I have checks in some places
    that would catch it as an incorrect polygon.
    *)

module Bez = Bezier.Make (Vec3)

type spec =
  [ `Direct
  | `Reindex
  | `Distance
  | `FastDistance
  | `Tangent
  ]

type slices =
  [ `Flat of int
  | `Mix of int list
  ]

let bezier_transition ?(k = 0.5) ~fn ~init a b =
  let step = 1. /. Float.of_int fn
  and bezs = List.map2 (fun a b -> Bez.make [ a; Vec3.lerp a b k; b ]) a b in
  let f j acc =
    let u = Float.of_int j *. step in
    List.map (fun bez -> bez u) bezs :: acc
  in
  Util.fold_init fn f init

let slice_profiles ?(closed = false) ~slices = function
  | [] | [ _ ]        -> invalid_arg "Too few profiles to slice."
  | hd :: tl as profs ->
    let len = List.length profs in
    let get_slices =
      match slices with
      | `Flat n -> Fun.const n
      | `Mix l  ->
        let a = Array.of_list l
        and n = len - if closed then 0 else 1 in
        if Array.length a <> n
        then
          invalid_arg
          @@ Printf.sprintf
               "`Mix slice entries (%i) do not match the number of transitions (%i)"
               (Array.length a)
               n;
        Array.get a
    in
    let f (acc, last, i) next =
      let n = get_slices i in
      let step = 1. /. Float.of_int n in
      let acc =
        let g j acc =
          let u = Float.of_int j *. step in
          List.map2 (fun a b -> Vec3.lerp a b u) last next :: acc
        in
        Util.fold_init n g acc
      in
      acc, next, i + 1
    in
    let profiles, last, i = List.fold_left f ([], hd, 0) tl in
    if closed
    then (
      let profiles, _, _ = f (profiles, last, i) hd in
      List.rev profiles )
    else List.rev profiles

type dp_map_dir =
  | Diag
  | Left
  | Up

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

let dp_extract_map m =
  let len_big = Array.length m.(0) - 1
  and len_small = Array.length m - 1 in
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

let distance_match a b =
  let a = Array.of_list a
  and b = Array.of_list b in
  let swap = Array.length a > Array.length b in
  let small, big = if swap then b, a else a, b in
  let len_big = Array.length big in
  let rec loop cost map poly i =
    let shifted =
      Array.init len_big (fun j -> big.(Util.index_wrap ~len:len_big (i + j)))
    in
    let cost', map' = dp_distance_array ~abort_thresh:cost small shifted in
    let cost, map, poly =
      if cost' < cost then cost', map', shifted else cost, map, poly
    in
    if i < len_big then loop cost map poly (i + 1) else map, poly
  in
  let map, shifted_big =
    let cost, map = dp_distance_array small big in
    loop cost map big 1
  in
  let small_map, big_map = dp_extract_map map in
  (* Duplicate points according to mappings and shift the new polygon (rotating
      its the point list) to handle the case when points from both ends of one
      curve map to a single point on the other. *)
  let shifting_map map poly =
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
  in
  let small' = shifting_map small_map small
  and big' = shifting_map big_map shifted_big in
  if swap then big', small' else small', big'
