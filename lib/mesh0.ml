(* TODO:
  - Think about the best way to integrate in roundExtrude. Since that module is
    quite long, including private implementation functions, but depends on on
    the Mesh.t type, it may be best to borrow from the JaneStreet Module0 style.
  - e.g. Mesh0 has the type, of_rows, of_ragged, as needed by roundExtrude,
    then Mesh includes Mesh0 and RoundExtrude, and fills in the rest.
  - if this works out well, then adopting this pattern elsewhere in the library
    would help to remove some of the messiness from the top level Scad_ml module.
*)
module IntTbl = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end)

type t =
  { n_points : int
  ; points : Vec3.t list
  ; faces : int list list
  }

type row_wrap =
  [ `Loop
  | `Both
  | `None
  | `Top
  | `Bot
  ]

let empty = { n_points = 0; points = []; faces = [] }
let n_points t = t.n_points
let points t = t.points
let faces t = t.faces
let make ~points ~faces = { n_points = List.length points; points; faces }

let of_rows ?(row_wrap = `Both) ?(col_wrap = true) layers =
  let looped =
    match row_wrap with
    | `Loop -> true
    | _     -> false
  in
  match layers with
  | []       -> empty
  | [ _ ]    -> invalid_arg "Only one layer provided."
  | hd :: tl ->
    let n_layers = List.length layers
    and n_facets = List.length hd
    and points = List.concat layers in
    if not (List.for_all (fun l -> List.length l = n_facets) tl)
    then invalid_arg "Inconsistent layer length.";
    let faces =
      let last_seg = n_layers - if looped then 1 else 2
      and last_face = n_facets - if col_wrap then 1 else 2 in
      let rec loop acc seg face =
        let acc =
          let s0 = seg mod n_layers
          and s1 = (seg + 1) mod n_layers
          and f1 = (face + 1) mod n_facets in
          [ (s0 * n_facets) + face
          ; (s0 * n_facets) + f1
          ; (s1 * n_facets) + f1
          ; (s1 * n_facets) + face
          ]
          :: acc
        in
        if face = last_face
        then if seg = last_seg then acc else loop acc (seg + 1) 0
        else loop acc seg (face + 1)
      in
      let faces = loop [] 0 0
      and top_offset = n_facets * (n_layers - 1) in
      let bottom_cap = List.init n_facets (fun i -> n_facets - 1 - i)
      and top_cap = List.init n_facets (fun i -> i + top_offset) in
      match row_wrap with
      | `Both         -> top_cap :: bottom_cap :: faces
      | `None | `Loop -> faces
      | `Top          -> top_cap :: faces
      | `Bot          -> bottom_cap :: faces
    in
    { n_points = n_layers * n_facets; points; faces = List.rev faces }

let of_ragged ?(looped = false) ?(rev = false) rows =
  let starts_lenghts, points =
    let f (start, starts_lengths, points) row =
      let g (i, ps) p = i + 1, p :: ps in
      let len, points = List.fold_left g (0, points) row in
      start + len, (start, len) :: starts_lengths, points
    in
    let _, starts_lengths, points = List.fold_left f (0, [], []) rows in
    List.rev starts_lengths, List.rev points
  in
  match starts_lenghts with
  | [] | [ _ ] -> empty
  | hd :: tl   ->
    let f ((start, len), faces) ((next_start, next_len) as next) =
      let faces =
        match next_len - len with
        | 0     ->
          let a i = [ i + start; i + start + 1; i + next_start ]
          and b i = [ i + start + 1; i + next_start + 1; i + next_start ] in
          Util.prepend_init (len - 1) a faces |> Util.prepend_init (len - 1) b
        | 1     ->
          let a i = [ i + start; i + start + 1; i + next_start + 1 ]
          and b i = [ i + start; i + next_start + 1; i + next_start ] in
          Util.prepend_init (len - 1) a faces |> Util.prepend_init len b
        | -1    ->
          let a i = [ i + start + 1; i + next_start + 1; i + next_start ]
          and b i = [ i + start; i + start + 1; i + next_start ] in
          Util.prepend_init (len - 2) a faces |> Util.prepend_init (len - 1) b
        | 2     ->
          let count = Float.(to_int @@ floor @@ ((of_int len -. 1.) /. 2.)) in
          let a i = [ i + start; i + start + 1; i + next_start + 1 ]
          and b i =
            let i = i + count in
            [ i + start; i + start + 1; i + next_start + 2 ]
          and c i = [ i + start; i + next_start + 1; i + next_start ]
          and d i =
            let i = i + count + 1 in
            [ i + start - 1; i + next_start + 1; i + next_start ]
          in
          Util.prepend_init count a faces
          |> Util.prepend_init (len - count - 1) b
          |> Util.prepend_init (count + 1) c
          |> Util.prepend_init (next_len - 2 - count) d
        | -2    ->
          let count = Float.(to_int @@ floor @@ ((of_int len -. 1.) /. 2.)) in
          let a i = [ i + next_start; i + start + 1; i + next_start + 1 ]
          and b i =
            let i = i + count - 1 in
            [ i + next_start; i + start + 2; i + next_start + 1 ]
          and c i = [ i + next_start; i + start; i + start + 1 ]
          and d i =
            let i = i + count in
            [ i + next_start - 1; i + start; i + start + 1 ]
          in
          Util.prepend_init (count - 1) a faces
          |> Util.prepend_init (len - count - 2) b
          |> Util.prepend_init count c
          |> Util.prepend_init (next_len + 1 - count) d
        | delta ->
          let s = Printf.sprintf "Unsupported layer length difference of %i" delta in
          invalid_arg s
      in
      next, faces
    in
    let _, all_faces = List.fold_left f (hd, []) (if looped then tl @ [ hd ] else tl)
    and verts = Array.of_list points in
    let faces =
      let cull_degenerate =
        let v = Array.unsafe_get verts in
        let not_degen a b = Float.compare (Vec3.distance (v a) (v b)) Util.epsilon = 1 in
        function
        | [ i0; i1; i2 ] as face ->
          if not_degen i0 i1 && not_degen i1 i2 && not_degen i2 i0
          then if rev then Some [ i2; i1; i0 ] else Some face
          else None
        | _                      -> failwith "unreachable"
      in
      List.filter_map cull_degenerate all_faces
    in
    { n_points = Array.length verts; points; faces }

let of_path2 ?(rev = false) layer =
  let n_points, points, face =
    List.fold_left
      (fun (n, ps, fs) p -> n + 1, Vec3.of_vec2 p :: ps, n :: fs)
      (0, [], [])
      layer
  in
  { n_points; points; faces = [ (if rev then List.rev face else face) ] }

let of_path3 ?(rev = false) layer =
  let n_points, points, face =
    List.fold_left (fun (n, ps, fs) p -> n + 1, p :: ps, n :: fs) (0, [], []) layer
  in
  { n_points; points; faces = [ (if rev then List.rev face else face) ] }

let of_poly2 ?rev = function
  | Poly2.{ outer; holes = [] } -> of_path2 ?rev outer
  | Poly2.{ outer; holes }      ->
    let points, faces = PolyHoles.partition ?rev ~holes outer in
    make ~points ~faces

let of_poly3 ?rev = function
  | Poly3.{ outer; holes = [] } -> of_path3 ?rev outer
  | Poly3.{ outer; holes }      ->
    let plane = Plane.of_normal ~point:(List.hd outer) @@ Path3.normal outer in
    let project = Path3.project plane
    and lift = Plane.lift plane in
    let holes = List.map project holes in
    let points, faces = PolyHoles.partition ?rev ~lift ~holes (project outer) in
    make ~points ~faces

let of_polygons polys =
  let lengths = Util.array_of_list_map List.length polys in
  let n = Array.length lengths in
  let offsets =
    let a = Array.make (n + 1) 0 in
    for i = 1 to n - 1 do
      a.(i) <- a.(i - 1) + lengths.(i - 1)
    done;
    a
  in
  let faces = List.init n (fun i -> List.init lengths.(i) (fun j -> j + offsets.(i))) in
  { n_points = offsets.(n); points = List.concat polys; faces }

let join = function
  | [] -> empty
  | [ t ] -> t
  | { n_points; points; faces } :: ts ->
    let f (n, ps, fs) t =
      let offset = List.map (List.map (( + ) n)) t.faces in
      n + t.n_points, t.points :: ps, offset :: fs
    in
    let n_points, ps, fs = List.fold_left f (n_points, [ points ], [ faces ]) ts in
    { n_points; points = List.concat (List.rev ps); faces = List.concat (List.rev fs) }

let merge_points ?(eps = Util.epsilon) { n_points; points; faces } =
  let drop = IntTbl.create 100
  and pts = Array.of_list points in
  let len = Array.length pts in
  let () =
    (* naive search if the mesh is small (avoid building search tree) *)
    if len < 400
    then
      for i = 0 to len - 2 do
        for j = i + 1 to len - 1 do
          if Vec3.approx ~eps pts.(i) pts.(j) then IntTbl.add drop j i
        done
      done
    else (
      let tree = BallTree3.make' pts in
      for i = 1 to len - 1 do
        match BallTree3.search_idxs ~radius:eps tree pts.(i) with
        | [] | [ _ ] -> () (* single result will be self *)
        | hd :: tl   ->
          let min_match = List.fold_left Int.min hd tl in
          if i <> min_match then IntTbl.add drop i min_match
      done )
  in
  let points =
    let f (i, acc) p = if IntTbl.mem drop i then i + 1, acc else i + 1, p :: acc in
    let _, points = Array.fold_left f (0, []) pts in
    List.rev points
  and faces =
    let lookup = Array.make len 0
    and off = ref 0
    and offsets = Array.make len 0 in
    for i = 1 to len - 1 do
      offsets.(i) <- !off;
      match IntTbl.find_opt drop i with
      | Some idx ->
        lookup.(i) <- idx - offsets.(idx);
        incr off
      | None     -> lookup.(i) <- i - !off
    done;
    let rec prune_face i first last acc = function
      | [ hd ]   ->
        let hd' = lookup.(hd) in
        if hd' <> last && hd' <> first && i >= 2
        then Some (List.rev @@ (hd' :: acc))
        else if i >= 3
        then Some (List.rev acc)
        else None
      | hd :: tl ->
        let hd' = lookup.(hd) in
        if hd' <> last
        then prune_face (i + 1) first hd' (hd' :: acc) tl
        else prune_face i first last acc tl
      | []       -> None
    in
    let f acc = function
      | []       -> acc
      | hd :: tl ->
        let hd' = lookup.(hd) in
        Util.prepend_opt (prune_face 1 hd' hd' [ hd' ] tl) acc
    in
    List.fold_left f [] faces
  in
  { n_points = n_points - IntTbl.length drop; points; faces }

let add_face face t = { t with faces = face :: t.faces }
let add_faces faces t = { t with faces = List.rev_append faces t.faces }
let rev_faces t = { t with faces = List.map List.rev t.faces }

let volume { n_points; points; faces } =
  if n_points = 0
  then 0.
  else (
    let pts = Array.of_list points in
    let rec sum_face total_vol p1 idxs =
      let calc total_vol p1 p2 p3 = Vec3.(dot (cross p3 p2) p1) +. total_vol in
      match idxs with
      | [ i2; i3 ]              -> calc total_vol p1 pts.(i2) pts.(i3)
      | i2 :: (i3 :: _ as rest) -> sum_face (calc total_vol p1 pts.(i2) pts.(i3)) p1 rest
      | _                       -> invalid_arg
                                     "Polyhedron contains face with fewer than 3 points."
    in
    let f total_vol = function
      | i1 :: idxs -> sum_face total_vol pts.(i1) idxs
      | []         -> invalid_arg "Polyhedron contains empty face."
    in
    List.fold_left f 0. faces /. 6. )

let area { n_points; points; faces } =
  if n_points = 0
  then 0.
  else (
    let pts = Array.of_list points in
    let f sum idxs =
      let face = List.map (fun i -> pts.(i)) idxs in
      let poly = Path3.(project (to_plane face) face) in
      sum +. Poly2.(area @@ make poly)
    in
    List.fold_left f 0. faces )

let centroid ?(eps = Util.epsilon) { n_points; points; faces } =
  if n_points = 0 then invalid_arg "No centroid for empty polyhedron.";
  let pts = Array.of_list points in
  let rec sum_face total_vol weighted_sum p1 idxs =
    let calc total_vol weighted_sum p1 p2 p3 =
      let vol = Vec3.(dot (cross p3 p2) p1) in
      let weighted = Vec3.(smul (add p1 (add p2 p3)) vol) in
      vol +. total_vol, Vec3.add weighted_sum weighted
    in
    match idxs with
    | [ i2; i3 ]              -> calc total_vol weighted_sum p1 pts.(i2) pts.(i3)
    | i2 :: (i3 :: _ as rest) ->
      let total_vol, weighted_sum = calc total_vol weighted_sum p1 pts.(i2) pts.(i3) in
      sum_face total_vol weighted_sum p1 rest
    | _                       -> invalid_arg
                                   "Polyhedron contains face with fewer than 3 points."
  in
  let total_vol, weighted_sum =
    let f (total_vol, weighted_sum) = function
      | i1 :: idxs -> sum_face total_vol weighted_sum pts.(i1) idxs
      | []         -> invalid_arg "Polyhedron contains empty face."
    in
    List.fold_left f (0., Vec3.zero) faces
  in
  if Math.approx ~eps total_vol 0.
  then invalid_arg "The polyhedron has self-intersections.";
  Vec3.(sdiv weighted_sum (total_vol *. 4.))

let enforce_winding w shape =
  let reverse =
    match w with
    | `CCW     -> Path2.is_clockwise shape
    | `CW      -> not @@ Path2.is_clockwise shape
    | `NoCheck -> false
  in
  if reverse then List.rev shape else shape

let translate p t = { t with points = Path3.translate p t.points }
let rotate r t = { t with points = Path3.rotate r t.points }
let rotate_about_pt r p t = { t with points = Path3.rotate_about_pt r p t.points }
let quaternion q t = { t with points = Path3.quaternion q t.points }
let quaternion_about_pt q p t = { t with points = Path3.quaternion_about_pt q p t.points }
let vector_rotate ax r = quaternion (Quaternion.make ax r)
let vector_rotate_about_pt ax r = quaternion_about_pt (Quaternion.make ax r)
let multmatrix m t = { t with points = Path3.multmatrix m t.points }
let scale s t = { t with points = Path3.scale s t.points }
let mirror ax t = rev_faces { t with points = Path3.mirror ax t.points }
let to_scad ?convexity { points; faces; _ } = Scad.polyhedron ?convexity points faces