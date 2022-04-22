open Vec

exception SelfIntersection of int
exception CrossIntersection of int * int
exception DuplicatePoints

type t =
  { outer : Vec2.t list
  ; holes : Vec2.t list list
  }

(* TODO: validate non-self-intersecting, enough points in each path, and
    that none of them interset with eachother, then protect the type.
    Make optional, but on by default: ?(validate = true)
   See is_region_simple (and _region_region_intersections) for example:
     https://github.com/revarbat/BOSL2/blob/master/regions.scad#L230
     https://github.com/revarbat/BOSL2/blob/master/regions.scad#L419
   TODO: make test cases for simple polygons to ensure this is working. *)
let validation ?(eps = Util.epsilon) = function
  | { outer = [] | [ _ ] | [ _; _ ]; _ } -> invalid_arg "Outer path has too few points."
  | { outer; holes } ->
    let paths = Array.map Array.of_list (Array.of_list (outer :: holes)) in
    Array.iteri
      (fun i p ->
        if not @@ Path2.is_simple' ~eps ~closed:true p then raise (SelfIntersection i) )
      paths;
    (* check for intersections *)
    let n = Array.length paths
    and p1_idx = ref 0 in
    while !p1_idx < n - 1 do
      let p1 = paths.(!p1_idx) in
      let len_p1 = Array.length p1
      and i = ref 0 in
      while !i < len_p1 - 1 do
        let a = p1.(!i)
        and b = p1.(Util.index_wrap ~len:len_p1 (!i + 1)) in
        let diff = Vec2.sub b a in
        let dist = Vec2.norm diff in
        if dist > eps
        then (
          let s1 = Vec2.{ a; b } in
          let s1_normal = { x = -.diff.y /. dist; y = diff.x /. dist } in
          let ref_v = Vec2.dot s1.a s1_normal
          and p2_idx = ref (!p1_idx + 1) in
          while !p2_idx < n do
            let last_signal = ref 0
            and p2 = paths.(!p2_idx) in
            let len_p2 = Array.length p2 in
            for j = 0 to len_p2 - 1 do
              let v = Vec2.dot p2.(j) s1_normal -. ref_v in
              if Float.abs v >= eps
              then (
                let signal = Int.of_float @@ Math.sign v in
                if signal * !last_signal < 0
                   && Vec2.line_intersection
                        ~eps
                        ~bounds1:(true, true)
                        ~bounds2:(true, true)
                        s1
                        Vec2.{ a = p2.(j); b = p2.(Util.index_wrap ~len:len_p2 (j + 1)) }
                      |> Option.is_some
                then raise (CrossIntersection (!p1_idx, !p2_idx));
                last_signal := signal )
            done;
            incr p2_idx
          done;
          incr i )
      done;
      incr p1_idx
    done;
    (* check for duplicate points *)
    let pts = Util.flatten_array paths in
    let len = Array.length pts in
    if len < 400
    then
      for i = 0 to len - 2 do
        for j = i + 1 to len - 1 do
          if Vec2.approx ~eps pts.(i) pts.(j) then raise DuplicatePoints
        done
      done
    else (
      let tree = BallTree2.make' pts in
      for i = 1 to len - 1 do
        match BallTree2.search_idxs ~radius:eps tree pts.(i) with
        | [] | [ _ ] -> () (* single result will be self *)
        | _          -> raise DuplicatePoints
      done )

let is_simple ?eps t =
  try
    validation ?eps t;
    true
  with
  | _ -> false

let make ?(validate = true) ?(holes = []) outer =
  let t = { outer; holes } in
  if validate
  then (
    validation t;
    t )
  else t

let circle ?fn r = make @@ Path2.circle ?fn r

let wedge ?fn ~centre ~radius ~start angle =
  { outer = Path2.arc ?fn ~wedge:true ~centre ~radius ~start angle; holes = [] }

let square ?center dims = make (Path2.square ?center dims)

let ring ?fn ~thickness r =
  if thickness < r
  then make ~holes:[ List.rev @@ Path2.circle ?fn (r -. thickness) ] (Path2.circle ?fn r)
  else invalid_arg "Ring thickness must be less than the outer radius."

let box ?center ~thickness dims =
  if thickness.x < dims.x && thickness.y < dims.y
  then (
    let holes = [ List.rev @@ Path2.square ?center (Vec2.sub dims thickness) ] in
    make ~holes (Path2.square ?center dims) )
  else invalid_arg "Box thicknesses must be less than the outer dimensions."

let bbox { outer; _ } = Path2.bbox outer
let centroid ?eps { outer; _ } = Path2.centroid ?eps outer

let area ?signed { outer; holes } =
  Path2.area ?signed outer
  -. List.fold_left (fun sum h -> Path2.area ?signed h +. sum) 0. holes

let map f { outer; holes } = { outer = f outer; holes = List.map f holes }

let offset ?fn ?fs ?fa ?closed ?check_valid mode =
  map (Offset.offset ?fn ?fs ?fa ?closed ?check_valid mode)

let translate p = map (Path2.translate p)
let rotate r = map (Path2.rotate r)
let rotate_about_pt r p = map (Path2.rotate_about_pt r p)
let scale s = map (Path2.scale s)
let mirror ax = map (Path2.mirror ax)

let to_scad ?convexity { outer; holes } =
  match holes with
  | []    -> Scad.polygon ?convexity outer
  | holes ->
    let _, points, paths =
      let f (i, points, paths) h =
        let i, points, path =
          let g (i, points, path) p = i + 1, p :: points, i :: path in
          List.fold_left g (i, points, []) h
        in
        i, points, path :: paths
      in
      List.fold_left f (0, [], []) (outer :: holes)
    in
    Scad.polygon ?convexity ~paths:(List.rev paths) (List.rev points)
