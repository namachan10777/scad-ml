open Vec

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
*)
let make ?(holes = []) outer = { outer; holes }
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
