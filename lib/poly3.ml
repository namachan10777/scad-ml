open! Vec

type t =
  { outer : Vec3.t list
  ; holes : Vec3.t list list
  }

let of_poly2 ?(plane = Plane.xy) Poly2.{ outer; holes } =
  let f = Path2.lift plane in
  { outer = f outer; holes = List.map f holes }

let to_poly2 ?(validate = false) ?(plane = Plane.xy) { outer; holes } =
  let f = Path3.project plane in
  Poly2.make ~validate ~holes:(List.map f holes) (f outer)

let make ?(validate = true) ?(holes = []) outer =
  let plane = Path3.to_plane outer in
  if not validate
  then { outer; holes }
  else (
    let coplanar = Plane.are_points_on plane in
    if coplanar outer && List.for_all coplanar holes
    then of_poly2 ~plane @@ to_poly2 ~validate:true ~plane { outer; holes }
    else invalid_arg "Polygon contains non-coplanar points." )

let circle ?fn ?plane r = { outer = Path3.circle ?fn ?plane r; holes = [] }

let wedge ?fn ?plane ~centre ~radius ~start angle =
  { outer = Path3.arc ?fn ?plane ~wedge:true ~centre ~radius ~start angle; holes = [] }

let square ?center ?plane dims = { outer = Path3.square ?center ?plane dims; holes = [] }

let ring ?fn ?(plane = Plane.xy) ~thickness r =
  let Poly2.{ outer; holes } = Poly2.ring ?fn ~thickness r in
  { outer = Path2.lift plane outer; holes = List.map (Path2.lift plane) holes }

let box ?center ?(plane = Plane.xy) ~thickness dims =
  let Poly2.{ outer; holes } = Poly2.box ?center ~thickness dims in
  { outer = Path2.lift plane outer; holes = List.map (Path2.lift plane) holes }

let bbox { outer; _ } = Path3.bbox outer
let centroid ?eps { outer; _ } = Path3.centroid ?eps outer

let area ?signed { outer; holes } =
  Path3.area ?signed outer
  -. List.fold_left (fun sum h -> Path3.area ?signed h +. sum) 0. holes

let map f { outer; holes } = { outer = f outer; holes = List.map f holes }

let offset ?fn ?fs ?fa ?check_valid mode t =
  let plane = Path3.to_plane t.outer in
  let f p =
    Offset.offset ?fn ?fs ?fa ~closed:true ?check_valid mode (Path2.of_path3 ~plane p)
    |> Path2.lift plane
  in
  map f t

let translate p = map (Path3.translate p)
let rotate r = map (Path3.rotate r)
let rotate_about_pt r p = map (Path3.rotate_about_pt r p)
let scale s = map (Path3.scale s)
let mirror ax = map (Path3.mirror ax)
let multmatrix m = map (Path3.multmatrix m)
