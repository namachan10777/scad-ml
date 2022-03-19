open Vec

type t =
  { outer : Vec2.t list
  ; holes : Vec2.t list list
  }

(* TODO: validate non-intersecting / enough points polygon at creation, and
    protect the type? *)
let make ?(holes = []) outer = { outer; holes }
let circle ?fn r = make @@ Path2.circle ?fn r
let square ?center dims = make (Path2.square ?center dims)

let centroid ?(eps = Util.epsilon) { outer; _ } =
  match outer with
  | [] | [ _ ] | [ _; _ ] -> invalid_arg "Polygon must have more than two points."
  | p0 :: p1 :: tl        ->
    let f (area_sum, p_sum, p1) p2 =
      let { z = area; _ } = Vec2.(cross (sub p2 p0) (sub p1 p0)) in
      area +. area_sum, Vec2.(add p_sum (add p0 (add p1 p2))), p2
    in
    let area_sum, p_sum, _ = List.fold_left f (0., Vec2.zero, p1) tl in
    if Math.approx ~eps area_sum 0.
    then invalid_arg "The polygon is self-intersecting, or its points are collinear.";
    Vec2.(sdiv p_sum (area_sum *. 3.))

let area ?(signed = false) { outer; holes } =
  let aux = function
    | [] | [ _ ] | [ _; _ ] -> 0.
    | p0 :: p1 :: tl        ->
      let f (area, p1) p2 =
        (area +. Vec2.(Vec3.get_z (cross (sub p1 p0) (sub p2 p0)))), p2
      in
      let area, _ = List.fold_left f (0., p1) tl in
      (if signed then area else Float.abs area) /. 2.
  in
  aux outer -. List.fold_left (fun sum h -> aux h +. sum) 0. holes

let map f { outer; holes } = { outer = f outer; holes = List.map f holes }

let offset ?fn ?fs ?fa ?closed ?check_valid mode t =
  map (Offset.offset ?fn ?fs ?fa ?closed ?check_valid mode) t

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
