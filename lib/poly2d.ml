open Vec

type t = Vec2.t list

(* module Round : Rounding.S with type vec := Vec2.t = Rounding.Make (Vec2) (Path2d) *)

let circle ?(fn = 30) r =
  let s = 2. *. Float.pi /. Float.of_int fn in
  let f i =
    let a = s *. Float.of_int i in
    v2 (r *. Float.cos a) (r *. Float.sin a)
  in
  List.init fn f

let square ?(center = false) { x; y } =
  if center
  then (
    let x' = x /. 2.
    and y' = y /. 2. in
    Vec2.[ v x' y'; v (-.x') y'; v (-.x') (-.y'); v x' (-.y') ] )
  else Vec2.[ v 0. 0.; v x 0.; v x y; v 0. y ]

let offset = Offset2d.offset
let translate = Path2d.translate
let rotate = Path2d.rotate
let rotate_about_pt = Path2d.rotate_about_pt
let scale = Path2d.scale
let mirror = Path2d.mirror

let to_scad ?convexity ?holes t =
  match holes with
  | Some holes ->
    let _, points, paths =
      let f (i, points, paths) h =
        let i, points, path =
          let g (i, points, path) p = i + 1, p :: points, i :: path in
          List.fold_left g (i, points, []) h
        in
        i, points, path :: paths
      in
      List.fold_left f (0, [], []) (t :: holes)
    in
    Scad.polygon ?convexity ~paths:(List.rev paths) (List.rev points)
  | None       -> Scad.polygon ?convexity t
