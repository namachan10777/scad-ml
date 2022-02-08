type t = Vec2.t list

let circle ?(fn = 30) r =
  let s = 2. *. Float.pi /. Float.of_int fn in
  let f i =
    let a = s *. Float.of_int i in
    [ r *. Float.cos a; r *. Float.sin a ]
  in
  List.init fn f

let square ?(center = false) (x, y) =
  if center
  then (
    let x' = x /. 2.
    and y' = y /. 2. in
    [ x', y'; -.x', y'; -.x', -.y'; x', -.y' ] )
  else [ 0., 0.; x, 0.; x, y; 0., y ]

let offset = Offset2d.offset
let translate = Path2d.translate
let rotate = Path2d.rotate
let rotate_about_pt = Path2d.rotate_about_pt
let scale = Path2d.scale
let mirror = Path2d.mirror
