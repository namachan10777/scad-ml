open Util

type t = Vec2.t list

(* Negative = CCW *)
(* TODO: CCW should be positive, check that this is indeed backwards and fix *)
let clockwise_sign' ps =
  let len = Array.length ps
  and sum = ref 0. in
  for i = 0 to len - 1 do
    let x0, y0 = ps.(index_wrap ~len i)
    and x1, y1 = ps.(index_wrap ~len (i + 1)) in
    sum := !sum +. ((x0 -. x1) *. (y0 +. y1))
  done;
  Float.(of_int @@ compare !sum 0.)

let clockwise_sign ps = clockwise_sign' (Array.of_list ps)

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

let translate = Path2d.translate
let rotate = Path2d.rotate
let rotate_about_pt = Path2d.rotate_about_pt
let scale = Path2d.scale
let mirror = Path2d.mirror
