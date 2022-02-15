(* https://github.com/RonaldoCMP/Polygon-stuffs/blob/master/polyHolePartition.scad *)

type tag =
  | Outer
  | Inner of int

type id =
  { p : Vec2.t
  ; tag : tag
  }

(* check if point p is within the CCW triangle described by p1, p2, and p3. *)
let in_tri ~p1 ~p2 ~p3 p =
  Float.equal 1. (Vec2.clockwise_sign p1 p p2)
  && Float.equal 1. (Vec2.clockwise_sign p2 p p3)

(* find closest intersect on the outer path made with rightward horizontal ray
   from point p *)
let outer_intersect ((_, y) as p) outer =
  let len = Array.length outer in
  let seg_idx = ref 0
  and out_x = ref Float.infinity
  and out_y = ref 0. in
  let update i (inter_x, inter_y) =
    if inter_x < !out_x
    then (
      seg_idx := i;
      out_x := inter_x;
      out_y := inter_y )
  in
  for i = 0 to len - 1 do
    let ((_, yo1) as po1) = outer.(i)
    and ((_, yo2) as po2) = outer.(Util.index_wrap ~len (i + 1)) in
    if Float.equal (-1.) (Vec2.clockwise_sign p po1 po2)
    then
      if Float.equal y yo1
      then update i po1
      else if Float.equal y yo2
      then update i po2
      else if yo1 < y && yo2 >= y
      then (
        let u = (y -. yo2) /. (yo1 -. yo2) in
        update i (Vec2.lerp po1 po2 u) )
  done;
  if Float.is_infinite !out_x then raise (Failure "Invalid input polygons.");
  !seg_idx, (!out_x, !out_y)

(* Find a bridge between the point p (in the interior of poly outer) and a
    vertex (given by index) in the outer path. *)
let bridge_to_outer ((x, y) as p) outer =
  let seg_idx, intersect = outer_intersect p outer
  and len = Array.length outer in
  let next_idx = Util.index_wrap ~len (seg_idx + 1) in
  let ((seg_x, _) as seg) = outer.(seg_idx)
  and ((next_x, _) as next) = outer.(next_idx) in
  let first, valid_candidate =
    if seg_x > x || next_x <= x
    then
      ( seg_idx
      , fun i ->
          let ((_, vy) as v) = outer.(i) in
          vy < y && in_tri ~p1:seg ~p2:p ~p3:v intersect )
    else
      ( next_idx
      , fun i ->
          let ((_, vy) as v) = outer.(i) in
          vy > y && in_tri ~p1:v ~p2:p ~p3:next intersect )
  in
  let idx = ref first
  and min_x = ref @@ Vec2.get_x outer.(first) in
  for i = 0 to len - 1 do
    if valid_candidate i
    then (
      let cx, _ = outer.(i) in
      if cx < !min_x then min_x := cx;
      idx := i )
  done;
  !idx

(* Right-most extreme vertices of each hole sorted in descending order.
   Returns: (hole index, vertex index, x max) *)
let extremes inners =
  let max_x (i, idx, m) x = if x > m then i + 1, i, x else i + 1, idx, m in
  let xs =
    Array.init (Array.length inners) (fun i ->
        let _, idx, mx = Array.fold_left max_x (0, 0, Float.min_float) inners.(i) in
        i, idx, mx )
  in
  Array.sort (fun (_, _, x1) (_, _, x2) -> Float.compare x2 x1) xs;
  xs
