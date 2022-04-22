open Cairo
open Vec

let path_to_outlines path =
  (* NOTE: Path.fold returns empty for me, while conversion to array first then
    folding works as expected. Perhaps open up an issue? *)
  let f ?(fn = 16) (paths, ps, last_p) = function
    | MOVE_TO (x, y) -> paths, ps, v2 x y
    | LINE_TO (x, y) -> paths, last_p :: ps, v2 x y
    | CURVE_TO (x1, y1, x2, y2, x3, y3) ->
      let bez = Bezier2.make' [| last_p; v2 x1 y1; v2 x2 y2; v2 x3 y3 |] in
      paths, Bezier2.curve ~fn ~rev:true ~endpoint:false ~init:ps bez, v2 x3 y3
    (* | CLOSE_PATH -> (last_p :: ps) :: paths, [], last_p *)
    | CLOSE_PATH -> ps :: paths, [], last_p
  in
  let ps, _, _ = Path.fold path f ([], [], v2 0. 0.) in
  List.rev_map (List.map @@ fun Vec.{ x; y } -> v2 x (-.y)) ps

(* NOTE: The paths drawn by cairo are sometimes closed with the final point being a
    duplicate of the first. Thus, I should not prepend the last point when
    CLOSE_PATH is hit if the first point is the same.

   TODO: more efficient than grabbing the last element? Is checking whether a
    first_p state param is empty (and setting it when adding a point) on each
    element worth it instead? *)
let pathdata_to_outlines ?(fn = 5) data =
  let f (paths, ps, last_p) = function
    | MOVE_TO (x, y) -> paths, ps, v2 x y
    | LINE_TO (x, y) -> paths, last_p :: ps, v2 x y
    | CURVE_TO (x1, y1, x2, y2, x3, y3) ->
      let bez = Bezier2.make' [| last_p; v2 x1 y1; v2 x2 y2; v2 x3 y3 |] in
      paths, Bezier2.curve ~fn ~rev:true ~endpoint:false ~init:ps bez, v2 x3 y3
    | CLOSE_PATH ->
      let path =
        match ps with
        | [] -> [ last_p ]
        | _  -> if Vec2.approx (Util.last_element ps) last_p then ps else last_p :: ps
      in
      path :: paths, [], last_p
  in
  let paths, _, _ = Array.fold_left f ([], [], v2 0. 0.) data in
  List.rev_map (List.map @@ fun Vec.{ x; y } -> v2 x (-.y)) paths

let glyph_outline ?fn ?(center = false) ?weight ~font char =
  let s = String.of_seq (Seq.return char)
  and cr = create (Image.create Image.A1 ~w:1 ~h:1) in
  select_font_face ?weight cr font;
  scale cr 1. 1.;
  set_font_size cr 10.;
  let te = text_extents cr s in
  if center
  then (
    let x = 0.5 -. (te.width /. 2.) -. te.x_bearing
    and y = 0.5 -. (te.height /. 2.) -. te.y_bearing in
    move_to cr x y )
  else move_to cr 0. 0.5;
  Path.text cr s;
  (* Path.glyph cr [| { index = 30; x = 0.; y = 0. } |]; *)
  match pathdata_to_outlines ?fn Path.(to_array @@ copy cr) with
  (* match path_to_outlines (Path.copy cr) with *)
  | [] -> Poly2.{ outer = []; holes = [] }
  | outer :: holes -> { outer; holes }

(* TODO: need to check whether paths that follow the first are actually
    contained within it. If they are not, they should be split into their own
    shape. For example '!' is not actually an outer shape with holes. There are
    also going to be cases where there are multiple shapes with holes, and I'm
    not sure whether the paths coming out from Cairo will not jump back and
    forth between "polygons", mixing inner and outer paths. *)
let text ?fn ?(center = false) ?weight ~font txt =
  let ctxt = create (Image.create Image.A1 ~w:1 ~h:1) in
  select_font_face ?weight ctxt font;
  scale ctxt 1. 1.;
  set_font_size ctxt 10.;
  let te = text_extents ctxt txt in
  if center
  then (
    let x = 0.5 -. (te.width /. 2.) -. te.x_bearing
    and y = 0.5 -. (te.height /. 2.) -. te.y_bearing in
    move_to ctxt x y )
  else move_to ctxt 0. 0.5;
  let f acc c =
    let s = String.make 1 c in
    Path.text ctxt s;
    let acc =
      match pathdata_to_outlines ?fn Path.(to_array @@ copy ctxt) with
      | []          -> acc
      | outer :: tl ->
        let rec aux polys outer holes = function
          | []                    -> Poly2.{ outer; holes } :: polys
          | (pt :: _ as hd) :: tl ->
            ( match Path2.point_inside outer pt with
            | `Inside -> aux polys outer (hd :: holes) tl
            | _       -> aux (Poly2.{ outer; holes } :: polys) hd [] tl )
          | _                     -> aux polys outer holes tl
        in
        aux acc outer [] tl
    in
    let x, y = Path.get_current_point ctxt in
    Path.clear ctxt;
    move_to ctxt x y;
    acc
  in
  String.fold_left f [] txt
