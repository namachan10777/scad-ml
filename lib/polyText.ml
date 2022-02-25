open Vec
open Cairo

type glyph_outline =
  { outer : Vec2.t list
  ; inner : Vec2.t list list
  }

let path_to_outlines path =
  (* NOTE: Path.fold returns empty for me, while conversion to array first then
    folding works as expected. Perhaps open up an issue? *)
  let f ?(fn = 16) (paths, ps, last_p) = function
    | MOVE_TO (x, y) -> paths, ps, v2 x y
    | LINE_TO (x, y) -> paths, last_p :: ps, v2 x y
    | CURVE_TO (x1, y1, x2, y2, x3, y3) ->
      let bez = Bezier2d.make' [| last_p; v2 x1 y1; v2 x2 y2; v2 x3 y3 |] in
      paths, Bezier2d.curve ~fn ~rev:true ~endpoint:false ~init:ps bez, v2 x3 y3
    | CLOSE_PATH -> (last_p :: ps) :: paths, [], last_p
  in
  let ps, _, _ = Path.fold path f ([], [], v2 0. 0.) in
  List.rev_map (List.map @@ fun Vec2.{ x; y } -> v2 x (-.y)) ps

let pathdata_to_outlines ?(fn = 16) data =
  let f (paths, ps, last_p) = function
    | MOVE_TO (x, y) -> paths, ps, v2 x y
    | LINE_TO (x, y) -> paths, last_p :: ps, v2 x y
    | CURVE_TO (x1, y1, x2, y2, x3, y3) ->
      let bez = Bezier2d.make' [| last_p; v2 x1 y1; v2 x2 y2; v2 x3 y3 |] in
      paths, Bezier2d.curve ~fn ~rev:true ~endpoint:false ~init:ps bez, v2 x3 y3
    | CLOSE_PATH -> (last_p :: ps) :: paths, [], last_p
  in
  let ps, _, _ = Array.fold_left f ([], [], v2 0. 0.) data in
  List.rev_map (List.map @@ fun Vec2.{ x; y } -> v2 x (-.y)) ps

let glyph_outline ?(center = false) ?weight ~font char =
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
  match pathdata_to_outlines Path.(to_array @@ copy cr) with
  (* match path_to_outlines (Path.copy cr) with *)
  | [] -> { outer = []; inner = [] }
  | outer :: inner -> { outer; inner }

(* Should there be a global context that is just cleared out at every usage? Or
    a new one each time toplevel function that takes a whole string is used?
    Seem like it can be pretty cheap, so maybe just for each string (not just
    char level like this, since usually you'll want word/phrase)

   For multiple characters:
   - clear path and move redo the move operation that was at the end of the last
    character
   - thus the relative positions are preserved, while the clearing allows
    unambiguous segregation of the outer/inner paths for each character
   - initial move_to using text extents based on the alignment/anchoring
    (centre) option given to the top-level function? (will likely require use of
   text extent on the whole string to get the sizing)

   Note that from the one check I did so far, the letter is upside down, so the
    coordinate system is flipped at the least.
*)
