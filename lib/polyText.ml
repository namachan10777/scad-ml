open Cairo

type glyph_outline =
  { outer : (float * float) list
  ; inner : (float * float) list list
  }

let path_to_vec2 path =
  (* NOTE: Path.fold returns empty for me, while conversion to array first then
    folding works as expected. Perhaps open up an issue? *)
  let f (paths, ps, last_p) = function
    | MOVE_TO (x, y) -> paths, ps, (x, y)
    | LINE_TO (x, y) -> paths, (x, y) :: last_p :: ps, (x, y)
    | CURVE_TO (x1, y1, x2, y2, x3, y3) ->
      paths, (x3, y3) :: (x2, y2) :: (x1, y1) :: ps, (x3, y3)
    | CLOSE_PATH -> ps :: paths, [], last_p
  in
  let ps, _, _ = Path.fold path f ([], [], (0., 0.)) in
  ps

let path_array_to_vec2 ?(fn = 10) data =
  (* NOTE: CURVE_TO actual impl is as a cubic bezier, from the current point
    (last_p), to the last control point (x3, y3) *)
  (*  TODO: decide on a sensible rule on when points are added to the line. For
    line_to does it make sense to just add the last, and move to the next pos?
    Then CLOSE_PATH added the final position? *)
  let f (paths, ps, last_p) = function
    | MOVE_TO (x, y) -> paths, ps, (x, y)
    | LINE_TO (x, y) -> paths, last_p :: ps, (x, y)
    | CURVE_TO (x1, y1, x2, y2, x3, y3) ->
      let bez = Bezier2d.make' [| last_p; x1, y1; x2, y2; x3, y3 |] in
      paths, Bezier2d.curve ~fn ~rev:true ~endpoint:false ~init:ps bez, (x3, y3)
    | CLOSE_PATH -> (last_p :: ps) :: paths, [], last_p
  in
  let ps, _, _ = Array.fold_left f ([], [], (0., 0.)) data in
  List.map (List.map @@ fun (x, y) -> x, 1. -. y) ps

let glyph_outline ?weight ~font char =
  let s = String.of_seq (Seq.return char)
  and cr = create (Image.create Image.A1 ~w:1 ~h:1) in
  select_font_face ?weight cr font;
  let te = text_extents cr s in
  move_to
    cr
    (0.5 -. (te.width /. 2.) -. te.x_bearing)
    (0.5 -. (te.height /. 2.) -. te.y_bearing);
  Path.text cr s;
  match List.rev @@ path_array_to_vec2 Path.(to_array @@ copy cr) with
  | []             -> { outer = []; inner = [] }
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
