(** Types pertaining to the configuration of OpenSCAD text objects. *)

type h_align =
  | Left
  | Center
  | Right

let h_align_to_string = function
  | Left   -> "left"
  | Center -> "center"
  | Right  -> "right"

type v_align =
  | Top
  | Center
  | Baseline

let v_align_to_string = function
  | Top      -> "top"
  | Center   -> "center"
  | Baseline -> "baseline"

type direction =
  | LeftToRight
  | RightToLeft
  | TopToBottom
  | BottomToTop

let direction_to_string = function
  | LeftToRight -> "ltr"
  | RightToLeft -> "rtl"
  | TopToBottom -> "ttb"
  | BottomToTop -> "btt"

type t =
  { text : string
  ; size : float option
  ; font : string option
  ; halign : h_align option
  ; valign : v_align option
  ; spacing : float option
  ; direction : direction option
  ; language : string option
  ; script : string option
  ; fn : int option
  }
