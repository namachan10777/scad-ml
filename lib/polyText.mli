(** Point path representations (using {!Poly2.t}) of text via
   {{:https://github.com/Chris00/ocaml-cairo} ocaml-cairo} import of system
    fonts. {b NOTE:} Still somewhat experimental. *)

val text
  :  ?fn:int
  -> ?center:bool
  -> ?slant:Cairo.slant
  -> ?weight:Cairo.weight
  -> ?size:float
  -> font:string
  -> string
  -> Poly2.t list
