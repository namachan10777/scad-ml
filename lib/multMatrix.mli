(** An affine transformation matrix.

    To be used with OpenSCADs
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#multmatrix}multmatrix},
    which is applied in this library with {!Scad.multmatrix}. *)

type t = private float array array

val of_row_list : (float * float * float * float) list -> (t, string) result
val of_row_list_exn : (float * float * float * float) list -> t
val to_string : t -> string

include SquareMatrix.Ops with type t := t
