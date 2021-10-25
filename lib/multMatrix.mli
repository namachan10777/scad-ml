(** An affine transformation matrix.

    To be used with OpenSCADs
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#multmatrix}
    multmatrix}, which is applied in this library with {!Scad.multmatrix}. *)

type t = private float array array

val of_row_list_exn : (float * float * float * float) list -> t
(** [of_row_list_exn l]

    Create a rotation matrix from a list [l] of three or four rows. If only
    three rows are provided, the final row is set to [\[| 0.; 0.; 0.; 1. |\]]
    Throws an exception if [l] is not the correct length. *)

val of_row_list : (float * float * float * float) list -> (t, string) result
val to_string : t -> string

include SquareMatrix.Ops with type t := t
