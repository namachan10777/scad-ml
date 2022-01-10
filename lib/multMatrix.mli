(** An affine transformation matrix.

    To be used with OpenSCADs
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language#multmatrix}multmatrix},
    which is applied in this library with {!Scad.multmatrix}. *)

type t = private float array array

(** [of_row_list_exn l]

    Create an affine transformation matrix from a list [l] of three or four
    rows. If only three rows are provided, the final row is set to [[| 0.; 0.; 0.; 1. |]].
    Throws an exception if [l] is not the correct length. *)
val of_row_list_exn : (float * float * float * float) list -> t

val of_row_list : (float * float * float * float) list -> (t, string) result

(** [of_rot_matrix r v]

    Create an affine transformation matrix from a 3x3 rotation matrix [r] and a
    translation vector [v]. *)
val of_rotmatrix : RotMatrix.t -> Vec3.t -> t

(** [scaling v]

    Create an affine transformation matrix from the xyz scaling vector [v]. *)
val scaling : Vec3.t -> t

(** [translation v]

    Create an affine transformation matrix from the xyz translation vector [v]. *)
val translation : Vec3.t -> t

(** [transform t v]

    Apply the affine transformation matrix [t] to the vector [v]. *)
val transform : t -> Vec3.t -> Vec3.t

val to_string : t -> string

include SquareMatrix.Ops with type t := t
