(** A rotation matrix.

    Used in conjunction with {!module:Quaternion} to provide an additional means
    of rotating OpenSCAD objects and vectors ({!Vec3.t}). *)

type t = private float array array

(** [of_row_list_exn l]

    Create a rotation matrix from a list [l] of three rows. Throws an exception
    if [l] is not the correct length. *)
val of_row_list_exn : (float * float * float) list -> t

val of_row_list : (float * float * float) list -> (t, string) result

(** [of_col_list_exn l]

    Create a rotation matrix from a list [l] of three columns. Throws an
    exception if [l] is not the correct length. *)
val of_col_list_exn : (float * float * float) list -> t

val of_col_list : (float * float * float) list -> (t, string) result

(** [to_euler t]

    Convert [t] to an equivalent (x, y, z) euler rotation vector. *)
val to_euler : t -> Vec3.t

(** [transform t v]

    Apply rotation to the vector [v] via matrix multiplication with [t]. *)
val transform : t -> Vec3.t -> Vec3.t

val to_string : t -> string

include SquareMatrix.Ops with type t := t
