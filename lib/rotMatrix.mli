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

(** [align_exn a b]

    Calculate a rotation matrix that would bring [a] into alignment with [b].
    Throws an exception if the vectors are equal or zero. *)
val align_exn : Vec3.t -> Vec3.t -> t

val align : Vec3.t -> Vec3.t -> (t, string) result

(** [to_euler t]

    Convert [t] to an equivalent (x, y, z) euler rotation vector. *)
val to_euler : t -> Vec3.t

(** [trace t]

    Sum the elements on the main diagonal (upper left to lower right) of [t]. *)
val trace : t -> float

(** [get t r c]

    Get the element at [r]ow and [c]olumn of [t]. Equivalent to [t.(r).(c)]. *)
val get : t -> int -> int -> float

include SquareMatrix.Ops with type t := t
