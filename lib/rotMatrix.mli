(** A rotation matrix.

    Used in conjunction with {!module:Quaternion} to provide an additional means
    of rotating OpenSCAD objects and vectors ({!Vec3.t}). *)

type t = private float array array

val of_row_list_exn : (float * float * float) list -> t
val of_row_list : (float * float * float) list -> (t, string) result
val of_col_list_exn : (float * float * float) list -> t
val of_col_list : (float * float * float) list -> (t, string) result
val align_exn : Vec3.t -> Vec3.t -> t
val align : Vec3.t -> Vec3.t -> (t, string) result
val to_euler : t -> Vec3.t
val trace : t -> float
val get : t -> int -> int -> float

include SquareMatrix.Ops with type t := t
