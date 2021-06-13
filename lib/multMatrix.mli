(** TODO: Add basic matrix operations, plan is to keep private so that mutations
 * cannot be done directly on the matrix, operations will return new matrices. *)
type t = private float array array

val of_row_list : (float * float * float * float) list -> (t, string) result
val of_row_list_exn : (float * float * float * float) list -> t
val to_string : t -> string
