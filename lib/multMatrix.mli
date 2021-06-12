(** TODO: Add basic matrix operations, plan is to keep private so that mutations
 * cannot be done directly on the matrix, operations will return new matrices. *)
type t = private float array array

val of_list : float list list -> (t, string) result
val of_list_exn : float list list -> t
val to_string : t -> string
