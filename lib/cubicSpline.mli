type boundary =
  [ `Quadratic
  | `NotAKnot
  | `Periodic
  | `Natural
  ]

type coefs =
  { a : float
  ; b : float
  ; c : float
  ; d : float
  }

(* Abstracted to protect array access *)
type t

val len : t -> int
val xmins : t -> float list
val xmaxs : t -> float list
val coefs : t -> coefs list
val get_xmin : t -> int -> float
val get_xmax : t -> int -> float
val get_coefs : t -> int -> coefs
val coefs_to_string : coefs -> string
val fit : ?boundary:boundary -> (float * float) list -> t
val extrapolate : t -> float -> float option
val extrapolate_path : t -> float list -> (float * float) list
val interpolate : t -> int -> (float * float) list
