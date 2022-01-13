(** Boundary condition for cubic spline fitting. *)
type boundary =
  [ `Quadratic
  | `NotAKnot
  | `Periodic
  | `Natural
  ]

type plane =
  [ `XY
  | `YZ
  | `XZ
  ]

(** Cubic spline coefficients *)
type coef =
  { a : float
  ; b : float
  ; c : float
  ; d : float
  }

(** Calculated coefficients along with the X ranges that they apply to.
    Abstracted to protect array access *)
type t

(** [len t]

    Length of the X range and coefficient arrays stored in [t]. *)
val len : t -> int

(** [xmins t]

    Minimum X values to which the coefficients at the same indices within [t] apply. *)
val xmins : t -> float list

(** [xmaxs t]

    Maximum X values to which the coefficients at the same indices within [t] apply. *)
val xmaxs : t -> float list

(** [coefs t]

    Coefficients describing a cubic spline, as calculated by {!fit}. *)
val coefs : t -> coef list

(** {1 Index getters } *)

val get_xmin : t -> int -> float option
val get_xmax : t -> int -> float option
val get_coef : t -> int -> coef option
val get_xmin_exn : t -> int -> float
val get_xmax_exn : t -> int -> float
val get_coef_exn : t -> int -> coef

(** [coef_to_string c]

    Show contents of [c] as a string. *)
val coef_to_string : coef -> string

(** [fit ?boundary ps]

    Calculate cubic spline coefficients with the [boundary] condition (defaults
    to [`Natural]) for the 2-dimensional control points [ps]. *)
val fit : ?boundary:boundary -> (float * float) list -> t

(** [extrapolate t x]

    Calculate the corresponding dependent value (e.g. [Some y]) for the given
    independent value [x] using the cubic spline fit coefficient in [t]. If [x]
    does not fall within the range of the control values used to generate [t],
    [None] is returned. *)
val extrapolate : t -> float -> float option

(** [extrapolate_path t xs]

    Use [t] to extrapolate [xs] into a 2-dimensional cubic spline path. *)
val extrapolate_path : t -> float list -> (float * float) list

(** [interpolate_path t n]

    Use [t] to interpolate 2-dimensional cubic spline path with [n] evently
    spaced points. *)
val interpolate_path : t -> int -> (float * float) list

(** [path_to_3d ?plane ps]

    Project the 2-dimensional path [ps] from [plane] (defaults to [`XY]) into
    three dimensions. *)
val path_to_3d : ?plane:plane -> (float * float) list -> (float * float * float) list
