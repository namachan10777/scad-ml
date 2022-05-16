(** {1 Float operations} *)

(** [deg_of_rad r] *)
val deg_of_rad : float -> float

(** [rad_of_deg d] *)
val rad_of_deg : float -> float

(** [sign v] *)
val sign : Float.t -> float

(** [clamp ~min ~max v] *)
val clamp : min:'a -> max:'a -> 'a -> 'a

(** [lerp a b u] *)
val lerp : float -> float -> float -> float

(** [lerpn ?endpoint a b n] *)
val lerpn : ?endpoint:bool -> float -> float -> int -> float list

(** [quant ~q v] *)
val quant : q:float -> float -> float

(** [quant_down ~q v] *)
val quant_down : q:float -> float -> float

(** [quant_up ~q v] *)
val quant_up : q:float -> float -> float

(** [approx ?eps a b] *)
val approx : ?eps:Float.t -> float -> float -> bool

(** [law_of_cosines a b c] *)
val law_of_cosines : float -> float -> float -> float

(** [posmod a m]

    Compute the positive modulo [m] of [a]. The resulting value will be in the
    range of [0.] to [m -. 1.]. *)
val posmod : float -> float -> float

(** {1 2d matrix operations} *)

(** [mat_dims m]

    Return the dimensions ([(n_rows, n_cols)] of the 2d matrix [m]. An
    [Invalid_argument] exception is raised if the row lengths are inconsistent. *)
val mat_dims : 'a array array -> int * int

(** [matmul a b]

    Matrix multiplication of [a] and [b]. [Invalid_argument] is raised if the
    inner dimensions do not match, or if the rows of either matrix are ragged. *)
val matmul : float array array -> float array array -> float array array

(** [transpose m]

    Transpose rows and columns of the 2d matrix [m]. *)
val transpose : 'a array array -> 'a array array

(** {1 Polynomials} *)

(** [real_roots ?eps ?tol p]

    Compute the real roots of the real polynomial [p]. The polynomial is
    specified as [[|a_n; ...; a_1; a_0|]] where [a_n] is the [x ** n]
    coefficient.

    - [eps] is used to determine if the imaginary parts of the roots are zero
    - [tol] is the tolerance for the complex polynomial root finder

    Adapted from the [real_roots] function found in the
    {:{https://github.com/revarbat/BOSL2/blob/master/math.scad#L1361} BOSL2 math
    module}. *)
val real_roots : ?eps:float -> ?tol:float -> float array -> float array
