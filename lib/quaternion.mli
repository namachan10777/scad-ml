type t = float * float * float * float

val id : t
val make : float * float * float -> float -> t
val add : t -> t -> t
val sub : t -> t -> t
val add_scalar : t -> float -> t
val sub_scalar : t -> float -> t
val scalar_sub_quat : t -> float -> t
val mul : t -> t -> t
val mul_scalar : t -> float -> t
val div_scalar : t -> float -> t
val negate : t -> t
val norm : t -> float
val normalize : t -> t
val dot : t -> t -> float
val conj : t -> t
val distance : t -> t -> float
val to_multmatrix : t -> MultMatrix.t
val to_string : t -> string
val get_x : t -> float
val get_y : t -> float
val get_z : t -> float
val get_w : t -> float
val get_ax : t -> float * float * float
val get_angle : t -> float
