type t = float * float * float

(** Zero vector = (0., 0., 0.) *)
val zero : t

val equal : t -> t -> bool

(** {1 Basic Arithmetic} *)

val horizontal_op : (float -> float -> float) -> t -> t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val negate : t -> t
val add_scalar : t -> float -> t
val sub_scalar : t -> float -> t
val mul_scalar : t -> float -> t
val div_scalar : t -> float -> t

(** {1 Vector Math} *)

val norm : t -> float
val distance : t -> t -> float
val normalize : t -> t
val dot : t -> t -> float
val cross : t -> t -> t
val mean : t list -> t

(** {1 Transformations} *)

val rotate_x : float -> t -> t
val rotate_y : float -> t -> t
val rotate_z : float -> t -> t
val rotate : t -> t -> t
val rotate_about_pt : t -> t -> t -> t
val translate : t -> t -> t
val scale : t -> t -> t
val mirror : t -> t -> t
val projection : t -> t

(** {1 Utilities} *)

val map : (float -> 'b) -> t -> 'b * 'b * 'b
val get_x : t -> float
val get_y : t -> float
val get_z : t -> float
val to_string : t -> string
val deg_of_rad : t -> t
val rad_of_deg : t -> t

(** {1 2d - 3d conversion} *)

val to_vec2 : t -> float * float
val of_vec2 : float * float -> t

(** {1 Infix operations} *)

(** [a <+> b]

    Add infix. *)
val ( <+> ) : t -> t -> t

(** [a <-> b]

    Subtract infix. *)
val ( <-> ) : t -> t -> t

(** [a <*> b]

    Multiplication infix. *)
val ( <*> ) : t -> t -> t

(** [a <*> b]

    Division infix. *)
val ( </> ) : t -> t -> t
