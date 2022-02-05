module type Vec = sig
  type t

  (** Zero vector = [(0., 0., 0.)] *)
  val zero : t

  val equal : t -> t -> bool

  (** [approx ?eps a b]

    Returns true if the distance between vectors [a] and [b] is less than or
    equal to the epsilon [eps]. *)
  val approx : ?eps:float -> t -> t -> bool

  (** {1 Basic Arithmetic} *)

  (** [horizontal_op f a b]

    Hadamard (element-wise) operation between vectors [a] and [b] using the
    function [f]. *)
  val horizontal_op : (float -> float -> float) -> t -> t -> t

  (** [add a b]

    Hadamard (element-wise) addition of vectors [a] and [b]. *)
  val add : t -> t -> t

  (** [sub a b]

    Hadamard (element-wise) subtraction of vector [b] from [a]. *)
  val sub : t -> t -> t

  (** [mul a b]

    Hadamard (element-wise) product of vectors [a] and [b]. *)
  val mul : t -> t -> t

  (** [div a b]

    Hadamard (element-wise) division of vector [a] by [b]. *)
  val div : t -> t -> t

  (** [negate t]

    Negation of all elements of [t]. *)
  val negate : t -> t

  (** [add_scalar t s]

    Element-wise addition of [s] to [t]. *)
  val add_scalar : t -> float -> t

  (** [sub_scalar t s]

    Element-wise subtraction of [s] from [t]. *)
  val sub_scalar : t -> float -> t

  (** [mul_scalar t s]

    Element-wise multiplication of [t] by [s]. *)
  val mul_scalar : t -> float -> t

  (** [div_scalar t s]

    Element-wise division of [t] by [s]. *)
  val div_scalar : t -> float -> t

  (** {1 Vector Math} *)

  (** [norm t]

    Calculate the vector norm (a.k.a. magnitude) of [t]. *)
  val norm : t -> float

  (** [distance a b]

    Calculate the magnitude of the difference (Hadamard subtraction) between [a]
    and [b]. *)
  val distance : t -> t -> float

  (** [normalize t]

    Normalize [t] to a vector for which the magnitude is equal to 1.
    e.g. [norm (normalize t) = 1.] *)
  val normalize : t -> t

  (** [dot a b]

    Vector dot product of [a] and [b]. *)
  val dot : t -> t -> float

  (** [cross a b]

    Vector cross product of [a] and [b]. In the case of 2d vectors, the cross
    product is performed with an assumed z = 0. *)
  val cross : t -> t -> float * float * float

  (** [mean l]

    Calculate the mean / average of all vectors in [l]. *)
  val mean : t list -> t

  (** [angle a b]

    Calculate the angle between the vectors [a] and [b]. *)
  val angle : t -> t -> float

  (** [angle_points a b c]

    Calculate the angle between the points [a], [b], and [c]. *)
  val angle_points : t -> t -> t -> float

  (** [clockwise_sign a b c]

    Returns the rotational ordering of the points [a], [b], and [c] as a signed
   float, [-1.] for clockwise, and [1.] for counter-clockwise. *)
  val clockwise_sign : t -> t -> t -> float

  (** [colinear p1 p2 p3]

    Returns [true] if [p2] lies on the line between [p1] and [p3]. *)
  val colinear : t -> t -> t -> bool

  (** [lerp a b u]

    Linearly interpolate between vectors [a] and [b]. *)
  val lerp : t -> t -> float -> t

  (** [lerpn a b n]

    Linearly interpolate [n] vectors between vectors [a] and [b]. If [endpoint]
    is [true], the last vector will be equal to [b], otherwise, it will be about
    [a + (b - a) * (1 - 1 / n)]. *)
  val lerpn : ?endpoint:bool -> t -> t -> int -> t list

  (** {1 Utilities} *)

  val map : (float -> float) -> t -> t
  val get_x : t -> float
  val get_y : t -> float
  val get_z : t -> float
  val get_xy : t -> float * float
  val to_string : t -> string

  (** [deg_of_rad t]

    Element-wise conversion of [t] from radians to degrees. *)
  val deg_of_rad : t -> t

  (** [rad_to_deg t]

    Element-wise conversion of [t] from degrees to radians. *)
  val rad_of_deg : t -> t

  (** {1 Infix operations} *)

  (** [a <+> b]

    Hadamard (element-wise) addition of [a] and [b]. *)
  val ( <+> ) : t -> t -> t

  (** [a <-> b]

    Hadamard (element-wise) subtraction of [b] from [a]. *)
  val ( <-> ) : t -> t -> t

  (** [a <*> b]

    Hadamard (element-wise) product of [a] and [b]. *)
  val ( <*> ) : t -> t -> t

  (** [a </> b]

    Hadamard (element-wise) division of [a] by [b]. *)
  val ( </> ) : t -> t -> t
end

module type ArcProvider = sig
  type vec

  val arc
    :  ?init:vec list
    -> ?rev:bool
    -> ?fn:int
    -> centre:vec
    -> radius:float
    -> start:float
    -> float
    -> vec list

  val arc_about_centre
    :  ?init:vec list
    -> ?rev:bool
    -> ?fn:int
    -> centre:vec
    -> vec
    -> vec
    -> vec list

  val arc_through
    :  ?init:vec list
    -> ?rev:bool
    -> ?fn:int
    -> vec
    -> vec
    -> vec
    -> vec list
end
