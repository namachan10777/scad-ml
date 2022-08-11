type t = Vec.v2 =
  { x : float
  ; y : float
  }

(** [v x y]

    Construct a vector from [x] and [y] coordinates. *)
val v : float -> float -> t

(** [of_tup (x, y)]

    Construct a vector from a tuple of xy coordinates. *)
val of_tup : float * float -> t

(** [to_tup t]

    Convert the vector [t] to a tuple of xy coordinates. *)
val to_tup : t -> float * float

include Vec.S with type t := t (** @inline *)

(** [bbox_area bb]

    Compute the area of the bounding box [bb]. *)
val bbox_area : bbox -> float

(** [left_of_line ?eps ~line t]

    Return [1.] if [t] is left of [line], [-1.] if it is to the right, and [0.]
    if it falls on (within [eps]) the [line]. Float is returned as this is
    simply a clockwise check. *)
val left_of_line : ?eps:float -> line:line -> t -> float

(** [line_intersection ?eps ?bounds1 ?bounds2 a b]

    Find the intersection (if it exists) between the lines [a] and [b].
    [bounds1] and [bounds2] indicate whether the ends of [a] and [b]
    respectively are bounded or are infinite rays. *)
val line_intersection
  :  ?eps:float
  -> ?bounds1:bool * bool
  -> ?bounds2:bool * bool
  -> line
  -> line
  -> t option

(** [line_normal p1 p2]

    Calculates the normal (perpendicular vector) of the line between [p1] and
    [p2]. *)
val line_normal : t -> t -> t

(** {1 Transformations}

    Equivalent to those found in {!module:Scad}. Quaternion operations are
    provided when this module is included in {!module:Scad_ml}. *)

(** [rotate ?about r t]

    Rotation of [t] by [r] (in radians) around the origin (or the point [about]
    if provided). *)
val rotate : ?about:t -> float -> t -> t

(** [zrot ?about r t]

    Rotation of [t] by [r] (in radians) around the origin (or the point [about]
    if provided). Alias to {!rotate}. *)
val zrot : ?about:t -> float -> t -> t

(** [translate p t]

    Translate [t] along the vector [p]. Equivalent to {!val:add}. *)
val translate : t -> t -> t

(** [scale s t]

    Scale [t] by factors [s]. Equivalent to {!val:mul}. *)
val scale : t -> t -> t

(** [mirror ax t]

    Mirrors [t] on a plane through the origin, defined by the normal vector [ax]. *)
val mirror : t -> t -> t

(** {1 2d - 3d conversion} *)

(** [of_vec3 v]

    Drop the z coordinate from [v] to create a 2d vector. *)
val of_vec3 : Vec.v3 -> t

(** [to_vec3 ?z v]

    Create a 3d vector from the 2d vector [v] by adding a [z] coordinate
    (default = [0.]) *)
val to_vec3 : ?z:float -> t -> Vec.v3
