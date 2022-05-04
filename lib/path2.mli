include Path.S with type vec := Vec.v2

(** Bounding box. *)
type bbox =
  { min : Vec2.t (** minimum x and y *)
  ; max : Vec2.t (** maximum x and y *)
  }

(** [of_tups ps]

    Create a 2d path from a list of xy coordinate tuples. *)
val of_tups : (float * float) list -> t

(** [clockwise_sign path]

    Returns the rotational ordering of [path] as a signed float, [-1.] for
   clockwise, and [1.] for counter-clockwise. If all points are collinear
   (within the tolerance of [eps]), [0.] is returned. *)
val clockwise_sign : ?eps:float -> t -> float

(** [is_clockwise path]

    Returns [true] if the rotational ordering of [path] is clockwise. *)
val is_clockwise : t -> bool

val clockwise_sign' : ?eps:float -> Vec2.t array -> float
val is_clockwise' : Vec2.t array -> bool

(** [self_intersection ?eps ?closed path]

    Find the points at which [path] intersects itself (within the tolerance of
    [eps]). If [closed] is [true], a line segment between the last and first
    points will be considered (default = [false]). *)
val self_intersections : ?eps:float -> ?closed:bool -> t -> t

val self_intersections' : ?eps:float -> ?closed:bool -> Vec2.t array -> t

(** [is_simple ?eps ?closed path]

    Return [true] if [path] is simple, e.g. contains no (paralell) reversals or
   self-intersections (within the tolerance [eps]).  If [closed] is [true], a
   line segment between the last and first points will be considered (default =
   [false]).*)
val is_simple : ?eps:float -> ?closed:bool -> t -> bool

val is_simple' : ?eps:float -> ?closed:bool -> Vec2.t array -> bool

(** [bbox t]

    Compute the 2d bounding box of the path [t]. *)
val bbox : t -> bbox

(** [centroid ?eps t]

    Compute the centroid of the path [t]. If [t] is collinear or
   self-intersecting (within [eps] tolerance), an [Invalid_argument] exception is
   raised. *)
val centroid : ?eps:float -> t -> Vec2.t

(** [area ?signed t]

    Compute the signed or unsigned area of the path [t] (unsigned by default). *)
val area : ?signed:bool -> t -> float

(** [point_inside ?eps ?nonzero t p]

    Determine whether the point [p] is inside, on the border of, or outside the
   closed path [t] (may be non-simple / contain self-intersections). If
   [nonzero] is [true], the {{:https://en.wikipedia.org/wiki/Nonzero-rule}
   Nonzero rule} is followed, wherein a point is considered inside the polygon
   formed by [t] regardless of the number of times the containing regions
   overlap, by default this is [false], and the
   {{:https://en.wikipedia.org/wiki/Even–odd_rule}Even-Odd rule} is followed (as
   with in OpenSCAD). *)
val point_inside
  :  ?eps:float
  -> ?nonzero:bool
  -> t
  -> Vec2.t
  -> [> `Inside | `OnBorder | `Outside ]

(** [arc ?rev ?fn ?wedge ~centre ~radius ~start a]

    Draw an arc of [a] radians with [radius] around the point [centre], beginning with the
    angle [start]. If [wedge] is [true], [centre] will be included as the last
    point of the returned path (default = [false]).

    - If [rev] is [true], the arc will end at [start], rather than begin there.
    - [fn] sets the number of points used to draw the arc (default = [16]) *)
val arc
  :  ?rev:bool
  -> ?fn:int
  -> ?wedge:bool
  -> centre:Vec2.t
  -> radius:float
  -> start:float
  -> float
  -> t

(** [arc_about_centre ?rev ?fn ?dir ?wedge ~centre p1 p2]

    Draw an arc between the points [p1] and [p2], about [centre]. [dir] can be
   provided to enforce clockwise or counter-clockwise winding direction. By
   default, the direction is computed automatically, though if [centre], [p1],
   and [p2] do not form a valid triangle (they're collinear), an
   [Invalid_argument] exception will be raised if [dir] is not provided.

    - See {!arc} for notes on [rev], [fn], and [wedge]. *)
val arc_about_centre
  :  ?rev:bool
  -> ?fn:int
  -> ?dir:[ `CW | `CCW ]
  -> ?wedge:bool
  -> centre:Vec2.t
  -> Vec2.t
  -> Vec2.t
  -> t

(** [arc_through ?rev ?fn  ?wedge p1 p2 p3]

    Draw an arc through the points [p1], [p2], and [p3]. If the points do not
   form a valid triangle (they're collinear), an [Invalid_argument] exception
   will be raised.

   - See {!arc} for notes on [rev], [fn], and [wedge]. *)
val arc_through : ?rev:bool -> ?fn:int -> ?wedge:bool -> Vec2.t -> Vec2.t -> Vec2.t -> t

(** {1 Roundovers} *)

(** [offset ?fn ?fs ?fa ?closed ?check_valid offset path]

    *)
val offset
  :  ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?closed:bool
  -> ?check_valid:[ `Quality of int | `No ]
  -> [< `Chamfer of float | `Delta of float | `Radius of float ]
  -> t
  -> t

include Rounding.S with type vec := Vec.v2

(** {1 2d-3d conversion} *)

(** [of_path3 p]

    Project the 3d path [p] onto the given [plane] (default = {!Plane.xy}). *)
val of_path3 : ?plane:Plane.t -> Vec3.t list -> t

(** [to_path3 t]

    Lift the 2d path [p] onto the given [plane] (default = {!Plane.xy}). *)
val to_path3 : ?plane:Plane.t -> t -> Vec3.t list

(** [lift plane t]

    Lift the 2d path [t] onto the 3d [plane]. *)
val lift : Plane.t -> t -> Vec3.t list

(** {1 Basic shapes} *)

(** [circle ?fn a]

    *)
val circle : ?fn:int -> float -> t

(** [square ?center dims]

    *)
val square : ?center:bool -> Vec2.t -> t

(** {1 Basic Transfomations} *)

val translate : Vec2.t -> t -> t
val rotate : float -> t -> t
val rotate_about_pt : float -> Vec2.t -> t -> t
val scale : Vec2.t -> t -> t
val mirror : Vec2.t -> t -> t
val multmatrix : MultMatrix.t -> t -> Vec3.t list

(** {1 Output} *)

val to_scad : ?convexity:int -> t -> Scad.d2
