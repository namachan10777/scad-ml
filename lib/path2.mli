(** 2d path generation (including arcs and basic shapes), manipulation
   (including offset and roundovers (see {!module:Round}), and measurement. *)

include Path.S with type vec := Vec2.t and type line := Vec2.line

(** [nearby_idxs ?min_tree_size ?radius path p]

    Find the indices of points within [radius] (default = [1e-9]) distance from
    the target point [p] in [path]. Match indices will be returned in arbitrary
    order (unsorted). When [path] is provided (eagerly on partial application),
    the length will be checked and a function to perform the search will be
    generated. If [path] is shorter than [min_tree_size], it will be a simple
    direct search otherwise a {!BallTree2.t} will be constructed. Thus, if you
    plan to search for more than one target point, take care to apply this
    function in two steps to avoid repeated length checks and closure/tree
    generations. *)
val nearby_idxs : ?min_tree_size:int -> ?radius:float -> Vec2.t list -> Vec2.t -> int list

(** [nearby_points ?min_tree_size ?radius path]

    Find the points within [radius] (default = [1e-9]) distance from the target
    point [p] in [path]. Matched points will be returned in arbitrary order
    (unsorted). When [path] is provided (eagerly on partial application), the
    length will be checked and a function to perform the search will be
    generated. If [path] is shorter than [min_tree_size], it will be a simple
    direct search otherwise a {!BallTree2.t} will be constructed. Thus, if you
    plan to search for more than one target point, take care to apply this
    function in two steps to avoid repeated length checks and closure/tree
    generations. *)
val nearby_points
  :  ?min_tree_size:int
  -> ?radius:float
  -> Vec2.t list
  -> Vec2.t
  -> Vec2.t list

(** {1 Creation and 2d-3d conversion} *)

(** [of_tups ps]

    Create a 2d path from a list of xy coordinate tuples. *)
val of_tups : (float * float) list -> t

(** [of_path3 p]

    Project the 3d path [p] onto the given [plane] (default = {!Plane.xy}). *)
val of_path3 : ?plane:Plane.t -> Vec3.t list -> t

(** [to_path3 t]

    Lift the 2d path [p] onto the given [plane] (default = {!Plane.xy}). *)
val to_path3 : ?plane:Plane.t -> t -> Vec3.t list

(** [lift plane t]

    Lift the 2d path [t] onto the 3d [plane]. *)
val lift : Plane.t -> t -> Vec3.t list

(** {1 Basic Shapes} *)

(** [circle ?fn r]

    Create a circular path of radius [r] with [fn] points (default = [30]). *)
val circle : ?fn:int -> float -> t

(** [square ?center dims]

    Create a rectangular path with xy [dims] (e.g. width and height). If
    [center] is [true] then the path will be centred around the origin (default
    = [false]). *)
val square : ?center:bool -> Vec2.t -> t

(** {1 Drawing Arcs} *)

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

(** {1 Roundovers}

    Outline offsets with optional rounding/chamfering as found in OpenSCADs 2d
    sub-system, as well as specification and application of non-offseting
    roundovers (circular, chamfer, and bezier (continuous curvature)) to 2d
    paths.

    Based on the {{:https://github.com/revarbat/BOSL2} BOSL2}
    {{:https://github.com/revarbat/BOSL2/blob/master/rounding.scad} rounding}
    module. *)

(** [offset ?fn ?fs ?fa ?closed ?check_valid spec path]

    Offset a 2d [path] (treated as [closed] by default) by the [spec]ified amount.
    - [`Delta d] will create a new outline whose sides are a fixed distance [d]
      (+ve out, -ve in) from the original outline.
    - [`Chamfer d] fixed distance offset by [d] as with delta, but with corners
      chamfered.
    - [`Radius r] creates a new outline as if a circle of some radius [r] is
      rotated around the exterior ([r > 0]) or interior ([r < 0]) original
      outline. [fn], [fs], and [fa] parameters govern the number of points that
      will be used for these arcs (they are ignored for delta and chamfer modes).
    - The [check_valid] default of [`Quality 1] will check the validity of
      shifted line segments by checking whether their ends and [n] additional
      points spaced throughout are far enough from the original path. If there are
      no points that have been offset by the target [d], a [Failure] exception will
      be raised. Checking can be turned off by setting this to [`No]. *)
val offset
  :  ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?closed:bool
  -> ?check_valid:[ `Quality of int | `No ]
  -> [< `Chamfer of float | `Delta of float | `Radius of float ]
  -> t
  -> t

include Rounding.S with type vec := Vec2.t

(** {1 Geometry } *)

(** [clockwise_sign path]

    Returns the rotational ordering of [path] as a signed float, [-1.] for
   clockwise, and [1.] for counter-clockwise. If all points are collinear
   (within the tolerance of [eps]), [0.] is returned. *)
val clockwise_sign : ?eps:float -> t -> float

(** [is_clockwise path]

    Returns [true] if the rotational ordering of [path] is clockwise. *)
val is_clockwise : t -> bool

(** [self_intersection ?eps ?closed path]

    Find the points at which [path] intersects itself (within the tolerance of
    [eps]). If [closed] is [true], a line segment between the last and first
    points will be considered (default = [false]). *)
val self_intersections : ?eps:float -> ?closed:bool -> t -> t

(** [is_simple ?eps ?closed path]

    Return [true] if [path] is simple, e.g. contains no (paralell) reversals or
   self-intersections (within the tolerance [eps]).  If [closed] is [true], a
   line segment between the last and first points will be considered (default =
   [false]).*)
val is_simple : ?eps:float -> ?closed:bool -> t -> bool

(** [bbox t]

    Compute the 2d bounding box of the path [t]. *)
val bbox : t -> Vec2.bbox

(** [centroid ?eps t]

    Compute the centroid of the path [t]. If [t] is collinear or
    self-intersecting (within [eps] tolerance), an [Invalid_argument] exception
    is raised. *)
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

(** {1 Basic Transfomations} *)

val translate : Vec2.t -> t -> t
val rotate : float -> t -> t
val rotate_about_pt : float -> Vec2.t -> t -> t
val multmatrix : MultMatrix.t -> t -> Vec3.t list
val quaternion : Quaternion.t -> t -> Vec3.t list
val quaternion_about_pt : Quaternion.t -> Vec3.t -> t -> Vec3.t list
val vector_rotate : Vec3.t -> float -> t -> Vec3.t list
val vector_rotate_about_pt : Vec3.t -> float -> Vec3.t -> t -> Vec3.t list
val scale : Vec2.t -> t -> t
val mirror : Vec2.t -> t -> t

(** {1 Debugging helpers} *)

val show_points : (int -> Scad.d3) -> t -> Scad.d3

(** {1 Output} *)

(** [to_scad ?convexity t]

    Create a {!Scad.t} from the path [t], via {!Scad.polygon}. *)
val to_scad : ?convexity:int -> t -> Scad.d2
