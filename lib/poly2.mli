(** 2d polygons (made up of an outer, and zero or more inner {!Path2.t}s) that map into
   {!Scad.d2}. Includes basic shape creation helpers, manipulations, (including
    offset and basic transformations), measurement, and validation. *)

(** [SelfIntersection i]

    Raised during polygon validation if a path is found to self-intersect. The
    index [i] refers to the culprit path (outer = [0], with holes increasing). *)
exception SelfIntersection of int

(** [CrossIntersection (i, j)]

    Raised during polygon validation if two paths are found to intersect
    eachother. The indices [i] and [j] refer to the paths in question
    (outer = [0], with holes increasing). *)
exception CrossIntersection of int * int

(** [DuplicatePoints]

    Raised during polygon validation if there are any duplicate points across
    all of the paths contained within {!type:t}. *)
exception DuplicatePoints

(** 2-dimensional polygon

    This type is kept private to force use of {!make}, which performs
    {!validation} by default, hopefully providing an early warning that the
    shape may have issues rendering in OpenSCAD. *)
type t = private
  { outer : Path2.t (** outer perimeter *)
  ; holes : Path2.t list (** inner paths to be subtracted from the outer shape *)
  }

(** {1 Creation and Validation} *)

(** [make ?validate ?holes outer]

    Create a 2d polygon from an [outer] path (perimeter), and zero or more
    [holes] (default = [[]]). If validate is [true] (as it is by default),
    {!val:validation} is performed, raising exceptions if the defined polygon is
    not simple (and thus, may cause problems in CGAL). *)
val make : ?validate:bool -> ?holes:Path2.t list -> Path2.t -> t

(** [add_holes ?validate ~holes t]

    Add [holes] to [t]. If validate is [true] (as it is by default),
    {!val:validation} is performed, raising exceptions if the defined polygon is
    not simple (and thus, may cause problems in CGAL). *)
val add_holes : ?validate:bool -> holes:Vec2.t list list -> t -> t

(** [validation ?eps t]

    Validate that [t] is a legal polygon, without self-intersections within
    paths, cross-intersections between paths, or any duplicate points. All
    checks are performed with the tolerance [eps]. If valid, unit will be
    returned, otherwise an exception will be raised.

    @raise SelfIntersection if any paths in [t] self-intersect.
    @raise CrossIntersection if any two paths in [t] intersect eachother.
    @raise DuplicatePoints if there are any duplicate points in [t]. *)
val validation : ?eps:float -> t -> unit

(** [is_simple ?eps t]

    Return [true] if [t] is a simple polygon, without self-intersections within
    paths, cross-intersections between paths, or any duplicate points. All
    checks are performed with the tolerance [eps]. *)
val is_simple : ?eps:float -> t -> bool

(** {1 Basic Shapes} *)

(** [circle ?fn r]

    Create a circle of radius [r] with [fn] points (default = [30]). *)
val circle : ?fn:int -> float -> t

(** [wedge ?fn ~centre ~radius ~start a]

    Create an arcing path (as in {!val:Path2.arc}), with the [centre] point
    included to close the path, forming a wedge. *)
val wedge : ?fn:int -> centre:Vec2.t -> radius:float -> start:float -> float -> t

(** [square ?center dims]

    Create a rectangular polygon with xy [dims] (e.g. width and height). If
    [center] is [true] then the path will be centred around the origin (default
    = [false]). *)
val square : ?center:bool -> Vec2.t -> t

(** [star ~r1 ~r2 n]

    Draw an [n] pointed star with inner radius [r1] and outer radius [r2]. *)
val star : r1:float -> r2:float -> int -> t

(** [ring ?fn ~thickness r]

    Create a circular empty ring of outer radius [r], with the given radial
    [thickness] (difference between outer and inner radii). Outer and inner
    paths are drawn with [fn] points (default = [30]). *)
val ring : ?fn:int -> thickness:float -> float -> t

(** [box ?center ~thickness dims]

    Create a rectangular empty box of outer xy dimensions [dims], with the given
    xy [thickness] (difference between outer and inner xy dimensions). If
    [center] is [true], then the path will be centred around the origin
    (default = [false]). *)
val box : ?center:bool -> thickness:Vec2.t -> Vec2.t -> t

(** {1 Geometry} *)

(** [bbox t]

    Compute the 2d bounding box of the polygon [t]. *)
val bbox : t -> Vec2.bbox

(** [centroid ?eps t]

    Compute the centroid of the outer path of the polygon [t]. If [t.outer] is
    collinear or self-intersecting (within [eps] tolerance), an
    [Invalid_argument] exception is raised. *)
val centroid : ?eps:float -> t -> Vec2.t

(** [area ?signed t]

    Compute the total area of the polygon [t]. If [signed] is [true], then the
    signed areas of the [outer] and [holes] paths of [t] will be summed (keep in
    mind that this is dependent on winding direction, which should generally be
    opposite between outer and inner paths), otherwise the unsigned (positive)
    area of the [holes] will be subtracted (default = [false]). *)
val area : ?signed:bool -> t -> float

(** {1 Offset and Basic Transformations} *)

(** [offset ?fn ?fs ?fa ?check_valid spec t]

    Offset outer and inner paths of [t] by the [spec]ified amount.
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
  -> ?check_valid:[ `No | `Quality of int ]
  -> [< `Chamfer of float | `Delta of float | `Radius of float ]
  -> t
  -> t

(** [map f t]

    Map the outer and inner paths of [t] with the function [f]. *)
val map : (Path2.t -> Path2.t) -> t -> t

val translate : Vec2.t -> t -> t
val rotate : float -> t -> t
val rotate_about_pt : float -> Vec2.t -> t -> t
val scale : Vec2.t -> t -> t
val mirror : Vec2.t -> t -> t

(** {1 Output} *)

(** [to_scad ?convexity t]

    Create a {!Scad.t} from the polygon [t], via {!Scad.polygon}. *)
val to_scad : ?convexity:int -> t -> Scad.d2
