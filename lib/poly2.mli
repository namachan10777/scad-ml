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

(** 2-dimensional polygon *)
type t =
  { outer : Path2.t (** outer perimeter *)
  ; holes : Path2.t list (** inner paths to be subtracted from the outer shape *)
  }

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

(** [make ?validate ?holes outer]

    Create a 2d polygon from an [outer] path (perimeter), and zero or more
    [holes] (default = [[]]). If validate is [true] (as it is by default),
    {!val:validation} is performed, raising exceptions if the defined polygon is
    not simple (and thus, may cause problems in CGAL). *)
val make : ?validate:bool -> ?holes:Path2.t list -> Path2.t -> t

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

(** [bbox t]

    Compute the 2d bounding box of the polygon [t]. *)
val bbox : t -> Path2.bbox

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

(** [map f t]

    *)
val map : (Path2.t -> Path2.t) -> t -> t

(** [offset ?fn ?fs ?fa ?closed ?check_valid spec t]

    *)
val offset
  :  ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?closed:bool
  -> ?check_valid:[ `No | `Quality of int ]
  -> [< `Chamfer of float | `Delta of float | `Radius of float ]
  -> t
  -> t

(** {1 Basic Transfomations} *)

val translate : Vec2.t -> t -> t
val rotate : float -> t -> t
val rotate_about_pt : float -> Vec2.t -> t -> t
val scale : Vec2.t -> t -> t
val mirror : Vec2.t -> t -> t

(** {1 Output} *)

(** [to_scad ?convexity t]

    *)
val to_scad : ?convexity:int -> t -> Scad.d2
