type t =
  { outer : Path3.t
  ; holes : Path3.t list
  }

(** [of_poly2 ?plane poly]

    Lift the 2d polygon [poly] onto [plane] (default = {!val:Plane.xy}). *)
val of_poly2 : ?plane:Plane.t -> Poly2.t -> t

(** [to_poly2 ?validate ?plane t]

    Project the 3d polygon [t] onto [plane] (default = {!Plane.xy}). If
    [validate] is [true], 2d polygon {!Poly2.validation} is performed (default =
    [true]). *)
val to_poly2 : ?validate:bool -> ?plane:Plane.t -> t -> Poly2.t

(** [make ?validate ?holes outer]

    Create a 3d polygon from an [outer] path (perimeter), and zero or more
    [holes] (default = [[]]). If validate is [true] (as it is by default), all
    paths are checked for coplanarity, then {!val:Poly2.validation} is performed
    on a 2d projection, raising exceptions if the defined polygon is not simple
    (and thus, may cause problems in CGAL). *)
val make : ?validate:bool -> ?holes:Path3.t list -> Path3.t -> t

(** [circle ?fn ?plane r]

    Create a circle of radius [r] with [fn] points (default = [30]), on the
    optionally provided [plane] (default = {!val:Plane.xy}). *)
val circle : ?fn:int -> ?plane:Plane.t -> float -> t

(** [wedge ?fn ?plane ~centre ~radius ~start a]

    Create an arcing path (as in {!val:Path3.arc}), with the [centre] point
    included to close the path, forming a wedge. The polygon is drawn on the
    provided [plane] (default = {!val:Plane.xy}). *)
val wedge
  :  ?fn:int
  -> ?plane:Plane.t
  -> centre:Vec3.t
  -> radius:float
  -> start:float
  -> float
  -> t

(** [square ?center ?plane dims]

    Create a rectangular polygon with xy [dims] (e.g. width and height). If
    [center] is [true] then the path will be centred around the origin
    (default = [false]). The polygon is drawn on the provided [plane]
    (default = {!val:Plane.xy}). *)
val square : ?center:bool -> ?plane:Plane.t -> Vec2.t -> t

(** [ring ?fn ?plane ~thickness r]

    Create a circular empty ring of outer radius [r], with the given radial
    [thickness] (difference between outer and inner radii). Outer and inner
    paths are drawn with [fn] points (default = [30]). The polygon is drawn on
    the provided [plane] (default = {!val:Plane.xy}). *)
val ring : ?fn:int -> ?plane:Plane.t -> thickness:float -> float -> t

(** [box ?center ?plane ~thickness dims]

    Create a rectangular empty box of outer xy dimensions [dims], with the given
    xy [thickness] (difference between outer and inner xy dimensions). If
    [center] is [true], then the path will be centred around the origin
    (default = [false]). The polygon is drawn on the provided [plane]
    (default = {!val:Plane.xy}). *)
val box : ?center:bool -> ?plane:Plane.t -> thickness:Vec2.t -> Vec2.t -> t

(** [bbox t]

    Compute the 3d bounding box of the polygon [t]. *)
val bbox : t -> Vec3.bbox

(** [centroid ?eps t]

    Compute the centroid of the outer path of the polygon [t]. If [t.outer] is
    collinear or self-intersecting (within [eps] tolerance), an
    [Invalid_argument] exception is raised. *)
val centroid : ?eps:float -> t -> Vec3.t

(** [area ?signed t]

    Compute the total area of the polygon [t]. If [signed] is [true], then the
    signed areas of the [outer] and [holes] paths of [t] will be summed (keep in
    mind that this is dependent on winding direction, which should generally be
    opposite between outer and inner paths), otherwise the unsigned (positive)
    area of the [holes] will be subtracted (default = [false]). *)
val area : ?signed:bool -> t -> float

(** [map f t]

    Map the outer and inner paths of [t] with the function [f]. *)
val map : (Path3.t -> Path3.t) -> t -> t

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

(** {1 Basic Transformations} *)

val translate : Vec3.t -> t -> t
val rotate : Vec3.t -> t -> t
val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t
val scale : Vec3.t -> t -> t
val mirror : Vec3.t -> t -> t
val multmatrix : MultMatrix.t -> t -> t
