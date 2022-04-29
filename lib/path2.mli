include Path.S with type vec := Vec.v2

type bbox =
  { min : Vec2.t
  ; max : Vec2.t
  }

(** [of_tups ps]

    Create a 2d path from a list of xy coordinate tuples. *)
val of_tups : (float * float) list -> t

(** [clockwise_sign path]

    *)
val clockwise_sign : t -> float

(** [is_clockwise path]

    *)
val is_clockwise : t -> bool

(** [clockwise_sign' path]

    *)
val clockwise_sign' : Vec2.t array -> float

(** [is_clockwise' path]

    *)
val is_clockwise' : Vec2.t array -> bool

(** [self_intersection ?eps ?closed path]

    *)
val self_intersections : ?eps:float -> ?closed:bool -> t -> t

(** [self_intersection' ?eps ?closed path]

    *)
val self_intersections' : ?eps:float -> ?closed:bool -> Vec2.t array -> t

(** [is_simple ?eps ?closed path]

    *)
val is_simple : ?eps:float -> ?closed:bool -> t -> bool

(** [is_simple' ?eps ?closed path]

    *)
val is_simple' : ?eps:float -> ?closed:bool -> Vec2.t array -> bool

(** [bbox t]

    *)
val bbox : t -> bbox

(** [centroid ?eps t]

    *)
val centroid : ?eps:float -> t -> Vec2.t

(** [area ?signed t]

    *)
val area : ?signed:bool -> t -> float

(** [point_inside ?eps ?nonzero t p]

    *)
val point_inside
  :  ?eps:float
  -> ?nonzero:bool
  -> t
  -> Vec2.t
  -> [> `Inside | `OnBorder | `Outside ]

(** [arc ?rev ?fn ?wedge ~centre ~radius ~start a]

    *)
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

    *)
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

    *)
val arc_through : ?rev:bool -> ?fn:int -> ?wedge:bool -> Vec2.t -> Vec2.t -> Vec2.t -> t

(** {1 Roundovers}*)

include Rounding.S with type vec := Vec.v2

(** {1 2d-3d conversion} *)

(** [of_path3 p]

    Project the 3d path [p] onto the given [plane] (default = {!Plane.xy}). *)
val of_path3 : ?plane:Plane.t -> Vec3.t list -> t

(** [to_path3 t]

    Lift the 2d path [p] onto the given [plane] (default = {!Plane.xy}). *)
val to_path3 : ?plane:Plane.t -> t -> Vec3.t list

(** [lift plane t]

    *)
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
