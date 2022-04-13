include Path.S with type vec := Vec.v2

type bbox =
  { min : Vec2.t
  ; max : Vec2.t
  }

val of_tups : (float * float) list -> t
val of_path3 : ?plane:Plane.t -> Vec3.t list -> t
val to_path3 : ?plane:Plane.t -> t -> Vec3.t list
val clockwise_sign' : Vec2.t array -> float
val is_clockwise' : Vec2.t array -> bool
val clockwise_sign : t -> float
val is_clockwise : t -> bool
val self_intersections' : ?eps:float -> Vec2.t array -> t
val self_intersections : ?eps:float -> t -> t
val is_simple' : ?eps:float -> ?closed:bool -> Vec2.t array -> bool
val is_simple : ?eps:float -> ?closed:bool -> t -> bool
val bbox : t -> bbox
val centroid : ?eps:float -> t -> Vec2.t
val area : ?signed:bool -> t -> float

val arc
  :  ?rev:bool
  -> ?fn:int
  -> ?wedge:bool
  -> centre:Vec2.t
  -> radius:float
  -> start:float
  -> float
  -> t

val arc_about_centre
  :  ?rev:bool
  -> ?fn:int
  -> ?dir:[ `CW | `CCW ]
  -> ?wedge:bool
  -> centre:Vec2.t
  -> Vec2.t
  -> Vec2.t
  -> t

val arc_through : ?rev:bool -> ?fn:int -> ?wedge:bool -> Vec2.t -> Vec2.t -> Vec2.t -> t

(** {1 3d conversion} *)

val lift : Plane.t -> t -> Vec3.t list

(** {1 Basic shapes} *)

val circle : ?fn:int -> float -> t
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
