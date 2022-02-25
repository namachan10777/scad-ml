include Path.S with type vec := Vec.v2

type bounds =
  { left : float
  ; right : float
  ; top : float
  ; bot : float
  }

val of_tups : (float * float) list -> Vec2.t list
val clockwise_sign' : Vec2.t array -> float
val is_clockwise' : Vec2.t array -> bool
val clockwise_sign : Vec2.t list -> float
val is_clockwise : Vec2.t list -> bool
val bounds : Vec2.t list -> bounds
val self_intersections' : ?eps:float -> Vec2.t array -> Vec2.t list
val self_intersections : ?eps:float -> Vec2.t list -> Vec2.t list
val is_simple' : ?eps:float -> ?closed:bool -> Vec2.t array -> bool
val is_simple : ?eps:float -> ?closed:bool -> Vec2.t list -> bool
val centroid : ?eps:float -> Vec2.t list -> Vec2.t
val area : ?signed:bool -> Vec2.t list -> float

val arc
  :  ?init:Vec2.t list
  -> ?rev:bool
  -> ?fn:int
  -> centre:Vec2.t
  -> radius:float
  -> start:float
  -> float
  -> Vec2.t list

val arc_about_centre
  :  ?init:Vec2.t list
  -> ?rev:bool
  -> ?fn:int
  -> ?dir:[ `CW | `CCW ]
  -> centre:Vec2.t
  -> Vec2.t
  -> Vec2.t
  -> Vec2.t list

val arc_through : ?init:t -> ?rev:bool -> ?fn:int -> Vec2.t -> Vec2.t -> Vec2.t -> t
val translate : Vec2.t -> t -> t
val rotate : float -> t -> t
val rotate_about_pt : float -> Vec2.t -> t -> t
val scale : Vec2.t -> t -> t
val mirror : Vec2.t -> t -> t
