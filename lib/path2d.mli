type t = Vec2.t list

val colinear : Vec2.t -> Vec2.t -> Vec2.t -> bool
val total_travel' : Vec2.t array -> float
val total_travel : t -> float
val cummulative_travel : t -> float list
val to_continuous : t -> float -> Vec2.t
val resample : freq:[< `N of int | `Spacing of float ] -> t -> t
val arc_through : ?init:t -> ?rev:bool -> ?fn:int -> Vec2.t -> Vec2.t -> Vec2.t -> t
val translate : Vec2.t -> t -> t
val rotate : float -> t -> t
val rotate_about_pt : float -> Vec2.t -> t -> t
val scale : Vec2.t -> t -> t
val mirror : Vec2.t -> t -> t
