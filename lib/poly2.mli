exception SelfIntersection of int
exception CrossIntersection of int * int
exception DuplicatePoints

type t =
  { outer : Path2.t
  ; holes : Path2.t list
  }

(** [validation ?eps t]

    *)
val validation : ?eps:float -> t -> unit

(** [is_simple ?eps t]

    *)
val is_simple : ?eps:float -> t -> bool

(** [make ?validate ?holes outer]

    *)
val make : ?validate:bool -> ?holes:Path2.t list -> Path2.t -> t

(** [circle ?fn r]

    *)
val circle : ?fn:int -> float -> t

(** [wedge ?fn ~centre ~radius ~start a]

    *)
val wedge : ?fn:int -> centre:Vec2.t -> radius:float -> start:float -> float -> t

(** [square ?center dims]

    *)
val square : ?center:bool -> Vec2.t -> t

(** [ring ?fn ~thickness r]

    *)
val ring : ?fn:int -> thickness:float -> float -> t

(** [box ?center ~thickness dims]

    *)
val box : ?center:bool -> thickness:Vec2.t -> Vec2.t -> t

(** [bbox t]

    *)
val bbox : t -> Path2.bbox

(** [centroid ?eps t]

    *)
val centroid : ?eps:float -> t -> Vec2.t

(** [area ?signed t]

    *)
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

(** [translate p t]

    *)
val translate : Vec2.t -> t -> t

(** [rotate r t]

    *)
val rotate : float -> t -> t

(** [rotate_about_pt r p t]

    *)
val rotate_about_pt : float -> Vec2.t -> t -> t

(** [scale s t]

    *)
val scale : Vec2.t -> t -> t

(** [mirror ax t]

    *)
val mirror : Vec2.t -> t -> t

(** [to_scad ?convexity t]

    *)
val to_scad : ?convexity:int -> t -> Scad.d2
