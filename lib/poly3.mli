type t =
  { outer : Path3.t
  ; holes : Path3.t list
  }

(** [of_poly2 ?plane poly]

    *)
val of_poly2 : ?plane:Plane.t -> Poly2.t -> t

(** [to_poly2 ?validate ?plane t]

    *)
val to_poly2 : ?validate:bool -> ?plane:Plane.t -> t -> Poly2.t

(** [make ?validate ?holes outer]

    *)
val make : ?validate:bool -> ?holes:Path3.t list -> Path3.t -> t

(** [circle ?fn ?plane r]

    *)
val circle : ?fn:int -> ?plane:Plane.t -> float -> t

(** [wedge ?fn ?plane ~centre ~radius ~start a]

    *)
val wedge
  :  ?fn:int
  -> ?plane:Plane.t
  -> centre:Vec3.t
  -> radius:float
  -> start:float
  -> float
  -> t

(** [square ?center ?plane dims]

    *)
val square : ?center:bool -> ?plane:Plane.t -> Vec2.t -> t

(** [ring ?fn ?plane ~thickness r]

    *)
val ring : ?fn:int -> ?plane:Plane.t -> thickness:float -> float -> t

(** [box ?center ?plane ~thickness dims]

    *)
val box : ?center:bool -> ?plane:Plane.t -> thickness:Vec2.t -> Vec2.t -> t

(** [bbox t]

    *)
val bbox : t -> Path3.bbox

(** [centroid ?eps t]

    *)
val centroid : ?eps:float -> t -> Vec3.t

(** [area ?signed t]

    *)
val area : ?signed:bool -> t -> float

(** [map f t]

    *)
val map : (Path3.t -> Path3.t) -> t -> t

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
val translate : Vec3.t -> t -> t

(** [rotate r t]

    *)
val rotate : Vec3.t -> t -> t

(** [rotate_about_pt r p t]

    *)
val rotate_about_pt : Vec3.t -> Vec3.t -> t -> t

(** [scale s t]

    *)
val scale : Vec3.t -> t -> t

(** [mirror ax t]

    *)
val mirror : Vec3.t -> t -> t

(** [multmatrix]

    *)
val multmatrix : MultMatrix.t -> t -> t
