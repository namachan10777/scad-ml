type t = Vec3.t list

val total_travel : t -> float
val cummulative_travel : t -> float list
val to_continuous : t -> float -> Vec3.t
val of_continuous : ?init:t -> ?rev:bool -> ?fn:int -> (float -> Vec3.t) -> t
val resample : freq:[< `N of int | `Spacing of float ] -> t -> t

val helix
  :  ?fn:int
  -> ?fa:float
  -> ?fs:float (* -> ?up:bool *)
  -> ?left:bool
  -> n_turns:int
  -> pitch:float
  -> ?r2:float
  -> float
  -> t

val scaler : len:int -> float * float -> int -> MultMatrix.t
val twister : len:int -> float -> int -> MultMatrix.t

(** [to_transforms t]

    Generate list of transformations that can be applied to three-dimensional
   vectors ({!Vec3.t} via {!MultMatrix.transform}) or shapes ({!Scad.d3} via
   {!Scad.multmatrix}), to move them along the path [t] (intended to be applied
   to the vector/shape from its original position each time). Tangents are used
   to estimate appropriate rotations for each translation, using quaternion
   alignment (from starting normal (0., 0. ,1.)) to the tangent vector by
   default, when [euler = false]. Setting [euler = true] will use euler
   rotations instead, which can have more desirable results in some scenarios,
   but fail in others. For instance, [euler] can generate an abrupt when the
   path tangent is exactly vertical. *)
val to_transforms : ?euler:bool -> ?scale:Vec2.t -> ?twist:float -> t -> MultMatrix.t list
