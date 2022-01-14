(** [quad ~p1 ~p2 ~p3 t]

    Quadratic Bézier float interpolation. *)
val quad : p1:float -> p2:float -> p3:float -> float -> float

(** [cubic ~p1 ~p2 ~p3 ~p4 t]

    Cubic Bézier float interpolation. *)
val cubic : p1:float -> p2:float -> p3:float -> p4:float -> float -> float

(** [quad_vec2 ~p1 ~p2 ~p3 t]

    Quadratic Bézier function, interpolating a path in 2d space starting from
    [p1] when [t = 0.] and ending at [p3] when [t = 1.]. *)
val quad_vec2 : p1:Vec2.t -> p2:Vec2.t -> p3:Vec2.t -> float -> Vec2.t

(** [cubic_vec2 ~p1 ~p2 ~p3 ~p4 t]

    Cubic Bézier function, interpolating a path in 2d space starting from
    [p1] when [t = 0.] and ending at [p4] when [t = 1.]. *)
val cubic_vec2 : p1:Vec2.t -> p2:Vec2.t -> p3:Vec2.t -> p4:Vec2.t -> float -> Vec2.t

(** [quad_vec3 ~p1 ~p2 ~p3 t]

    Quadratic Bézier function, interpolating a path in 3d space starting from
    [p1] when [t = 0.] and ending at [p3] when [t = 1.]. *)
val quad_vec3 : p1:Vec3.t -> p2:Vec3.t -> p3:Vec3.t -> float -> Vec3.t

(** [cubic_vec3 ~p1 ~p2 ~p3 ~p4 t]

    Cubic Bézier function, interpolating a path in 3d space starting from
    [p1] when [t = 0.] and ending at [p4] when [t = 1.]. *)
val cubic_vec3 : p1:Vec3.t -> p2:Vec3.t -> p3:Vec3.t -> p4:Vec3.t -> float -> Vec3.t

(** [curve ?init ?rev ~n_steps bez t]

    Draw a curve of [n_steps] points using the given Bézier function [bez]. Set
    [rev] to [true], to produce a curve in reverse order. Results are prepended
    to [init] if provided. Reverse order can be useful for building up a
    path/polygon with mixed prepending of points calculated by other means
    and/or multiple uses of {!curve}. *)
val curve : ?init:'a list -> ?rev:bool -> n_steps:int -> (float -> 'a) -> 'a list
