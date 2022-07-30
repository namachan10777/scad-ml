(** Cubic bezier easing.

    Adapted from {{:https://github.com/jchavarri/rebez} rebez}, which itself is
    an adaptation of {{:https://github.com/gre/bezier-easing} bezier-easing}. *)

(** [make p1 p2]

   Compute an easing function with from a cubic bezier curve with handle points
   [p1] and [p2]. The resulting function takes an [x] value in the range of [0.]
   to [1.] and returns the corresponding [y] value from the curve
   [Bezier2.make [{ x = 0.; y = 0.}; p1; p2; { x = 1.; y =1.}]]. *)
val make : Vec2.t -> Vec2.t -> float -> float
