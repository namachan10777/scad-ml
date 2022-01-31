val make : Vec2.t list -> float -> Vec2.t
val curve : ?init:Vec2.t list -> ?rev:bool -> ?fn:int -> (float -> Vec2.t) -> Vec2.t list
val travel : ?start_u:float -> ?end_u:float -> ?max_deflect:float -> Vec2.t list -> float
