val make : Vec3.t list -> float -> Vec3.t
val curve : ?init:Vec3.t list -> ?rev:bool -> ?fn:int -> (float -> Vec3.t) -> Vec3.t list
val travel : ?start_u:float -> ?end_u:float -> ?max_deflect:float -> Vec3.t list -> float
