val vec2 : Vec2.t list -> float -> Vec2.t
val vec3 : Vec3.t list -> float -> Vec3.t
val curve : ?init:'a list -> ?rev:bool -> ?fn:int -> (float -> 'a) -> 'a list

val travel_vec2
  :  ?start_u:float
  -> ?end_u:float
  -> ?max_deflect:float
  -> Vec2.t list
  -> float

val travel_vec3
  :  ?start_u:float
  -> ?end_u:float
  -> ?max_deflect:float
  -> Vec3.t list
  -> float
