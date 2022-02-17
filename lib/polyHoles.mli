val partition
  :  ?rev:bool
  -> ?lift:(Vec2.t -> Vec3.t)
  -> holes:Vec2.t list list
  -> Vec2.t list
  -> Vec3.t list * int list list
