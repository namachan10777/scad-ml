val offset
  :  ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?closed:bool
  -> ?check_valid:[ `Quality of int | `No ]
  -> [< `Chamfer of float | `Delta of float | `Radius of float ]
  -> Vec2.t list
  -> Vec2.t list

val offset_with_faces
  :  ?fn:int
  -> ?fs:float
  -> ?fa:float
  -> ?closed:bool
  -> ?check_valid:[ `Quality of int | `No ]
  -> ?flip_faces:bool
  -> ?start_idx:int
  -> [< `Chamfer of float | `Delta of float | `Radius of float ]
  -> Vec2.t list
  -> int * Vec2.t list * int list list
