val prism
  :  ?debug:bool
  -> ?fn:int
  -> ?k:float
  -> ?k_bot:float
  -> ?k_top:float
  -> ?k_sides:[< `Flat of float | `Mix of float list > `Flat ]
  -> ?joint_bot:float * float
  -> ?joint_top:float * float
  -> ?joint_sides:[< `Flat of float * float | `Mix of (float * float) list > `Flat ]
  -> Vec3.t list
  -> Vec3.t list
  -> Mesh0.t
