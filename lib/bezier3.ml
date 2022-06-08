include Bezier.Make (Vec3)

let translate p t u = Vec3.translate p (t u)
let rotate r t u = Vec3.rotate r (t u)
let rotate_about_pt r p t u = Vec3.rotate_about_pt r p (t u)
let multmatrix m t u = MultMatrix.transform m (t u)
let quaternion q t u = Quaternion.rotate_vec3 q (t u)
let quaternion_about_pt q p t u = Quaternion.rotate_vec3_about_pt q p (t u)
let vector_rotate ax a = quaternion (Quaternion.make ax a)
let vector_rotate_about_pt ax a p = quaternion_about_pt (Quaternion.make ax a) p
let scale s t u = Vec3.scale s (t u)
let mirror ax t u = Vec3.mirror ax (t u)
