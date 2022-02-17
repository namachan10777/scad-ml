type t = float * float * float * float

let make p1 p2 p3 =
  let ((a, b, c) as crx) = Vec3.(cross (sub p3 p1) (sub p2 p1)) in
  let n = Vec3.norm crx in
  if Math.approx 0. n then invalid_arg "Plane points must not be colinear";
  a /. n, b /. n, c /. n, Vec3.dot crx p1 /. n

let of_normal ?(point = Vec3.zero) ((x, y, z) as normal) =
  let n = Vec3.norm normal in
  if Math.approx 0. n then invalid_arg "Normal cannot be zero.";
  x /. n, y /. n, z /. n, Vec3.dot normal point /. n

let project (a, b, c, d) =
  let n = a, b, c in
  let cp = Vec3.(div_scalar (mul_scalar n d) (dot n n)) in
  let rot = Quaternion.(to_multmatrix @@ alignment n (0., 0., 1.)) in
  let m = MultMatrix.(mul rot (translation (Vec3.negate cp))) in
  fun p -> Vec3.to_vec2 @@ MultMatrix.transform m p

let lift (a, b, c, d) =
  let n = a, b, c in
  let cp = Vec3.(div_scalar (mul_scalar n d) (dot n n)) in
  let rot = Quaternion.(to_multmatrix @@ alignment (0., 0., 1.) n) in
  let m = MultMatrix.(mul (translation cp) rot) in
  fun p -> MultMatrix.transform m (Vec3.of_vec2 p)

let offset (a, b, c, d) = d /. Vec3.norm (a, b, c)

let normalize (a, b, c, d) =
  let n = Vec3.norm (a, b, c) in
  a /. n, b /. n, c /. n, d /. n

let distance_to_point (a, b, c, d) p = Vec3.dot (a, b, c) p -. d

let greatest_distance (a, b, c, d) points =
  let normal = a, b, c in
  let f (min, max) p =
    let n = Vec3.dot p normal in
    Float.min min n, Float.max max n
  in
  let min_norm, max_norm = List.fold_left f (Float.max_float, Float.min_float) points in
  Float.max (max_norm -. d) (d -. min_norm) /. Vec3.norm normal

let are_points_on ?(eps = Util.epsilon) t points = greatest_distance t points < eps
let is_point_above t p = distance_to_point t p > Util.epsilon
