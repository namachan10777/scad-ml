type t = float * float * float * float

(* let epsilon = 0.00000001
 * let phi = 1.61803399 (\* The golden mean *\)
 *
 * let half_pi = Core.pi /. 2.
 * let tau = Core.pi *. 2. *)

(* let quat_new x y z w =
 *   let arr = Array.create_float 4 in
 *   arr.(0) <- x;
 *   arr.(1) <- y;
 *   arr.(2) <- z;
 *   arr.(3) <- w;
 *   arr *)

(* let id = quat_new 0. 0. 0. 1. *)

let id = 0., 0., 0., 1.

let quat ax angle =
  let x, y, z = Math.norm ax in
  let s = Float.sin (angle /. 2.) in
  x *. s, y *. s, z *. s, Float.cos (angle /. 2.)

let basic_op op (x1, y1, z1, w1) (x2, y2, z2, w2) = op x1 x2, op y1 y2, op z1 z2, op w1 w2
let add = basic_op ( +. )
let sub = basic_op ( -. )
let add_scalar (x, y, z, w) s = x, y, z, w +. s
let sub_scalar (x, y, z, w) s = x, y, z, w -. s
let scalar_sub_quat (x, y, z, w) s = -.x, -.y, -.z, s -. w

let mul (x1, y1, z1, w1) (x2, y2, z2, w2) =
  let x = (y1 *. z2) -. (z1 *. y2) +. (w2 *. x1) +. (w1 *. x2)
  and y = (z1 *. x2) -. (x1 *. z2) +. (w2 *. y1) +. (w1 *. y2)
  and z = (x1 *. y2) -. (y1 *. x2) +. (w2 *. z1) +. (z2 *. w1)
  and w = (w1 *. w2) -. (x1 *. x2) -. (y1 *. y2) -. (z1 *. z2) in
  x, y, z, w

let mul_scalar (x, y, z, w) s = x *. s, y *. s, z *. s, w *. s
let div_scalar (x, y, z, w) s = x /. s, y /. s, z /. s, w /. s
let negate q = mul_scalar q (-1.)

(* function quat_dot(q1, q2) = q1[0]*q2[0]+q1[1]*q2[1]+q1[2]*q2[2]+ q1[3]*q2[3];
 *
 * function quat_norm(q) = sqrt(q[0]*q[0]+q[1]*q[1]+q[2]*q[2]+q[3]*q[3]);
 * function quat_normalize(q) = q/quat_norm(q);
 *
 * function quat_conj(q) = [-q[0], -q[1], -q[2], q[3]];
 *
 * function quat_distance(q1, q2) = quat_norm(quat_sub(q1-q2));
 *
 * // Converting quaternion to matrix4x4
 * function quat_to_mat4_s(q) = (vec4_lengthsqr(q)!=0) ? 2/vec4_lengthsqr(q) : 0;
 * function quat_to_mat4_xyzs(q, s) = [q[0]*s,q[1]*s, q[2]*s];
 * function quat_to_mat4_X(xyzs, x) = xyzs*x;
 * function _quat_xyzsw(xyzs, w) = xyzs*w;
 * function _quat_XYZ(xyzs, q)= [
 * 		quat_to_mat4_X(xyzs, q[0]),
 * 		quat_to_mat4_X(xyzs, q[1]),
 * 		quat_to_mat4_X(xyzs,q[2])
 * 		];
 *
 * function _quat_to_mat4(xyzsw, XYZ) = [
 * 		[(1.0-(XYZ[1][1]+XYZ[2][2])),  (XYZ[0][1]-xyzsw[2]), (XYZ[0][2]+xyzsw[1]), 0],
 *
 * 		[(XYZ[0][1]+xyzsw[2]), (1-(XYZ[0][0]+XYZ[2][2])), (XYZ[1][2]-xyzsw[0]), 0],
 * 		[(XYZ[0][2]-xyzsw[1]), (XYZ[1][2]+xyzsw[0]), (1.0-(XYZ[0][0]+XYZ[1][1])), 0],
 * 		[0,  0, 0, 1]
 * 		];
 *
 *
 * function quat_to_mat4(q) = _quat_to_mat4(
 * 	_quat_xyzsw(quat_to_mat4_xyzs(q, quat_to_mat4_s(q)),q[3]),
 * 	_quat_XYZ(quat_to_mat4_xyzs(q, quat_to_mat4_s(q)), q)); *)
