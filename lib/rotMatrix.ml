type t = float array array

let of_row_list_exn l =
  if List.length l = 3
  then (
    let mat = Array.make_matrix 3 3 0. in
    let set_row i (c0, c1, c2) =
      mat.(i).(0) <- c0;
      mat.(i).(1) <- c1;
      mat.(i).(2) <- c2
    in
    List.iteri set_row l;
    mat )
  else failwith "Rotation matrix must have 3 rows."

let of_row_list l =
  try Ok (of_row_list_exn l) with
  | Failure e -> Error e

let of_col_list_exn l =
  if List.length l = 3
  then (
    let mat = Array.make_matrix 3 3 0. in
    let set_col i (r0, r1, r2) =
      mat.(0).(i) <- r0;
      mat.(1).(i) <- r1;
      mat.(2).(i) <- r2
    in
    List.iteri set_col l;
    mat )
  else failwith "Rotation matrix must have 3 columns."

let of_col_list l =
  try Ok (of_col_list_exn l) with
  | Failure e -> Error e

let align_exn a b =
  if Vec3.(
       equal a b || equal a (negate b) || equal a (0., 0., 0.) || equal b (0., 0., 0.))
  then failwith "Vectors must not be equal or zero."
  else (
    let x = Vec3.normalize a
    and z = Vec3.(normalize (cross a b)) in
    let y = Vec3.(normalize (cross z x)) in
    of_col_list_exn [ x; y; z ] )

let align a b =
  try Ok (align_exn a b) with
  | Failure e -> Error e

let to_euler t =
  let x = Float.atan2 t.(2).(1) t.(2).(2) in
  let y =
    Float.atan2
      (-1. *. t.(2).(0))
      (Float.sqrt ((t.(2).(1) *. t.(2).(1)) +. (t.(2).(2) *. t.(2).(2))))
  in
  let z = Float.atan2 t.(1).(0) t.(0).(0) in
  x, y, z
