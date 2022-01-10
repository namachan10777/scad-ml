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

let to_euler t =
  let x = Float.atan2 t.(2).(1) t.(2).(2) in
  let y =
    Float.atan2
      (-1. *. t.(2).(0))
      (Float.sqrt ((t.(2).(1) *. t.(2).(1)) +. (t.(2).(2) *. t.(2).(2))))
  in
  let z = Float.atan2 t.(1).(0) t.(0).(0) in
  x, y, z

let transform t (x, y, z) =
  let v = [| x; y; z |]
  and a = Array.make 3 0. in
  for i = 0 to 2 do
    for j = 0 to 2 do
      a.(i) <- a.(i) +. (t.(i).(j) *. v.(j))
    done
  done;
  a.(0), a.(1), a.(2)

let to_string t =
  let row i =
    let comma = if i < 2 then "," else "" in
    Printf.sprintf "[%f, %f, %f]%s" t.(i).(0) t.(i).(1) t.(i).(2) comma
  in
  Printf.sprintf "[ %s\n %s\n %s ]" (row 0) (row 1) (row 2)

include (
  SquareMatrix.Make (struct
    let size = 3
  end) :
    SquareMatrix.Ops with type t := t )
