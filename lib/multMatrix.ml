type t = float array array

let of_row_list_exn l =
  let n_rows = List.length l in
  if n_rows = 3 || n_rows = 4
  then (
    let mat = Array.make_matrix 4 4 0. in
    let set_row i (c0, c1, c2, c3) =
      mat.(i).(0) <- c0;
      mat.(i).(1) <- c1;
      mat.(i).(2) <- c2;
      mat.(i).(3) <- c3
    in
    let rec loop i = function
      | r :: rows ->
        set_row i r;
        loop (i + 1) rows
      | [] when i = 3 -> mat.(i).(3) <- 1.
      | [] -> ()
    in
    loop 0 l;
    mat )
  else failwith "Matrix must have 3 or 4 rows."

let of_row_list l =
  try Ok (of_row_list_exn l) with
  | Failure e -> Error e

let to_string t =
  let row i =
    let comma = if i < 3 then "," else "" in
    Printf.sprintf "[%f, %f, %f, %f]%s" t.(i).(0) t.(i).(1) t.(i).(2) t.(i).(3) comma
  in
  Printf.sprintf "[ %s\n %s\n %s\n %s ]" (row 0) (row 1) (row 2) (row 3)

module Ops = SquareMatrix.Make (struct
  let size = 4
end)

let mul = Ops.mul
let add = Ops.add
let sub = Ops.sub
let transpose = Ops.transpose
let map = Ops.map
let mul_scalar = Ops.mul_scalar
let div_scalar = Ops.div_scalar
let add_scalar = Ops.add_scalar
let sub_scalar = Ops.sub_scalar
