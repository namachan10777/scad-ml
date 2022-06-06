open Vec

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
  else invalid_arg "Matrix must have 3 or 4 rows."

let of_row_list l =
  try Ok (of_row_list_exn l) with
  | Failure e -> Error e

let of_rotmatrix r { x; y; z } =
  let g = RotMatrix.get r in
  [| [| g 0 0; g 0 1; g 0 2; x |]
   ; [| g 1 0; g 1 1; g 1 2; y |]
   ; [| g 2 0; g 2 1; g 2 2; z |]
   ; [| 0.; 0.; 0.; 1. |]
  |]

let scaling { x; y; z } =
  [| [| x; 0.; 0.; 0. |]
   ; [| 0.; y; 0.; 0. |]
   ; [| 0.; 0.; z; 0. |]
   ; [| 0.; 0.; 0.; 1. |]
  |]

let translation { x; y; z } =
  [| [| 1.; 0.; 0.; x |]
   ; [| 0.; 1.; 0.; y |]
   ; [| 0.; 0.; 1.; z |]
   ; [| 0.; 0.; 0.; 1. |]
  |]

let transform t { x; y; z } =
  let v = [| x; y; z; 1. |]
  and a = Array.make 4 0. in
  for i = 0 to 3 do
    for j = 0 to 3 do
      a.(i) <- a.(i) +. (t.(i).(j) *. v.(j))
    done
  done;
  let w = a.(3) in
  (* project from cartesian to homogenous coordinates *)
  Vec.v3 (a.(0) /. w) (a.(1) /. w) (a.(2) /. w)

let to_string t =
  let row i =
    let comma = if i < 3 then "," else "" in
    Printf.sprintf "[%f, %f, %f, %f]%s" t.(i).(0) t.(i).(1) t.(i).(2) t.(i).(3) comma
  in
  Printf.sprintf "[ %s\n %s\n %s\n %s ]" (row 0) (row 1) (row 2) (row 3)

include (
  SquareMatrix.Make (struct
    let size = 4
  end) :
    SquareMatrix.Ops with type t := t )
