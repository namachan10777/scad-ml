type t = float array array

let of_list l =
  let n_rows = List.length l in
  if n_rows = 3 || n_rows = 4
  then (
    try
      let mat = Array.make_matrix 4 4 0. in
      let set_row i r =
        if List.length r = 4
        then List.iteri (fun j e -> mat.(i).(j) <- e) r
        else failwith "Row must be have length = 4."
      in
      let rec loop i = function
        | r :: rows ->
          set_row i r;
          loop (i + 1) rows
        | [] when i = 3 -> mat.(i).(3) <- 1.
        | [] -> ()
      in
      loop 0 l;
      Ok mat
    with
    | Failure e -> Result.error e )
  else Result.error "Matrix must have 3 or 4 rows."

let of_list_exn l =
  match of_list l with
  | Ok mat  -> mat
  | Error e -> failwith e

let to_string t =
  let row i =
    let comma = if i < 3 then "," else "" in
    Printf.sprintf "[%f, %f, %f, %f]%s" t.(i).(0) t.(i).(1) t.(i).(2) t.(i).(3) comma
  in
  Printf.sprintf "[ %s\n %s\n %s\n %s ]" (row 0) (row 1) (row 2) (row 3)
