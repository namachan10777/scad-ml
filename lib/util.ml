let fa = 12. *. Float.pi /. 180.
let fs = 2.
let epsilon = 1e-9
let index_wrap ~len i = ((i mod len) + len) mod len

let rev_array arr =
  let open Array in
  let swap i j =
    let tmp = get arr i in
    set arr i (get arr j);
    set arr j tmp
  in
  let i = ref 0
  and j = ref (length arr - 1) in
  while !i < !j do
    swap !i !j;
    incr i;
    decr j
  done

let array_of_list_rev l =
  match l with
  | []       -> [||]
  | hd :: tl ->
    let len = 1 + List.length tl in
    let a = Array.make len hd in
    let r = ref tl in
    (* Start at [len - 2] as [make] has placed [hd] at [t.(len - 1)]. *)
    for i = len - 2 downto 0 do
      match !r with
      | []       -> assert false
      | hd :: tl ->
        a.(i) <- hd;
        r := tl
    done;
    a

let deduplicate list ~equal =
  let rec loop acc last = function
    | []       -> last :: acc
    | hd :: tl -> if equal hd last then loop acc last tl else loop (last :: acc) hd tl
  in
  match list with
  | []       -> []
  | hd :: tl -> List.rev (loop [] hd tl)

let value_map_opt ~default f = function
  | Some a -> f a
  | None   -> default

let transpose_row_list l =
  let m = Array.of_list (List.map Array.of_list l) in
  let col_len = Array.length m
  and row_len = Array.length m.(0) in
  if Array.for_all (fun r -> Array.length r = row_len) m
  then List.init row_len (fun c -> List.init col_len (fun r -> m.(r).(c)))
  else raise (Invalid_argument "Input lists are ragged (not a matrix).")

let prepend_init n f init =
  let rec loop acc i = if i < n then loop (f i :: acc) (i + 1) else acc in
  loop init 0

let unzip l =
  let rec loop l1 l2 = function
    | []             -> l1, l2
    | (h1, h2) :: tl -> loop (h1 :: l1) (h2 :: l2) tl
  in
  loop [] [] (List.rev l)

(* TODO: Add support for fa and fs where applicable and update this accordingly?
https://github.com/openscad/openscad/blob/dd7f6c0256ccfbd1e6efa6c06b9a12ef3565c29c/src/GeometryEvaluator.cc#L1075
https://github.com/openscad/openscad/blob/5e1a7cddd26de6fcfee753ee1d0fde5c90f785cd/src/calc.cc#L72 *)
let helical_slices ?fn twist =
  let twist = Float.abs twist in
  let min_slices = Int.max 1 Float.(twist /. (pi /. 3.) |> ceil |> to_int) in
  let f n =
    Int.max min_slices Float.(Float.of_int n *. twist /. (2. *. pi) |> ceil |> to_int)
  in
  value_map_opt ~default:min_slices f fn

let helical_fragments ?fn ?(fa = fa) ?(fs = fs) radius =
  match fn with
  | Some n -> Int.max 3 n
  | None   -> Float.(to_int @@ max (min (2. *. pi /. fa) (radius *. pi *. 2. /. fs)) 5.)
