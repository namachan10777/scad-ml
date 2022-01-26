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

let value_map_opt ~default f = function
  | Some a -> f a
  | None   -> default

let helical_slices ?fn twist =
  let twist = Float.abs twist in
  let min_slices = Int.max 1 Float.(twist /. (pi /. 3.) |> ceil |> to_int) in
  let f n =
    Int.max min_slices Float.(Float.of_int n *. twist /. (2. *. pi) |> ceil |> to_int)
  in
  value_map_opt ~default:min_slices f fn
