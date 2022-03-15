open Vec

type tree =
  | Leaf of int array
  | Node of
      { pivot : int
      ; radius : float
      ; left : tree
      ; right : tree
      }

type t =
  { points : Vec3.t array
  ; tree : tree
  }

let projection points idxs =
  let mn = ref points.(idxs.(0))
  and mx = ref points.(idxs.(0))
  and len = Array.length idxs in
  for i = 1 to len - 1 do
    let { x; y; z } = points.(idxs.(i)) in
    (mn := Float.{ x = min !mn.x x; y = min !mn.y y; z = min !mn.z z });
    mx := Float.{ x = max !mx.x x; y = max !mx.y y; z = max !mx.z z }
  done;
  let { x = dx; y = dy; z = dz } = Vec3.sub !mx !mn in
  let project =
    match Float.(compare dx dy, compare dx dz, compare dy dz) with
    | 1, 1, _   -> Vec3.get_x
    | -1, _, 1  -> Vec3.get_y
    | _, -1, -1 -> Vec3.get_z
    | _         -> Vec3.get_x
  in
  Array.map (fun idx -> project points.(idx)) idxs

let make' ?(leaf_size = 25) points =
  let rec aux idxs =
    let len = Array.length idxs in
    if len <= leaf_size
    then Leaf idxs
    else (
      let projected = projection points idxs in
      let mean_proj =
        Array.fold_left (fun sum p -> sum +. p) 0. projected /. Float.of_int len
      in
      let local_pivot =
        let idx = ref 0
        and min = ref Float.max_float in
        for i = 0 to len - 1 do
          let d = Float.abs (projected.(i) -. mean_proj) in
          if d < !min
          then (
            min := d;
            idx := i )
        done;
        !idx
      in
      let pivot = idxs.(local_pivot) in
      let radius =
        let p = points.(pivot) in
        let f max idx = Float.max max Vec3.(distance p points.(idx)) in
        Array.fold_left f 0. idxs
      in
      let left, right =
        let l = ref []
        and r = ref [] in
        for i = 0 to len - 1 do
          if i <> local_pivot
          then (
            if projected.(i) <= mean_proj then l := idxs.(i) :: !l;
            if projected.(i) > mean_proj then r := idxs.(i) :: !r )
        done;
        aux (Util.array_of_list_rev !l), aux (Util.array_of_list_rev !r)
      in
      Node { pivot; radius; left; right } )
  in
  { points; tree = aux (Array.init (Array.length points) Fun.id) }

let make ?leaf_size points = make' ?leaf_size (Array.of_list points)

let search ?(radius = Util.epsilon) { points; tree } target =
  let rec aux = function
    | Leaf idxs ->
      let f acc i = if Vec3.approx ~eps:radius points.(i) target then i :: acc else acc in
      Array.fold_left f [] idxs
    | Node tree ->
      if not @@ Vec3.approx ~eps:(tree.radius +. radius) points.(tree.pivot) target
      then []
      else (
        let children = List.concat [ aux tree.left; aux tree.right ] in
        if Vec3.approx ~eps:radius points.(tree.pivot) target
        then tree.pivot :: children
        else children )
  in
  aux tree
