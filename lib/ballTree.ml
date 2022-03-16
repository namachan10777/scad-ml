module type S = sig
  type vec
  type t

  val ( .%() ) : t -> int -> vec
  val points : t -> vec list
  val points' : t -> vec array
  val make : ?leaf_size:int -> vec list -> t
  val make' : ?leaf_size:int -> vec array -> t
  val search_idxs : ?radius:float -> t -> vec -> int list
  val search_points : ?radius:float -> t -> vec -> vec list
end

module type Projection = sig
  type vec

  val proj : vec array -> int array -> float array
end

module Proj2 = struct
  let proj points idxs =
    let mn = ref points.(idxs.(0))
    and mx = ref points.(idxs.(0))
    and len = Array.length idxs in
    for i = 1 to len - 1 do
      let Vec2.{ x; y } = points.(idxs.(i)) in
      (mn := Float.{ x = min !mn.x x; y = min !mn.y y });
      mx := Float.{ x = max !mx.x x; y = max !mx.y y }
    done;
    let Vec2.{ x = dx; y = dy } = Vec2.sub !mx !mn in
    let project = if dx >= dy then Vec2.get_x else Vec2.get_y in
    Array.map (fun idx -> project points.(idx)) idxs
end

module Proj3 = struct
  let proj points idxs =
    let mn = ref points.(idxs.(0))
    and mx = ref points.(idxs.(0))
    and len = Array.length idxs in
    for i = 1 to len - 1 do
      let Vec3.{ x; y; z } = points.(idxs.(i)) in
      (mn := Float.{ x = min !mn.x x; y = min !mn.y y; z = min !mn.z z });
      mx := Float.{ x = max !mx.x x; y = max !mx.y y; z = max !mx.z z }
    done;
    let Vec3.{ x = dx; y = dy; z = dz } = Vec3.sub !mx !mn in
    let project =
      match Float.(compare dx dy, compare dx dz, compare dy dz) with
      | 1, 1, _   -> Vec3.get_x
      | -1, _, 1  -> Vec3.get_y
      | _, -1, -1 -> Vec3.get_z
      | _         -> Vec3.get_x
    in
    Array.map (fun idx -> project points.(idx)) idxs
end

module Make (V : Vec.S) (P : Projection with type vec := V.t) : S with type vec := V.t =
struct
  type tree =
    | Leaf of int array
    | Node of
        { pivot : int
        ; radius : float
        ; left : tree
        ; right : tree
        }

  type t =
    { points : V.t array
    ; tree : tree
    }

  let ( .%() ) t i = t.points.(i)
  let points t = List.init (Array.length t.points) (fun i -> t.points.(i))
  let points' t = Array.copy t.points

  let make' ?(leaf_size = 25) points =
    let rec aux idxs =
      let len = Array.length idxs in
      if len <= leaf_size
      then Leaf idxs
      else (
        let projected = P.proj points idxs in
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
          let f max idx = Float.max max V.(distance p points.(idx)) in
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

  let search_idxs ?(radius = Util.epsilon) { points; tree } target =
    let rec aux = function
      | Leaf idxs ->
        let f acc i = if V.approx ~eps:radius points.(i) target then i :: acc else acc in
        Array.fold_left f [] idxs
      | Node tree ->
        if not @@ V.approx ~eps:(tree.radius +. radius) points.(tree.pivot) target
        then []
        else (
          let children = List.rev_append (aux tree.left) (aux tree.right) in
          if V.approx ~eps:radius points.(tree.pivot) target
          then tree.pivot :: children
          else children )
    in
    aux tree

  let search_points ?radius t target =
    List.map (fun i -> t.points.(i)) (search_idxs ?radius t target)
end
