module Tbl = Hashtbl.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end)

let signed_pascals_triangle =
  let tbl = Tbl.create 10 in
  Tbl.add tbl 0 (Array.make 1 (-1.));
  fun n ->
    let len = Tbl.length tbl in
    if n > len - 1
    then
      for i = len - 1 to n do
        let row = Array.make (i + 2) (-1.)
        and last = Tbl.find tbl i in
        for j = 0 to i - 1 do
          let sign = if j mod 2 = 1 then -1. else 1. in
          row.(j + 1) <- sign *. (Float.abs last.(j) +. Float.abs last.(j + 1))
        done;
        row.(i + 1) <- (if i mod 2 = 0 then 1. else -1.);
        Tbl.add tbl (i + 1) row
      done
    else ();
    tbl

let bezier_matrix =
  let tbl = Tbl.create 10 in
  fun n ->
    match Tbl.find_opt tbl n with
    | Some m -> m
    | None   ->
      let tri = signed_pascals_triangle n
      and m = Array.make_matrix (n + 1) (n + 1) 0. in
      let nth_row = Tbl.find tri n in
      for i = 0 to n do
        let a = Tbl.find tri i
        and b = nth_row.(i) in
        for j = 0 to i do
          m.(i).(j) <- a.(j) *. b
        done
      done;
      Tbl.add tbl n m;
      m

let bez_vec2 ps ts =
  let ps = Array.of_list ps in
  let n = Array.length ps - 1 in
  let bm = bezier_matrix n
  and m = Array.make (n + 1) Vec2.zero in
  for i = 0 to n do
    for j = 0 to n do
      m.(i) <- Vec2.add m.(i) (Vec2.mul_scalar ps.(j) bm.(i).(j))
    done
  done;
  let f t =
    let pt = ref Vec2.zero in
    for i = 0 to n do
      pt := Vec2.add !pt (Vec2.mul_scalar m.(i) (Float.pow t (Float.of_int i)))
    done;
    !pt
  in
  List.map f ts

let bez_vec3 ps ts =
  let ps = Array.of_list ps in
  let n = Array.length ps - 1 in
  let bm = bezier_matrix n
  and m = Array.make (n + 1) Vec3.zero in
  for i = 0 to n do
    for j = 0 to n do
      m.(i) <- Vec3.add m.(i) (Vec3.mul_scalar ps.(j) bm.(i).(j))
    done
  done;
  let f t =
    let pt = ref Vec3.zero in
    for i = 0 to n do
      pt := Vec3.add !pt (Vec3.mul_scalar m.(i) (Float.pow t (Float.of_int i)))
    done;
    !pt
  in
  List.map f ts

module type V = sig
  type t

  val zero : t
  val add : t -> t -> t
  val mul_scalar : t -> float -> t
end

(* TODO: test whether cleaning up the repetition by using a hidden first class
   module version would lead to worse performance. *)
let bez (type a) (module V : V with type t = a) ps ts : a list =
  let ps = Array.of_list ps in
  let n = Array.length ps - 1 in
  let bm = bezier_matrix n
  and m = Array.make (n + 1) V.zero in
  for i = 0 to n do
    for j = 0 to n do
      m.(i) <- V.add m.(i) (V.mul_scalar ps.(j) bm.(i).(j))
    done
  done;
  let f t =
    let pt = ref V.zero in
    for i = 0 to n do
      pt := V.add !pt (V.mul_scalar m.(i) (Float.pow t (Float.of_int i)))
    done;
    !pt
  in
  List.map f ts

let bez_vec2' = bez (module Vec2)
let bez_vec3' = bez (module Vec3)
