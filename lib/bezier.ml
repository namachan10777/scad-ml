(* Based on: https://github.com/revarbat/BOSL2/blob/master/beziers.scad *)

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

module Make (V : Sigs.Vec) = struct
  open Path.Make (V)

  let make ps =
    let ps = Array.of_list ps in
    let n = Array.length ps - 1 in
    let bm = bezier_matrix n
    and m = Array.make (n + 1) V.zero in
    for i = 0 to n do
      let row = bm.(i) in
      for j = 0 to n do
        m.(i) <- V.add m.(i) (V.mul_scalar ps.(j) row.(j))
      done
    done;
    fun u ->
      let pt = ref V.zero in
      for i = 0 to n do
        pt := V.add !pt (V.mul_scalar m.(i) (Float.pow u (Float.of_int i)))
      done;
      !pt

  let curve ?(init = []) ?(rev = false) ?(fn = 16) bez =
    let dt = 1. /. Float.of_int fn *. if rev then 1. else -1. in
    let rec loop acc i t =
      if i <= fn then loop (bez t :: acc) (i + 1) (t +. dt) else acc
    in
    loop init 0 (if rev then 0. else 1.)

  let travel ?(start_u = 0.) ?(end_u = 1.) ?(max_deflect = 0.01) ps =
    let n_segs = List.length ps * 2
    and bz = make ps in
    let d = Float.of_int n_segs in
    let rec aux su eu =
      let path =
        let f i = bz (Math.lerp su eu (Float.of_int i /. d)) in
        Array.init (n_segs + 1) f
      in
      (* maximum deviation from a straight line *)
      let deflection =
        let mx = ref Float.min_float
        and p = Array.unsafe_get path in
        for i = 0 to n_segs - 2 do
          let mid_point = V.div_scalar (V.add (p i) (p (i + 2))) 2. in
          mx := Float.max !mx (V.distance (p (i + 1)) mid_point)
        done;
        !mx
      in
      if deflection <= max_deflect
      then total_travel' path
      else (
        let sum = ref 0. in
        for i = 0 to n_segs - 1 do
          let i = Float.of_int i in
          let su' = Math.lerp su eu (i /. d)
          and eu' = Math.lerp su eu ((i +. 1.) /. d) in
          sum := !sum +. aux su' eu'
        done;
        !sum )
    in
    aux start_u end_u
end

(* TODO: bezier_degenerate_vnf (and its dependencies) are the last required
    piece from beziers required to work on rounded. Some others would be nice
    to have though, such as:
   - bezier_closest_point would be easy since I have bisection in dometyl
   - bezpath functions (can be used to create a path which passes through all
   the given control points, generated by N degree beziers connected end to
   end) *)
