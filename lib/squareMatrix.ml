module type Ops = sig
  type t

  val mul : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val transpose : t -> t
  val map : (float -> float) -> t -> t
  val mul_scalar : t -> float -> t
  val div_scalar : t -> float -> t
  val add_scalar : t -> float -> t
  val sub_scalar : t -> float -> t
end

module type Config = sig
  val size : int
end

module type S = Ops with type t = float array array

module Make (C : Config) : S = struct
  include C

  type t = float array array

  let mul a b =
    let m = Array.make_matrix C.size C.size 0. in
    for i = 0 to C.size - 1 do
      for j = 0 to C.size - 1 do
        for k = 0 to C.size - 1 do
          m.(i).(j) <- m.(i).(j) +. (a.(i).(k) *. b.(k).(j))
        done
      done
    done;
    m

  let element_wise op a b =
    let m = Array.make_matrix C.size C.size 0. in
    for i = 0 to C.size - 1 do
      for j = 0 to C.size - 1 do
        m.(i).(j) <- op a.(i).(j) b.(i).(j)
      done
    done;
    m

  let add = element_wise ( +. )
  let sub = element_wise ( -. )

  let transpose t =
    let m = Array.make_matrix C.size C.size 0. in
    for i = 0 to C.size - 1 do
      for j = 0 to C.size - 1 do
        m.(i).(j) <- t.(j).(i)
      done
    done;
    m

  let map f t =
    let m = Array.make_matrix C.size C.size 0. in
    for i = 0 to C.size - 1 do
      for j = 0 to C.size - 1 do
        m.(i).(j) <- f t.(i).(j)
      done
    done;
    m

  let mul_scalar t s = map (( *. ) s) t
  let div_scalar t s = map (( *. ) (1. /. s)) t
  let add_scalar t s = map (( +. ) s) t
  let sub_scalar t s = map (( -. ) s) t
end
