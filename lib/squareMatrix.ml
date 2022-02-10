module type Ops = sig
  (** Matrix Arithmetic, and helpers *)

  type t

  (** The identity matrix. *)
  val id : t

  (** [of_row_matrix_exn m]

    Convert the float matrix [m] into a [t] if it is the correct shape,
    otherwise throw an [Invalid_argument] exception. *)
  val of_row_matrix_exn : float array array -> t

  (** [of_row_matrix m]

    Convert the float matrix [m] into a [t] if it is the correct shape. *)
  val of_row_matrix : float array array -> (t, string) result

  val mul : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val transpose : t -> t
  val map : (float -> float) -> t -> t
  val mul_scalar : t -> float -> t
  val div_scalar : t -> float -> t
  val add_scalar : t -> float -> t
  val sub_scalar : t -> float -> t

  (** [trace t]

    Sum the elements on the main diagonal (upper left to lower right) of [t]. *)
  val trace : t -> float

  (** [get t r c]

    Get the element at [r]ow and [c]olumn of [t]. Equivalent to [t.(r).(c)]. *)
  val get : t -> int -> int -> float
end

module type Config = sig
  val size : int
end

module type S = Ops with type t = float array array

module Make (C : Config) : S = struct
  include C

  type t = float array array

  let id =
    let m = Array.make_matrix C.size C.size 0. in
    for i = 0 to C.size - 1 do
      m.(i).(i) <- 1.
    done;
    m

  let of_row_matrix_exn rm =
    if Array.(length rm = C.size && for_all (fun c -> length c = C.size) rm)
    then (
      let m = Array.make_matrix C.size C.size 0. in
      for i = 0 to C.size - 1 do
        for j = 0 to C.size - 1 do
          m.(i).(j) <- rm.(i).(j)
        done
      done;
      m )
    else (
      let msg = Printf.sprintf "Input is not square matrix of size %i" C.size in
      raise (Invalid_argument msg) )

  let of_row_matrix rm =
    try Ok (of_row_matrix_exn rm) with
    | Invalid_argument msg -> Error msg

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
  let sub_scalar t s = map (( +. ) (-1. *. s)) t

  let trace t =
    let a = ref 0. in
    for i = 0 to C.size - 1 do
      a := !a +. t.(i).(i)
    done;
    !a

  let get t r c = t.(r).(c)
end
