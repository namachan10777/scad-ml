(** {0 Flat spiral} *)
open Scad_ml

(** A plain, centred square with which to draw our spiral. *)
let square = Poly2.square ~center:true (v2 10. 10.)

(** A series of affine transformation matrices describing a spiral. *)
let transforms =
  let step = 0.001 in
  let f i =
    let t = Float.of_int i *. step in
    MultMatrix.(
      mul
        (vector_rotation (v3 0. 0. 1.) (t *. Float.pi *. 40.))
        (translation (v3 (10. +. (500. *. t)) 0. 0.)))
  in
  List.init (Int.of_float (1. /. step) + 1) f

(** {{!Scad_ml.Mesh.sweep} [Mesh.sweep]} applies each of the transforms to
    [square] in its {i original} state, linking up each resulting loop of
    points with the next to form a mesh that we can convert into an OpenSCAD polyhedron. *)
let () = Scad.to_file "spiral.scad" @@ Mesh.(to_scad @@ sweep ~transforms square)

(** {%html:
    <p style="text-align:center;">
    <img src="../assets/spiral.png" style="width:150mm;"/>
    </p> %}
    *)
