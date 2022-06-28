(** {0 Rounded sweep with holes} *)
open Scad_ml

(** Create a bezier spline function which passes through all of points in
    [control], and interpolate [20] points along it to create our [path]. *)
let path =
  let control = Vec3.[ v 0. 0. 2.; v 0. 20. 20.; v 40. 10. 0.; v 50. 10. 5. ] in
  Bezier3.curve ~fn:20 @@ Bezier3.of_path control

(** Draw a 2d polygon with a chamfered square outline, with two circular holes. *)
let poly =
  let holes =
    let s = Path2.circle ~fn:90 2.
    and d = 1.9 in
    Path2.[ translate (v2 (-.d) (-.d)) s; translate (v2 d d) s ]
  and outer =
    Path2.square ~center:true (v2 10. 10.)
    |> Path2.Round.(flat ~corner:(chamf (`Width 1.)))
    |> Path2.roundover
  in
  Poly2.make ~holes outer

let () = Scad.to_file "rounded_polyhole_poly.scad" (Poly2.to_scad poly)

(** {%html: <img src="../assets/rounded_polyhole_poly.png"
    style="width:150mm;"/> %}
    *)

(** [Mesh.sweep] derived functions take a [~spec] parameter that specfies what
    to do with the end faces of the extruded mesh. By default, both caps are the
    same as the input polygon, but in this example, we will be rounding them
    over.

    To build our [spec], we'll use the [capped] constructor which takes
    specification types for how we would like to treat the bottom and top faces
    of our extrusion.

    Here we apply a negative (outward flaring) chamfer to the bottom face,
    setting [holes] to [`Same], so that the circular holes in [poly] are also
    expanded, rather than pinched off (default is ([~holes:`Flip]), which
    negates the outer roundover for inner paths). For the top face, we specify
    a positive (inward) circular roundover, leaving [holes] as its default since
    we want the holes to flare out instead. *)
let spec =
  Mesh.Cap.(
    capped
      ~bot:(round ~holes:`Same @@ chamf ~height:(-1.2) ~angle:(Float.pi /. 8.) ())
      ~top:(round @@ circ (`Radius 0.5)))

(** Extrude [poly] along [path], with rounding over the end caps according to
    [spec]. *)
let mesh = Mesh.(merge_points @@ path_extrude ~path ~spec poly)

(** Convert our mesh into an OpenSCAD polyhedron and output to file. *)
let () = Scad.to_file "rounded_polyhole_sweep.scad" (Mesh.to_scad mesh)

(** {%html: <img src="../assets/rounded_polyhole_sweep.png"
    style="width:150mm;"/> %} *)
