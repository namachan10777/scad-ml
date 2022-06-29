(** {0 Rounded sweep with holes} *)
open Scad_ml

(** Create a bezier spline function which passes through all of points in
    [control] using {{!Scad_ml.Bezier3.of_path} [Bezier3.of_path]}, and
   interpolate [20] points along it to create our [path] with
   {{!Scad_ml.Bezier3.curve} [Bezier3.curve]}. *)
let path =
  let control = Vec3.[ v 0. 0. 2.; v 0. 20. 20.; v 40. 10. 0.; v 50. 10. 5. ] in
  Bezier3.curve ~fn:20 @@ Bezier3.of_path control

(** We can quickly visualize [path] by using the {{!Scad_ml.Path3.show_points}
    [Path3.show_points]} helper, which places a {{!Scad_ml.Scad.d3}
    [Scad.d3]} shape at each point along the path (this takes a function from
    index to shape, rather than a shape directly to allow for differentiating
    the points, {i e.g.} numbering with {{!Scad_ml.Scad.text} [Scad.text]}). *)
let () =
  Scad.to_file "bezier_spline_path.scad"
  @@ Path3.show_points (fun _ -> Scad.sphere 1.) path

(** {%html:
    <p style="text-align:center;">
    <img src="../assets/bezier_spline_path.png" style="width:150mm;"/>
    </p> %}
    *)

(** Draw a 2d polygon with a chamfered square outline, and two circular holes.
    Chamfering the square outer path is accomplished via
    {{!Scad_ml.Path2.roundover} [Path2.roundover]}, which takes a
    {{!Scad_ml.Path2.Round.t} [Path2.Round.t]} specifation, built here
    using the {{!Scad_ml.Path2.Round.flat} [Path2.Round.flat]} constructor,
    that tells {{!Scad_ml.Path2.roundover} [Path2.roundover]} to apply
    [~corner] to all of the points of the given path. *)
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

let () = Scad.to_file "chamfered_square_with_holes.scad" (Poly2.to_scad poly)

(** {%html:
    <p style="text-align:center;">
    <img src="../assets/chamfered_square_with_holes.png" style="width:150mm;"/>
    </p> %}
    *)

(** {{!Scad_ml.Mesh.sweep} [Mesh.sweep]} derived functions take a [~spec]
   parameter that specifies what to do with the end faces of the extruded mesh.
   By default, both caps are flat and identical to the input polygon
   ({{!Scad_ml.Mesh.Cap.flat_caps} [Cap.flat_caps]}), but in this example, we
   will be rounding them over.

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

(** {%html:
    <p style="text-align:center;">
    <img src="../assets/rounded_polyhole_sweep.png" style="width:150mm;"/>
    </p> %}
    *)
