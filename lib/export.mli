(** Exporting [.scad] scripts through the OpenSCAD command line interface. *)

exception FailedExport of string * string

module ExtSet : Set.S with type elt = string

(** Set of 2D output format file extensions: [".dxf"], [".svg"], [".csg"] *)
val d2_exts : ExtSet.t

(** Set of 3D output format file extensions: [".stl"], [".off"], [".amf"],
   [".3mf"], [".csg"], [".wrl"] *)
val d3_exts : ExtSet.t

(** [legal_ext allowed path]

    Check whether the extention of the file at [path] is in the [allowed],
    returning it as the error string if not. *)
val legal_ext : ExtSet.t -> string -> (unit, string) result

(** [script out_path in_path]

    Export the [.scad] script at [in_path] to a file at the given [out_path], in
    a format dictated by the extension of [out_path]. See documentation for the
    [-o] argument in the
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Using_OpenSCAD_in_a_command_line_environment}
    OpenSCAD cli docs} for available output formats. Raises [FailedExport (out_path,
    err)] if OpenSCAD fails to export the the script, where [err] is the captured
    Stderror output (e.g. CGAL failures). *)
val script : string -> string -> unit
