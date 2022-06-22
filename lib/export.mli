(** Exporting [.scad] scripts through the OpenSCAD command line interface. *)

exception FailedExport of string * string

(** [script out_path in_path]

    Export the [.scad] script at [in_path] to a file at the given [out_path], in
    a format dictated by the extension of [out_path]. See documentation for the
    [-o] argument in the
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Using_OpenSCAD_in_a_command_line_environment}
    OpenSCAD cli docs} for available output formats. Raises [FailedExport (out_path,
    err)] if OpenSCAD fails to export the the script, where [err] is the captured
    Stderror output (e.g. CGAL failures). *)
val script : string -> string -> unit
