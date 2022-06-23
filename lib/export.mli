(** Exporting [.scad] scripts through the OpenSCAD command line interface. *)

(** [FailedExport (path, error)]

    Exception raised on failed export of a scad model, to a file a [path].
    [error] is the captured stderr output of the OpenSCAD process (usually CGAL
    errors). *)
exception FailedExport of string * string

(** {1 2D/3D formats} *)

(** [script out_path in_path]

    Export the [.scad] script at [in_path] to a file at the given [out_path], in
    a format dictated by the extension of [out_path]. See documentation for the
    [-o] argument in the
    {{:https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Using_OpenSCAD_in_a_command_line_environment}
    OpenSCAD CLI docs} for available output formats. Raises
    [FailedExport (out_path, err)] if OpenSCAD fails to export the the
    script, where [err] is the captured Stderror output (e.g. CGAL failures). *)
val script : string -> string -> unit

(** {1 Images (PNG)} *)

(** OpenSCAD colour palettes *)
type colorscheme =
  | Cornfield
  | Metallic
  | Sunset
  | Starnight
  | BeforeDawn
  | Nature
  | DeepOcean
  | Solarized
  | Tomorrow
  | TomorrowNight
  | Monotone

(** View projection (as in OpenSCAD GUI) *)
type projection =
  | Perspective
  | Orthogonal

(** Camera position and orientation configuration *)
type camera =
  | Auto (** Automatically positon to view all of the object, and point at it's centre. *)
  | Gimbal of
      { translation : Vec3.t (** positional shift vector *)
      ; rotation : Vec3.t (** euler rotation vector for orientation *)
      ; distance : [ `Auto | `D of float ]
            (** distance from origin, or automatically distanced such that all
                  of the object is visible *)
      }
  | Eye of
      { lens : Vec3.t (** lens position vector *)
      ; center : Vec3.t (** center position vector (which lens pointed towards) *)
      ; view_all : bool (** override distance to ensure all object is visible *)
      }

val auto : camera
val gimbal : ?translation:Vec.v3 -> ?rotation:Vec.v3 -> [ `Auto | `D of float ] -> camera
val eye : ?view_all:bool -> ?center:Vec.v3 -> Vec.v3 -> camera

(** [snapshot ?render ?colorscheme ?projection ?size ?camera out_path in_path]

    *)
val snapshot
  :  ?render:bool
  -> ?colorscheme:colorscheme
  -> ?projection:projection
  -> ?size:int * int
  -> ?camera:camera
  -> string
  -> string
  -> unit

(** {1 Extension checking helpers} *)

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
