include module type of V4l2common

type t

type frame = < raw : string; decode : string >

val init : string -> config -> t

val start : t -> unit

val stop : t -> unit

val get_frame : t -> frame
