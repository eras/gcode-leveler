include V4l2config

type t'c
type t = {
  t'c : t'c;
}

external v4l2_open : string -> int -> int -> t'c = "v4l2_open"

external v4l2_start : t'c -> unit = "v4l2_start"

external v4l2_get_frame : t'c -> string = "v4l2_get_frame"

let destruct :  t -> unit = fun _ ->
  failwith "destruct not implemented"

let init device_name config =
  let t'c = v4l2_open device_name config.width config.height in
  let t = {
    t'c = t'c
  } in
    Gc.finalise destruct t;
    t

let start t = v4l2_start t.t'c

let get_frame t = v4l2_get_frame t.t'c
