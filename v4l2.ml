include V4l2config

type t'c
type t = {
  t'c			: t'c;
  mutable started	: bool
}

external v4l2_open : string -> int -> int -> t'c = "v4l2_open"

external v4l2_done : t'c -> unit = "v4l2_done"

external v4l2_start : t'c -> unit = "v4l2_start"

external v4l2_stop : t'c -> unit = "v4l2_stop"

external v4l2_get_frame : t'c -> string = "v4l2_get_frame"

let destruct t =
  if t.started then v4l2_stop t.t'c;
  v4l2_done t.t'c

let init device_name config =
  let t'c = v4l2_open device_name config.width config.height in
  let t = {
    t'c = t'c;
    started = false;
  } in
    Gc.finalise destruct t;
    t

let start t = 
  match t.started with
    | false ->
	v4l2_start t.t'c;
	t.started <- true
    | true _ ->
	()

let stop t = 
  match t.started with
    | true ->
	v4l2_stop t.t'c;
	t.started <- false
    | false ->
	()

let get_frame t = 
  match t.started with
    | false ->
	v4l2_start t.t'c;
	let image = v4l2_get_frame t.t'c in
	  v4l2_stop t.t'c;
	  image
    | true ->
	v4l2_get_frame t.t'c
