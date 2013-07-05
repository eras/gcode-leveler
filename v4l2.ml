include V4l2common

open Bigarray

type t'c
type t = {
  t'c			: t'c;
  mutable started	: bool
}

type array_frame = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type rgb_array_frame = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external v4l2_open : string -> int -> int -> t'c = "v4l2_open"

external v4l2_done : t'c -> unit = "v4l2_done"

external v4l2_start : t'c -> unit = "v4l2_start"

external v4l2_stop : t'c -> unit = "v4l2_stop"

external v4l2_get_frame : t'c -> array_frame = "v4l2_get_frame"

external v4l2_decode_mjpeg : array_frame -> rgb_array_frame = "v4l2_decode_mjpeg"

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

let string_of_bigarray array_frame =
  let len = Array1.dim array_frame in
  let s = String.create len in
    for c = 0 to len - 1 do
      String.set s c array_frame.{c}
    done;
    s

let get_frame t = 
  let raw =
    match t.started with
      | false ->
	  v4l2_start t.t'c;
	  let image = v4l2_get_frame t.t'c in
	    v4l2_stop t.t'c;
	    image
      | true ->
	  v4l2_get_frame t.t'c
  in
    (object
       method raw = string_of_bigarray raw
       method rgb = string_of_bigarray (v4l2_decode_mjpeg raw)
     end)

