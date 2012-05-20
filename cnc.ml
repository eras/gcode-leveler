open BatPervasives

type t = { 
  fd		: Unix.file_descr;
  mutable line	: int;
}

let reader fd =
  let buf = String.create 1024 in
  let lb = LineBuffer.create () in
  let rec loop () =
    let n = Unix.read fd buf 0 (String.length buf) in
      if n > 0 
      then (
	LineBuffer.append_substring lb buf 0 n |>
	    List.iter (Printf.printf "CNC<-%s\n%!");
	loop ()
      ) else ()
  in 
    loop ()

let connect device bps =
  let fd = Unix.openfile device [Unix.O_RDWR] 0 in
  let tio = Unix.tcgetattr fd in
  let tio = { 
    tio with 
      Unix.c_clocal	= false;
      c_obaud		= bps;
      c_ibaud		= bps;
      c_csize		= 8;
      c_cstopb		= 1;
      c_inlcr           = false;
      c_icrnl		= false;
      c_opost           = false;
      c_isig		= false;
      c_icanon		= false;
      c_echo		= false;
      c_vtime           = 1;
      c_vmin		= 1;
  } in
  let _ = Unix.tcsetattr fd Unix.TCSANOW tio in
  let _ = Thread.create reader fd in
    { fd; line = 1 }

let name_of_axis = function
  | `X -> "X"
  | `Y -> "Y"
  | `Z -> "Z"

let send t msg =
  let msg = Printf.sprintf "N%d %s" t.line msg in
  let checksum = BatString.explode msg |> List.map Char.code |> List.fold_left (lxor) 0 in
  let msg = Printf.sprintf "%s*%d" msg checksum in
  let _ = Printf.printf "->CNC: %s\n%!" msg in
  let msg = msg ^ "\n" in
    ignore (Unix.write t.fd msg 0 (String.length msg));
    t.line <- t.line + 1

let home t axis =
  send t ("G28 " ^ String.concat " " (List.map (fun axis -> name_of_axis axis ^ "0") axis))

let move t axis =
  let movements = 
    axis 
  |> List.map (function
		 | `X x -> "X", x
		 | `Y y -> "Y", y
		 | `Z z -> "Z", z
	      ) 
  |> List.map (uncurry (Printf.sprintf "%s%.3f"))
  in
    send t ("G1 " ^ String.concat " " movements)

let where t =
  send t "M114"
