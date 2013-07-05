open Batteries
open V4l2

let main () =
  let v = init (Sys.argv.(1)) { V4l2.width = 640; height = 480 } in
  let _ = start v in
    for i = 0 to 9 do
      let frame = get_frame v in
      let raw = frame#raw in
      let decoded = frame#decode in
	Printf.printf "%d %d\n%!" (String.length raw) (String.length decoded);
	output_file (Printf.sprintf "%04d.jpg" i) raw;
	output_file (Printf.sprintf "%04d.raw" i) decoded;
    done;
    Printf.printf "Complete\n%!";
    stop v
    

let _ = 
  main ();
  Gc.major ();
  Gc.compact ();
  Gc.major ()

