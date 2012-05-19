open V4l2

let main () =
  let v = init (Sys.argv.(1)) { V4l2.width = 640; height = 480 } in
  let _ = start v in
    for i = 0 to 10 do
      let frame = get_frame v in
	Printf.printf "%d\n%!" (String.length frame);
    done;
    stop v
    

let _ = 
  main ();
  Gc.major ();
  Gc.compact ();
  Gc.major ()

