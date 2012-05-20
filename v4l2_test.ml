open V4l2

let main () =
  let v = init (Sys.argv.(1)) { V4l2.width = 640; height = 480 } in
  let _ = start v in
    for i = 0 to 10 do
      let frame = (get_frame v)#raw in
	Printf.printf "%d\n%!" (String.length frame);
	BatFile.write_lines (Printf.sprintf "%04d.jpg" i) (BatEnum.singleton frame);
    done;
    stop v
    

let _ = 
  main ();
  Gc.major ();
  Gc.compact ();
  Gc.major ()

