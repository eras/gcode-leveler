open V4l2

let main () =
  let v = init (Sys.argv.(1)) { V4l2.width = 640; height = 480 } in
  let _ = start v in
    while true do
      let frame = get_frame v in
	Printf.printf "%d\n%!" (String.length frame);
    done

let _ = main ()
