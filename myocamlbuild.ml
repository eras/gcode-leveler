open Ocamlbuild_plugin
open Command

let _ = dispatch begin function
  | After_rules ->
      flag ["c"; "compile"] (S[A"-ccopt"; A"-g"]);
      flag ["ocaml"; "link"; "use_v4l2"] (S[A"-custom"; A"-ccopt"; A"-g -ljpeg -Wall -W -Wno-unused-parameter"]);
      dep ["link"; "ocaml"; "use_v4l2"] ["v4l2_stubs.o"]
  | _ -> ()
end
