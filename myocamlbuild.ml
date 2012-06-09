open Ocamlbuild_plugin
open Command

let _ = dispatch begin function
  | After_rules ->
      flag ["c"; "compile"] (S[A"-ccopt"; A"-g"]);
      flag ["ocaml"; "link"; "use_v4l2"; "native"] (S[A"-ccopt"; A"-g -ljpeg -Wall -W -Wno-unused-parameter"]);
      flag ["ocaml"; "link"; "use_v4l2"; "byte"] (S[A"-custom"]);
      dep ["link"; "ocaml"; "use_v4l2"] ["v4l2_stubs.o"]
  | _ -> ()
end
