open Ocamlbuild_plugin
open Command

let _ = dispatch begin function
  | After_rules ->
      flag ["ocaml"; "link"; "use_v4l2"] (S[A"-custom"; A"-ccopt"; A"-Wall -W -Wno-unused-parameter"]);
      dep ["link"; "ocaml"; "use_v4l2"] ["v4l2_stubs.o"]
  | _ -> ()
end
