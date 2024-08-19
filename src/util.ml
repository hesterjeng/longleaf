let pyprint x =
  let open Pyops in
  let builtins = Py.import "builtins" in
  let p = builtins.&("print") in
  let _ = p [| x |] in
  ()

include Ppx_yojson_conv_lib.Yojson_conv
