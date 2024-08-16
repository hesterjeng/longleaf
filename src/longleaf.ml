module Log = (val Logs.src_log Logs.(Src.create "longleaf"))

module A = struct
  let x = 0
end

module Libraries = struct
  let numpy = ref Py.none

  let pandas = ref Py.none

  let yfinance = ref Py.none

  let init () =
    try
      numpy := Py.import "numpy" ;
      pandas := Py.import "pandas" ;
      yfinance := Py.import "yfinance"
    with Py.E _ as e ->
      Log.err (fun k ->
          k "@[Failed to import module, are we in the correct environment?@]@." ) ;
      raise e
end

module Dataframe = struct
  let index x =
    Option.(
      let+ index = Py.Object.get_attr_string x "index" in
      index )
end

let run () =
  Libraries.init () ;
  let _ = Python_examples.ocaml_value_in_python () in
  let _ = Python_examples.ocaml_function_in_python () in
  let _ = Python_examples.call_python_function_from_ocaml () in
  ()
