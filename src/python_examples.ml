module Log = (val Logs.src_log Logs.(Src.create "py-examples"))

let ocaml_value_in_python () =
  let m = Py.Import.add_module "ocaml" in
  Py.Module.set m "example_value"
  @@ Py.List.of_list_map Py.Int.of_int [ 1; 2; 3 ];
  Py.Run.eval ~start:Py.File
    "from ocaml import example_value\nprint(example_value)"

let ocaml_function_in_python () =
  let me = Py.Import.add_module "mymod" in
  let hello args =
    Printf.printf "Hello, %s!\n" (Py.String.to_string args.(0));
    Py.none
  in
  (* Py.Module.set me "hello" (Py.Callable.of_function hello) ; *)
  Py.Module.set_function me "hello" hello;
  Py.Run.eval ~start:Py.File "from mymod import hello\nhello('John')"

let call_python_function_from_ocaml () =
  let builtins = Py.Eval.get_builtins () in
  let sorted_python = Py.Dict.find_string builtins "sorted" in
  let sorted = Py.Callable.to_function sorted_python in
  let result =
    sorted [| Py.List.of_list_map Py.Float.of_float [ 3.0; 2.0 ] |]
    |> Py.List.to_list_map Py.Float.to_float
  in
  assert (List.equal Float.equal result [ 2.0; 3.0 ]);
  Log.app (fun k -> k "%a" Format.(list float) result);
  ()
