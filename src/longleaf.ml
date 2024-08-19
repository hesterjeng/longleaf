module Log = (val Logs.src_log Logs.(Src.create "longleaf"))

module A = struct
  let x = 0
end

let pyprint x =
  let open Pyops in
  let builtins = Py.import "builtins" in
  let p = builtins.&("print") in
  let _ = p [| x |] in
  ()

module Tickers = struct
  let get_tickers () =
    let open Pyops in
    let sp600_url =
      {|https://en.wikipedia.org/wiki/List_of_S%26P_600_companies|}
      |> Py.String.of_string
    in
    let pandas = Py.import "pandas" in
    let read_html = pandas.&("read_html") in
    let tables = read_html [| sp600_url |] in
    let col = tables.![Py.Int.of_int 0] in
    let ticker_symbols = col.!$["Symbol"] in
    Py.List.to_array_map Py.Object.to_string ticker_symbols
end

module Dataframe = struct
  let index x =
    Option.(
      let+ index = Py.Object.get_attr_string x "index" in
      index)
end

let run () =
  let _ = Python_examples.ocaml_value_in_python () in
  let _ = Python_examples.ocaml_function_in_python () in
  let _ = Python_examples.call_python_function_from_ocaml () in
  let data = Data.of_string "AAPL" in
  Log.app (fun k -> k "%a" Data.pp data);
  ()
