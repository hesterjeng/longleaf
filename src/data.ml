module Log = (val Logs.src_log Logs.(Src.create "data"))
open Util

module Farray = struct
  (* type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t [@@deriving show] *)
  type t = Owl_dense_ndarray.D.arr

  let pp fmt x = Owl_dense_ndarray.Generic.pp_dsnda fmt x

  let of_pyobject x : t =
    let numpy_array = Py.Object.call_method x "to_numpy" [||] in
    Util.pyprint numpy_array;
    Numpy.to_bigarray Bigarray.float64 Bigarray.c_layout numpy_array

  let yojson_of_t x =
    let arr = Owl_dense_ndarray.D.to_array x in
    yojson_of_array yojson_of_float arr

  let t_of_yojson x =
    let arr = array_of_yojson float_of_yojson x in
    Owl_dense_ndarray.D.of_array arr [| Array.length arr |]
end

type t = {
  name : string;
  date : Farray.t;
  open_p : Farray.t;
  high_p : Farray.t;
  low_p : Farray.t;
  close_p : Farray.t;
  adj_close : Farray.t;
  volume : Farray.t;
}
[@@deriving show, yojson]

(** Uses Python and the yfinance library to download information about the tickers.
 There may be multiple tickers here. *)
let get_data period tickers : Pytypes.pyobject =
  let open Pyops in
  let tickers = Array.map Py.String.of_string tickers in
  let yfinance = Py.import "yfinance" in
  let download args keywords =
    Py.Module.get_function_with_keywords yfinance "download" args keywords
  in
  let dataframe =
    download tickers @@ [ ("period", Py.String.of_string period) ]
  in
  let reset_index = dataframe.@$("reset_index") in
  let dataframe_reset = Py.Callable.to_function reset_index [||] in
  dataframe_reset

let of_pyobject name (dataframe : Pytypes.pyobject) : t =
  let open Pyops in
  let date =
    (dataframe.!$["Date"]
    |> Py.List.to_array_map @@ fun datetime64 ->
       Py.Object.call_method datetime64 "timestamp" [||] |> Py.Float.to_float)
    |> fun x -> Owl_dense_ndarray.D.of_array x [| Array.length x |]
  in
  let open_p = Farray.of_pyobject dataframe.!$["Open"] in
  let high_p = Farray.of_pyobject dataframe.!$["High"] in
  let low_p = Farray.of_pyobject dataframe.!$["Low"] in
  let close_p = Farray.of_pyobject dataframe.!$["Close"] in
  let adj_close = Farray.of_pyobject dataframe.!$["Adj Close"] in
  (* Log.err (fun k -> k "@[Trying volume@]@."); *)
  let volume =
    dataframe.!$["Volume"]
    |> Py.List.to_array_map Py.Int.to_int
    |> Array.map Float.of_int
    |> fun x -> Owl_dense_ndarray.D.of_array x [| Array.length x |]
  in
  { name; date; open_p; high_p; low_p; close_p; adj_close; volume }

let to_pyobject (x : t) =
  let open Pyops in
  let pandas = Py.import "pandas" in
  let data_dict = Py.Dict.create () in
  Py.Dict.set_item_string data_dict "Date" @@ Numpy.of_bigarray x.open_p;
  Py.Dict.set_item_string data_dict "Open" @@ Numpy.of_bigarray x.open_p;
  Py.Dict.set_item_string data_dict "High" @@ Numpy.of_bigarray x.open_p;
  Py.Dict.set_item_string data_dict "Low" @@ Numpy.of_bigarray x.open_p;
  Py.Dict.set_item_string data_dict "Close" @@ Numpy.of_bigarray x.open_p;
  Py.Dict.set_item_string data_dict "Adj Close" @@ Numpy.of_bigarray x.open_p;
  Py.Dict.set_item_string data_dict "Volume" @@ Numpy.of_bigarray x.open_p;
  let dataframe = pandas.&("DataFrame") [| data_dict |] in
  dataframe

(** Given a single ticker, use the yfinance API to download its data and put it into type `data`  *)
let of_string (name : string) =
  let dataframe = get_data "1mo" [| name |] in
  of_pyobject name dataframe
