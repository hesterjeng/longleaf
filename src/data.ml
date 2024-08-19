module Log = (val Logs.src_log Logs.(Src.create "data"))

module Farray = struct
  (* type t = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t [@@deriving show] *)
  type t = Owl_dense_ndarray.D.arr

  let pp fmt x = Owl_dense_ndarray.D.print ~fmt x
  let of_pyobject x : t = Numpy.to_bigarray Bigarray.float64 Bigarray.c_layout x
end

type t = {
  name : string;
  date : Time.t array;
  open_p : Farray.t;
  high_p : Farray.t;
  low_p : Farray.t;
  close_p : Farray.t;
  adj_close : Farray.t;
  volume : int array;
}
[@@deriving show]
(** Column oriented "dataframe".  We are maintaining an invariant that when we are working with
the pandas dataframes, we are removing the index and making it a column. *)

(* type data = t *)

(** Alternative structure that may be a little bit easier to read/work with *)
(* module Row_oriented = struct *)

(*   type datapoint = *)
(*     { name: string *)
(*     ; date: Time.t *)
(*     ; open_p: float *)
(*     ; high_p: float *)
(*     ; low_p: float *)
(*     ; close_p: float *)
(*     ; adj_close: float *)
(*     ; volume: int } *)
(*   [@@deriving show] *)

(*   type t = datapoint array *)

(*   let of_data (x : data) : t = *)
(*     let length = Array.length x.date in *)
(*     let init : datapoint option array = Array.make length None in *)
(*     for i = 0 to length do *)
(*       let datapoint = *)
(*         Some *)
(*           { name= x.name *)
(*           ; date= x.date.(i) *)
(*           ; open_p= Float.Array.(x.open_p.(i)) *)
(*           ; high_p= x.high_p.(i) *)
(*           ; low_p= x.low_p.(i) *)
(*           ; close_p= x.close_p.(i) *)
(*           ; adj_close= x.adj_close.(i) *)
(*           ; volume= x.volume.(i) } *)
(*       in *)
(*       Array.set init i datapoint *)
(*     done ; *)
(*     let res = Array.filter_map Fun.id init in *)
(*     res *)
(* end *)

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

(* let of_pyobject name (dataframe : Pytypes.pyobject) : t = *)
(*   let open Pyops in *)
(*   let date = *)
(*     dataframe.!$["Date"] *)
(*     |> ( Py.List.to_array_map *)
(*        @@ fun datetime64 -> *)
(*        Py.Object.call_method datetime64 "timestamp" [||] |> Py.Float.to_float ) *)
(*     |> Array.map Time.of_timestamp *)
(*   in *)
(*   let open_p = dataframe.!$["Open"] |> Py.List.to_array_map Py.Float.to_float in *)
(*   let high_p = dataframe.!$["High"] |> Py.List.to_array_map Py.Float.to_float in *)
(*   let low_p = dataframe.!$["Low"] |> Py.List.to_array_map Py.Float.to_float in *)
(*   let close_p = *)
(*     dataframe.!$["Close"] |> Py.List.to_array_map Py.Float.to_float *)
(*   in *)
(*   let adj_close = *)
(*     dataframe.!$["Adj Close"] |> Py.List.to_array_map Py.Float.to_float *)
(*   in *)
(*   let volume = dataframe.!$["Volume"] |> Py.List.to_array_map Py.Int.to_int in *)
(*   let length = Array.length date in *)
(*   let convert_array =  *)
(*   {name; date; open_p; high_p; low_p; close_p; adj_close; volume} *)

let of_pyobject name (dataframe : Pytypes.pyobject) : t =
  let open Pyops in
  let date =
    dataframe.!$["Date"]
    |> ( Py.List.to_array_map @@ fun datetime64 ->
         Py.Object.call_method datetime64 "timestamp" [||] |> Py.Float.to_float
       )
    |> Array.map Time.of_timestamp
  in
  (* let open_p = dataframe.!$["Open"] |> Py.List.to_array_map Py.Float.to_float in *)
  let open_p = Farray.of_pyobject dataframe.!$["Open"] in
  let high_p = Farray.of_pyobject dataframe.!$["High"] in
  let low_p = Farray.of_pyobject dataframe.!$["Low"] in
  let close_p = Farray.of_pyobject dataframe.!$["Close"] in
  let adj_close = Farray.of_pyobject dataframe.!$["Adj Close"] in
  let volume = dataframe.!$["Volume"] |> Py.List.to_array_map Py.Int.to_int in
  let length = Array.length date in
  (* let convert_array = *)
  { name; date; open_p; high_p; low_p; close_p; adj_close; volume }

(* let to_pyobject (x : t) = *)
(*   let pandas = Py.import "pandas" in *)
(*   let data_dict = Py.Dict.create () in *)
(*   Py.Dict.set_item_string data_dict "Open" @@ *)
(*   Py.Array.numpy x.open_p *)

(*   ; *)

(** Given a single ticker, use the yfinance API to download its data and put it into type `data`  *)
let of_string (name : string) =
  let dataframe = get_data "1mo" [| name |] in
  of_pyobject name dataframe

(** Use Python json parsing to read the file x into a Data.t *)
let of_json (x : string) =
  let open Pyops in
  let pandas = Py.import "pandas" in
  let read_json = pandas.&("read_json") in
  let tables = read_json [| Py.String.of_string x |] in
  let to_datetime = Py.Module.get_function_with_keywords pandas "to_datetime" in
  tables.!$["Date"] <-
    (let dt = tables.!$["Date"] in
     to_datetime [| dt |] [ ("unit", Py.String.of_string "s") ]);
  (* let set_index = tables.@$("set_index") |> Py.Callable.to_function in *)
  (* let _ = set_index [|Py.String.of_string "Date"|] in *)
  tables

(* let to_json (x : t) = *)
(*   let pandas = Py.import "pandas" in *)
