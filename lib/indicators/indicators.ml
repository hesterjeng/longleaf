module TA = Tacaml.F
module Config = Config

let get_ohclv (x : Data.t) : Tacaml.Ohlcv.t =
  {
    open_ = Data.get_row x Open;
    high = Data.get_row x High;
    close = Data.get_row x Close;
    low = Data.get_row x Low;
    volume = Data.get_row x Volume;
  }

let rsi_offset = 14
let fso_offset = 17
let sma_offset = 13

let compute_all (config : Config.t) (bars : Bars.t) =
  match config.compute_live with
  | false ->
    Eio.traceln "Precomputing indicators because of Indicator_config.t";
    (* let ( let* ) = Result.( let* ) in *)
    (* let* length = Bars.length bars in *)
    (* Bars.fold bars (Ok ()) @@ fun _ data acc -> *)
    (* let* _ = acc in *)
    (* let close = Data.get_row data Close in *)
    (* let ohclv = get_ohclv data in *)
    (* let rsi = Data.get_row data RSI |> Data.Row.slice 14 (length - 14) in *)
    (* let slow_k = Data.get_row data FSO_K |> Data.Row.slice 17 (length - 17) in *)
    (* let slow_d = Data.get_row data FSO_D |> Data.Row.slice 17 (length - 17) in *)
    (* let sma = Data.get_row data SMA |> Data.Row.slice 14 (length - 14) in *)
    (* let* outbeg, _ = TA.ta_rsi (0, length - 1) close 14 rsi in *)
    (* assert (outbeg = rsi_offset); *)
    (* let* outbeg, _ = TA.ta_stoch (0, length - 1) ohclv 14 3 3 slow_k slow_d in *)
    (* assert (outbeg = fso_offset); *)
    (* let* outbeg, _ = TA.ta_sma (0, length - 1) close 14 sma in *)
    (* assert (outbeg = sma_offset); *)
    Result.return ()
  | true ->
    Eio.traceln "Not precomputing indicators because of Indicator_config.t";
    Result.return ()

let initialize () =
  match TA.ta_initialize () with
  | Ok () -> ()
  | Error e ->
    Eio.traceln "Problem when initializing TA-Lib";
    invalid_arg e

(* let top () = *)
(*   TA.F. *)
