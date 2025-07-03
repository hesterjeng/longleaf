module TA = Tacaml.F

let get_ohclv (x : Data.t) : TA.ohlcv =
  {
    open_ = Data.get_row x Open;
    high = Data.get_row x High;
    close = Data.get_row x Close;
    low = Data.get_row x Low;
    volume = Data.get_row x Volume;
  }

let compute_all (bars : Bars.t) =
  let ( let* ) = Result.( let* ) in
  let* length = Bars.length bars in
  Bars.fold bars (Ok ()) @@ fun _ data acc ->
  let* _ = acc in
  let close = Data.get_row data Close in
  let ohclv = get_ohclv data in
  let rsi = Data.get_row data RSI |> Data.Row.slice 14 (length - 14) in
  let slow_k = Data.get_row data FSO_K |> Data.Row.slice 17 (length - 17) in
  let slow_d = Data.get_row data FSO_D |> Data.Row.slice 17 (length - 17) in
  let* _, _ = TA.ta_rsi 0 (length - 1) close 14 rsi in
  let* _, _ = TA.ta_stoch 0 (length - 1) ohclv 14 3 1 3 1 slow_k slow_d in
  (* Eio.traceln "%a" (Data.pp_row RSI) data; *)
  (* Eio.traceln "%d" length; *)
  (* Eio.traceln "%d, %d" outBegIdx outNBElement; *)
  (* Eio.traceln "%a" (Data.pp_row FSO_K) data; *)
  (* Error.fatal "NYI" *)
  Result.return ()

let initialize () =
  match TA.ta_initialize () with
  | Ok () -> ()
  | Error e ->
    Eio.traceln "Problem when initializing TA-Lib";
    invalid_arg e

(* let top () = *)
(*   TA.F. *)
