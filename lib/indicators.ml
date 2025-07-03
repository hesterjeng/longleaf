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
  let rsi = Data.get_row data RSI in
  let slow_k = Data.get_row data FSO_K in
  let slow_d = Data.get_row data FSO_D in
  let* () = Error.guard_res @@ TA.ta_rsi 0 (length - 1) close 14 0 rsi in
  let* () =
    Error.guard_res
    @@ TA.ta_stoch 0 (length - 1) ohclv 14 14 0 14 0 0 (-1) slow_k slow_d
  in
  Eio.traceln "%a" Bars.pp bars;
  Error.fatal "NYI"

let initialize () =
  match TA.ta_initialize () with
  | Ok () ->
    Eio.traceln "Initialized TA-Lib";
    Ok ()
  | Error e -> Error.fatal e

(* let top () = *)
(*   TA.F. *)
