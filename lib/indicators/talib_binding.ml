[@@@warning "-33"]

module Bars = Longleaf_bars
module Data = Bars.Data

let common = Tacaml.Defaults.common_indicators

module Input = struct
  open Tacaml.Input

  let ohclv (x : Data.t) : (Tacaml.Ohlcv.t, Error.t) result =
    let ( let* ) = Result.( let* ) in
    let* open_ = Data.get_row x Open in
    let* high = Data.get_row x High in
    let* close = Data.get_row x Close in
    let* low = Data.get_row x Low in
    let* volume = Data.get_row x Volume in
    Result.return @@ { Tacaml.Ohlcv.open_; high; close; low; volume }

  let of_data (x : Tacaml.t) (data : Data.t) =
    let ( let* ) = Result.( let* ) in
    let input = Tacaml.input x in
    let* ohclv = ohclv data in
    match input with
    | Flag.OhlcvFlag -> Result.return @@ Ohlcv ohclv
    | Flag.FloatBAFlag -> Result.return @@ FloatBA ohclv.close
    | Flag.FloatBA2Flag -> Result.return @@ FloatBA2 (ohclv.open_, ohclv.volume)
end

module Output = struct
  (* open Tacaml.Output *)
  (* open Data.Type *)
  module Flag = Tacaml.Output.Flag

  let of_data (x : Tacaml.t) (data : Data.t) : (Tacaml.Output.t, _) result =
    let ( let* ) = Result.( let* ) in
    (* let f1 = CustomTacaml x in *)
    let output = Tacaml.output x in
    match output with
    | Flag.FloatBAFlag i ->
      let* row = Data.get_row data (Tacaml i) in
      Result.return @@ Tacaml.Output.FloatBA row
    | Flag.FloatBA2Flag (f1, f2) ->
      let* row1 = Data.get_row data (Tacaml f1) in
      let* row2 = Data.get_row data (Tacaml f2) in
      Result.return @@ Tacaml.Output.FloatBA2 (row1, row2)
    | Flag.FloatBA3Flag (f1, f2, f3) ->
      let* row1 = Data.get_row data (Tacaml f1) in
      let* row2 = Data.get_row data (Tacaml f2) in
      let* row3 = Data.get_row data (Tacaml f3) in
      Result.return @@ Tacaml.Output.FloatBA3 (row1, row2, row3)
    | Flag.IntBAFlag i ->
      let* row = Data.get_int_row data (Tacaml i) in
      Result.return @@ Tacaml.Output.IntBA row
    | Flag.IntBA2Flag (i, j) ->
      let* row1 = Data.get_int_row data (Tacaml i) in
      let* row2 = Data.get_int_row data (Tacaml j) in
      Result.return @@ Tacaml.Output.IntBA2 (row1, row2)
end

let calculate ?i (indicator : Tacaml.t) (data : Data.t) =
  let ( let* ) = Result.( let* ) in
  let* input = Input.of_data indicator data in
  let* output = Output.of_data indicator data in
  let* _ =
    try Tacaml.calculate ?i indicator input output with
    | Invalid_argument msg as e when String.equal msg "index out of bounds" ->
      Eio.traceln "===== INDEX OUT OF BOUNDS ERROR =====";
      Eio.traceln "Indicator: %a" Tacaml.pp indicator;
      Eio.traceln "Data size: %d" (Data.size data);
      (match i with
       | Some tick ->
         Eio.traceln "Processing tick: %d" tick;
         Eio.traceln "Tick valid: %b" (tick >= 0 && tick < Data.size data)
       | None -> Eio.traceln "Processing all ticks");
      (* Try to get more info about what index failed *)
      Eio.traceln "Exception: %s" (Printexc.to_string e);
      Eio.traceln "Backtrace:";
      Eio.traceln "%s" (Printexc.get_backtrace ());
      Eio.traceln "=====================================";
      let s = Printexc.to_string e in
      Error.fatal @@ Format.asprintf "Indicator %a failed with index out of bounds (data.size=%d): %s"
        Tacaml.pp indicator (Data.size data) s
    | e ->
      Eio.traceln "===== INDICATOR COMPUTATION ERROR =====";
      Eio.traceln "Indicator: %a" Tacaml.pp indicator;
      Eio.traceln "Data size: %d" (Data.size data);
      (match i with
       | Some tick -> Eio.traceln "Processing tick: %d" tick
       | None -> Eio.traceln "Processing all ticks");
      Eio.traceln "Exception: %s" (Printexc.to_string e);
      Eio.traceln "Backtrace:";
      Eio.traceln "%s" (Printexc.get_backtrace ());
      Eio.traceln "========================================";
      let s = Printexc.to_string e in
      Error.fatal @@ Format.asprintf "Indicator %a failed: %s" Tacaml.pp indicator s
  in
  (* Eio.traceln "talib_binding.calculate: %d %d" a b; *)
  Result.return ()
