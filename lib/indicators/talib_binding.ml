[@@@warning "-33"]

module Data = Bars.Data

let ohclv (x : Data.t) : Tacaml.Ohlcv.t =
  {
    open_ = Data.get_row x Open;
    high = Data.get_row x High;
    close = Data.get_row x Close;
    low = Data.get_row x Low;
    volume = Data.get_row x Volume;
  }

module Input = struct
  open Tacaml.Input

  let of_data (Pack indicators : Tacaml.t) (data : Data.t) :
      (Tacaml.Input.t, Error.t) result =
    match indicators with
    | Sma _ -> Result.return @@ Ohlcv (ohclv data)
    | _ -> Error.fatal "Input for indicator not yet implemented"
end

module Output = struct
  open Tacaml.Output
  open Data.Type

  let of_data (Pack indicators : Tacaml.t) (data : Data.t) =
    match indicators with
    | Sma _ ->
      let output = Tacaml (F Sma) in
      Result.return @@ FloatBA (Data.get_row data output)
    | _ -> Error.fatal "Output for indicator not yet implemented"
end

let calculate ?i (indicator : Tacaml.t) (data : Data.t) =
  let ( let* ) = Result.( let* ) in
  let* input = Input.of_data indicator data in
  let* output = Output.of_data indicator data in
  let* _ = Tacaml.calculate ?i indicator input output in
  Result.return ()
