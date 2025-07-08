[@@@warning "-33"]

module Data = Bars.Data

module Input = struct
  open Tacaml.Input

  let ohclv (x : Data.t) : (t, Error.t) result =
    let ( let* ) = Result.( let* ) in
    let* open_ = Data.get_row x Open in
    let* high = Data.get_row x High in
    let* close = Data.get_row x Close in
    let* low = Data.get_row x Low in
    let* volume = Data.get_row x Volume in
    Result.return @@ Ohlcv { Tacaml.Ohlcv.open_; high; close; low; volume }

  let of_data (Pack indicators : Tacaml.t) (data : Data.t) :
      (Tacaml.Input.t, Error.t) result =
    match indicators with
    | Sma _ -> ohclv data
    | _ -> Error.fatal "Input for indicator not yet implemented"
end

module Output = struct
  open Tacaml.Output
  open Data.Type

  let float_ba data output =
    Result.map (fun x -> FloatBA x) @@ Data.get_row data output

  let of_data (Pack indicators : Tacaml.t) (data : Data.t) =
    match indicators with
    | Sma _ ->
      let output = Tacaml (F Sma) in
      float_ba data output
    | _ -> Error.fatal "Output for indicator not yet implemented"
end

let calculate ?i (indicator : Tacaml.t) (data : Data.t) =
  let ( let* ) = Result.( let* ) in
  let* input = Input.of_data indicator data in
  let* output = Output.of_data indicator data in
  let* _ = Tacaml.calculate ?i indicator input output in
  Result.return ()
