(* module TA = Tacaml.F *)
module Config = Config
module Data = Bars.Data

type t = Tacaml of Tacaml.t

let compute ?i (indicators : t list) (config : Config.t) (bars : Bars.t) =
  match config.compute_live with
  | false ->
    Eio.traceln "Precomputing indicators because of Indicator_config.t";
    let ( let* ) = Result.( let* ) in
    Bars.fold bars (Ok ()) @@ fun _ data acc ->
    let* _ = acc in
    let* () =
      Result.fold_l
        (fun _ indicator ->
          match indicator with
          | Tacaml ind ->
            let* () = Talib_binding.calculate ?i ind data in
            Result.return ())
        () indicators
    in
    Result.return ()
  | true ->
    Eio.traceln "Not precomputing indicators because of Indicator_config.t";
    Result.return ()

let initialize () =
  match Tacaml.initialize () with
  | Ok () -> ()
  | Error e ->
    Eio.traceln "Problem when initializing TA-Lib";
    invalid_arg e
