let top_ options =
  let ( let* ) = Result.( let* ) in
  let gadt = Gadt.macd_bollinger_momentum in
  let* res = Gadt.run gadt options in
  Result.return res
(* let dummy = Strategy.dummy top *)

let top options =
  match top_ options with
  | Ok x -> x
  | Error e ->
    Eio.traceln "%a" Error.pp e;
    0.0

(* module Make : Strategy.BUILDER = functor (_ : sig end) -> (val dummy) *)
