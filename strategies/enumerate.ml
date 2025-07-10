module Work_pool = Longleaf_lib.Util.Work_pool

let run_l options l =
  let ( let* ) = Result.( let* ) in
  Eio.traceln "Enumerate mode E1";
  let* vals = Result.map_l (Gadt.run options) l in
  Eio.traceln "%a" (List.pp Float.pp) vals;
  Result.return 0.0

let top_ options =
  let ( let* ) = Result.( let* ) in
  (* let* () = *)
  (*   Indicators.compute_all ~eio_env:Backend.env *)
  (*     options.indicators_config state.bars *)
  (* in *)
  let* res = run_l options Gadt_examples.all_strategies in
  let res = 0.1234 in
  Result.return res
(* let dummy = Strategy.dummy top *)

let top options =
  match top_ options with
  | Ok x -> x
  | Error e ->
    Eio.traceln "%a" Error.pp e;
    0.0

(* module Make : Strategy.BUILDER = functor (_ : sig end) -> (val dummy) *)
