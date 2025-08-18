module Work_pool = Longleaf_util.Work_pool
module Error = Longleaf_core.Error

let run_l bars options l =
  let ( let* ) = Result.( let* ) in
  Eio.traceln "Enumerate mode E1";
  let mutices = Longleaf_state.Mutex.create [] in
  let* vals = Result.map_l (Gadt_strategy.run bars options mutices) l in
  Eio.traceln "%a" (List.pp Float.pp) vals;
  Result.return 0.0

let top_ bars options = invalid_arg "NYI"
(* let ( let* ) = Result.( let* ) in *)
(* let* _res = run_l bars options Gadt_examples.all_strategies in *)
(* let res = 0.1234 in *)
(* Result.return res *)

let top bars options =
  match top_ bars options with
  | Ok x -> x
  | Error e ->
    Eio.traceln "%a" Error.pp e;
    0.0

(* module Make : Strategy.BUILDER = functor (_ : sig end) -> (val dummy) *)
