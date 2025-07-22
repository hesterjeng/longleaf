module Work_pool = Longleaf_util.Work_pool
module Error = Longleaf_core.Error

let run_l options l =
  let ( let* ) = Result.( let* ) in
  Eio.traceln "Enumerate mode E1";
  let mutices = Longleaf_state.Mutex.create () in
  let* vals = Result.map_l (Gadt.run options mutices) l in
  Eio.traceln "%a" (List.pp Float.pp) vals;
  Result.return 0.0

let top_ options =
  let ( let* ) = Result.( let* ) in
  let* _res = run_l options Gadt_examples.all_strategies in
  let res = 0.1234 in
  Result.return res

let top options =
  match top_ options with
  | Ok x -> x
  | Error e ->
    Eio.traceln "%a" Error.pp e;
    0.0

(* module Make : Strategy.BUILDER = functor (_ : sig end) -> (val dummy) *)
