module type S = sig
  val run : unit -> float
  val shutdown : unit -> unit
end

let dummy f =
  let module Dummy = struct
    let run () =
      let _ = f () in
      0.0

    let shutdown () = ()
  end in
  (module Dummy : S)

module type BUILDER = functor (_ : Backend.S) -> S

let mk_options switch eio_env flags target : Options.t =
  let longleaf_env = Util.Environment.make () in
  let mutices = Server.Longleaf_mutex.create () in
  {
    symbols = Ticker_collections.sp100;
    eio_env;
    longleaf_env;
    switch;
    flags;
    tick = 600.0;
    indicators_config = Indicators.Config.default;
    target;
    mutices;
  }

(** Helper function to reduce code duplication. *)
let run (module Strat : BUILDER) options =
  (* let options = run_options context in *)
  let ( let* ) = Result.( let* ) in
  let* backend = Backend.make options in
  let module Backend = (val backend) in
  let module S = Strat (Backend) in
  Eio.traceln "Applied strategy functor to backend, running %s."
    options.flags.strategy_arg;
  let res = S.run () in
  Backend.shutdown ();
  Result.return res
