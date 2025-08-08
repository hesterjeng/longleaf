module Backend = Longleaf_backend
module Options = Longleaf_core.Options
module Util = Longleaf_util
module Environment = Longleaf_core.Environment

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

type builder = (module BUILDER)

let mk_options switch eio_env flags target tacaml_indicators : Options.t =
  let longleaf_env = Environment.make () in
  (* let mutices = Server.Longleaf_mutex.create () in *)
  {
    symbols = Ticker_collections.sp100;
    eio_env;
    longleaf_env;
    switch;
    flags;
    tick = 600.0;
    target;
    tacaml_indicators;
  }

(** Helper function to reduce code duplication. *)
let run (module Strat : BUILDER) bars options mutices =
  (* let options = run_options context in *)
  let ( let* ) = Result.( let* ) in
  let* backend = Backend.make mutices bars options in
  let module Backend = (val backend) in
  let module S = Strat (Backend) in
  Eio.traceln "Applied strategy functor to backend, running %s."
    options.flags.strategy_arg;
  let res = S.run () in
  Backend.shutdown ();
  Result.return res
