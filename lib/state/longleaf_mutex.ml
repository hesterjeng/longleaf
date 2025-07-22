module Pmutex = Longleaf_util.Pmutex

type t = {
  shutdown_mutex : bool Pmutex.t;
  state_mutex : unit State.t Pmutex.t;
  symbols_mutex : string option Pmutex.t;
  target_symbol : string option Pmutex.t;
}

let create config =
  let shutdown_mutex = Pmutex.make false in
  let state_mutex = Pmutex.make @@ State.empty config in
  let symbols_mutex = Pmutex.make None in
  (* let indicators_mutex = Pmutex.make @@ Indicators.empty Precomputed in *)
  let target_symbol = Pmutex.make None in
  {
    shutdown_mutex;
    state_mutex;
    symbols_mutex;
    (* indicators_mutex; *)
    target_symbol;
  }

let pp : t Format.printer = fun fmt _x -> Format.fprintf fmt "<mutex>"

(* let create (module X : sig type context val x : content end) = *)
(*   create () *)
