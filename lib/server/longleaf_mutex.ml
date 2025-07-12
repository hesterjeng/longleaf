type 'a t = {
  shutdown_mutex : bool Pmutex.t;
  data_mutex : Bars.t Pmutex.t;
  (* indicators_mutex : Indicators.t Pmutex.t; *)
  trading_state_mutex : 'a State.t option Pmutex.t;
  symbols_mutex : string option Pmutex.t;
  target_symbol : string option Pmutex.t;
}

let create () =
  let shutdown_mutex = Pmutex.make false in
  let data_mutex = Pmutex.make @@ Bars.empty () in
  let trading_state_mutex =
    Pmutex.make None
    (* State.Core.empty () *)
  in
  let symbols_mutex = Pmutex.make None in
  (* let indicators_mutex = Pmutex.make @@ Indicators.empty Precomputed in *)
  let target_symbol = Pmutex.make None in
  {
    shutdown_mutex;
    data_mutex;
    trading_state_mutex;
    symbols_mutex;
    (* indicators_mutex; *)
    target_symbol;
  }

let pp : 'a t Format.printer = fun fmt _x -> Format.fprintf fmt "<mutex>"
