module Run_options = struct
  type t = {
    symbols : string list;
    tick : float;
    overnight : bool;
    resume_after_liquidate : bool;
  }
end

module Run_context = struct
  type t = {
    eio_env : Eio_unix.Stdenv.base;
    longleaf_env : Environment.t;
    switch : Eio.Switch.t;
    preload : Options.Preload.t;
    target : string option;
    save_received : bool;
    mutices : Longleaf_mutex.t;
    run_options : Run_options.t option;
  }
end

module type RUN_CONTEXT = sig end

module type S = sig
  val top : Options.Runtype.t -> unit
end

module Make (StrategyBuilder : Strategy.BUILDER) (Context : RUN_CONTEXT) : S =
struct
  module Input : Backend.BACKEND_INPUT = struct
    include Context

    let run_options =
      Option.get_exn_or
        "Must have run options to instantiate strategy and backend"
        Context.run_options

    let symbols = run_options.symbols
    let overnight = run_options.overnight
    let resume_after_liquidate = run_options.resume_after_liquidate
    let tick = run_options.tick

    (* Target *)
    let target =
      let ( let+ ) = Option.( let+ ) in
      let+ res =
        Context.target
        |> Option.map @@ fun f -> Yojson.Safe.from_file f |> Bars.t_of_yojson
      in
      Bars.sort (Ord.opp Item.compare) res;
      res

    (* Preload *)
    let bars =
      match Context.preload with
      | None -> Bars.empty ()
      | Download -> invalid_arg "Downloading data for preload NYI"
      | File file ->
          Eio.traceln "Preloading bars from %s" file;
          let res = Yojson.Safe.from_file file |> Bars.t_of_yojson in
          Bars.sort Item.compare res;
          res
  end

  module Backtesting : Backend.S = Backend.Backtesting (Input)
  module Alpaca : Backend.S = Backend.Alpaca (Input)

  let live () = invalid_arg "Live trading is not implemented yet"
  let manual () = invalid_arg "Cannot create a strategy with manual runtype"

  let paper () =
    Eio.traceln "@[Creating Alpaca backend with:@.tick: %f@.symbols: %a@]@."
      Input.tick
      List.(pp String.pp)
      Input.symbols;
    let module Strategy = StrategyBuilder (Alpaca) in
    Strategy.run ()

  let listener = paper

  let backtest () =
    let module Strategy = StrategyBuilder (Backtesting) in
    Strategy.run ()

  let top (runtype : Options.Runtype.t) =
    let res =
      match runtype with
      | Live -> live ()
      | Manual -> manual ()
      | Paper -> paper ()
      | Listener -> listener ()
      | BuyAndHold -> backtest ()
      | Backtest -> backtest ()
    in
    Alpaca.shutdown ();
    Backtesting.shutdown ();
    Eio.traceln "Result: %s" res;
    ()
end

module DoubleTop = struct
  let run_options : Run_options.t =
    {
      symbols =
        [
          "NVDA";
          "TSLA";
          "AAPL";
          "MSFT";
          "NFLX";
          "META";
          "AMZN";
          "AMD";
          "AVGO";
          "ELV";
          "UNH";
          "MU";
          "V";
          "GOOG";
          "SMCI";
          "MSTR";
          "UBER";
          "LLY";
          "SPY";
        ];
      tick = 600.0;
      overnight = false;
      resume_after_liquidate = true;
    }

  module Make = Make (Data) (Double_top.DoubleTop)
end

module LowBall = struct
  let run_options : Run_options.t =
    {
      symbols =
        [
          "NVDA";
          "TSLA";
          "AAPL";
          "MSFT";
          "NFLX";
          "META";
          "AMZN";
          "AMD";
          "AVGO";
          "ELV";
          "UNH";
          "MU";
          "V";
          "GOOG";
          "SMCI";
          "MSTR";
          "UBER";
          "LLY";
        ];
      tick = 600.0;
      overnight = true;
      resume_after_liquidate = true;
    }

  module Make = Make (Data) (Buy_low_bollinger.BuyLowBollinger)
end

module Listener = struct
  let run_options : Run_options.t =
    {
      symbols =
        [
          "NVDA";
          "TSLA";
          "AAPL";
          "MSFT";
          "NFLX";
          "META";
          "AMZN";
          "AMD";
          "AVGO";
          "ELV";
          "UNH";
          "MU";
          "V";
          "GOOG";
          "SMCI";
          "MSTR";
          "UBER";
          "LLY";
        ];
      tick = 600.0;
      overnight = true;
      resume_after_liquidate = true;
    }

  module Make = Make (Data) (Listener.Make)
end

module BuyAndHold = struct
  let run_options : Run_options.t =
    {
      symbols = [ "SPY" ];
      tick = 600.0;
      overnight = true;
      resume_after_liquidate = true;
    }

  module Make = Make (Data) (Buy_and_hold.Make)
end
