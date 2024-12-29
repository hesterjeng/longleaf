module type RUN_DATA = sig
  (* val bars : Bars.t *)
  val symbols : string list
  val tick : float
  val overnight : bool
  val resume_after_liquidate : bool
end

module type RUN_CONTEXT = sig
  val eio_env : Eio_unix.Stdenv.base
  val longleaf_env : Environment.t
  val switch : Eio.Switch.t
  val preload : Options.Preload.t
  val target : string option
  val save_received : bool
end

module type S = sig
  val top : Options.Runtype.t -> unit
end

module Make
    (Data : RUN_DATA)
    (StrategyBuilder : Strategies.STRAT_BUILDER)
    (LongleafMutex : Backend.LONGLEAF_MUTEX)
    (Context : RUN_CONTEXT) : S = struct
  module Input : Backend.BACKEND_INPUT = struct
    include Data
    include Context

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

  module Backtesting : Backend.S = Backend.Backtesting (Input) (LongleafMutex)

  module Alpaca : Backend.S =
    Backend.Alpaca
      (Input)
      (Ticker.Make (struct
        let tick = Input.tick
      end))
      (LongleafMutex)

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
      | Backtest -> backtest ()
    in
    Alpaca.shutdown ();
    Backtesting.shutdown ();
    Eio.traceln "Result: %s" res;
    ()
end

module DoubleTop = struct
  module Data : RUN_DATA = struct
    let symbols =
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
      ]

    let tick = 600.0
    let overnight = false
    let resume_after_liquidate = true
  end

  module Make = Make (Data) (Double_top.DoubleTop)
end

module Listener = struct
  module Data : RUN_DATA = struct
    let symbols =
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
      ]

    let tick = 600.0
    let overnight = true
    let resume_after_liquidate = true
  end

  module Make = Make (Data) (Listener.Make)
end
