module type RUN_DATA = sig
  (* val bars : Bars.t *)
  val symbols : string list
  val tick : float
  val overnight : bool
end

module type RUN_CONTEXT = sig
  val eio_env : Eio_unix.Stdenv.base
  val longleaf_env : Environment.t
  val switch : Eio.Switch.t
  val preload : Options.Preload.t
  val target : string option
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
      Bars.sort (Ord.opp Bars.Item.compare) res;
      res

    (* Preload *)
    let bars =
      match Context.preload with
      | None -> Bars.empty
      | Download -> invalid_arg "Downloading data for preload NYI"
      | File file ->
          let res = Yojson.Safe.from_file file |> Bars.t_of_yojson in
          Bars.sort Bars.Item.compare res;
          res
  end

  module Backtesting () : Backend.S =
    Backend.Backtesting (Input) (LongleafMutex)

  module Alpaca () : Backend.S =
    Backend.Alpaca
      (Input)
      (Ticker.Make (struct
        let tick = Input.tick
      end))
      (LongleafMutex)

  let top (runtype : Options.Runtype.t) =
    let backend =
      match runtype with
      | Live -> invalid_arg "Live trading is not implemented yet."
      | Manual -> invalid_arg "Cannot create a strategy with manual runtype."
      | Paper ->
          Eio.traceln
            "@[Creating Alpaca paper backend with:@.tick: %f@.symbols: %a@]@."
            Input.tick
            List.(pp String.pp)
            Input.symbols;
          (module Alpaca () : Backend.S)
      | Backtest -> (module Backtesting () : Backend.S)
    in
    let module Backend = (val backend) in
    let module Strategy = StrategyBuilder (Backend) in
    let res = Strategy.run () in
    Backend.shutdown ();
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
      ]

    let tick = 600.0
    let overnight = false
  end

  module Make = Make (Data) (Double_top.DoubleTop)
end
