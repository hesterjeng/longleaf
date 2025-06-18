module Context = Options.Context
module Collections = Ticker_collections

(** Function for creating the options given a context, it has some sensible
    defaults. If you want to use other options, you may need to create your own
    Options.t value. *)
let run_options (context : Context.t) : Options.t =
  let symbols =
    match context.runtype with
    | RandomTickerBacktest
    | MultiRandomTickerBacktest ->
      let arr = Array.of_list Collections.sp100 in
      let eighty_percent =
        (Array.length arr |> Float.of_int) *. 0.8 |> Int.of_float
      in
      Owl_stats.choose arr eighty_percent |> Array.to_list
    | _ -> Collections.sp100
  in
  {
    symbols;
    tick = 600.0;
    overnight = true;
    resume_after_liquidate = true;
    indicators_config : Indicator_config.t =
      { fft = false; compare_preloaded = context.compare_preloaded };
    dropout = false;
    randomized_backtest_length = 1000;
    context;
  }

(** Helper function to reduce code duplication. *)
let top ?(run_options = run_options)
    (module Strat : Strategy.BUILDER) context =
  let options = run_options context in
  (* let bars = *)
  (*   match bars with *)
  (*   | None -> invalid_arg "Run.top: Cannot run_generic with bars None" *)
  (*   | Some x -> x *)
  (* in *)
  (* let () = check_bars options in *)
  let module Backend =
    (val Backend.make options |> function
         | Ok x -> x
         | Error _ -> invalid_arg "Error while creating backend")
  in
  let module S = Strat (Backend) in
  Eio.traceln "Applied strategy functor to backend, running.";
  let res = S.run () in
  Backend.shutdown ();
  res
