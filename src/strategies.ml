module type S = sig
  val run : unit -> string
  val shutdown : unit -> unit
end

module type STRAT_BUILDER = functor (_ : Backend.S) -> S

module Log = (val Logs.src_log Logs.(Src.create "strategies"))

module Strategy_utils (Backend : Backend.S) = struct
  type signal = Shutdown | Continue [@@deriving show]

  let num_iterations = ref 0

  let listen_tick orders bars : signal =
    (* if !num_iterations > 3 then exit 1; *)
    let res =
      num_iterations := !num_iterations + 1;
      Eio.Fiber.any
      @@ [
           (fun () ->
             Pmutex.set Backend.LongleafMutex.data_mutex bars;
             Pmutex.set Backend.LongleafMutex.orders_mutex orders;
             match Backend.next_market_open () with
             | None ->
                 Backend.Ticker.tick Backend.env;
                 Continue
             | Some open_time -> (
                 let open_time = Ptime.to_float_s open_time in
                 Eio.traceln "@[Waiting until market open...@]@.";
                 Eio.Time.sleep_until Backend.env#clock open_time;
                 Eio.traceln "@[Market is open, resuming.@]@.";
                 Eio.Time.now Backend.env#clock |> Ptime.of_float_s |> function
                 | Some t ->
                     Eio.traceln "@[Current time: %a@]@." Time.pp t;
                     Continue
                 | None ->
                     Eio.traceln
                       "@[Detected an illegal time!  Shutting down.@]@.";
                     Shutdown));
           (fun () ->
             while
               let shutdown = Pmutex.get Backend.LongleafMutex.shutdown_mutex in
               not shutdown
             do
               Ticker.OneSecond.tick Backend.env
             done;
             Eio.traceln "@[Shutdown command received by shutdown mutex.@]@.";
             Shutdown);
           (fun () ->
             let close_time = Backend.next_market_close () in
             let now =
               Eio.Time.now Backend.env#clock |> Ptime.of_float_s |> function
               | Some t -> t
               | None ->
                   invalid_arg
                     "Unable to get clock time in listen_tick for market close"
             in
             let time_until_close =
               Ptime.diff close_time now |> Ptime.Span.to_float_s
             in
             while Backend.overnight || time_until_close >=. 600.0 do
               Ticker.Forever.tick Backend.env
             done;
             Eio.traceln
               "@[Liquidating because we are within 10 minutes to market \
                close.@]@.";
             Shutdown);
         ]
    in
    (* Eio.traceln "@[%a@]@." pp_signal res; *)
    res

  let run ~init_state step =
    let rec go prev =
      let stepped = step prev in
      match stepped with
      | Ok x -> go x
      | Error s -> (
          let try_liquidating () =
            Eio.traceln
              "@[Trying to liquidate because of a signal or error: %s@]@." s;
            let liquidate = { prev with State.current = `Liquidate } in
            go liquidate
          in
          match prev.current with
          | `Liquidate | `Finished _ ->
              Eio.traceln "@[Exiting run.@]@.";
              s
          | _ -> try_liquidating ())
    in
    go init_state

  let get_filename () = Lots_of_words.select () ^ "_" ^ Lots_of_words.select ()

  let output_data (state : _ State.t) filename =
    Eio.traceln "cash: %f" (Backend.get_cash ());
    match Backend.is_backtest with
    | false -> ()
    | true ->
        Eio.traceln "Saving all bars...";
        Bars.print_to_file ~filename state.bars "live";
        Bars.print_to_file ~filename Backend.received_data "received"

  let output_order_history (state : _ State.t) filename =
    let json_str =
      Order_history.yojson_of_t state.order_history |> Yojson.Safe.to_string
    in
    let filename = Format.sprintf "data/order_history_%s.json" filename in
    let oc = open_out filename in
    output_string oc json_str;
    close_out oc

  let handle_nonlogical_state (current : State.nonlogical_state)
      (state : _ State.t) =
    (* Eio.traceln "There are %d bindings in state.bars" *)
    (*   (Bars.Hashtbl.length state.bars); *)
    match current with
    | `Initialize ->
        let symbols_str = String.concat "," Backend.symbols in
        Pmutex.set Backend.LongleafMutex.symbols_mutex (Some symbols_str);
        Result.return @@ { state with current = `Listening }
    | `Listening -> (
        match listen_tick state.order_history state.bars with
        | Continue ->
            let open Result.Infix in
            let+ latest = Backend.latest_bars Backend.symbols in
            Bars.append latest state.bars;
            { state with current = `Ordering; latest }
        | Shutdown ->
            Eio.traceln "Attempting to liquidate positions before shutting down";
            Result.return { state with current = `Liquidate })
    | `Liquidate ->
        Backend.liquidate state;
        Result.return
        @@ { state with current = `Finished "Liquidation finished" }
    | `Finished code ->
        Eio.traceln "@[Reached finished state.@]@.";
        let bars = state.bars in
        Vector.iter (fun order -> Bars.add_order order bars)
        @@ Pmutex.get Backend.LongleafMutex.orders_mutex;
        Pmutex.set Backend.LongleafMutex.data_mutex bars;
        let filename = get_filename () in
        output_data state filename;
        output_order_history state filename;
        Result.fail code
end
