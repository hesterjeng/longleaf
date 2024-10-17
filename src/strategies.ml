module State = struct
  type state =
    [ `Initialize | `Listening | `Ordering | `Liquidate | `Finished of string ]
  [@@deriving show { with_path = false }]

  type 'a t = { env : Environment.t; current : state; content : 'a }
  type ('a, 'b) status = Running of 'a t | Shutdown of 'b

  let continue x = Running x
  let shutdown x = Shutdown x

  let init env =
    { env; current = `Initialize; content = Trading_types.Bars.empty }
end

module type S = sig
  val run : Environment.t -> string Lwt.t
end

module Log = (val Logs.src_log Logs.(Src.create "strategies"))

let check_time backtest =
  let open CalendarLib in
  let time = Time.now () in
  let open_time = Calendar.Time.lmake ~hour:8 ~minute:30 () in
  let close_time = Calendar.Time.lmake ~hour:16 () in
  if
    Calendar.Time.compare time open_time = 1
    && Calendar.Time.compare time close_time = -1
  then Lwt_result.return ()
  else if backtest then Lwt_result.return ()
  else
    Lwt_result.ok
      (Log.app (fun k -> k "Waiting because market is closed");
       Lwt.return_unit)

let listen_tick backtest (tick : unit -> (unit, 'a) Lwt_result.t) =
  let open Lwt_result.Syntax in
  let* () = check_time backtest in
  let* () =
    Lwt.pick
      [
        (let* _ = tick () in
         Lwt_result.return ());
        Lwt_result.error @@ Util.listen_for_input ();
      ]
  in
  Lwt_result.return ()

let run step env =
  let init = State.init env in
  let open Lwt.Syntax in
  let rec go prev =
    let* stepped = step prev in
    match stepped with
    | Ok x -> (
        match x with
        | State.Running now -> (go [@tailcall]) now
        | Shutdown code -> Lwt.return code)
    | Error s -> (
        let try_liquidating () =
          let liquidate = { prev with current = `Liquidate } in
          go liquidate
        in
        match prev.current with
        | `Liquidate -> Lwt.return s
        | _ -> try_liquidating ())
  in
  go init

let output_data backtest get_cash (state : _ State.t) =
  let backtest = if backtest then "test" else "live" in
  let json =
    Trading_types.Bars.yojson_of_t state.content |> Yojson.Safe.to_string
  in
  let tail = Lots_of_words.select () ^ "_" ^ Lots_of_words.select () in
  let filename = Format.sprintf "data/%s_%s" backtest tail in
  let oc = open_out filename in
  output_string oc json;
  close_out oc;
  Log.app (fun k -> k "cash: %f" (get_cash ()))

let ok_code = Cohttp.Code.status_of_code 200

module SimpleStateMachine (Backend : Backend.S) : S = struct
  open Trading_types
  open Lwt_result.Syntax
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))

  let step (state : 'a State.t) : (('a, 'b) State.status, string) Lwt_result.t =
    let env = state.env in
    (* Format.printf "\r\x1b[2K%s%!" (State.show_state state.current); *)
    (* Unix.sleepf 0.01; *)
    match state.current with
    | `Initialize ->
        Lwt_result.return @@ State.continue { state with current = `Listening }
    | `Listening ->
        let* () = listen_tick Backend.backtesting Backend.Ticker.tick in
        Lwt_result.return @@ State.continue { state with current = `Ordering }
    | `Liquidate ->
        let* _ = Backend.liquidate env in
        Lwt_result.return
        @@ State.continue
             { state with current = `Finished "Successfully liquidated" }
    | `Finished code ->
        output_data Backend.backtesting Backend.get_cash state;
        Lwt_result.return @@ State.shutdown code
    | `Ordering ->
        let* latest_bars = Backend.latest_bars env [ "MSFT"; "NVDA" ] in
        let msft = Bars.price latest_bars "MSFT" in
        let nvda = Bars.price latest_bars "NVDA" in
        let cash_available = Backend.get_cash () in
        let qty =
          match cash_available >=. 0.0 with
          | true ->
              let tenp = cash_available *. 0.5 in
              let max_amt = tenp /. nvda in
              if max_amt >=. 1.0 then Float.round max_amt |> Float.to_int else 0
          | false -> 0
        in
        (* Actually do the trade *)
        let* () =
          if msft <. nvda then Lwt_result.return ()
          else
            let order : Order.t =
              {
                symbol = "NVDA";
                side = Side.Buy;
                tif = TimeInForce.Day;
                order_type = OrderType.Market;
                qty;
                price = nvda;
              }
            in
            let* _json_resp = Backend.create_order env order in
            Lwt_result.return ()
        in
        let new_bars = Bars.combine [ latest_bars; state.content ] in
        Lwt_result.return
        @@ State.continue
             { state with current = `Listening; content = new_bars }

  let run = run step
end
