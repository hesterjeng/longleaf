open State_machine

module SimpleStateMachine (Backend : BACKEND) : STRAT = struct
  let () = Random.self_init ()

  open Trading_types
  open State
  open Lwt_result.Syntax
  module Log = (val Logs.src_log Logs.(Src.create "simple-state-machine"))

  let ok_code = Cohttp.Code.status_of_code 200

  let listen_tick state =
    let open CalendarLib in
    let time = Time.now () in
    let open_time = Calendar.Time.lmake ~hour:8 ~minute:30 () in
    let close_time = Calendar.Time.lmake ~hour:16 () in
    Log.app (fun k -> k "@[%s@]@." (Util.show_calendar_time_t time));
    let* () =
      if
        Calendar.Time.compare time open_time = 1
        && Calendar.Time.compare time close_time = -1
      then (
        Log.app (fun k -> k "Market is open");
        Lwt_result.return ())
      else
        Lwt_result.ok
          (Log.app (fun k -> k "Waiting because market is closed");
           Lwt_unix.sleep 5.0)
    in
    let* () =
      Lwt.pick
        [
          (let* _ = Backend.Ticker.tick () in
           Lwt_result.return ());
          Lwt_result.error @@ Util.listen_for_input ();
        ]
    in
    Lwt_result.return @@ continue { state with current = Ordering }

  (* TODO: Handle market closed with live backend rather than requesting all through the night *)
  let step (state : 'a State.t) : (('a, 'b) State.status, string) Lwt_result.t =
    let env = state.env in
    match state.current with
    | Initialize ->
        Log.app (fun k -> k "Running");
        Lwt_result.return @@ continue { state with current = Listening }
    | Listening -> listen_tick state
    | Liquidate ->
        Log.app (fun k -> k "Liquidate");
        let* _ = Backend.liquidate env in
        Lwt_result.return
        @@ continue { state with current = Finished "Successfully liquidated" }
    | Finished code ->
        let json =
          Trading_types.Bars.yojson_of_t state.content |> Yojson.Safe.to_string
        in
        let filename = Format.sprintf "data/live_%s" (Util.rfc339 ()) in
        let oc = open_out filename in
        output_string oc json;
        close_out oc;
        Log.app (fun k -> k "cash: %f" (Backend.get_cash ()));
        Lwt_result.return @@ shutdown code
    | Ordering ->
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
            Log.app (fun k -> k "Placed order");
            Lwt_result.return ()
        in
        let new_bars = Bars.combine [ latest_bars; state.content ] in
        let* () = Lwt_result.ok @@ Lwt_unix.sleep 0.01 in
        Lwt_result.return
        @@ continue { state with current = Listening; content = new_bars }

  let run env =
    let init = init env in
    let open Lwt.Syntax in
    let rec go prev =
      let* stepped = step prev in
      match stepped with
      | Ok x -> (
          match x with
          | Running now -> go now
          | Shutdown code -> Lwt.return code)
      | Error s -> (
          let try_liquidating () =
            let liquidate = { prev with current = Liquidate } in
            let* liquidated = go liquidate in
            Lwt.return liquidated
          in
          match prev.current with
          | Liquidate -> Lwt.return s
          | _ -> try_liquidating ())
    in
    go init
end
