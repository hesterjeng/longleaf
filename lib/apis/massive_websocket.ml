(* Massive.io WebSocket client for real-time market data *)
(* https://massive.io/docs/stocks/ws_getting-started *)

module Bars = Longleaf_bars
module Data = Bars.Data

(* Massive WebSocket authentication message *)
type auth_message = {
  action : string;
  params : string;
}
[@@deriving yojson]

(* Massive WebSocket subscription message *)
type subscribe_message = {
  action : string;
  params : string;  (* Comma-separated channels like "AM.AAPL,AM.MSFT" *)
}
[@@deriving yojson]

(* Massive status message *)
type status_message = {
  ev : string;          (* Event type: "status" *)
  status : string;      (* e.g., "auth_success", "connected" *)
  message : string;
}
[@@deriving show, yojson] [@@yojson.allow_extra_fields]

(* Massive aggregate per second message *)
type aggregate_message = {
  ev : string;          (* Event type: "AS" for aggregates per second *)
  sym : string;         (* Stock ticker symbol *)
  v : int;              (* Tick volume *)
  av : int;             (* Accumulated volume for the day *)
  op : float;           (* Official opening price (0 until 9:30am market open) *)
  vw : float;           (* Volume-weighted average price *)
  o : float;            (* Open price for this minute *)
  c : float;            (* Close price for this minute *)
  h : float;            (* High price for this minute *)
  l : float;            (* Low price for this minute *)
  a : float;            (* Day's volume-weighted average price *)
  z : int;              (* Average trade size *)
  s : int;              (* Start timestamp (Unix milliseconds) *)
  e : int;              (* End timestamp (Unix milliseconds) *)
  otc : bool option; [@yojson.option]  (* OTC ticker indicator (optional) *)
}
[@@deriving show, yojson]

(* Parse message as aggregate or status *)
type massive_message =
  | Status of status_message
  | Aggregate of aggregate_message
  | Unknown of string

(* WebSocket client for Massive *)
module Client = struct
  type t = {
    conn : Websocket.Connection.t;
    massive_key : string;
    mutable subscribed_tickers : Instrument.t list;
    mutable reconnect_attempts : int;
    mutable last_reconnect_time : float option;
  }

  (* Exponential backoff configuration *)
  let base_delay = 1.0 (* 1 second *)
  let max_delay = 60.0 (* 60 seconds max *)
  let max_reconnect_attempts = 10

  (* Calculate backoff delay with exponential backoff *)
  let calculate_backoff_delay attempt =
    let delay = base_delay *. (2.0 ** Float.of_int (min attempt 6)) in
    Float.min delay max_delay

  (* Connect to Massive WebSocket with retry logic *)
  let rec connect_with_retry ~sw ~env ~massive_key ~attempt ~use_delayed () =
    let ( let* ) = Result.( let* ) in
    let base_host = if use_delayed then "delayed.massive.com" else "socket.massive.com" in
    let url = Uri.of_string ("wss://" ^ base_host ^ "/stocks") in
    let authenticator = Https.authenticator () in

    if attempt > 0 then
      Eio.traceln "Massive WebSocket: Connection attempt %d/%d" attempt max_reconnect_attempts;

    Eio.traceln "Massive WebSocket: Connecting to %s..." (Uri.to_string url);

    match Websocket.Connection.handshake ~sw ~env ~authenticator url with
    | Ok conn ->
      Eio.traceln "Massive WebSocket: Connected successfully";
      let now = Unix.gettimeofday () in
      Ok {
        conn;
        massive_key;
        subscribed_tickers = [];
        reconnect_attempts = attempt;
        last_reconnect_time = Some now;
      }
    | Error e when attempt < max_reconnect_attempts ->
      let delay = calculate_backoff_delay attempt in
      Eio.traceln "Massive WebSocket: Connection failed: %s"
        (match e with
         | `InvalidScheme s -> "Invalid scheme: " ^ s
         | `InvalidUrl s -> "Invalid URL: " ^ s
         | `DnsError s -> "DNS error: " ^ s
         | `TlsError s -> "TLS error: " ^ s
         | `HandshakeError s -> "Handshake error: " ^ s);
      Eio.traceln "Massive WebSocket: Retrying in %.1f seconds (attempt %d/%d)"
        delay attempt max_reconnect_attempts;
      Eio.Time.sleep (Eio.Stdenv.clock env) delay;
      connect_with_retry ~sw ~env ~massive_key ~attempt:(attempt + 1) ~use_delayed ()
    | Error e ->
      Eio.traceln "Massive WebSocket: Max reconnection attempts reached, giving up";
      Error e

  (* Initial connection *)
  let connect ~sw ~env ~massive_key ?(use_delayed=false) () =
    let ( let* ) = Result.( let* ) in
    let* client = connect_with_retry ~sw ~env ~massive_key ~attempt:0 ~use_delayed () in

    (* Send authentication message immediately after connection *)
    let auth_msg : auth_message = {
      action = "auth";
      params = massive_key;
    } in
    let json = yojson_of_auth_message auth_msg in
    let msg_str = Yojson.Safe.to_string json in

    Eio.traceln "Massive WebSocket: Sending authentication";
    let* () = Websocket.Connection.send_text client.conn msg_str in

    (* Wait for auth confirmation *)
    let rec wait_for_auth retry_count =
      if retry_count > 10 then
        Error (`HandshakeError "Timed out waiting for auth confirmation")
      else
        let* frame = Websocket.Connection.receive client.conn in
        match frame.Websocket.Frame.opcode with
        | Text ->
          (try
            let json = Yojson.Safe.from_string frame.payload in
            (* Massive sends arrays of messages *)
            let messages = Yojson.Safe.Util.to_list json in
            let rec check_messages msgs =
              match msgs with
              | [] -> wait_for_auth (retry_count + 1)  (* No auth message yet *)
              | msg :: rest ->
                (try
                  let status_msg : status_message = status_message_of_yojson msg in
                  let ev_field = status_msg.ev in
                  let status_field = status_msg.status in
                  if String.equal ev_field "status" && String.equal status_field "auth_success" then begin
                    Eio.traceln "Massive WebSocket: Authentication successful";
                    Ok ()
                  end else if String.equal ev_field "status" && String.equal status_field "auth_failed" then
                    Error (`HandshakeError ("Authentication failed: " ^ status_msg.message))
                  else
                    check_messages rest
                with _ ->
                  (* Not a status message, skip *)
                  check_messages rest)
            in
            check_messages messages
          with
          | e ->
            Eio.traceln "Massive WebSocket: Error parsing auth response: %s"
              (Printexc.to_string e);
            wait_for_auth (retry_count + 1))
        | _ -> wait_for_auth (retry_count + 1)
    in

    let* () = wait_for_auth 0 in
    Ok client

  (* Subscribe to tickers *)
  let subscribe client tickers =
    let ( let* ) = Result.( let* ) in

    let ticker_symbols = List.map Instrument.symbol tickers in
    (* Build subscription params: "AS.AAPL,AS.MSFT,AS.TSLA" *)
    let params =
      List.map (fun sym -> "AS." ^ sym) ticker_symbols
      |> String.concat ","
    in

    let sub_msg = {
      action = "subscribe";
      params = params;
    } in

    let json = yojson_of_subscribe_message sub_msg in
    let msg_str = Yojson.Safe.to_string json in

    Eio.traceln "Massive WebSocket: Subscribing to %d tickers: [%s]"
      (List.length tickers)
      (String.concat ", " (List.take 5 ticker_symbols @
        (if List.length ticker_symbols > 5 then ["..."] else [])));

    let* () = Websocket.Connection.send_text client.conn msg_str in

    client.subscribed_tickers <- tickers;
    Eio.traceln "Massive WebSocket: Subscription request sent successfully";
    Ok ()

  (* Parse a Massive message from JSON *)
  let parse_message json =
    (* First check what event type this is *)
    let ev_type = match Yojson.Safe.Util.member "ev" json with
      | `String s -> s
      | _ -> "unknown"
    in

    match ev_type with
    | "status" ->
      (try
        let msg = status_message_of_yojson json in
        Status msg
       with e ->
         Eio.traceln "Massive WebSocket: Failed to parse status message: %s" (Printexc.to_string e);
         Unknown ev_type)
    | "AS" ->
      (try
        let msg = aggregate_message_of_yojson json in
        Aggregate msg
       with
       | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, json_value) ->
         Eio.traceln "Massive WebSocket: Failed to parse AS message";
         Eio.traceln "  Error: %s" (Printexc.to_string exn);
         Eio.traceln "  At JSON value: %s" (Yojson.Safe.to_string json_value);
         Eio.traceln "  Full message: %s" (Yojson.Safe.to_string json);
         Unknown ev_type
       | e ->
         Eio.traceln "Massive WebSocket: Failed to parse AS message: %s" (Printexc.to_string e);
         Eio.traceln "Massive WebSocket: Raw JSON: %s" (Yojson.Safe.to_string json);
         Unknown ev_type)
    | _ -> Unknown ev_type

  (* Receive and parse next message *)
  let receive_update client =
    let ( let* ) = Result.( let* ) in

    let* frame = Websocket.Connection.receive client.conn in

    match frame.Websocket.Frame.opcode with
    | Text ->
      (* Parse JSON message - Massive sends arrays *)
      (try
        let json = Yojson.Safe.from_string frame.payload in
        let messages = Yojson.Safe.Util.to_list json in

        (* Parse all messages and filter for aggregates *)
        let aggregates = List.filter_map (fun msg_json ->
          match parse_message msg_json with
          | Status status ->
            Eio.traceln "Massive WebSocket: Status - %s: %s" status.status status.message;
            None
          | Aggregate agg ->
            let timestamp = Ptime.of_float_s (Float.of_int agg.s /. 1000.0) in
            let timestamp_str = match timestamp with
              | Some t -> Ptime.to_rfc3339 t
              | None -> "invalid"
            in
            Eio.traceln "Massive WebSocket: Received aggregate for %s at %s" agg.sym timestamp_str;
            Some agg
          | Unknown ev ->
            Eio.traceln "Massive WebSocket: Unknown event type: %s" ev;
            None
        ) messages in

        Ok aggregates
      with
      | Yojson.Json_error e ->
        Eio.traceln "Massive WebSocket: JSON parse error: %s" e;
        Eio.traceln "Massive WebSocket: Failed payload: %s" frame.payload;
        Error (`JsonError e)
      | e ->
        Eio.traceln "Massive WebSocket: Parse error: %s" (Printexc.to_string e);
        Eio.traceln "Massive WebSocket: Failed payload: %s" frame.payload;
        Error (`ParseError (Printexc.to_string e)))
    | Ping ->
      (* Respond to ping with pong *)
      let pong_frame = Websocket.Frame.{
        fin = true;
        opcode = Pong;
        mask = true;
        payload = frame.payload;
      } in
      let encoded = Websocket.Frame.encode pong_frame in
      Eio.Flow.copy_string encoded client.conn.flow;
      Error `Ping (* Signal caller to try again *)
    | Close ->
      Eio.traceln "Massive WebSocket: Received CLOSE frame from server";
      Error `ConnectionClosed
    | _ ->
      Eio.traceln "Massive WebSocket: Unexpected frame type: %s"
        (Websocket.Opcode.show frame.opcode);
      Error (`UnexpectedFrame (Websocket.Opcode.show frame.opcode))

  (* Check if float is valid (not NaN or infinite) *)
  let is_valid_float f =
    match classify_float f with
    | FP_normal | FP_subnormal | FP_zero -> true
    | FP_infinite | FP_nan -> false

  (* Update bars with received data *)
  let update_bars bars tick aggregates =
    let ( let* ) = Result.( let* ) in

    List.fold_left
      (fun acc agg ->
        let* () = acc in

        (* Parse instrument *)
        let* instrument = Instrument.of_string_res agg.sym in

        (* Parse timestamp - use start timestamp *)
        let timestamp_s = Float.of_int agg.s /. 1000.0 in
        let* timestamp =
          match Ptime.of_float_s timestamp_s with
          | Some t -> Ok t
          | None -> Error (`JsonError "Invalid timestamp from Massive")
        in

        (* Validate all price fields before updating *)
        if not (is_valid_float agg.o && is_valid_float agg.h &&
                is_valid_float agg.l && is_valid_float agg.c) then begin
          Eio.traceln "Massive WebSocket: Skipping invalid data for %s (NaN/Inf values detected)"
            agg.sym;
          Ok ()  (* Skip this update but don't fail *)
        end else begin
          (* Get or create data array for this instrument *)
          let* data_array = Bars.get bars instrument in

          (* Update the bar data *)
          Data.set data_array Data.Type.Time tick (Ptime.to_float_s timestamp);
          Data.set data_array Data.Type.Open tick agg.o;
          Data.set data_array Data.Type.High tick agg.h;
          Data.set data_array Data.Type.Low tick agg.l;
          Data.set data_array Data.Type.Close tick agg.c;
          Data.set data_array Data.Type.Last tick agg.c;
          Data.set data_array Data.Type.Volume tick (Float.of_int agg.v);

          Ok ()
        end)
      (Ok ()) aggregates

  (* Main update loop - receives updates and applies to bars *)
  let latest client bars _tickers tick =
    let ( let* ) = Result.( let* ) in

    (* Receive updates in a loop until we get valid data *)
    let rec receive_loop retry_count =
      match receive_update client with
      | Ok [] ->
        (* Empty update, try again *)
        if retry_count > 0 && retry_count mod 100 = 0 then
          Eio.traceln "Massive WebSocket: Still waiting for data (tried %d times)" retry_count;
        receive_loop (retry_count + 1)
      | Ok aggregates -> (* Non-empty list *)
        Ok aggregates
      | Error `Ping ->
        (* Handled ping/pong, try again *)
        receive_loop retry_count
      | Error `ConnectionClosed ->
        Eio.traceln "Massive WebSocket: Connection closed by server";
        Error `ConnectionClosed
      | Error (`JsonError s) ->
        Eio.traceln "Massive WebSocket: JSON error: %s" s;
        Error (`JsonError s)
      | Error (`ParseError s) ->
        Eio.traceln "Massive WebSocket: Parse error: %s" s;
        Error (`ParseError s)
      | Error (`UnexpectedFrame s) ->
        Eio.traceln "Massive WebSocket: Unexpected frame: %s" s;
        Error (`UnexpectedFrame s)
      | Error (`ReadError s) ->
        Eio.traceln "Massive WebSocket: Read error: %s" s;
        Error (`ReadError s)
      | Error (`InvalidOpcode i) ->
        Eio.traceln "Massive WebSocket: Invalid opcode: %d" i;
        Error (`InvalidOpcode i)
    in

    let* aggregates = receive_loop 0 in
    (* Convert Error.t to generic error for compatibility *)
    match update_bars bars tick aggregates with
    | Ok () -> Ok ()
    | Error e ->
      Eio.traceln "Massive WebSocket: Error updating bars: %s" (Error.show e);
      Error (`MissingData (Error.show e))

  (* Close connection *)
  let close client =
    Eio.traceln "Massive WebSocket: Closing connection (reconnect attempts: %d)"
      client.reconnect_attempts;
    Websocket.Connection.close client.conn

  (* Reconnect and resubscribe *)
  let reconnect ~sw ~env ~use_delayed client =
    let ( let* ) = Result.( let* ) in

    Eio.traceln "Massive WebSocket: Attempting to reconnect...";
    let* new_client = connect ~sw ~env ~massive_key:client.massive_key ~use_delayed () in

    (* Update reconnect count *)
    new_client.reconnect_attempts <- client.reconnect_attempts + 1;

    (* Resubscribe to previous tickers if any *)
    if List.length client.subscribed_tickers > 0 then begin
      Eio.traceln "Massive WebSocket: Resubscribing to %d tickers after reconnect"
        (List.length client.subscribed_tickers);
      let* () = subscribe new_client client.subscribed_tickers in
      Ok new_client
    end else
      Ok new_client

  (* Background fiber that continuously updates bars *)
  let start_background_updates ~sw ~env ~use_delayed client bars get_current_tick =
    Eio.traceln "Massive WebSocket: Starting background update fiber";

    Eio.Fiber.fork ~sw (fun () ->
      Eio.traceln "Massive WS background: Fiber started, entering update loop";
      let rec update_loop client_ref =
        Eio.traceln "Massive WS background: Calling receive_update...";
        match receive_update !client_ref with
        | Ok [] ->
          (* Empty update, try again *)
          Eio.traceln "Massive WS background: Received empty update, retrying";
          update_loop client_ref
        | Ok aggregates ->
          (* Get current tick from the strategy *)
          let current_tick = get_current_tick () in
          Eio.traceln "Massive WS: Writing %d symbols to tick %d" (List.length aggregates) current_tick;

          (* Update bars with new data *)
          (match update_bars bars current_tick aggregates with
           | Ok () -> ()
           | Error e ->
             Eio.traceln "Massive WebSocket: Error updating bars: %s" (Error.show e));

          (* Explicit yield to prevent starving other fibers during message floods *)
          Eio.Fiber.yield ();

          (* Continue loop *)
          update_loop client_ref
        | Error `Ping ->
          (* Ping handled, continue *)
          update_loop client_ref
        | Error `ConnectionClosed ->
          Eio.traceln "Massive WebSocket: Connection closed, reconnecting...";
          (match reconnect ~sw ~env ~use_delayed !client_ref with
           | Ok new_client ->
             client_ref := new_client;
             Eio.traceln "Massive WebSocket: Reconnected successfully";
             update_loop client_ref
           | Error e ->
             Eio.traceln "Massive WebSocket: Reconnection failed: %s"
               (match e with
                | `ConnectionClosed -> "Connection closed"
                | `InvalidScheme s -> "Invalid scheme: " ^ s
                | `InvalidUrl s -> "Invalid URL: " ^ s
                | `DnsError s -> "DNS error: " ^ s
                | `TlsError s -> "TLS error: " ^ s
                | `HandshakeError s -> "Handshake error: " ^ s
                | `WriteError s -> "Write error: " ^ s
                | `InvalidOpcode i -> "Invalid opcode: " ^ string_of_int i
                | `ReadError s -> "Read error: " ^ s);
             (* Wait before retrying *)
             Eio.Time.sleep (Eio.Stdenv.clock env) 5.0;
             update_loop client_ref)
        | Error (`JsonError s) ->
          Eio.traceln "Massive WebSocket: JSON error: %s" s;
          update_loop client_ref
        | Error (`ParseError s) ->
          Eio.traceln "Massive WebSocket: Parse error: %s" s;
          update_loop client_ref
        | Error (`ReadError s) ->
          Eio.traceln "Massive WebSocket: Read error: %s" s;
          update_loop client_ref
        | Error (`UnexpectedFrame s) ->
          Eio.traceln "Massive WebSocket: Unexpected frame: %s" s;
          update_loop client_ref
        | Error (`InvalidOpcode i) ->
          Eio.traceln "Massive WebSocket: Invalid opcode: %d" i;
          update_loop client_ref
      in

      let client_ref = ref client in
      update_loop client_ref
    )
end

(* Module compatible with Massive_api interface *)
module type CONFIG = sig
  val client : Client.t
end

module Make (Config : CONFIG) = struct
  let latest bars tickers tick =
    Client.latest Config.client bars tickers tick
end
