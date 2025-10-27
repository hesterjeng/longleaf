(* Tiingo WebSocket client for real-time market data *)
(* https://www.tiingo.com/documentation/websockets/iex *)

module Bars = Longleaf_bars
module Data = Bars.Data

(* Tiingo WebSocket message types *)
type subscribe_message = {
  eventName : string;
  authorization : string;
  eventData : subscribe_data;
}
[@@deriving yojson]

and subscribe_data = {
  thresholdLevel : int;
  tickers : string list;
}
[@@deriving yojson]

(* Tiingo data format *)
type tiingo_data = {
  ticker : string;
  timestamp : string;
  last : float; [@key "last"]
  open_ : float; [@key "open"]
  high : float;
  low : float;
  volume : int;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

type tiingo_message = {
  messageType : string;
  data : tiingo_data list;
}
[@@deriving yojson] [@@yojson.allow_extra_fields]

(* WebSocket client for Tiingo *)
module Client = struct
  type t = {
    conn : Websocket.Connection.t;
    tiingo_key : string;
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

  (* Connect to Tiingo WebSocket with retry logic *)
  let rec connect_with_retry ~sw ~env ~tiingo_key ~attempt () =
    let ( let* ) = Result.( let* ) in
    let url = Uri.of_string "wss://api.tiingo.com/iex" in
    let authenticator = Https.authenticator () in

    if attempt > 0 then
      Eio.traceln "Tiingo WebSocket: Connection attempt %d/%d" attempt max_reconnect_attempts;

    Eio.traceln "Tiingo WebSocket: Connecting to %s..." (Uri.to_string url);

    match Websocket.Connection.handshake ~sw ~env ~authenticator url with
    | Ok conn ->
      Eio.traceln "Tiingo WebSocket: Connected successfully";
      let now = Unix.gettimeofday () in
      Ok {
        conn;
        tiingo_key;
        subscribed_tickers = [];
        reconnect_attempts = attempt;
        last_reconnect_time = Some now;
      }
    | Error e when attempt < max_reconnect_attempts ->
      let delay = calculate_backoff_delay attempt in
      Eio.traceln "Tiingo WebSocket: Connection failed: %s"
        (match e with
         | `InvalidScheme s -> "Invalid scheme: " ^ s
         | `InvalidUrl s -> "Invalid URL: " ^ s
         | `DnsError s -> "DNS error: " ^ s
         | `TlsError s -> "TLS error: " ^ s
         | `HandshakeError s -> "Handshake error: " ^ s);
      Eio.traceln "Tiingo WebSocket: Retrying in %.1f seconds (attempt %d/%d)"
        delay attempt max_reconnect_attempts;
      Eio.Time.sleep (Eio.Stdenv.clock env) delay;
      connect_with_retry ~sw ~env ~tiingo_key ~attempt:(attempt + 1) ()
    | Error e ->
      Eio.traceln "Tiingo WebSocket: Max reconnection attempts reached, giving up";
      Error e

  (* Initial connection *)
  let connect ~sw ~env ~tiingo_key () =
    connect_with_retry ~sw ~env ~tiingo_key ~attempt:0 ()

  (* Subscribe to tickers *)
  let subscribe client tickers =
    let ( let* ) = Result.( let* ) in

    let ticker_symbols = List.map Instrument.symbol tickers in

    let sub_msg = {
      eventName = "subscribe";
      authorization = client.tiingo_key;
      eventData = {
        thresholdLevel = 5; (* Adjust based on needs *)
        tickers = ticker_symbols;
      };
    } in

    let json = yojson_of_subscribe_message sub_msg in
    let msg_str = Yojson.Safe.to_string json in

    Eio.traceln "Tiingo WebSocket: Subscribing to %d tickers: [%s]"
      (List.length tickers)
      (String.concat ", " (List.take 5 ticker_symbols @
        (if List.length ticker_symbols > 5 then ["..."] else [])));

    let* () = Websocket.Connection.send_text client.conn msg_str in

    client.subscribed_tickers <- tickers;
    Eio.traceln "Tiingo WebSocket: Subscription request sent successfully";
    Ok ()

  (* Receive and parse next message *)
  let receive_update client =
    let ( let* ) = Result.( let* ) in

    let* frame = Websocket.Connection.receive client.conn in

    match frame.Websocket.Frame.opcode with
    | Text ->
      (* Parse JSON message *)
      (try
        let json = Yojson.Safe.from_string frame.payload in

        (* Log raw JSON for debugging *)
        Eio.traceln "Tiingo WebSocket: Raw JSON received:";
        Eio.traceln "%s" (Yojson.Safe.pretty_to_string json);

        let msg = tiingo_message_of_yojson json in

        (* Log parsed message details *)
        Eio.traceln "Tiingo WebSocket: messageType=%s, data_count=%d"
          msg.messageType (List.length msg.data);

        (* Log first data item if present for debugging *)
        (match msg.data with
         | first :: _ ->
           Eio.traceln "Tiingo WebSocket: First item - ticker=%s, timestamp=%s, last=%f, open=%f, high=%f, low=%f, volume=%d"
             first.ticker first.timestamp first.last first.open_ first.high first.low first.volume
         | [] ->
           Eio.traceln "Tiingo WebSocket: Empty data array");

        Ok msg.data
      with
      | Yojson.Json_error e ->
        Eio.traceln "Tiingo WebSocket: JSON parse error: %s" e;
        Eio.traceln "Tiingo WebSocket: Failed payload: %s" frame.payload;
        Error (`JsonError e)
      | e ->
        Eio.traceln "Tiingo WebSocket: Parse error: %s" (Printexc.to_string e);
        Eio.traceln "Tiingo WebSocket: Failed payload: %s" frame.payload;
        Error (`ParseError (Printexc.to_string e)))
    | Ping ->
      (* Respond to ping with pong *)
      Eio.traceln "Tiingo WebSocket: Received PING, responding with PONG";
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
      Eio.traceln "Tiingo WebSocket: Received CLOSE frame from server";
      Error `ConnectionClosed
    | _ ->
      Eio.traceln "Tiingo WebSocket: Unexpected frame type: %s"
        (Websocket.Opcode.show frame.opcode);
      Error (`UnexpectedFrame (Websocket.Opcode.show frame.opcode))

  (* Update bars with received data *)
  let update_bars bars tick data_items =
    let ( let* ) = Result.( let* ) in

    List.fold_left
      (fun acc item ->
        let* () = acc in

        (* Parse instrument *)
        let* instrument = Instrument.of_string_res item.ticker in

        (* Parse timestamp *)
        let* timestamp =
          try Ok (Time.of_string item.timestamp)
          with e -> Error (`JsonError ("Time parse error: " ^ Printexc.to_string e))
        in

        (* Get or create data array for this instrument *)
        let* data_array = Bars.get bars instrument in

        (* Update the bar data *)
        Data.set data_array Data.Type.Time tick (Ptime.to_float_s timestamp);
        Data.set data_array Data.Type.Open tick item.open_;
        Data.set data_array Data.Type.High tick item.high;
        Data.set data_array Data.Type.Low tick item.low;
        Data.set data_array Data.Type.Close tick item.last; (* Use last as close *)
        Data.set data_array Data.Type.Last tick item.last;
        Data.set data_array Data.Type.Volume tick (Float.of_int item.volume);

        Ok ())
      (Ok ()) data_items

  (* Main update loop - receives updates and applies to bars *)
  let latest client bars _tickers tick =
    let ( let* ) = Result.( let* ) in

    (* Receive updates in a loop until we get valid data *)
    let rec receive_loop retry_count =
      match receive_update client with
      | Ok [] ->
        (* Empty update, try again *)
        if retry_count > 0 && retry_count mod 100 = 0 then
          Eio.traceln "Tiingo WebSocket: Still waiting for data (tried %d times)" retry_count;
        receive_loop (retry_count + 1)
      | Ok data_items -> (* Non-empty list *)
        Ok data_items
      | Error `Ping ->
        (* Handled ping/pong, try again *)
        receive_loop retry_count
      | Error `ConnectionClosed ->
        Eio.traceln "Tiingo WebSocket: Connection closed by server";
        Error `ConnectionClosed
      | Error (`JsonError s) ->
        Eio.traceln "Tiingo WebSocket: JSON error: %s" s;
        Error (`JsonError s)
      | Error (`ParseError s) ->
        Eio.traceln "Tiingo WebSocket: Parse error: %s" s;
        Error (`ParseError s)
      | Error (`UnexpectedFrame s) ->
        Eio.traceln "Tiingo WebSocket: Unexpected frame: %s" s;
        Error (`UnexpectedFrame s)
      | Error (`ReadError s) ->
        Eio.traceln "Tiingo WebSocket: Read error: %s" s;
        Error (`ReadError s)
      | Error (`InvalidOpcode i) ->
        Eio.traceln "Tiingo WebSocket: Invalid opcode: %d" i;
        Error (`InvalidOpcode i)
    in

    let* data_items = receive_loop 0 in
    (* Convert Error.t to generic error for compatibility *)
    match update_bars bars tick data_items with
    | Ok () ->
      Eio.traceln "Tiingo WebSocket: Successfully updated %d instruments at tick %d"
        (List.length data_items) tick;
      Ok ()
    | Error e ->
      Eio.traceln "Tiingo WebSocket: Error updating bars: %s" (Error.show e);
      Error (`MissingData (Error.show e))

  (* Close connection *)
  let close client =
    Eio.traceln "Tiingo WebSocket: Closing connection (reconnect attempts: %d)"
      client.reconnect_attempts;
    Websocket.Connection.close client.conn

  (* Reconnect and resubscribe *)
  let reconnect ~sw ~env client =
    let ( let* ) = Result.( let* ) in

    Eio.traceln "Tiingo WebSocket: Attempting to reconnect...";
    let* new_client = connect_with_retry ~sw ~env
      ~tiingo_key:client.tiingo_key
      ~attempt:(client.reconnect_attempts + 1) () in

    (* Resubscribe to previous tickers if any *)
    if List.length client.subscribed_tickers > 0 then begin
      Eio.traceln "Tiingo WebSocket: Resubscribing to %d tickers after reconnect"
        (List.length client.subscribed_tickers);
      let* () = subscribe new_client client.subscribed_tickers in
      Ok new_client
    end else
      Ok new_client

  (* Background fiber that continuously updates bars *)
  let start_background_updates ~sw ~env client bars get_current_tick =
    Eio.traceln "Tiingo WebSocket: Starting background update fiber";

    Eio.Fiber.fork ~sw (fun () ->
      let rec update_loop client_ref =
        match receive_update !client_ref with
        | Ok [] ->
          (* Empty update, try again *)
          update_loop client_ref
        | Ok data_items ->
          (* Get current tick from the strategy *)
          let current_tick = get_current_tick () in

          (* Update bars with new data *)
          (match update_bars bars current_tick data_items with
           | Ok () ->
             Eio.traceln "Tiingo WebSocket: Background fiber updated %d instruments at tick %d"
               (List.length data_items) current_tick
           | Error e ->
             Eio.traceln "Tiingo WebSocket: Background fiber error updating bars: %s"
               (Error.show e));

          (* Continue loop *)
          update_loop client_ref
        | Error `Ping ->
          (* Ping handled, continue *)
          update_loop client_ref
        | Error `ConnectionClosed ->
          Eio.traceln "Tiingo WebSocket: Background fiber detected connection closed, attempting reconnect";
          (match reconnect ~sw ~env !client_ref with
           | Ok new_client ->
             client_ref := new_client;
             Eio.traceln "Tiingo WebSocket: Background fiber reconnected successfully";
             update_loop client_ref
           | Error e ->
             Eio.traceln "Tiingo WebSocket: Background fiber reconnection failed: %s"
               (match e with
                | `ConnectionClosed -> "Connection closed"
                | `InvalidScheme s -> "Invalid scheme: " ^ s
                | `InvalidUrl s -> "Invalid URL: " ^ s
                | `DnsError s -> "DNS error: " ^ s
                | `TlsError s -> "TLS error: " ^ s
                | `HandshakeError s -> "Handshake error: " ^ s
                | `WriteError s -> "Write error: " ^ s);
             (* Wait before retrying *)
             Eio.Time.sleep (Eio.Stdenv.clock env) 5.0;
             update_loop client_ref)
        | Error (`JsonError s) ->
          Eio.traceln "Tiingo WebSocket: Background fiber JSON error: %s" s;
          update_loop client_ref
        | Error (`ParseError s) ->
          Eio.traceln "Tiingo WebSocket: Background fiber parse error: %s" s;
          update_loop client_ref
        | Error (`ReadError s) ->
          Eio.traceln "Tiingo WebSocket: Background fiber read error: %s" s;
          update_loop client_ref
        | Error (`UnexpectedFrame s) ->
          Eio.traceln "Tiingo WebSocket: Background fiber unexpected frame: %s" s;
          update_loop client_ref
        | Error (`InvalidOpcode i) ->
          Eio.traceln "Tiingo WebSocket: Background fiber invalid opcode: %d" i;
          update_loop client_ref
      in

      let client_ref = ref client in
      update_loop client_ref
    )
end

(* Module compatible with Tiingo_api interface *)
module type CONFIG = sig
  val client : Client.t
end

module Make (Config : CONFIG) = struct
  let latest bars tickers tick =
    Client.latest Config.client bars tickers tick
end
