(* WebSocket client implementation for Eio *)
(* Implements RFC 6455: The WebSocket Protocol *)

(* WebSocket frame opcodes *)
module Opcode = struct
  type t =
    | Continuation
    | Text
    | Binary
    | Close
    | Ping
    | Pong
  [@@deriving show, eq]

  let to_int = function
    | Continuation -> 0x0
    | Text -> 0x1
    | Binary -> 0x2
    | Close -> 0x8
    | Ping -> 0x9
    | Pong -> 0xA

  let of_int = function
    | 0x0 -> Ok Continuation
    | 0x1 -> Ok Text
    | 0x2 -> Ok Binary
    | 0x8 -> Ok Close
    | 0x9 -> Ok Ping
    | 0xA -> Ok Pong
    | n -> Error (`InvalidOpcode n)
end

(* WebSocket frame structure *)
module Frame = struct
  type t = {
    fin : bool;
    opcode : Opcode.t;
    mask : bool;
    payload : string;
  }
  [@@deriving show]

  (* Random state for generating masks *)
  let random_state = Random.State.make_self_init ()

  (* Generate a 4-byte random mask *)
  let generate_mask () =
    let mask = Bytes.create 4 in
    for i = 0 to 3 do
      Bytes.set mask i (Char.chr (Random.State.int random_state 256))
    done;
    Bytes.to_string mask

  (* Apply XOR mask to payload - optimized *)
  let apply_mask mask payload =
    let mask_len = String.length mask in
    let payload_len = String.length payload in
    let result = Bytes.create payload_len in
    for i = 0 to payload_len - 1 do
      let mask_byte = String.unsafe_get mask (i mod mask_len) in
      let payload_byte = String.unsafe_get payload i in
      Bytes.unsafe_set result i (Char.chr (Char.code payload_byte lxor Char.code mask_byte))
    done;
    Bytes.unsafe_to_string result

  (* Encode a frame to bytes for sending *)
  let encode frame =
    let payload_len = String.length frame.payload in
    let buf = Buffer.create (10 + payload_len) in

    (* Byte 0: FIN + RSV + Opcode *)
    let byte0 =
      (if frame.fin then 0x80 else 0x00) lor
      (Opcode.to_int frame.opcode)
    in
    Buffer.add_char buf (Char.chr byte0);

    (* Byte 1: MASK + Payload length *)
    let mask_bit = if frame.mask then 0x80 else 0x00 in
    if payload_len < 126 then begin
      Buffer.add_char buf (Char.chr (mask_bit lor payload_len))
    end else if payload_len < 65536 then begin
      Buffer.add_char buf (Char.chr (mask_bit lor 126));
      Buffer.add_char buf (Char.chr (payload_len lsr 8));
      Buffer.add_char buf (Char.chr (payload_len land 0xFF))
    end else begin
      Buffer.add_char buf (Char.chr (mask_bit lor 127));
      (* 64-bit length - we'll only use lower 32 bits *)
      for _ = 0 to 3 do Buffer.add_char buf '\x00' done;
      Buffer.add_char buf (Char.chr ((payload_len lsr 24) land 0xFF));
      Buffer.add_char buf (Char.chr ((payload_len lsr 16) land 0xFF));
      Buffer.add_char buf (Char.chr ((payload_len lsr 8) land 0xFF));
      Buffer.add_char buf (Char.chr (payload_len land 0xFF))
    end;

    (* Masking key and masked payload *)
    if frame.mask then begin
      let mask = generate_mask () in
      Buffer.add_string buf mask;
      let masked_payload = apply_mask mask frame.payload in
      Buffer.add_string buf masked_payload
    end else begin
      Buffer.add_string buf frame.payload
    end;

    Buffer.contents buf

  (* Decode a frame from bytes *)
  let decode flow =
    let ( let* ) = Result.( let* ) in

    (* Read exactly n bytes *)
    let read_exact n =
      let buf = Cstruct.create n in
      try
        Eio.Flow.read_exact flow buf;
        Ok (Cstruct.to_string buf)
      with
      | End_of_file ->
        Eio.traceln "WebSocket decode: EOF while reading";
        Error `ConnectionClosed
      | e ->
        Eio.traceln "WebSocket decode: read error: %s" (Printexc.to_string e);
        Error (`ReadError (Printexc.to_string e))
    in

    (* Read first 2 bytes *)
    let* header = read_exact 2 in
    let byte0 = Char.code (String.unsafe_get header 0) in
    let byte1 = Char.code (String.unsafe_get header 1) in

    Eio.traceln "WebSocket decode: header bytes: 0x%02X 0x%02X (opcode=%d, len=%d)"
      byte0 byte1 (byte0 land 0x0F) (byte1 land 0x7F);

    let fin = (byte0 land 0x80) <> 0 in
    let* opcode = Opcode.of_int (byte0 land 0x0F) in
    let mask = (byte1 land 0x80) <> 0 in
    let payload_len = byte1 land 0x7F in

    (* Read extended payload length if needed *)
    let* payload_len =
      if payload_len < 126 then
        Ok payload_len
      else if payload_len = 126 then
        let* len_bytes = read_exact 2 in
        Ok ((Char.code (String.unsafe_get len_bytes 0) lsl 8) lor
            (Char.code (String.unsafe_get len_bytes 1)))
      else begin
        let* len_bytes = read_exact 8 in
        (* Only use lower 32 bits *)
        let len =
          (Char.code (String.unsafe_get len_bytes 4) lsl 24) lor
          (Char.code (String.unsafe_get len_bytes 5) lsl 16) lor
          (Char.code (String.unsafe_get len_bytes 6) lsl 8) lor
          (Char.code (String.unsafe_get len_bytes 7))
        in
        Ok len
      end
    in

    (* Read mask key if present *)
    let* mask_key =
      if mask then read_exact 4
      else Ok ""
    in

    (* Read payload *)
    let* payload =
      if payload_len = 0 then Ok ""
      else read_exact payload_len
    in

    (* Unmask payload if needed *)
    let payload =
      if mask then apply_mask mask_key payload
      else payload
    in

    Ok { fin; opcode; mask = false; payload }
end

(* WebSocket connection *)
module Connection = struct
  type t = {
    flow : Eio.Flow.two_way_ty Eio.Resource.t;
    url : Uri.t;
    mutable closed : bool;
    mutable leftover : string;  (* Buffered bytes from handshake *)
  }

  (* Perform WebSocket handshake *)
  let handshake ~sw ~env ~authenticator url =
    let ( let* ) = Result.( let* ) in

    (* Parse URL *)
    let* scheme =
      match Uri.scheme url with
      | Some "ws" | Some "wss" -> Ok (Uri.scheme url |> Option.get_exn_or "ws")
      | Some s -> Error (`InvalidScheme s)
      | None -> Error (`InvalidScheme "missing")
    in

    let* hostname =
      match Uri.host url with
      | Some h -> Ok h
      | None -> Error (`InvalidUrl "missing host")
    in

    let port =
      match Uri.port url with
      | Some p -> p
      | None -> if String.equal scheme "wss" then 443 else 80
    in

    let path =
      let p = Uri.path url in
      if String.equal p "" then "/" else p
    in

    (* Connect to server using DNS resolution *)
    Eio.traceln "WebSocket: Resolving %s" hostname;
    let net = Eio.Stdenv.net env in

    let* sock_addr =
      try
        let addrs = Unix.getaddrinfo hostname (string_of_int port)
          [Unix.AI_SOCKTYPE Unix.SOCK_STREAM] in
        match addrs with
        | [] -> Error (`DnsError "No addresses found")
        | { Unix.ai_addr = Unix.ADDR_INET (addr, _); _ } :: _ ->
          (* Convert Unix.inet_addr to Eio.Net.Ipaddr *)
          (* Both are 4 or 16 byte strings internally, use Eio_unix for conversion *)
          let eio_addr = Eio_unix.Net.Ipaddr.of_unix addr in
          Ok (`Tcp (eio_addr, port))
        | { Unix.ai_addr = Unix.ADDR_UNIX _; _ } :: _ ->
          Error (`DnsError "Unix domain sockets not supported")
      with e -> Error (`DnsError (Printexc.to_string e))
    in

    Eio.traceln "WebSocket: Connecting to %a:%d" Eio.Net.Sockaddr.pp sock_addr port;
    let tcp_flow = Eio.Net.connect ~sw net sock_addr in

    (* Wrap with TLS if wss:// *)
    let* flow =
      if String.equal scheme "wss" then begin
        try
          let* tls_config =
            match Tls.Config.client ~authenticator () with
            | Error (`Msg msg) -> Error (`TlsError msg)
            | Ok cfg -> Ok cfg
          in
          let domain_name = Domain_name.(of_string_exn hostname |> host_exn) in
          let tls_flow = Tls_eio.client_of_flow ~host:domain_name tls_config tcp_flow in
          Ok (tls_flow :> Eio.Flow.two_way_ty Eio.Resource.t)
        with e -> Error (`TlsError (Printexc.to_string e))
      end else
        Ok (tcp_flow :> Eio.Flow.two_way_ty Eio.Resource.t)
    in

    (* Generate WebSocket key *)
    let ws_key =
      let random_bytes = Bytes.create 16 in
      let rng = Random.State.make_self_init () in
      for i = 0 to 15 do
        Bytes.set random_bytes i (Char.chr (Random.State.int rng 256))
      done;
      Base64.encode_string (Bytes.to_string random_bytes)
    in

    (* Send handshake request *)
    let handshake_request = Printf.sprintf
      "GET %s HTTP/1.1\r\n\
       Host: %s\r\n\
       Upgrade: websocket\r\n\
       Connection: Upgrade\r\n\
       Sec-WebSocket-Key: %s\r\n\
       Sec-WebSocket-Version: 13\r\n\
       \r\n"
      path hostname ws_key
    in

    Eio.traceln "WebSocket: Sending handshake";
    Eio.Flow.copy_string handshake_request flow;

    (* Read handshake response - buffered approach for performance *)
    (* Returns ALL data read, including any bytes after \r\n\r\n *)
    let buf = Cstruct.create 4096 in
    let rec read_until_double_crlf acc =
      let got = Eio.Flow.single_read flow buf in
      let chunk = Cstruct.(to_string (sub buf 0 got)) in
      let combined = acc ^ chunk in
      match String.index_opt combined '\n' with
      | None -> read_until_double_crlf combined
      | Some _ ->
        (* Check for \r\n\r\n *)
        let rec find_end pos =
          if pos + 3 >= String.length combined then
            read_until_double_crlf combined
          else if Char.equal (String.unsafe_get combined pos) '\r' &&
                  Char.equal (String.unsafe_get combined (pos + 1)) '\n' &&
                  Char.equal (String.unsafe_get combined (pos + 2)) '\r' &&
                  Char.equal (String.unsafe_get combined (pos + 3)) '\n' then
            combined  (* Return ALL data, not just headers *)
          else
            find_end (pos + 1)
        in
        find_end 0
    in

    let full_data = read_until_double_crlf "" in

    (* Find the end of HTTP headers *)
    let header_end =
      let rec find_end pos =
        if pos + 3 >= String.length full_data then
          String.length full_data  (* No end found, use all *)
        else if Char.equal (String.unsafe_get full_data pos) '\r' &&
                Char.equal (String.unsafe_get full_data (pos + 1)) '\n' &&
                Char.equal (String.unsafe_get full_data (pos + 2)) '\r' &&
                Char.equal (String.unsafe_get full_data (pos + 3)) '\n' then
          pos + 4
        else
          find_end (pos + 1)
      in
      find_end 0
    in

    let response = String.sub full_data 0 header_end in
    let leftover =
      if header_end < String.length full_data then
        String.sub full_data header_end (String.length full_data - header_end)
      else ""
    in

    Eio.traceln "WebSocket: Received handshake response (%d bytes, %d leftover)"
      (String.length response) (String.length leftover);
    if String.length leftover > 0 then
      Eio.traceln "WebSocket: WARNING - Found %d leftover bytes after HTTP headers!" (String.length leftover);
    Eio.traceln "WebSocket: Response: %s" (String.sub response 0 (min 200 (String.length response)));

    (* Validate response - check for 101 status *)
    let* () =
      if String.length response >= 12 &&
         String.equal (String.sub response 0 12) "HTTP/1.1 101" then
        Ok ()
      else
        Error (`HandshakeError ("Server did not return 101: " ^ String.sub response 0 (min 50 (String.length response))))
    in

    Ok { flow; url; closed = false; leftover }

  (* Send a text message *)
  let send_text conn text =
    if conn.closed then begin
      Eio.traceln "WebSocket send_text: connection is closed!";
      Error `ConnectionClosed
    end else begin
      let frame = Frame.{ fin = true; opcode = Text; mask = true; payload = text } in
      let encoded = Frame.encode frame in
      Eio.traceln "WebSocket send_text: sending %d bytes (payload: %d bytes)"
        (String.length encoded) (String.length text);
      try
        Eio.Flow.copy_string encoded conn.flow;
        Eio.traceln "WebSocket send_text: sent successfully";
        Ok ()
      with e ->
        Eio.traceln "WebSocket send_text: ERROR: %s" (Printexc.to_string e);
        Error (`WriteError (Printexc.to_string e))
    end

  (* Receive next frame - uses leftover buffer first, then reads from flow *)
  let receive conn =
    if conn.closed then
      Error `ConnectionClosed
    else begin
      (* Create a buffered decode that uses leftover bytes first *)
      let leftover_ref = ref conn.leftover in

      let read_exact_buffered n =
        let buf = Cstruct.create n in
        let rec fill_buf offset remaining =
          if remaining <= 0 then
            Ok (Cstruct.to_string buf)
          else begin
            (* First use any leftover bytes *)
            let leftover = !leftover_ref in
            if String.length leftover > 0 then begin
              let use_len = min remaining (String.length leftover) in
              for i = 0 to use_len - 1 do
                Cstruct.set_char buf (offset + i) (String.get leftover i)
              done;
              leftover_ref := String.sub leftover use_len (String.length leftover - use_len);
              fill_buf (offset + use_len) (remaining - use_len)
            end else begin
              (* Read from flow *)
              try
                let read_buf = Cstruct.create remaining in
                Eio.Flow.read_exact conn.flow read_buf;
                Cstruct.blit read_buf 0 buf offset remaining;
                Ok (Cstruct.to_string buf)
              with
              | End_of_file ->
                Eio.traceln "WebSocket decode: EOF while reading";
                Error `ConnectionClosed
              | e ->
                Eio.traceln "WebSocket decode: read error: %s" (Printexc.to_string e);
                Error (`ReadError (Printexc.to_string e))
            end
          end
        in
        fill_buf 0 n
      in

      let ( let* ) = Result.( let* ) in

      (* Read first 2 bytes *)
      let* header = read_exact_buffered 2 in
      let byte0 = Char.code (String.unsafe_get header 0) in
      let byte1 = Char.code (String.unsafe_get header 1) in

      Eio.traceln "WebSocket decode: header bytes: 0x%02X 0x%02X (opcode=%d, len=%d)"
        byte0 byte1 (byte0 land 0x0F) (byte1 land 0x7F);

      let fin = (byte0 land 0x80) <> 0 in
      let* opcode = Opcode.of_int (byte0 land 0x0F) in
      let mask = (byte1 land 0x80) <> 0 in
      let payload_len = byte1 land 0x7F in

      (* Read extended payload length if needed *)
      let* payload_len =
        if payload_len < 126 then
          Ok payload_len
        else if payload_len = 126 then
          let* len_bytes = read_exact_buffered 2 in
          Ok ((Char.code (String.unsafe_get len_bytes 0) lsl 8) lor
              (Char.code (String.unsafe_get len_bytes 1)))
        else begin
          let* len_bytes = read_exact_buffered 8 in
          (* Only use lower 32 bits *)
          let len =
            (Char.code (String.unsafe_get len_bytes 4) lsl 24) lor
            (Char.code (String.unsafe_get len_bytes 5) lsl 16) lor
            (Char.code (String.unsafe_get len_bytes 6) lsl 8) lor
            (Char.code (String.unsafe_get len_bytes 7))
          in
          Ok len
        end
      in

      (* Read mask key if present *)
      let* mask_key =
        if mask then read_exact_buffered 4
        else Ok ""
      in

      (* Read payload *)
      let* payload =
        if payload_len = 0 then Ok ""
        else read_exact_buffered payload_len
      in

      (* Unmask payload if needed *)
      let payload =
        if mask then Frame.apply_mask mask_key payload
        else payload
      in

      (* Update connection's leftover buffer *)
      conn.leftover <- !leftover_ref;

      Ok Frame.{ fin; opcode; mask = false; payload }
    end

  (* Close connection *)
  let close conn =
    if not conn.closed then begin
      conn.closed <- true;
      let close_frame = Frame.{ fin = true; opcode = Close; mask = true; payload = "" } in
      let encoded = Frame.encode close_frame in
      try
        Eio.Flow.copy_string encoded conn.flow;
        (* Eio flows are closed when the switch exits, no explicit close needed *)
        ()
      with _ -> ()
    end
end
