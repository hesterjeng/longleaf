module Headers = Piaf.Headers

module OpenAI = struct
  let create_request_body prompt =
    `Assoc
      [
        ("model", `String "gpt-3.5-turbo");
        ( "messages",
          `List
            [ `Assoc [ ("role", `String "user"); ("content", `String prompt) ] ]
        );
      ]
    |> Yojson.Safe.to_string |> Piaf.Body.of_string

  (* let handle_response (response : Yojson.Safe.t) = *)
  (*   match response with *)
  (*   | *)

  let chat ~sw ~env (content : string) =
    let openai_key = Unix.getenv "OPENAI_KEY" in
    let uri = Uri.of_string "https://api.openai.com/v1/chat/completions" in
    let headers =
      Headers.of_list
        [
          ("Content-Type", "application/json");
          ("Authorization", "Bearer " ^ openai_key);
        ]
      |> Headers.to_list
    in
    let body = create_request_body content in
    match Piaf.Client.Oneshot.post ~headers ~body ~sw env uri with
    | Ok { body; _ } ->
      Yojson.Safe.from_string
      @@
      (match Piaf.Body.to_string body with
      | Ok body_str -> body_str
      | Error e -> invalid_arg @@ Piaf.Error.to_string e)
    | Error (`Connect_error e) ->
      Eio.traceln "@[Unable to connect to host: %s@]@." e;
      invalid_arg e
    | Error e -> invalid_arg @@ Piaf.Error.to_string e
end

module Anthropic = struct
  let create_request_body prompt =
    `Assoc
      [
        ("model", `String "claude-3-5-sonnet-20241022");
        ( "messages",
          `List
            [ `Assoc [ ("role", `String "user"); ("content", `String prompt) ] ]
        );
        ("max_tokens", `Int 1024);
      ]
    |> Yojson.Safe.to_string |> Piaf.Body.of_string

  (* let handle_response (response : Yojson.Safe.t) = *)
  (*   match response with *)
  (*   | *)

  let chat ~sw ~env (content : string) =
    let openai_key = Unix.getenv "ANTHROPIC_KEY" in
    let uri = Uri.of_string "https://api.anthropic.com/v1/messages" in
    let headers =
      Headers.of_list
        [
          ("Content-Type", "application/json");
          ("x-api-key", openai_key);
          ("anthropic_version", "2023-06-01");
        ]
      |> Headers.to_list
    in
    let body = create_request_body content in
    match Piaf.Client.Oneshot.post ~headers ~body ~sw env uri with
    | Ok { body; _ } ->
      Yojson.Safe.from_string
      @@
      (match Piaf.Body.to_string body with
      | Ok body_str -> body_str
      | Error e ->
        Eio.traceln
          "@[Error while trying to convert response body to string:@]@.";
        invalid_arg @@ Piaf.Error.to_string e)
    | Error (`Connect_error e) ->
      Eio.traceln "@[Unable to connect to host: %s@]@." e;
      invalid_arg e
    | Error e -> invalid_arg @@ Piaf.Error.to_string e
end
