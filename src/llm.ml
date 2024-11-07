module Headers = Piaf.Headers

let create_request_body prompt =
  `Assoc
    [
      ("model", `String "gpt-4");
      ( "messages",
        `List
          [ `Assoc [ ("role", `String "user"); ("content", `String prompt) ] ]
      );
    ]
  |> Yojson.Safe.to_string |> Piaf.Body.of_string

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
  | Ok { body; _ } -> (
      Yojson.Safe.from_string
      @@
      match Piaf.Body.to_string body with
      | Ok body_str -> body_str
      | Error e -> invalid_arg @@ Piaf.Error.to_string e)
  | Error e -> invalid_arg @@ Piaf.Error.to_string e
