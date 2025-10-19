let () =
  Eio_main.run @@ fun env ->
  Eio.traceln "@[Preparing to send shutdown command.@]@.";
  let uri = Uri.of_string "http://localhost:8080/shutdown" in
  try
    let client = Cohttp_eio.Client.make (Eio.Stdenv.net env) in
    let resp, _body = Cohttp_eio.Client.get client uri in
    let status = Cohttp.Response.status resp in
    Eio.traceln "@[Response status: %s@]@."
      (Cohttp.Code.string_of_status status);
    Eio.traceln "@[Shutdown command sent.@]@.";
    exit 0
  with
  | e ->
    Eio.traceln "@[Error: %s@]@." (Printexc.to_string e);
    Eio.traceln "@[Error while sending shutdown command.@]@.";
    exit 1
