let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Eio.traceln "@[Preparing to send shutdown command.@]@.";
  let uri = Uri.of_string "http://localhost:8080/shutdown" in
  match Piaf.Client.Oneshot.get ~sw env uri with
  | Ok r ->
      Eio.traceln "@[%a@]@." Piaf.Response.pp_hum r;
      Eio.traceln "@[Shutdown command sent.@]@.";
      ()
  | Error e ->
      Eio.traceln "@[%a@]@." Piaf.Error.pp_hum e;
      Eio.traceln "@[Error while sending shutdown command.]@.";
      ()
