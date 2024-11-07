module Html = struct
  let plotly_graph_html () =
    let plot_data =
      `Assoc
        [
          ( "data",
            `List
              [
                `Assoc
                  [
                    ("x", `List [ `Float 1.0; `Float 2.0; `Float 3.0 ]);
                    ("y", `List [ `Float 4.0; `Float 5.0; `Float 6.0 ]);
                    ("mode", `String "lines+markers");
                    ("name", `String "Sample Data");
                  ];
              ] );
          ( "layout",
            `Assoc
              [
                ("title", `String "Sample Plotly Graph");
                ("xaxis", `Assoc [ ("title", `String "X Axis") ]);
                ("yaxis", `Assoc [ ("title", `String "Y Axis") ]);
              ] );
        ]
    in

    let json_data = Yojson.Basic.to_string plot_data in
    let plotly_html = Template.render json_data in
    plotly_html
end

open Piaf
open Eio.Std

let prom, resolver = Promise.create ()

let connection_handler ~set_mutex (params : Request_info.t Server.ctx) =
  match params.request with
  | { Request.meth = `GET; target = "/"; _ } ->
      let html = Html.plotly_graph_html () in
      Response.of_string ~body:html `OK
  | { Request.meth = `GET; target = "/shutdown"; _ } ->
      Promise.resolve resolver true;
      set_mutex ();
      Response.of_string ~body:"Shutdown command sent" `OK
  | { Request.meth = `GET; _ } ->
      let html = Html.plotly_graph_html () in
      Response.of_string ~body:html `OK
  | _ ->
      let headers = Headers.of_list [ ("connection", "close") ] in
      Response.of_string ~headers `Method_not_allowed ~body:""

let run ~sw ~host ~port env handler =
  let config =
    Server.Config.create ~buffer_size:0x1000 ~domains:1 (`Tcp (host, port))
  in
  let server = Server.create ~config handler in
  let command = Server.Command.start ~sw env server in
  (* Server.Command.shutdown command *)
  command

let start ~sw ~set_mutex env =
  let host = Eio.Net.Ipaddr.V4.loopback in
  Eio.traceln "Server listening on port 8080";
  run ~sw ~host ~port:8080 env @@ connection_handler ~set_mutex

(* let setup_log ?style_renderer level = *)
(*   Logs_threaded.enable (); *)
(*   Fmt_tty.setup_std_outputs ?style_renderer (); *)
(*   Logs.set_level ~all:true level; *)
(*   Logs.set_reporter (Logs_fmt.reporter ()) *)

let top ~set_mutex env =
  (* setup_log (Some Info); *)
  Switch.run (fun sw ->
      let openai_response =
        Llm.Anthropic.chat ~sw ~env "What is your favorite color?"
      in
      Eio.traceln "@[OpenAI response:@]@.@[%a@]@." Yojson.Safe.pp
        openai_response;
      let command = start ~set_mutex ~sw env in
      let _ =
        let _ = Promise.await prom in
        Ticker.OneSecond.tick env;
        Server.Command.shutdown command
      in
      ())
