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

let connection_handler (params : Request_info.t Server.ctx) =
  match params.request with
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
  command

let start ~sw env =
  let host = Eio.Net.Ipaddr.V4.loopback in
  run ~sw ~host ~port:8080 env connection_handler

(* let setup_log ?style_renderer level = *)
(*   Logs_threaded.enable (); *)
(*   Fmt_tty.setup_std_outputs ?style_renderer (); *)
(*   Logs.set_level ~all:true level; *)
(*   Logs.set_reporter (Logs_fmt.reporter ()) *)

let top env =
  (* setup_log (Some Info); *)
  Switch.run (fun sw ->
      let _command = start ~sw env in
      ())
