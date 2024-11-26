type mutices = {
  shutdown_mutex : bool Pmutex.t;
  data_mutex : Bars.t Pmutex.t;
  orders_mutex : Order_history.t Pmutex.t;
}

module Html = struct
  let plotly_graph_html plot_data =
    let json_data = Yojson.Safe.to_string plot_data in
    let plotly_html = Template.render json_data in
    plotly_html

  let plotly_graph_html_default () =
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
    plotly_graph_html plot_data
end

open Piaf
open Eio.Std

let prom, resolver = Promise.create ()

let connection_handler ~(mutices : mutices) (params : Request_info.t Server.ctx)
    =
  match params.request with
  | { Request.meth = `GET; target = "/"; _ } ->
      let html = Html.plotly_graph_html_default () in
      Response.of_string ~body:html `OK
  | { Request.meth = `GET; target = "/shutdown"; _ } ->
      Promise.resolve resolver true;
      Pmutex.set mutices.shutdown_mutex true;
      Response.of_string ~body:"Shutdown command sent" `OK
  | { Request.meth = `GET; target = "/orders"; _ } ->
      let orders = Pmutex.get mutices.orders_mutex in
      let body = Order_history.yojson_of_t orders |> Yojson.Safe.to_string in
      Response.of_string ~body `OK
  | { Request.meth = `GET; target = "/graphs"; _ } ->
      let bars = Pmutex.get mutices.data_mutex in
      let orders = Pmutex.get mutices.orders_mutex in
      Hashtbl.iter (fun time order -> Bars.add_order time order bars) orders;
      let body = Bars.Plotly.of_bars bars "NVDA" |> Yojson.Safe.to_string in
      Response.of_string ~body `OK
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

let start ~sw ~(mutices : mutices) env =
  let host = Eio.Net.Ipaddr.V4.loopback in
  Eio.traceln "Server listening on port 8080";
  run ~sw ~host ~port:8080 env @@ connection_handler ~mutices

(* let setup_log ?style_renderer level = *)
(*   Logs_threaded.enable (); *)
(*   Fmt_tty.setup_std_outputs ?style_renderer (); *)
(*   Logs.set_level ~all:true level; *)
(*   Logs.set_reporter (Logs_fmt.reporter ()) *)

let top ~(mutices : mutices) env =
  (* setup_log (Some Info); *)
  Switch.run (fun sw ->
      (* let openai_response = *)
      (*   Llm.Anthropic.chat ~sw ~env "What is your favorite color?" *)
      (* in *)
      (* Eio.traceln "@[OpenAI response:@]@.@[%a@]@." Yojson.Safe.pp *)
      (*   openai_response; *)
      let command = start ~mutices ~sw env in
      let _ =
        let _ = Promise.await prom in
        Ticker.OneSecond.tick env;
        Server.Command.shutdown command
      in
      ())
