open Eio.Std
open Bin_prot.Std

let socket_path = "/tmp/longleaf_daemon.sock"

type strategy_request = {
  strategy_name : string;
} [@@deriving bin_io]

type server_response = {
  message : string;
} [@@deriving bin_io]

let top env sw =
  let net = Eio.Stdenv.net env in
  let listening = Eio.Net.datagram_socket ~sw net (`Unix socket_path) in
  ()

let connection_handler =
  let open
(* let port = Eio.Net.listen *)
