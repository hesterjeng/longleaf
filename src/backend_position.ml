(* A module for containing the information about the current position for backtesting *)
(* Maybe also for keeping track of things during live trading, but ideally we should *)
(* be using the true information from the server for this rather than keeping track. *)
(* TODO: *)
(* Maybe a warning if the position the brokerage thinks we have and this diverges is a good idea. *)

type t = { position : (string, int) Hashtbl.t; mutable cash : float }
[@@deriving show]

let make () = { position = Hashtbl.create 0; cash = 100000.0 }
