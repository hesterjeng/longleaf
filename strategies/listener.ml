(** Listener strategy that never trades - only collects market data. This
    strategy uses the GADT system to ensure type safety while never generating
    buy or sell signals. *)

open Gadt

(** A data collection listener strategy that never buys or sells *)
let listener_strategy =
  {
    name = "Listener";
    buy_trigger = Bool false;
    (* Never buy *)
    sell_trigger = Bool false;
    (* Never sell *)
    max_positions = 0;
    (* No positions allowed *)
    position_size = 0.0;
    (* No position sizing needed *)
  }
