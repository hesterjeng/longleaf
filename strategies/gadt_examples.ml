open Gadt_fo.Constant
open Gadt

let all_strategies : Gadt.strategy List.Ref.t = List.Ref.create ()

let register x =
  List.Ref.push all_strategies x;
  x

let stupid =
  register
  @@ {
       name = "Dumb strategy";
       buy_trigger = close >. Float 100.0 &&. (rsi 18 >. Float 40.0);
       sell_trigger = close <. Float 100.0;
       max_positions = 1;
       position_size = 1.0;
     }

let all_strategies = !all_strategies
