open Gadt_fo.Variable
open Gadt

let all_strategies : Gadt.strategy List.Ref.t = List.Ref.create ()

let register x =
  List.Ref.push all_strategies x;
  x

let stupid =
  register
  @@ {
       name = "Dumb strategy";
       buy_trigger =
         close >. Float 100.0 &&. (Gadt_fo.Constant.rsi 18 >. Float 40.0);
       sell_trigger = close <. Float 100.0;
       max_positions = 1;
       position_size = 1.0;
     }

let dual_sma_rsi =
  (* Dual SMA + RSI *)
  register
  @@ {
       name = "SMARSI";
       (* Buy when fast SMA > slow SMA and RSI < 30 (oversold) *)
       buy_trigger = sma >. tema &&. (rsi <. Float 30.0);
       (* Sell when fast SMA < slow SMA or RSI > 70 (overbought) *)
       sell_trigger = sma <. tema ||. (rsi >. Float 70.0);
       max_positions = 2;
       position_size = 0.5;
     }

let all_strategies = !all_strategies
