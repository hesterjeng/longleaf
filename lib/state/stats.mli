type t = {
  num_orders : int;
  num_buy_orders : int;
  num_sell_orders : int;
  total_volume : int;
  total_cash_traded : float;
  symbols_traded : int;
  profit_loss : float;
}

val make : Order.t list Vector.vector -> Longleaf_bars.t -> t
val from_positions : Positions.t -> Longleaf_bars.t -> t
