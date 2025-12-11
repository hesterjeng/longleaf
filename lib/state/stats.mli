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

module TradeStats : sig
  type t = {
    num_trades : int;
    num_winners : int;
    num_losers : int;
    num_breakeven : int;
    win_rate : float;
    loss_rate : float;
    avg_return : float;
    median_return : float;
    avg_winner : float;
    avg_loser : float;
    profit_factor : float;
    expectancy : float;
    std_dev : float;
    sharpe : float;
    max_winner : float;
    max_loser : float;
    total_profit : float;
    total_loss : float;
    t_statistic : float;
    p_value : float option;
  }
  [@@deriving show, yojson]

  val compute : Order.t list -> t option
  val to_string : t -> string

  val has_edge :
    ?min_trades:int -> ?min_win_rate:float -> ?max_p_value:float -> t -> bool
end
