type raw = {
  asset_id : string;
  symbol : string;
  exchange : string;
  asset_class : string;
  avg_entry_price : float;
  qty : int;
  side : string;
  market_value : float;
  cost_basis : float;
  unrealized_pl : float;
  unrealized_plpc : float;
  unrealized_intraday_pl : float;
  unrealized_intraday_plpc : float;
  current_price : float;
  lastday_price : float;
  change_today : float;
  asset_marginable : bool;
}
[@@deriving show, yojson]
