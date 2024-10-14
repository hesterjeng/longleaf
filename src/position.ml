module Side = struct
  type t = Long | Short [@@deriving show]

  let t_of_yojson (x : Yojson.Safe.t) =
    match x with
    | `String "long" -> Long
    | `String "short" -> Short
    | _ -> invalid_arg "unknown side in Position.Side.t_of_yojson"

  let yojson_of_t (x : t) : Yojson.Safe.t =
    match x with Long -> `String "long" | Short -> `String "short"
end

type raw = {
  asset_id : string;
  symbol : string;
  exchange : string;
  asset_class : string;
  avg_entry_price : float;
  qty : int;
  side : Side.t;
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

type alpaca_position_response = raw list [@@deriving show, yojson]

type t = {
  symbol : string;
  qty : int;
  side : Side.t;
  current_price : float;
  avg_entry_price : float;
}
[@@deriving show, yojson]

let t_of_raw (x : raw) : t =
  let symbol = x.symbol in
  let qty = x.qty in
  let side = x.side in
  let current_price = x.current_price in
  let avg_entry_price = x.avg_entry_price in
  { symbol; qty; side; current_price; avg_entry_price }
