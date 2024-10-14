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
  avg_entry_price : string;
  qty : string;
  side : Side.t;
  market_value : string;
  cost_basis : string;
  unrealized_pl : string;
  unrealized_plpc : string;
  unrealized_intraday_pl : string;
  unrealized_intraday_plpc : string;
  current_price : string;
  lastday_price : string;
  change_today : string;
  asset_marginable : bool;
}
[@@deriving show, yojson] [@@yojson.allow_extra_fields]

type alpaca_position_response = raw list [@@deriving show, yojson]

let alpaca_position_response_of_yojson x =
  try alpaca_position_response_of_yojson x
  with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, _) ->
    let err = Printexc.to_string e in
    invalid_arg @@ Format.asprintf "%s" err

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
  let qty =
    x.qty |> Int.of_string |> function
    | Some x -> x
    | None -> invalid_arg "expected int in position.ml"
  in
  let side = x.side in
  let current_price =
    x.current_price |> Float.of_string_opt |> function
    | Some x -> x
    | None -> invalid_arg "expected float in position.ml"
  in
  let avg_entry_price =
    x.avg_entry_price |> Float.of_string_opt |> function
    | Some x -> x
    | None -> invalid_arg "expected float in position.ml"
  in
  { symbol; qty; side; current_price; avg_entry_price }
