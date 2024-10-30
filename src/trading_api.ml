open Cohttp
module Log = (val Logs.src_log Logs.(Src.create "trading-api"))
open Trading_types

(* Create the headers based on the current environment *)
let h (env : Environment.t) =
  Header.init () |> fun h ->
  Header.add h "APCA-API-KEY-ID" env.apca_api_key_id |> fun h ->
  Header.add h "APCA-API-SECRET-KEY" env.apca_api_secret_key

module Clock = struct
  type t = {
    is_open : bool;
    timestamp : string;
    next_open : string;
    next_close : string;
  }
  [@@deriving show, yojson]

  let get (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/clock" in
    let headers = h env in
    Util.get ~headers ~uri
end

module Accounts = struct
  let float_of_yojson yojson =
    match yojson with
    | `Float v -> v
    | `Int i -> float_of_int i
    | `Intlit str | `String str -> float_of_string str
    | _ -> invalid_arg "float_of_yojson: float needed"

  type t = {
    cash : float;
    long_market_value : float;
    short_market_value : float;
    position_market_value : float;
    buying_power : float;
    initial_margin : float;
    maintenance_margin : float;
    daytrade_count : int;
    pattern_day_trader : bool;
    margin_enabled : bool;
    status : string;
  }
  [@@deriving show, yojson] [@@yojson.allow_extra_fields]

  let default_account =
    {
      cash = 100000.0;
      buying_power = 0.0;
      long_market_value = 0.0;
      short_market_value = 0.0;
      position_market_value = 0.0;
      maintenance_margin = 0.0;
      initial_margin = 0.0;
      daytrade_count = 0;
      pattern_day_trader = false;
      margin_enabled = true;
      status = "Not sure what should go here";
    }

  let t_of_yojson x =
    try t_of_yojson x
    with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, _j) ->
      let err = Printexc.to_string e in
      invalid_arg @@ Format.asprintf "%s" err

  let get_account (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/account" in
    let headers = h env in
    Util.get ~headers ~uri
end

module Assets = struct
  type asset = { id : string }
  [@@deriving show, yojson] [@@yojson.allow_extra_fields]

  type t = asset list [@@deriving show, yojson]

  let get_assets (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/assets" in
    let headers = h env in
    Util.get ~headers ~uri
end

module Positions = struct
  let get_all_open_positions (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/positions" in
    let headers = h env in
    Util.get ~headers ~uri

  let close_all_positions (env : Environment.t) (cancel_orders : bool) =
    let cancel_orders = if cancel_orders then "true" else "false" in
    let uri =
      Uri.with_path env.apca_api_base_url "/v2/positions" |> fun u ->
      Uri.add_query_param' u ("cancel_orders", cancel_orders)
    in
    let headers = h env in
    Util.delete ~headers ~uri
end

module Orders = struct
  let create_market_order (env : Environment.t) (order : Order.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
    let headers = h env in
    let body =
      `Assoc
        [
          ("symbol", `String order.symbol);
          ("type", `String (OrderType.to_string order.order_type));
          ("time_in_force", `String (TimeInForce.to_string order.tif));
          ("side", `String (Side.to_string order.side));
          ("qty", `String (Int.to_string order.qty));
        ]
      |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string
    in
    Util.post ~headers ~body ~uri

  let get_all_orders (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
    let headers = h env in
    Util.get ~headers ~uri

  let delete_all_orders (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
    let headers = h env in
    Util.delete ~headers ~uri

  let get_order_by_id (env : Environment.t) (id : OrderId.t) =
    let uri =
      Uri.with_path env.apca_api_base_url @@ "/v2/orders" |> fun u ->
      Uri.with_path u @@ OrderId.to_string id
    in
    let headers = h env in
    Util.get ~headers ~uri

  let delete_order_by_id (env : Environment.t) (id : OrderId.t) =
    let uri =
      Uri.with_path env.apca_api_base_url @@ "/v2/orders" |> fun u ->
      Uri.with_path u @@ OrderId.to_string id
    in
    let headers = h env in
    Util.delete ~headers ~uri
end
