module Log = (val Logs.src_log Logs.(Src.create "trading-api"))
open Trading_types
module Headers = Piaf.Headers

(* Create the headers based on the current environment *)

module Make (Alpaca : Util.ALPACA_SERVER) = struct
  let client = Alpaca.client
  let longleaf_env = Alpaca.longleaf_env
  let get = Util.get_piaf ~client
  let delete = Util.delete_piaf ~client
  let post = Util.post_piaf ~client

  let headers () =
    Headers.of_list
      [
        ("APCA-API-KEY-ID", longleaf_env.apca_api_key_id);
        ("APCA-API-SECRET-KEY", longleaf_env.apca_api_secret_key);
      ]

  module Clock = struct
    type t = {
      is_open : bool;
      timestamp : string;
      next_open : string;
      next_close : string;
    }
    [@@deriving show, yojson]

    let get () =
      let endpoint = "/v2/clock" in
      let headers = headers () in
      get ~headers ~endpoint
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

    let get_account () =
      let endpoint = "/v2/account" in
      let headers = headers () in
      get ~headers ~endpoint
  end

  module Assets = struct
    type asset = { id : string }
    [@@deriving show, yojson] [@@yojson.allow_extra_fields]

    type t = asset list [@@deriving show, yojson]

    let get_assets () =
      let endpoint = "/v2/assets" in
      let headers = headers () in
      get ~headers ~endpoint
  end

  module Positions = struct
    let get_all_open_positions () =
      let endpoint = "/v2/positions" in
      let headers = headers () in
      get ~headers ~endpoint

    let close_all_positions () (cancel_orders : bool) =
      let cancel_orders = if cancel_orders then "true" else "false" in
      let endpoint = "/v2/positions" in
      let headers =
        headers () |> fun h -> Headers.add h "cancel_orders" cancel_orders
      in
      delete ~headers ~endpoint
  end

  module Orders = struct
    let create_market_order () (order : Order.t) =
      let endpoint = "/v2/orders" in
      let headers = headers () in
      let body =
        `Assoc
          [
            ("symbol", `String order.symbol);
            ("type", `String (OrderType.to_string order.order_type));
            ("time_in_force", `String (TimeInForce.to_string order.tif));
            ("side", `String (Side.to_string order.side));
            ("qty", `String (Int.to_string order.qty));
          ]
      in
      post ~headers ~body ~endpoint

    let get_all_orders () =
      let endpoint = "/v2/orders" in
      let headers = headers () in
      get ~headers ~endpoint

    let delete_all_orders () =
      let endpoint = "/v2/orders" in
      let headers = headers () in
      delete ~headers ~endpoint

    let get_order_by_id () (id : OrderId.t) =
      let endpoint = Format.asprintf "/v2/orders/%s" (OrderId.to_string id) in
      let headers = headers () in
      get ~headers ~endpoint

    let delete_order_by_id () (id : OrderId.t) =
      let endpoint = Format.asprintf "/v2/orders/%s" (OrderId.to_string id) in
      let headers = headers () in
      delete ~headers ~endpoint
  end
end
