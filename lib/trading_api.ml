open Trading_types
module Headers = Piaf.Headers
module Response = Piaf.Response
module Status = Piaf.Status

(* Create the headers based on the current environment *)

module Make (Alpaca : Util.CLIENT) = struct
  let client = Alpaca.client
  let longleaf_env = Alpaca.longleaf_env
  let get = Util.get_piaf ~client
  let delete = Util.delete_piaf ~client
  let post = Util.post_piaf ~client

  let headers =
    Headers.of_list
      [
        ("APCA-API-KEY-ID", longleaf_env.apca_api_key_id);
        ("APCA-API-SECRET-KEY", longleaf_env.apca_api_secret_key);
      ]

  module Clock = struct
    type t = {
      is_open : bool;
      timestamp : Time.t;
      next_open : Time.t;
      next_close : Time.t;
    }
    [@@deriving show, yojson]

    let get () =
      let endpoint = "/v2/clock" in
      Result.map t_of_yojson @@ get ~headers ~endpoint
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
      (* margin_enabled : bool; *)
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
        (* margin_enabled = true; *)
        status = "Default account";
      }

    let t_of_yojson x =
      try t_of_yojson x
      with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, _j) ->
        Eio.traceln "@[%a]@." Yojson.Safe.pp x;
        let err = Printexc.to_string e in
        invalid_arg @@ Format.asprintf "[account error] %s" err

    let get_account () =
      let endpoint = "/v2/account" in
      Result.map t_of_yojson @@ get ~headers ~endpoint
  end

  module Assets = struct
    type asset = { id : string }
    [@@deriving show, yojson] [@@yojson.allow_extra_fields]

    type t = asset list [@@deriving show, yojson]

    let get_assets () =
      let endpoint = "/v2/assets" in
      get ~headers ~endpoint
  end

  module Positions = struct
    let get_all_open_positions () =
      let endpoint = "/v2/positions" in
      get ~headers ~endpoint

    let close_all_positions (cancel_orders : bool) =
      let cancel_orders = if cancel_orders then "true" else "false" in
      let endpoint =
        Uri.of_string "/v2/positions" |> fun u ->
        Uri.add_query_param' u ("cancel_orders", cancel_orders) |> Uri.to_string
      in
      delete ~headers ~endpoint
  end

  module Orders = struct
    type response = { id : string; status : Trading_types.Status.t }
    [@@deriving show { with_path = false }, yojson]
    [@@yojson.allow_extra_fields]

    let create_market_order (order : Order.t) =
      let ( let+ ) = Result.( let+ ) in
      let endpoint = "/v2/orders" in
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
      let response = post ~headers ~body ~endpoint in
      match Response.status response with
      | `OK -> (
          Response.body response |> Piaf.Body.to_string |> function
          | Ok x ->
              let json = Yojson.Safe.from_string x in
              Eio.traceln "@[response from create_market_order:@[%a@]@.@]@."
                Yojson.Safe.pp json;
              let response_t = response_of_yojson json in
              Pmutex.set order.id response_t.id;
              Pmutex.set order.status response_t.status;
              Ok ()
          | Error e ->
              Eio.traceln
                "@[Error when converting create_market_order reponse body to \
                 string: %a@]@."
                Piaf.Error.pp_hum e;
              Result.fail
              @@ `FatalError
                   "Error converting create_market_order response body to \
                    string")
      | s ->
          Eio.traceln "@[[error] Status %a in create_market_order@]@."
            Status.pp_hum s;
          Eio.traceln "@[Order: %a@]@." Order.pp order;
          let _ =
            let+ account = Accounts.get_account () in
            Eio.traceln "@[Account: %a@]@." Accounts.pp account;
            let+ body = Response.body response |> Piaf.Body.to_string in
            Eio.traceln "@[Body: %s@]@." body
          in
          Eio.traceln "@[Response: %a@]@." Response.pp_hum response;
          Result.fail @@ `FatalError "Bad response in create_market_order"

    let get_all_orders () =
      let endpoint = "/v2/orders" in
      get ~headers ~endpoint

    let delete_all_orders () =
      let endpoint = "/v2/orders" in
      delete ~headers ~endpoint

    let get_order_by_id (id : OrderId.t) =
      let endpoint = Format.asprintf "/v2/orders/%s" (OrderId.to_string id) in
      get ~headers ~endpoint

    let delete_order_by_id (id : OrderId.t) =
      let endpoint = Format.asprintf "/v2/orders/%s" (OrderId.to_string id) in
      delete ~headers ~endpoint
  end
end
