open Trading_types
module Headers = Cohttp.Header
module Response = Cohttp.Response
module Status = Cohttp.Code
module Util = Longleaf_util
module Pmutex = Longleaf_util.Pmutex

(* Base URL determination based on runtype *)
let base_url runtype =
  match runtype with
  | Runtype.Live -> "https://api.alpaca.markets"
  | Runtype.Paper -> "https://paper-api.alpaca.markets"
  | _ ->
    "https://paper-api.alpaca.markets" (* default to paper for non-live modes *)

(* Helper to construct full URL from base and path *)
let make_url base path = base ^ path

module type CONFIG = sig
  val client : Cohttp_eio.Client.t
  val longleaf_env : Environment.t
  val runtype : Runtype.t
end

module Make (Config : CONFIG) = struct
  let base = base_url Config.runtype

  let headers =
    Headers.of_list
      [
        ("APCA-API-KEY-ID", Config.longleaf_env.apca_api_key_id);
        ("APCA-API-SECRET-KEY", Config.longleaf_env.apca_api_secret_key);
      ]

  let get path =
    Tools.get_cohttp ~client:Config.client ~headers
      ~endpoint:(make_url base path)

  let delete path =
    Tools.delete_cohttp ~client:Config.client ~headers
      ~endpoint:(make_url base path)

  let post path body =
    Tools.post_cohttp ~client:Config.client ~headers
      ~endpoint:(make_url base path) ~body

  module Clock = struct
    type t = {
      is_open : bool;
      timestamp : Time.t;
      next_open : Time.t;
      next_close : Time.t;
    }
    [@@deriving show, yojson]

    let get () = Result.map t_of_yojson @@ get "/v2/clock"
  end

  module Accounts = struct
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
      account_number : string;
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
        account_number = "NONE";
        maintenance_margin = 0.0;
        initial_margin = 0.0;
        daytrade_count = 0;
        pattern_day_trader = false;
        (* margin_enabled = true; *)
        status = "Default account";
      }

    let t_of_yojson_ (x : Yojson.Safe.t) =
      let ( let* ) = Result.( let* ) in
      let module J = Longleaf_util.Json in
      let* cash = J.float_of_string_member "cash" x in
      let* long_market_value = J.float_of_string_member "long_market_value" x in
      let* short_market_value =
        J.float_of_string_member "short_market_value" x
      in
      let* position_market_value =
        J.float_of_string_member "position_market_value" x
      in
      let* account_number = J.string_member "account_number" x in
      let* buying_power = J.float_of_string_member "buying_power" x in
      let* initial_margin = J.float_of_string_member "initial_margin" x in
      let* maintenance_margin =
        J.float_of_string_member "maintenance_margin" x
      in
      let* daytrade_count = J.int_member "daytrade_count" x in
      let* pattern_day_trader = J.bool_member "pattern_day_trader" x in
      let* status = J.string_member "status" x in
      Result.return
      @@ {
           cash;
           long_market_value;
           short_market_value;
           position_market_value;
           account_number;
           buying_power;
           initial_margin;
           maintenance_margin;
           daytrade_count;
           pattern_day_trader;
           status;
         }

    let t_of_yojson x =
      match t_of_yojson_ x with
      | Ok _ as res -> res
      | Error e -> Error.json e

    let get_account () =
      let ( let* ) = Result.( let* ) in
      let* received = get "/v2/account" in
      let* res = t_of_yojson received in
      Result.return res
  end

  module Assets = struct
    type asset = { id : string }
    [@@deriving show, yojson] [@@yojson.allow_extra_fields]

    type t = asset list [@@deriving show, yojson]

    let get_assets () = get "/v2/assets"
  end

  module Positions = struct
    let get_all_open_positions () = get "/v2/positions"

    let close_all_positions (cancel_orders : bool) =
      let cancel_orders_str = if cancel_orders then "true" else "false" in
      let endpoint =
        Uri.of_string (make_url base "/v2/positions") |> fun u ->
        Uri.add_query_param' u ("cancel_orders", cancel_orders_str)
        |> Uri.to_string
      in
      Tools.delete_cohttp ~client:Config.client ~headers ~endpoint
  end

  module Orders = struct
    type response = { id : string; status : Trading_types.Status.t }
    [@@deriving show { with_path = false }, yojson]
    [@@yojson.allow_extra_fields]

    let create_market_order (order : Order.t) : (unit, _) result =
      let ( let* ) = Result.( let* ) in
      let body : Yojson.Safe.t =
        `Assoc
          [
            ("symbol", `String (Instrument.symbol order.symbol));
            ("type", `String (OrderType.to_string order.order_type));
            ("time_in_force", `String (TimeInForce.to_string order.tif));
            ("side", `String (Side.to_string order.side));
            ("qty", `String (Int.to_string order.qty));
          ]
      in
      let* response = post "/v2/orders" body in
      let* response_t =
        try Ok (response_of_yojson response) with
        | _ ->
          Error.fatal "Error converting create market order response to json"
      in
      Pmutex.set order.id response_t.id;
      Pmutex.set order.status response_t.status;
      Ok ()

    (* match Response.status response with *)
    (* | `OK -> *)
    (*   (try *)
    (*      let body_str = Cohttp_eio.Body.to_string (Response.body response) in *)
    (*      let json = Yojson.Safe.from_string body_str in *)
    (*      Eio.traceln "@[response from create_market_order:@[%a@]@.@]@." *)
    (*        Yojson.Safe.pp json; *)
    (*      let response_t = response_of_yojson json in *)
    (*      Pmutex.set order.id response_t.id; *)
    (*      Pmutex.set order.status response_t.status; *)
    (*      Ok () *)
    (*    with *)
    (*   | e -> *)
    (*     Eio.traceln *)
    (*       "@[Error when converting create_market_order response body to \ *)
      (*        string: %s@]@." *)
    (*       (Printexc.to_string e); *)
    (*     Result.fail *)
    (*     @@ `FatalError *)
    (*          "Error converting create_market_order response body to string") *)
    (* | s -> *)
    (*   Eio.traceln "@[[error] Status %s in create_market_order@]@." *)
    (*     (Status.string_of_status s); *)
    (*   Eio.traceln "@[Order: %a@]@." Order.pp order; *)
    (*   let _ = *)
    (*     let+ account = Accounts.get_account () in *)
    (*     Eio.traceln "@[Account: %a@]@." Accounts.pp account; *)
    (*     try *)
    (*       let body_str = Cohttp_eio.Body.to_string (Response.body response) in *)
    (*       Eio.traceln "@[Body: %s@]@." body_str; *)
    (*       Ok () *)
    (*     with *)
    (*     | e -> *)
    (*       Eio.traceln "@[Error reading body: %s@]@." (Printexc.to_string e); *)
    (*       Ok () *)
    (*   in *)
    (*   Result.fail @@ `FatalError "Bad response in create_market_order" *)

    let get_all_orders () = get "/v2/orders"
    let delete_all_orders () = delete "/v2/orders"

    let get_order_by_id (id : OrderId.t) =
      get (Format.asprintf "/v2/orders/%s" (OrderId.to_string id))

    let delete_order_by_id (id : OrderId.t) =
      delete (Format.asprintf "/v2/orders/%s" (OrderId.to_string id))
  end
end
