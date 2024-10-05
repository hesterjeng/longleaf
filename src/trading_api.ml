open Cohttp
open Lwt.Syntax
module Log = (val Logs.src_log Logs.(Src.create "trading-api"))
open Trading_types

(* Create the headers based on the current environment *)
let h (env : Environment.t) =
  Header.init () |> fun h ->
  Header.add h "APCA-API-KEY-ID" env.apca_api_key_id |> fun h ->
  Header.add h "APCA-API-SECRET-KEY" env.apca_api_secret_key

module Clock = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
    let* body_json = Util.get ~headers ~uri in
    Lwt.return @@ t_of_yojson body_json
end

module Accounts = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
    initial_margin : float;
    maintenance_margin : float;
    daytrade_count : int;
    pattern_day_trader : bool;
    status : string;
  }
  [@@deriving show, yojson] [@@yojson.allow_extra_fields]

  let t_of_yojson x =
    try t_of_yojson x
    with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (e, _j) ->
      let err = Printexc.to_string e in
      invalid_arg @@ Format.asprintf "%s" err

  let get_account (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/account" in
    let headers = h env in
    let* body_json = Util.get ~headers ~uri in
    Lwt.return @@ t_of_yojson body_json
end

module Assets = struct
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives

  type asset = { id : string }
  [@@deriving show, yojson] [@@yojson.allow_extra_fields]

  type t = asset list [@@deriving show, yojson]

  let get_assets (env : Environment.t) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/assets" in
    let headers = h env in
    let* body_json = Util.get ~headers ~uri in
    Lwt.return @@ t_of_yojson body_json
end

module Positions = struct
  let close_all_positions (env : Environment.t) (cancel_orders : bool) =
    let cancel_orders = if cancel_orders then "true" else "false" in
    let uri =
      Uri.with_path env.apca_api_base_url "/v2/positions" |> fun u ->
      Uri.add_query_param' u ("cancel_orders", cancel_orders)
    in
    let headers = h env in
    let* body_json = Util.delete ~headers ~uri in
    Log.app (fun k -> k "Closed all positions");
    Log.app (fun k -> k "%a" Yojson.Safe.pp body_json);
    Lwt.return_unit
end

module Orders = struct
  let create_market_order (env : Environment.t) (symbol : string)
      (side : Side.t) (tif : TimeInForce.t) (order_type : OrderType.t)
      (qty : int) =
    let uri = Uri.with_path env.apca_api_base_url "/v2/orders" in
    let headers = h env in
    let body =
      `Assoc
        [
          ("symbol", `String symbol);
          ("type", `String (OrderType.to_string order_type));
          ("time_in_force", `String (TimeInForce.to_string tif));
          ("side", `String (Side.to_string side));
          ("qty", `String (Int.to_string qty));
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
