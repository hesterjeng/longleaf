module Type = struct
  type t = Put [@name "put"] | Call [@name "call"] [@@deriving show, yojson]
end

module Style = struct
  type t = European [@name "european"] | American [@name "american"]
  [@@deriving show, yojson]
end

module Status = struct
  type t = Active [@name "active"] | Inactive [@name "inactive"]
  [@@deriving show, yojson]
end

module Deliverable = struct
  module Type = struct
    type t = Equity [@name "equity"] | Cash [@name "cash"]
    [@@deriving show, yojson]
  end

  module Settlement_type = struct
    type t =
      | T0 [@name "T+0"]
      | T1 [@name "T+1"]
      | T2 [@name "T+2"]
      | T3 [@name "T+3"]
      | T4 [@name "T+4"]
      | T5 [@name "T+5"]
    [@@deriving show, yojson]
  end

  module Settlement_method = struct
    type t = BTOB | CADF | CAFX | CCC [@@deriving show, yojson]
  end

  type t = {
    ty : Type.t; [@key "type"]
    symbol : string;
    asset_id : string;
    amount : float;
    allocation_percentage : float;
    settlement_type : Settlement_type.t;
    settlement_method : Settlement_method.t;
  }
  [@@deriving show, yojson] [@@yojson.allow_extra_fields]
end

module Request = struct
  type t = {
    underlying_symbols : string;
    (* show_deliverables : bool; *)
    expiration_date : Time.t option; [@yojson.option]
    expiration_date_gte : Time.t option; [@yojson.option]
    expiration_date_lte : Time.t option; [@yojson.option]
        (* default next weekend *)
    ty : Type.t; [@key "type"]
    style : Style.t;
    strike_price_gte : float option; [@yojson.option]
    strike_price_lte : float option; [@yojson.option]
    page_token : string option; [@yojson.option]
  }
  [@@deriving show, yojson]

  let to_query_params (x : t) =
    [
      Some ("underlying_symbols", x.underlying_symbols);
      (match x.expiration_date with
      | Some x -> Some ("expiration_date", Time.to_string x)
      | None -> None);
      (match x.expiration_date_gte with
      | Some x -> Some ("expiration_date_gte", Time.to_string x)
      | None -> None);
      (match x.expiration_date_lte with
      | Some x -> Some ("expiration_date_lte", Time.to_string x)
      | None -> None);
      (match x.strike_price_gte with
      | Some x -> Some ("strike_price_gte", Float.to_string x)
      | None -> None);
      (match x.strike_price_lte with
      | Some x -> Some ("strike_price_lte", Float.to_string x)
      | None -> None);
      (match x.page_token with
      | Some x -> Some ("page_token", x)
      | None -> None);
      Some ("type", Type.show x.ty);
      Some ("style", Style.show x.style);
    ]
    |> List.filter_map Fun.id
end

module Response = struct
  (* Response item from https://paper-api.alpaca.markets/v2/options/contracts endpoint*)
  (* The actual response is an array of these *)

  type t = {
    id : string;
    symbol : string;
    name : string;
    underlying_symbol : string;
    ty : Type.t; [@key "type"]
    status : Status.t;
    tradable : bool;
    expiration_date : string;
    underlying_asset_id : string;
    strike_price : float;
    multiplier : float;
    size : float;
    deliverables : Deliverable.t list;
  }
  [@@deriving show, yojson] [@@yojson.allow_extra_fields]

  type response = {
    option_contracts : t list;
    next_page_token : string option; [@yojson.option]
  }
  [@@deriving show, yojson]

  let t_of_yojson_res x =
    try Result.return @@ t_of_yojson x
    with _ ->
      let msg =
        Format.asprintf
          "[error] Unable to construct Contract.Response.t from @[%a@]@."
          Yojson.Safe.pp x
      in
      Error.json msg

  let response_of_yojson_res x =
    try Result.return @@ response_of_yojson x
    with _ ->
      let msg =
        Format.asprintf
          "[error] Unable to construct Contract.Response.response from @[%a@]@."
          Yojson.Safe.pp x
      in
      Error.json msg

  (* Get all of the contracts available corresponding to the request. *)
  (*  The important thing is the symbol of the contract you want. *)
  (*   You can then buy/sell this option normally, like other securities.  *)
  (* i/e using a function of type Backend_intf.place_order *)
  let rec top (longleaf_env : Environment.t) client (request : Request.t) =
    let ( let* ) = Result.( let* ) in
    let headers =
      Piaf.Headers.of_list
        [
          ("APCA-API-KEY-ID", longleaf_env.apca_api_key_id);
          ("APCA-API-SECRET-KEY", longleaf_env.apca_api_secret_key);
        ]
    in
    let endpoint =
      Uri.of_string "/v2/positions" |> fun u ->
      Uri.add_query_params' u (Request.to_query_params request) |> Uri.to_string
    in
    let* res = Util.get_piaf ~client ~headers ~endpoint in
    let* response = response_of_yojson_res res in
    let* next =
      match response.next_page_token with
      | None -> Result.return []
      | Some page_token ->
          let next_request = { request with page_token = Some page_token } in
          let* res = top longleaf_env client next_request in
          Result.return res
    in
    Result.return @@ response.option_contracts @ next
end

module Position = struct
  module Single = struct
    type 'a t = { qty : int; content : 'a } [@@deriving show]
  end

  type t = Response.t Single.t list [@@deriving show]
end
