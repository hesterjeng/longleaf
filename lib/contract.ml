module Type = struct
  type t = Put [@name "put"] | Call [@name "call"] | Forward
  [@@deriving show, yojson]

  let of_string = function
    | "P"
    | "put"
    | "Put" ->
      Put
    | "C"
    | "Call"
    | "call" ->
      Call
    | x -> invalid_arg (Format.asprintf "Not P or C for call: %s" x)
end

module Style = struct
  type t = European [@name "european"] | American [@name "american"]
  [@@deriving show, yojson]
end

module Status = struct
  type t = Active [@name "active"] | Inactive [@name "inactive"]
  [@@deriving show, yojson]

  let active = Active
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

let compare _ _ = 0
let equal (x : t) (y : t) = String.equal x.symbol y.symbol

type response = {
  option_contracts : t list;
  next_page_token : string option; [@yojson.option]
}
[@@deriving show, yojson]

let t_of_yojson_res x =
  try Result.return @@ t_of_yojson x with
  | _ ->
    let msg =
      Format.asprintf
        "[error] Unable to construct Contract.Response.t from @[%a@]@."
        Yojson.Safe.pp x
    in
    Error.json msg

let response_of_yojson_res x =
  try Result.return @@ response_of_yojson x with
  | _ ->
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
let rec get_all (longleaf_env : Util.Environment.t) client (request : Request.t)
    =
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
      let* res = get_all longleaf_env client next_request in
      Result.return res
  in
  Result.return @@ response.option_contracts @ next

module Chain = struct
  module Internal = struct
    let generate_occ_symbol ~underlying ~expiry ~option_type ~strike =
      let expiry_str =
        let y, m, d = Ptime.to_date expiry in
        Format.asprintf "%02d%02d%02d" (y mod 100) m d
      in
      let type_char =
        match option_type with
        | Type.Call -> "C"
        | Type.Put -> "P"
        | Type.Forward -> "F"
      in
      let strike_str =
        let whole_part = int_of_float strike in
        let frac_part =
          int_of_float ((strike -. float_of_int whole_part) *. 1000.0)
        in
        Format.asprintf "%05d%03d" whole_part frac_part
      in
      Format.asprintf "%s%s%s%s" underlying expiry_str type_char strike_str

    let strike_interval current_price =
      if current_price <=. 25.0 then 2.5
      else if current_price <=. 200.0 then 5.0
      else 10.0

    let generate_strikes ~current_price ~range_pct =
      let interval = strike_interval current_price in
      let min_strike = current_price *. (1.0 -. range_pct) in
      let max_strike = current_price *. (1.0 +. range_pct) in

      let round_to_interval price =
        Float.round (price /. interval) *. interval
      in

      let rec build_strikes acc strike =
        if strike >. max_strike then List.rev acc
        else build_strikes (strike :: acc) (strike +. interval)
      in
      build_strikes [] (round_to_interval min_strike)

    let next_third_friday year month =
      let first_day =
        Ptime.of_date (year, month, 1)
        |> Option.get_exn_or
             "Unable to get first_day in Contract.next_third_friday"
      in
      let first_weekday = Ptime.weekday first_day in
      let days_to_friday =
        match first_weekday with
        | `Mon -> 4
        | `Tue -> 3
        | `Wed -> 2
        | `Thu -> 1
        | `Fri -> 0
        | `Sat -> 6
        | `Sun -> 5
      in
      let first_friday = 1 + days_to_friday in
      let third_friday = first_friday + 14 in
      Ptime.of_date (year, month, third_friday)
      |> Option.get_exn_or
           "next_third_friday: Unbale to convert YMD to Ptime in contract.ml"

    let generate_monthly_expirations ~current_date ~num_months =
      let year, month, _ = Ptime.to_date current_date in
      let rec generate_months acc remaining_months current_year current_month =
        if remaining_months <= 0 then (
          let result = List.rev acc in
          List.iter
            (fun expiry -> assert (Ptime.compare expiry current_date > 0))
            result;
          result)
        else
          let adj_month = if current_month > 12 then 1 else current_month in
          let adj_year =
            if current_month > 12 then current_year + 1 else current_year
          in
          let expiry = next_third_friday adj_year adj_month in
          assert (
            Ptime.compare expiry current_date > 0
            || remaining_months < num_months);
          if Ptime.compare expiry current_date > 0 then
            generate_months (expiry :: acc) (remaining_months - 1) adj_year
              (adj_month + 1)
          else generate_months acc remaining_months adj_year (adj_month + 1)
      in
      generate_months [] num_months year month

    let generate_weekly_expirations ~current_date ~num_weeks =
      let rec generate_weeks acc remaining_weeks current_date =
        if remaining_weeks <= 0 then (
          let result = List.rev acc in
          List.iter
            (fun expiry -> assert (Ptime.compare expiry current_date > 0))
            result;
          result)
        else
          let next_friday =
            let weekday = Ptime.weekday current_date in
            let days_to_friday =
              match weekday with
              | `Mon -> 4
              | `Tue -> 3
              | `Wed -> 2
              | `Thu -> 1
              | `Fri -> 0
              | `Sat -> 6
              | `Sun -> 5
            in
            let days_to_add =
              if days_to_friday = 0 then 7 else days_to_friday
            in
            Ptime.add_span current_date
              (Ptime.Span.of_int_s (days_to_add * 86400))
            |> Option.get_exn_or "contract.ml:  Error while generating span"
          in
          assert (Ptime.compare next_friday current_date > 0);
          generate_weeks (next_friday :: acc) (remaining_weeks - 1)
            (Ptime.add_span next_friday (Ptime.Span.of_int_s (7 * 86400))
            |> Option.get_exn_or
                 "contract.ml: Error while generating span after friday")
      in
      generate_weeks [] num_weeks current_date

    let create_contract ~underlying ~expiry ~option_type ~strike =
      let symbol =
        generate_occ_symbol ~underlying ~expiry ~option_type ~strike
      in
      let expiry_str = Time.to_ymd expiry in
      {
        id = symbol;
        symbol;
        name =
          Format.asprintf "%s %s %s %.2f" underlying expiry_str
            (match option_type with
            | Call -> "Call"
            | Put -> "Put"
            | Forward -> "Forward")
            strike;
        underlying_symbol = underlying;
        ty = option_type;
        status = Status.active;
        tradable = true;
        expiration_date = expiry_str;
        underlying_asset_id = underlying ^ "_ASSET";
        strike_price = strike;
        multiplier = 100.0;
        size = 100.0;
        deliverables =
          [
            {
              ty = Deliverable.Type.Equity;
              symbol = underlying;
              asset_id = underlying ^ "_ASSET";
              amount = 100.0;
              allocation_percentage = 100.0;
              settlement_type = Deliverable.Settlement_type.T1;
              settlement_method = Deliverable.Settlement_method.BTOB;
            };
          ];
      }

    let generate_option_chain ~underlying ~current_price ~current_date
        ?(range_pct = 0.25) ?(num_monthly = 3) ?(num_weekly = 2) () =
      let strikes = generate_strikes ~current_price ~range_pct in
      let monthly_expirations =
        generate_monthly_expirations ~current_date ~num_months:num_monthly
      in
      let weekly_expirations =
        generate_weekly_expirations ~current_date ~num_weeks:num_weekly
      in

      let all_expirations =
        List.sort_uniq ~cmp:Ptime.compare
          (monthly_expirations @ weekly_expirations)
      in

      let contracts =
        List.fold_left
          (fun acc expiry ->
            List.fold_left
              (fun acc2 strike ->
                let call =
                  create_contract ~underlying ~expiry ~option_type:Type.Call
                    ~strike
                in
                let put =
                  create_contract ~underlying ~expiry ~option_type:Type.Put
                    ~strike
                in
                call :: put :: acc2)
              acc strikes)
          [] all_expirations
      in

      List.rev contracts
  end

  module Export = struct
    let top ~underlying ~current_price ~current_date ?(range_pct = 0.25)
        ?(num_monthly = 3) ?(num_weekly = 2) () : t list =
      Internal.generate_option_chain ~underlying ~current_price ~current_date
        ~range_pct ~num_monthly ~num_weekly ()
  end

  include Export
end
