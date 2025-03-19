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
  [@@deriving show, yojson]
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
[@@deriving show, yojson]
