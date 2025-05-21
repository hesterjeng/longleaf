module Search = Longleaf_lib.Astar
module Error = Longleaf_lib.Error

module EnumeratedValue = struct
  type t =
    | Ten
    | Twenty
    | Thirty
    | Fourty
    | Fifty
    | Sixty
    | Seventy
    | Eighty
    | Ninety
  [@@deriving yojson, eq, show, variants]

  let to_float = function
    | Ten -> 10.0
    | Twenty -> 20.0
    | Thirty -> 30.0
    | Fourty -> 40.0
    | Fifty -> 50.0
    | Sixty -> 60.0
    | Seventy -> 70.0
    | Eighty -> 80.0
    | Ninety -> 90.0

  let all =
    List.map (fun x -> `String (fst x)) Variants.descriptions
    |> List.map t_of_yojson
end

module EnumeratedSignal = struct
  module Atom = struct
    type t =
      | FSO_k_gt of EnumeratedValue.t
      | FSO_k_lt of EnumeratedValue.t
      | FSO_d_gt of EnumeratedValue.t
      | FSO_d_lt of EnumeratedValue.t
      | RSI_gt of EnumeratedValue.t
      | RSI_lt of EnumeratedValue.t
    [@@deriving yojson, eq, show, variants]

    let all : t list =
      let add acc var = var.Variantslib.Variant.constructor :: acc in
      let constructors =
        Variants.fold ~init:[] ~fso_k_gt:add ~fso_k_lt:add ~fso_d_gt:add
          ~fso_d_lt:add ~rsi_gt:add ~rsi_lt:add
      in
      List.flat_map (fun f -> List.map f EnumeratedValue.all) constructors

    let to_boolean_func (x : t) =
     fun (indicators : Indicators.t) (instrument : Instrument.t) ->
      let ( let+ ) = Result.( let+ ) in
      let+ i = Indicators.get_top indicators instrument in
      match x with
      | FSO_k_gt v -> i.fso.k >. EnumeratedValue.to_float v
      | FSO_k_lt v -> i.fso.k <. EnumeratedValue.to_float v
      | FSO_d_gt v -> i.fso.d >. EnumeratedValue.to_float v
      | FSO_d_lt v -> i.fso.d <. EnumeratedValue.to_float v
      | RSI_gt v -> i.relative_strength_index >. EnumeratedValue.to_float v
      | RSI_lt v -> i.relative_strength_index <. EnumeratedValue.to_float v

    (* let all = *)
  end

  type t = Empty | Atom of Atom.t [@@deriving yojson, eq, show]

  let neighbors (x : t) =
    match x with Empty -> List.map (fun x -> Atom x) Atom.all | Atom _ -> []

  let to_signal_function (x : t) =
   fun (state : 'a State.t) (instrument : Instrument.t) ->
    match x with
    | Empty -> Result.return @@ Signal.make instrument false
    | Atom a ->
        let ( let* ) = Result.( let* ) in
        let boolean_func = Atom.to_boolean_func a in
        let* res = boolean_func state.indicators instrument in
        Result.return @@ Signal.make instrument res

  let to_buy_trigger (x : t) =
    let module X : Template.Buy_trigger.INPUT = struct
      let pass = fun state -> to_signal_function x state
      let score = fun _ _ -> Result.return 0.0
      let num_positions = 1
    end in
    let module Buy_trigger : Template.Buy_trigger.S =
      Template.Buy_trigger.Make (X) in
    (module Buy_trigger : Template.Buy_trigger.S)

  let to_sell_trigger (x : t) =
    let module X : Template.Sell_trigger.S = struct
      let make (state : 'a State.t) ~(buying_order : Order.t) =
        let signal = to_signal_function x state buying_order.symbol in
        signal
    end in
    (module X : Template.Sell_trigger.S)
  (* let module Buy_trigger : Template.Buy_trigger.S = Template.Buy_trigger.Make (X) in *)
  (* (module Buy_trigger : Template.Buy_trigger.S) *)

  let to_strategy (buy : t) (sell : t) =
    let buy = to_buy_trigger buy in
    let sell = to_sell_trigger sell in
    let module Strat = Template.Make ((val buy)) ((val sell)) in
    (module Strat : Strategy.BUILDER)
end

(* type strategy = { buy : EnumeratedSignal.t; sell : EnumeratedSignal.t } *)
