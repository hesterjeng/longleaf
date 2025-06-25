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
  [@@deriving yojson, eq, show, variants, ord]

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
    let add acc var = var.Variantslib.Variant.constructor :: acc in
    Variants.fold ~init:[] ~ten:add ~twenty:add ~thirty:add ~fourty:add
      ~fifty:add ~sixty:add ~seventy:add ~eighty:add ~ninety:add
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
    [@@deriving yojson, eq, show, variants, ord]

    let all : t list =
      let add acc var = var.Variantslib.Variant.constructor :: acc in
      let constructors =
        Variants.fold ~init:[] ~fso_k_gt:add ~fso_k_lt:add ~fso_d_gt:add
          ~fso_d_lt:add ~rsi_gt:add ~rsi_lt:add
      in
      List.flat_map (fun f -> List.map f EnumeratedValue.all) constructors

    let to_boolean_func (x : t) (state : 'a State.t) (instrument : Instrument.t)
        =
      let ( let* ) = Result.( let* ) in
      let* data = Bars.get state.bars instrument in
      let res =
        match x with
        | FSO_k_gt v -> Data.get_top data FSO_K >. EnumeratedValue.to_float v
        | FSO_k_lt v -> Data.get_top data FSO_K <. EnumeratedValue.to_float v
        | FSO_d_gt v -> Data.get_top data FSO_D >. EnumeratedValue.to_float v
        | FSO_d_lt v -> Data.get_top data FSO_D <. EnumeratedValue.to_float v
        | RSI_gt v -> Data.get_top data RSI >. EnumeratedValue.to_float v
        | RSI_lt v -> Data.get_top data RSI <. EnumeratedValue.to_float v
      in
      Result.return res

    (* let all = *)
  end

  module AtomSet = struct
    include Set.Make (Atom)

    let t_of_yojson (json : Yojson.Safe.t) : t =
      match json with
      | `List l -> List.map Atom.t_of_yojson l |> of_list
      | _ -> invalid_arg "Invalid json in AtomSet.t_of_yojson"

    let yojson_of_t (x : t) : Yojson.Safe.t =
      let l = to_list x in
      `List (List.map Atom.yojson_of_t l)

    let pp : t Format.printer =
     fun fmt x -> Format.fprintf fmt "%a" (List.pp Atom.pp) (to_list x)
  end

  (* module Operator = struct *)
  (*   type t = And | Or [@@deriving yojson, eq, show] *)

  (* end *)

  type t = Empty | And of AtomSet.t | Or of AtomSet.t
  [@@deriving yojson, eq, show, ord]

  let is_empty = function
    | Empty -> true
    | _ -> false

  let and_ l = And l
  let or_ l = Or l

  let is_and = function
    | And _ -> true
    | _ -> false

  let and_singletons = and_ @@ AtomSet.of_list Atom.all
  let or_singletons = or_ @@ AtomSet.of_list Atom.all

  let neighbors (x : t) =
    match x with
    | Empty ->
      List.map (fun x -> and_ @@ AtomSet.add x AtomSet.empty) Atom.all
      @ List.map (fun x -> or_ @@ AtomSet.add x AtomSet.empty) Atom.all
    | And l -> List.map (fun x -> and_ @@ AtomSet.add x l) Atom.all
    | Or l -> List.map (fun x -> or_ @@ AtomSet.add x l) Atom.all

  let to_signal_function (x : t) =
    let ( let* ) = Result.( let* ) in
    fun (state : 'a State.t) (instrument : Instrument.t) :
        (Signal.t, Error.t) result ->
      (* let time = state.time in *)
      let and_ atom acc =
        let* acc = acc in
        match acc with
        | false -> Result.return false
        | true ->
          let boolean_func = Atom.to_boolean_func atom in
          let* res = boolean_func state instrument in
          Result.return res
      in
      let or_ atom acc =
        let* acc = acc in
        match acc with
        | true -> Result.return true
        | false ->
          let boolean_func = Atom.to_boolean_func atom in
          let* res = boolean_func state instrument in
          Result.return res
      in
      match x with
      | Empty -> Result.return @@ Signal.make instrument false
      | And l ->
        let* res = AtomSet.fold and_ l (Ok true) in
        Result.return @@ Signal.make instrument res
      | Or l ->
        let* res = AtomSet.fold or_ l (Ok false) in
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

let run_astar (context : Options.t) ~(buy : EnumeratedSignal.t)
    ~(sell : EnumeratedSignal.t) =
  let x = EnumeratedSignal.to_strategy buy sell in
  Strategy.run x context
