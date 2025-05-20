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

    let to_boolean_func (type a) (x : t) =
     fun (state : a State.t) (instrument : Instrument.t) ->
      let ( let+ ) = Result.( let+ ) in
      let+ i = Indicators.get_top state.indicators instrument in
      match x with
      | FSO_k_gt v -> i.fso.k >. EnumeratedValue.to_float v
      | FSO_k_lt v -> i.fso.k <. EnumeratedValue.to_float v
      | FSO_d_gt v -> i.fso.d >. EnumeratedValue.to_float v
      | FSO_d_lt v -> i.fso.d <. EnumeratedValue.to_float v
      | RSI_gt v -> i.relative_strength_index >. EnumeratedValue.to_float v
      | RSI_lt v -> i.relative_strength_index <. EnumeratedValue.to_float v
  end

  type t = Empty | Atom of Atom.t

  let to_signal_function (type a) (x : t) =
   fun (state : a State.t) (instrument : Instrument.t) ->
    match x with
    | Empty -> Result.return @@ Signal.make instrument false
    | Atom a ->
        let ( let* ) = Result.( let* ) in
        let boolean_func = Atom.to_boolean_func a in
        let* res = boolean_func state instrument in
        Result.return @@ Signal.make instrument res

  let to_buy_trigger (x : t) =
    let pass = to_signal_function x in
    let module X : Template.Buy_trigger.INPUT = struct
      let pass = pass
      let score = fun _ _ -> Result.return 0.0
      let num_positions = 1
    end in
    ()
end

type strategy = { buy : EnumeratedSignal.t; sell : EnumeratedSignal.t }
