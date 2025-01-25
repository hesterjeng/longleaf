module Direction = struct
  type t = Above | Below [@@deriving show { with_path = false }]
end

module Reason = struct
  type t = string list [@@deriving show]

  let make indicator direction amt =
    [ Format.asprintf "%s %a %f" indicator Direction.pp direction amt ]
end

module Flag = struct
  type t = Pass of Reason.t | Fail of Reason.t

  let pass x = Pass x
  let fail x = Fail x

  let conjunction state (l : (state:'a State.t -> t) list) =
    List.fold_left
      (fun acc current ->
        match (acc, current ~state) with
        | Some (Pass acc), Pass curr -> Some (Pass (curr @ acc))
        | Some (Pass _), Fail res -> Some (Fail res)
        | (Some (Fail _) as failure), _ -> failure
        | None, Pass res -> Some (Pass res)
        | None, Fail res -> Some (Fail res))
      None l
    |> function
    | Some res -> res
    | None -> Fail [ "signal.ml: empty conjunction" ]

  let disjunction state (l : (state:'a State.t -> t) list) =
    List.fold_left
      (fun acc current ->
        match (acc, current ~state) with
        | _, (Pass _ as success) -> Some success
        | acc, Fail _ -> acc)
      None l
    |> function
    | Some res -> res
    | None -> Fail [ "signal.ml: empty disjunction" ]
end

module Indicator = struct
  type 'a t = string -> Direction.t -> 'a -> Flag.t

  let of_indicator (state : _ State.t) (indicator : Indicators.Point.t -> float)
      name : 'a t =
   fun symbol direction target ->
    let indicators =
      Indicators.get state.indicators symbol
      |> Option.get_exn_or "signal.ml: Unable to get indicators for symbol"
    in
    let point =
      Vector.top indicators
      |> Option.get_exn_or "signal.ml: Empty indicators for symbol"
    in
    let value = indicator point in
    let reason = Reason.make name direction target in
    match (direction, value >=. target) with
    | Above, true -> Pass reason
    | Above, false -> Fail reason
    | Below, true -> Fail reason
    | Below, false -> Pass reason

  let rsi ~state : 'a t = of_indicator state Indicators.Point.rsi "RSI"

  let awesome ~state : 'a t =
    of_indicator state Indicators.Point.awesome "Awesome"

  let upper_bb ~state : 'a t =
    of_indicator state Indicators.Point.upper_bollinger "Upper BB(2)"

  let lower_bb ~state : 'a t =
    of_indicator state Indicators.Point.lower_bollinger "Lower BB(2)"

  let and_ o f =
    match o with Flag.Pass res -> f res | Flag.Fail _ as failure -> failure

  let or_ o f =
    match o with Flag.Pass _ as success -> success | Flag.Fail res -> f res

  let ( let&& ) = and_
  let ( let|| ) = or_

  (* let conjunction (l : 'a t list) = *)
end

type t = {
  symbol : string; [@compare fun _ _ -> 0]
  reason : string list; [@compare fun _ _ -> 0]
  score : float;
}
[@@deriving show, ord]

(* type 'a t = 'a State.t -> string -> Flag.t *)

(* let attempt_using (state : _ State.t) symbol = *)
(*   let rsi = rsi state in *)
(*   let awesome = awesome state in *)
(*   let&& _ = rsi symbol Above 40.0 in *)
(*   let&& _ = awesome symbol Above 1.0 in *)
(*   Pass "Succeeded" *)

(* let sell state symbol = *)
(*   let rsi = rsi state in *)
(*   let awesome = awesome state in *)
(*   let lower_bb = lower_bb state in *)
(*   let price = price state symbol in *)
(*   let|| _ = rsi symbol Below 40.0 in *)
(*   let|| _ = awesome symbol Below 0.5 in *)
(*   let|| _ = lower_bb symbol Below price in *)
(*   Fail "no reason" *)
