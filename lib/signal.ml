module Direction = struct

type t =
  | Above
  | Below [@@deriving show]

end

module Reason = struct

  type t = string [@@deriving show]

  let make indicator direction amt =
    Format.asprint

end

module Sig = struct
  type 'a t = Pass of 'a | Fail of Reason.t
end


type res = Pass of float | Fail of float
type 'a t = 'a State.t -> string -> res option

let rsi ty target : 'a t =
 fun state symbol ->
  let open Option in
  let* indicators = Indicators.get state.indicators symbol in
  let* point = Vector.top indicators in
  let rsi = Indicators.Point.rsi point in
  match ty with
  | Above -> if rsi >=. target then Pass rsi else Fail
  | Below -> if rsi <=. target then Some (Pass rsi) else None

let awesome_above : 'a t =
 fun state symbol ->
  let open Option in
  let* indicators = Indicators.get state.indicators symbol in
  let* point = Vector.top indicators in
  let awesome = Indicators.Point.awesome point in
  if Indicators.Point.awesome point >=. 1.0 then Some (Fail awesome) else None

let bind o f =
  match o with Some (Pass res) -> f res | Some (Fail _) -> None | None -> None

let map o f =
  match o with
  | Some (Pass res) -> Some (f res)
  | Some (Fail _) -> None
  | None -> None

let first o f =
  match o with Some (Pass _) as res -> res | Some (Fail _) | None -> f ()

let ( let&& ) = bind
let ( let|| ) = first

let attempt_using : 'a t =
 fun state symbol ->
  let&& x = Examples.small_rsi state symbol in
  let&& y = Examples.above_awesome state symbol in
  Some (Pass x)

let sell : 'a t =
 fun state symbol ->
  let|| () = Examples.small_rsi state symbol in
  let|| () = Examples.small_rsi state symbol in
  None
