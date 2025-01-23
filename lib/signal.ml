module Sig = struct
  type ('a, 'b) t = Pass of 'a | Fail of 'b
end

type res = Pass of float | Fail of float
type 'a t = 'a State.t -> string -> res option

module Examples = struct
  let small_rsi : 'a t =
   fun state symbol ->
    let open Option in
    let* indicators = Indicators.get state.indicators symbol in
    let* point = Vector.top indicators in
    let rsi = Indicators.Point.rsi point in
    if rsi <=. 40.0 then Some (Pass rsi) else None

  let above_awesome : 'a t =
   fun state symbol ->
    let open Option in
    let* indicators = Indicators.get state.indicators symbol in
    let* point = Vector.top indicators in
    let awesome = Indicators.Point.awesome point in
    if Indicators.Point.awesome point >=. 1.0 then Some (Fail awesome) else None
end

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
