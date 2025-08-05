open Gadt
module Pack = Tacaml.Pack
module Safe = Tacaml.Safe

module Variant = struct

  module Lookup = Map.Make(String)

  type lookup = int Lookup.t

  let tacaml (x : Tacaml.Indicator.t) : lookup -> float array -> (_ t, Error.t) result =
    let ( let* ) = Result.( let* ) in
    match x with
    | F (UpperAccBand _) ->
      fun lookup arr ->
        (* let timeperiod = Int.of_float arr.(0) in *)
        let* i = match Lookup.get "timeperiod" lookup with
          | Some x -> Result.return x
          | None -> Error.fatal "[ fo_strategy.ml ] Unable to find index of timeperiod"
        in
        let timeperiod = Int.of_float arr.(i) in
        let indicator = Tacaml.Indicator.(F (UpperAccBand { timeperiod })) in
        Result.return @@
        Data (Tacaml indicator)
    | _ -> invalid_arg "NYI"

  (* let top (x : Data.Type.t) = *)
  (*   match x with *)
  (*   | Tacaml x -> tacaml x *)
  (*   | _ -> x *)

  (* type ('a, 'b) exp = 'a t ->  *)
end
