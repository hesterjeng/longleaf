module Hashtbl = Hashtbl.Make (String)

let accumulation_distribution_line (l : Item.t list) =
  let _, res =
    List.fold_map
      (fun previous_adl (x : Item.t) ->
        let open Float in
        let open Item in
        let close = close x in
        let low = low x in
        let high = high x in
        let volume = volume x in
        let money_flow_multiplier =
          (close - low - (high - close)) / (high - low)
        in
        let money_flow_volume = money_flow_multiplier * Float.of_int volume in
        let adl = previous_adl + money_flow_volume in
        (adl, adl))
      0.0 l
  in
  res

let simple_moving_average (l : Item.t list) =
  let n = List.length l in
  let close = List.map (fun (x : Item.t) -> Item.close x) l in
  let sma_i i =
    let start = Int.max (i - n) 0 in
    let range = List.range start i in
    let vals =
      List.map
        (fun i ->
          match List.get_at_idx i close with
          | Some p -> p
          | None ->
              invalid_arg @@ Format.sprintf "Unable to get price at index %d" i)
        range
    in
    List.fold_left (fun x y -> x +. y) 0.0 vals /. Float.of_int i
  in
  List.mapi (fun i _ -> sma_i i) close

module Point = struct
  type t = { timestamp : Time.t; adl : float; sma : float }
  [@@deriving show, yojson]
end

module Latest = struct
  type t = Point.t Hashtbl.t

  let empty () : t = Hashtbl.create 0

  let pp : t Format.printer =
   fun fmt x ->
    let seq = Hashtbl.to_seq x in
    let pp = Seq.pp @@ Pair.pp String.pp Point.pp in
    Format.fprintf fmt "@[%a@]@." pp seq

  let get x symbol =
    match Hashtbl.find_opt x symbol with
    | Some x -> x
    | None -> invalid_arg "Unable to find price of symbol (Indicators.Latest)"

  let of_latest (x : Bars.t) (latest : Bars.Latest.t) = invalid_arg "NYI"
end
