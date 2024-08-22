module Log = (val Logs.src_log Logs.(Src.create "dataframe"))

type t = Owl.Dataframe.t

module Element = struct
  module Ticker = struct
    type t = { index : string; info_type : string; price : float }

    let of_json (info_raw, price_raw) =
      let open Result in
      let info_type, index = Parsing.top info_raw in
      let+ price =
        match price_raw with
        | `Float f -> Ok f
        | _ -> Error "Price is not a float..."
      in
      { index; info_type; price }
  end

  type t = { datetime : int; information : Ticker.t list }

  let date (x, content) =
    let date, _ = Parsing.top x in
    match (date, content) with
    | "Date", `Int i | "Datetime", `Int i -> Some i
    | _ -> None

  let find_date x =
    let res = List.find_map date x in
    match res with
    | Some date -> Ok date
    | None -> Error "Unable to find date/datetime in JSON element."

  let of_json (x : Yojson.Safe.t) =
    match x with
    | `Assoc l ->
        let open Result in
        let+ datetime = find_date l in
        let rest = List.filter (fun x -> Option.is_none @@ date x) l in
        let+ information = Result.map_l Ticker.of_json rest in
        { datetime; information }
    | _ -> invalid_arg @@ Format.asprintf "Not OK: %a" Yojson.Safe.pp x
end

let of_json (x : Yojson.Safe.t) =
  match x with
  | `List l -> List.map Element.of_json l
  | _ -> invalid_arg "Dataframe.of_json"
