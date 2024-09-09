module Log = (val Logs.src_log Logs.(Src.create "dataframe"))

module Element = struct
  module Info_type = struct
    type t = AdjClose | Close | High | Low | Open
    [@@deriving show { with_path = false }, eq]

    let of_string = function
      | "Adj Close" | "AdjClose" -> AdjClose
      | "Close" -> Close
      | "High" -> High
      | "Low" -> Low
      | "Open" -> Open
      | _ -> invalid_arg "Unknown information type"
  end

  module Ticker = struct
    type t = { index : string; info_type : Info_type.t; price : float }
    [@@deriving show, eq]

    let of_json (info_raw, price_raw) =
      Log.app (fun k -> k "Attempting to create Ticker.t from json");
      let open Result in
      let info_type_raw, index = Parsing.top info_raw in
      let info_type = Info_type.of_string info_type_raw in
      let+ price =
        match price_raw with
        | `Float f -> Ok f
        | _ ->
            Error
              (Format.asprintf "Price is not a float... %s %a" info_raw
                 Yojson.Safe.pp price_raw)
      in
      { index; info_type; price }

    let find index ty l =
      let res =
        List.find_opt
          (fun (x : t) ->
            Info_type.equal ty x.info_type && String.equal x.index index)
          l
      in
      match res with
      | Some s -> s.price
      | None -> invalid_arg "Unable to find ticker info with price"
  end

  type t = {
    datetime : int; [@printer Time.pp_int]
    information : Ticker.t array;
  }
  [@@deriving show]

  let compare x y = Ord.int x.datetime y.datetime

  let date (x, content) =
    let date, _ = Parsing.top x in
    match (date, content) with
    | "Date", `Int i | "Datetime", `Int i -> Some i
    | _ -> None

  let volume (x, content) =
    let date, _ = Parsing.top x in
    match (date, content) with "Volume", `Int i -> Some i | _ -> None

  let find_date x =
    let res = List.find_map date x in
    match res with
    | Some date -> Ok date
    | None -> Error "Unable to find date/datetime in JSON element."

  let find_volume x =
    let res = List.find_map volume x in
    match res with
    | Some volume -> Ok volume
    | None -> Error "Unable to find volume in JSON element."

  let of_json (x : Yojson.Safe.t) =
    match x with
    | `Assoc l ->
        let open Result in
        let* datetime = find_date l in
        (* Volume field may not be present *)
        (* let* volume = find_volume l in *)
        let rest =
          List.filter
            (fun x ->
              (Option.is_none @@ date x) && (Option.is_none @@ volume x))
            l
        in
        let+ information = Result.map_l Ticker.of_json rest in
        let information = Array.of_list information in
        { datetime; information }
    | _ -> Result.fail @@ Format.asprintf "Not OK: %a" Yojson.Safe.pp x
end

type parsed = Element.t array [@@deriving show]

let of_json (x : Yojson.Safe.t) : (parsed, string) result =
  let open Result in
  match x with
  | `List l ->
      let* parsed = Result.map_l Element.of_json l in
      let array = Array.of_list parsed in
      Array.sort Element.compare array;
      Log.app (fun k -> k "%a" pp_parsed array);
      Ok array
  | _ -> Error "Dataframe.of_json:  This is not a list"
