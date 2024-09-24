module Log = (val Logs.src_log Logs.(Src.create "dataframe"))

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
    (* Log.app (fun k -> k "Attempting to create Ticker.t from json"); *)
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

module Element = struct
  type t = {
    datetime : int; [@printer Time.pp_int]
    information : Ticker.t array;
  }
  [@@deriving show]

  let compare x y = Ord.int x.datetime y.datetime

  let find_info (x : t) (info_type : Info_type.t) (index : string) =
    let ticker =
      Array.find_opt
        (fun (ticker : Ticker.t) ->
          String.equal ticker.index index
          && Info_type.equal ticker.info_type info_type)
        x.information
    in
    match ticker with
    | None -> invalid_arg "Could not find info type for index at element"
    | Some s -> s.price

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
type t = { index : string; data : Owl_dataframe.t }

let pp fmt x =
  Format.fprintf fmt "@[   +-------------+ %s +-------------+@]@.@[%a@]@."
    x.index Owl_pretty.pp_dataframe x.data

module Conversion = struct
  let collect_indices (x : Element.t) =
    let information = x.information in
    Array.map (fun (x : Ticker.t) -> x.index) information |> Array.to_list

  let collect_all_indices (x : parsed) =
    let l = Array.to_list x in
    List.flat_map collect_indices l |> List.uniq ~eq:String.equal

  let collect_info_types (x : Element.t) =
    let information = x.information in
    Array.map (fun (x : Ticker.t) -> x.info_type) information |> Array.to_list

  let collect_all_info_types (x : parsed) =
    let l = Array.to_list x in
    List.flat_map collect_info_types l
    |> List.uniq ~eq:Info_type.equal
    |> Array.of_list

  let top (x : parsed) (index : string) =
    let info_types = collect_all_info_types x in
    let times_series =
      let arr = Array.map (fun (x : Element.t) -> x.datetime) x in
      Owl.Dataframe.pack_int_series arr
    in
    let data_types, data_cols =
      let get_data info_type =
        Array.map (fun x -> Element.find_info x info_type index) x
      in
      let res =
        Array.map
          (fun (info_type : Info_type.t) ->
            ( Info_type.show info_type,
              get_data info_type |> Owl.Dataframe.pack_float_series ))
          info_types
      in
      Array.split res
    in
    let data = Array.append [| times_series |] data_cols in
    let column_names = Array.append [| "Datetime" |] data_types in
    let dataframe = Owl.Dataframe.make ~data column_names in
    (* Log.app (fun k -> k "%a" Owl_pretty.pp_dataframe dataframe); *)
    { index; data = dataframe }
end

let of_json (x : Yojson.Safe.t) : (parsed, string) result =
  let open Result.Infix in
  match x with
  | `List l ->
      let* parsed = Result.map_l Element.of_json l in
      let array = Array.of_list parsed in
      Array.sort Element.compare array;
      (* Log.app (fun k -> k "%a" pp_parsed array); *)
      let indices_found = Conversion.collect_all_indices array in
      (* Log.app (fun k -> k "%a" Format.(list string) indices_found); *)
      let dataframes =
        List.map (fun x -> Conversion.top array x) indices_found
      in
      (* Log.app (fun k -> k "%a" Format.(list pp) dataframes); *)
      Ok array
  | _ -> Error "Dataframe.of_json:  This is not a list"
