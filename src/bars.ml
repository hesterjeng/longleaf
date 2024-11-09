module Bar_item = struct
  type t = {
    timestamp : Time.t; [@key "t"]
    open_ : float; [@key "o"]
    high : float; [@key "h"]
    low : float; [@key "l"]
    close : float; [@key "c"]
    volume : int; [@key "v"]
    trade_count : int; [@key "n"]
    volume_weighted : float; [@key "vw"]
  }
  [@@deriving show { with_path = false }, yojson]

  let compare x y = Ptime.compare x.timestamp y.timestamp
end

module Data = struct
  type t = (string * Bar_item.t list) list [@@deriving show, yojson]

  let empty : t = []
  let original_t_of_yojson = t_of_yojson

  let t_of_yojson (x : Yojson.Safe.t) =
    try
      match x with
      | `Assoc s ->
          List.map
            (fun ((ticker, data) : string * Yojson.Safe.t) ->
              ( ticker,
                match data with
                | `List l -> List.map Bar_item.t_of_yojson l
                | `Assoc _ -> [ Bar_item.t_of_yojson data ]
                | a ->
                    Util.Util_log.err (fun k -> k "%a" Yojson.Safe.pp a);
                    invalid_arg "The data must be stored as a list" ))
            s
      | _ -> invalid_arg "Bars must be a toplevel Assoc"
    with _ ->
      Format.printf "@[Trying Data.original_t_of_yojson@]@.";
      original_t_of_yojson x
end

type t = {
  data : Data.t;
  next_page_token : string option; [@default None]
  currency : string option; [@default None]
}
[@@deriving show { with_path = false }, yojson] [@@yojson.allow_extra_fields]

let empty : t = { data = Data.empty; next_page_token = None; currency = None }
let tickers (x : t) = List.map fst x.data

(* FIXME: This function does a lot of work to ensure that things are in the correct order *)
let combine (l : t list) : t =
  let keys =
    List.flat_map (fun x -> List.Assoc.keys x.data) l
    |> List.uniq ~eq:String.equal
  in
  let get_data key =
    let data =
      List.flat_map
        (fun (x : t) ->
          match List.Assoc.get ~eq:String.equal key x.data with
          | Some found -> found
          | None -> [])
        l
    in
    List.sort Bar_item.compare data
  in
  let data = List.map (fun key -> (key, get_data key)) keys in
  { data; next_page_token = None; currency = None }

let get bars ticker = List.Assoc.get ~eq:String.equal ticker bars

let price x ticker =
  let bars = x.data in
  match List.Assoc.get ~eq:String.equal ticker bars with
  | Some [ info ] -> info
  | Some _ -> invalid_arg "Multiple bar items on latest bar?"
  | None ->
      invalid_arg
      @@ Format.asprintf "Unable to get price info for ticker %s" ticker
