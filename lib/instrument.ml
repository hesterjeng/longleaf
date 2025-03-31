type t = Security of string | Contract of Contract.t [@@deriving show]

let symbol = function Security x -> x | Contract x -> x.symbol

let pp_string : t Format.printer =
 fun fmt x ->
  let str = symbol x in
  Format.fprintf fmt "%s" str

let equal x y = String.equal (symbol x) (symbol y)
let hash x = String.hash @@ symbol x
let yojson_of_t (x : t) : Yojson.Safe.t = `String (symbol x)

let of_string (x : string) =
  let l = String.to_list x |> List.map Char.to_string in
  let first_int =
    List.find_mapi
      (fun i s -> match Int.of_string s with Some _ -> Some i | _ -> None)
      l
  in
  match first_int with
  | None -> Security x
  | Some i ->
      let underlying, rest = String.take_drop i x in
      let underlying_symbol =
        String.filter (fun x -> not @@ Char.equal x ' ') underlying
      in
      let yy, rest = String.take_drop 2 rest in
      let mm, rest = String.take_drop 2 rest in
      let dd, rest = String.take_drop 2 rest in
      let expiration_date = Format.asprintf "20%s-%s-%s" yy mm dd in
      let ty, rest = String.take_drop 1 rest in
      let ty = Contract.Type.of_string ty in
      let strike_price =
        String.drop_while (fun c -> not @@ Char.equal c '0') rest
        |> Float.of_string_opt
        |> Option.get_exn_or
             "Expected to get a float in Instrument.Parsing.t_of_yojson"
      in
      Contract
        {
          symbol = x;
          name = x;
          underlying_symbol;
          multiplier = 100.0;
          deliverables = [];
          underlying_asset_id = "Unknown";
          id = "Unknown";
          ty;
          size = 100.0;
          tradable = true;
          status = Active;
          expiration_date;
          strike_price;
        }

let t_of_yojson (json : Yojson.Safe.t) =
  match json with
  | `String s -> of_string s
  | _ -> invalid_arg "Instrument.t_of_yojson NYI"

let security x = Security x
