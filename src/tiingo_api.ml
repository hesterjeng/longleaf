module Headers = Piaf.Headers

type item = {
  ticker : string;
  timestamp : Time.t;
  last : float;
  open_ : float; [@key "open"]
  high : float;
  ask_price : float; [@key "askPrice"]
  bid_price : float; [@key "bidPrice"]
  low : float;
  close : float;
  volume : int;
}
[@@deriving show { with_path = false }, yojson] [@@yojosn.allow_extra_fields]

type t = item list [@@deriving show { with_path = false }, yojson]

let tiingo_client eio_env sw =
  let res =
    Piaf.Client.create ~sw eio_env @@ Uri.of_string "https://api.tiingo.com"
  in
  match res with
  | Ok x -> x
  | Error _ -> invalid_arg "Unable to create trading client"

module Make (Tiingo : Util.CLIENT) = struct
  let client = Tiingo.client

  let tiingo_key =
    match Tiingo.longleaf_env.tiingo_key with
    | Some s -> s
    | None -> invalid_arg "No tiingo key when trying to make tiingo connection"

  let headers =
    Headers.of_list
      [
        ("Content-Type", "application/json");
        ("Authorization", Format.asprintf "Token %s" tiingo_key);
      ]

  let get = Util.get_piaf ~client:Tiingo.client
  let endpoint = Uri.of_string "/iex/"

  let test () =
    let endpoint = Uri.of_string "/api/test" |> Uri.to_string in
    get ~headers ~endpoint

  let latest tickers : t =
    let endpoint =
      Uri.add_query_params' endpoint [ ("tickers", String.concat "," tickers) ]
      |> Uri.to_string
    in
    Eio.traceln "@[endpoint: %s@]@." endpoint;
    let resp = get ~headers ~endpoint in
    Eio.traceln "@[%a@]@." Yojson.Safe.pp resp;
    t_of_yojson resp
end
