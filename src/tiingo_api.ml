module Headers = Piaf.Headers

(* WIP *)
type data =
  {
    ticker : string;
  }
  [@@deriving show { with_path = false }, yojson]

module Make (Tiingo : Util.CLIENT) = struct
  let client = Tiingo.client
  let tiingo_key = Tiingo.longleaf_env.tiingo_key

  let headers =
    match tiingo_key with
    | Some key ->
        Headers.of_list [ ("Authorization", Format.asprintf "Token %s" key) ]
    | None -> invalid_arg "No tiingo key when trying to make tiingo connection"

  let get = Util.get_piaf ~client:Tiingo.client
  let iex_endpoint = Uri.of_string "iex"

  let latest_bars tickers =
    let endpoint =
      Uri.add_query_param' iex_endpoint ("tickers", String.concat "," tickers)
      |> Uri.to_string
    in
    get ~headers ~endpoint
end
