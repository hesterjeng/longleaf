(* HTTPS/TLS configuration for Cohttp-eio clients *)

(* Create X509 certificate authenticator from system certificate store *)
let authenticator () =
  match X509.Authenticator.chain_of_trust () with
  | Ok x -> x
  | Error (`Msg m) ->
      invalid_arg (Printf.sprintf "Failed to create X509 authenticator: %s" m)

(* Create HTTPS wrapper function for Cohttp-eio client *)
let make_https ~authenticator =
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Error (`Msg msg) -> invalid_arg (Printf.sprintf "TLS config error: %s" msg)
    | Ok cfg -> cfg
  in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

(* Initialize the RNG - must be called once at program startup *)
let init_rng () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)
