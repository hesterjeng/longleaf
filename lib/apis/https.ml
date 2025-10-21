(* HTTPS/TLS configuration for Cohttp-eio clients *)

(* Load system CA certificates from common locations *)
let load_system_cas () =
  let cert_paths = [
    "/etc/ssl/certs/ca-certificates.crt";  (* Debian/Ubuntu *)
    "/etc/pki/tls/certs/ca-bundle.crt";    (* Fedora/RHEL *)
    "/etc/ssl/ca-bundle.pem";               (* OpenSUSE *)
    "/etc/ssl/cert.pem";                    (* OpenBSD/macOS *)
    "/usr/local/share/certs/ca-root-nss.crt"; (* FreeBSD *)
  ] in
  let rec try_load = function
    | [] -> Error (`Msg "No system CA certificates found")
    | path :: rest ->
        if Sys.file_exists path then
          try
            let data =
              let ic = open_in path in
              let len = in_channel_length ic in
              let buf = Bytes.create len in
              really_input ic buf 0 len;
              close_in ic;
              Bytes.to_string buf
            in
            X509.Certificate.decode_pem_multiple data
          with _ -> try_load rest
        else
          try_load rest
  in
  try_load cert_paths

(* Create X509 certificate authenticator from system certificate store *)
let authenticator () =
  match load_system_cas () with
  | Ok cas ->
      let time () = Some (Ptime_clock.now ()) in
      X509.Authenticator.chain_of_trust ~time cas
  | Error (`Msg m) ->
      invalid_arg (Printf.sprintf "Failed to load system CA certificates: %s" m)

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
let init_rng () = Mirage_crypto_rng_unix.use_default ()
