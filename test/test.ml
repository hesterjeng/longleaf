(* let bad_time () = *)
(*   let _ = Longleaf_lib.Time.of_string "2025-03-03T16:59:00" in *)
(*   () *)

let good_time () =
  let _ = Longleaf_core.Time.of_string "2025-03-03T16:59:00Z" in
  ()

(* Mock cohttp-eio client for testing with HTTPS enabled *)
let mock_client env =
  let () = Longleaf_apis.Https.init_rng () in
  let authenticator = Longleaf_apis.Https.authenticator () in
  let https = Longleaf_apis.Https.make_https ~authenticator in
  Cohttp_eio.Client.make
    ~https:(Some https)
    (Eio.Stdenv.net env)

(* Test function for get_account *)
let test_get_account env () =
  (* Create a test client module that implements the CONFIG interface *)
  let module Test_client : Longleaf_apis.Trading_api.CONFIG = struct
    let longleaf_env = Longleaf_core.Environment.make ()
    let client = mock_client env
    let runtype = Longleaf_core.Runtype.Paper
  end in
  let module Test_trading_api = Longleaf_apis.Trading_api.Make (Test_client) in
  (* Since we're using a mock client, this will likely fail with a network error
     or authentication error, but it will test that the functor is properly instantiated
     and the function signature is correct *)
  let res =
    match Test_trading_api.Accounts.get_account () with
    | Ok account ->
      Eio.traceln "%a" Test_trading_api.Accounts.pp account;
      true
    | Error e ->
      Eio.traceln "ERROR: %a" Longleaf_core.Error.pp e;
      false
  in
  (* cohttp-eio clients don't require explicit shutdown *)
  res

let () =
  let account = Eio_main.run @@ fun env -> test_get_account env () in
  Alcotest.run "Utils"
    [
      ("time", [ Alcotest.test_case "With timezone" `Quick good_time ]);
      ( "trading_api",
        [
          ( Alcotest.test_case "trading api" `Quick @@ fun _ ->
            Alcotest.(check bool) "Got account from Alpaca" account true );
        ] );
    ]
