(* let bad_time () = *)
(*   let _ = Longleaf_lib.Time.of_string "2025-03-03T16:59:00" in *)
(*   () *)

let good_time () =
  let _ = Longleaf_core.Time.of_string "2025-03-03T16:59:00Z" in
  ()

(* Test timezone-aware market timing functions *)
let test_time_of_day_et () =
  (* Create timestamp for 2024-01-15 14:30:00 UTC (should be 9:30 AM EST) *)
  let ptime_est =
    match Ptime.of_date_time ((2024, 1, 15), ((14, 30, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid test timestamp"
  in
  let timestamp_market_open = Ptime.to_float_s ptime_est in
  let tod = Longleaf_core.Time.time_of_day_et timestamp_market_open in
  (* Debug: print actual value *)
  Printf.printf "Time of day ET (EST): %.2f (expected 570.0)\n" tod;
  (* Should be 570 minutes since midnight ET (9:30 AM) *)
  Alcotest.(check bool) "Market open time" (Float.abs (tod -. 570.0) < 1.0) true;

  (* Create timestamp for 2024-07-15 13:30:00 UTC (should be 9:30 AM EDT) *)
  let ptime_edt =
    match Ptime.of_date_time ((2024, 7, 15), ((13, 30, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid test timestamp DST"
  in
  let timestamp_dst = Ptime.to_float_s ptime_edt in
  let tod_dst = Longleaf_core.Time.time_of_day_et timestamp_dst in
  Printf.printf "Time of day ET (EDT): %.2f (expected 570.0)\n" tod_dst;
  (* Should also be 570 minutes since midnight ET (9:30 AM EDT) *)
  Alcotest.(check bool)
    "Market open time (DST)"
    (Float.abs (tod_dst -. 570.0) < 1.0)
    true

let test_minutes_since_open () =
  (* Create timestamp for 2024-01-15 14:45:00 UTC (should be 9:45 AM EST = 15 mins after open) *)
  let ptime =
    match Ptime.of_date_time ((2024, 1, 15), ((14, 45, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid test timestamp"
  in
  let timestamp = Ptime.to_float_s ptime in
  let mins = Longleaf_core.Time.minutes_since_open timestamp in
  (* Should be approximately 15 minutes *)
  Alcotest.(check bool)
    "15 minutes since open"
    (Float.abs (mins -. 15.0) < 1.0)
    true

let test_minutes_until_close () =
  (* Create timestamp for 2024-01-15 20:30:00 UTC (should be 3:30 PM EST = 30 mins before close) *)
  let ptime =
    match Ptime.of_date_time ((2024, 1, 15), ((20, 30, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid test timestamp"
  in
  let timestamp = Ptime.to_float_s ptime in
  let mins = Longleaf_core.Time.minutes_until_close timestamp in
  (* Should be approximately 30 minutes *)
  Alcotest.(check bool)
    "30 minutes until close"
    (Float.abs (mins -. 30.0) < 1.0)
    true

let test_is_open () =
  (* During market hours: 2024-01-15 15:00:00 UTC = 10:00 AM EST *)
  let ptime_open =
    match Ptime.of_date_time ((2024, 1, 15), ((15, 0, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid test timestamp"
  in
  let timestamp_open = Ptime.to_float_s ptime_open in
  Alcotest.(check bool)
    "Is open at 10 AM"
    (Longleaf_core.Time.is_open timestamp_open)
    true;

  (* Before market hours: 2024-01-15 14:00:00 UTC = 9:00 AM EST *)
  let ptime_before =
    match Ptime.of_date_time ((2024, 1, 15), ((14, 0, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid test timestamp"
  in
  let timestamp_before = Ptime.to_float_s ptime_before in
  Alcotest.(check bool)
    "Is closed at 9 AM"
    (Longleaf_core.Time.is_open timestamp_before)
    false;

  (* After market hours: 2024-01-15 21:30:00 UTC = 4:30 PM EST *)
  let ptime_after =
    match Ptime.of_date_time ((2024, 1, 15), ((21, 30, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid test timestamp"
  in
  let timestamp_after = Ptime.to_float_s ptime_after in
  Alcotest.(check bool)
    "Is closed at 4:30 PM"
    (Longleaf_core.Time.is_open timestamp_after)
    false

let test_is_close () =
  (* 20 minutes before close: 2024-01-15 20:40:00 UTC = 3:40 PM EST *)
  let ptime_close =
    match Ptime.of_date_time ((2024, 1, 15), ((20, 40, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid test timestamp"
  in
  let timestamp_close = Ptime.to_float_s ptime_close in
  (* Should be true with 30 minute threshold *)
  Alcotest.(check bool)
    "Is close (20 mins, threshold 30)"
    (Longleaf_core.Time.is_close timestamp_close 30.0)
    true;
  (* Should be false with 15 minute threshold *)
  Alcotest.(check bool)
    "Not close (20 mins, threshold 15)"
    (Longleaf_core.Time.is_close timestamp_close 15.0)
    false

let test_is_near_open () =
  (* 10 minutes after open: 2024-01-15 14:40:00 UTC = 9:40 AM EST *)
  let ptime_near =
    match Ptime.of_date_time ((2024, 1, 15), ((14, 40, 0), 0)) with
    | Some t -> t
    | None -> failwith "Invalid test timestamp"
  in
  let timestamp_near = Ptime.to_float_s ptime_near in
  (* Should be true with 15 minute threshold *)
  Alcotest.(check bool)
    "Is near open (10 mins, threshold 15)"
    (Longleaf_core.Time.is_near_open timestamp_near 15.0)
    true;
  (* Should be false with 5 minute threshold *)
  Alcotest.(check bool)
    "Not near open (10 mins, threshold 5)"
    (Longleaf_core.Time.is_near_open timestamp_near 5.0)
    false

(* Mock cohttp-eio client for testing with HTTPS enabled *)
let mock_client env =
  let () = Longleaf_apis.Https.init_rng () in
  let authenticator = Longleaf_apis.Https.authenticator () in
  let https = Longleaf_apis.Https.make_https ~authenticator in
  Cohttp_eio.Client.make ~https:(Some https) (Eio.Stdenv.net env)

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
      ( "time",
        [
          Alcotest.test_case "With timezone" `Quick good_time;
          Alcotest.test_case "Time of day ET" `Quick test_time_of_day_et;
          Alcotest.test_case "Minutes since open" `Quick test_minutes_since_open;
          Alcotest.test_case "Minutes until close" `Quick
            test_minutes_until_close;
          Alcotest.test_case "Is open" `Quick test_is_open;
          Alcotest.test_case "Is close" `Quick test_is_close;
          Alcotest.test_case "Is near open" `Quick test_is_near_open;
        ] );
      ( "trading_api",
        [
          ( Alcotest.test_case "trading api" `Quick @@ fun _ ->
            Alcotest.(check bool) "Got account from Alpaca" account true );
        ] );
    ]
