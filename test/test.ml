let bad_time () =
  let _ = Longleaf_lib.Time.of_string "2025-03-03T16:59:00" in
  ()

let good_time () =
  let _ = Longleaf_lib.Time.of_string "2025-03-03T16:59:00Z" in
  ()

let () =
  Alcotest.run "Utils"
    [
      ( "time",
        [
          Alcotest.test_case "No timezone" `Quick bad_time;
          Alcotest.test_case "With timezone" `Quick good_time;
          (* Alcotest.test_case "Lower case" `Quick test_lowercase; *)
          (* Alcotest.test_case "Capitalization" `Quick test_capitalize; *)
        ] );
      (* ( "string-concat", *)
      (*   [ Alcotest.test_case "String mashing" `Quick test_str_concat ] ); *)
      (* ( "list-concat", *)
      (*   [ Alcotest.test_case "List mashing" `Slow test_list_concat ] ); *)
    ]
