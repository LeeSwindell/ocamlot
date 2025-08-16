open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_common

let test_types _switch () =
  let order_id = "order-123" in
  let instrument_id = "AAPL" in
  Alcotest.(check string) "order_id" "order-123" order_id;
  Alcotest.(check string) "instrument_id" "AAPL" instrument_id;
  Lwt.return ()

let test_error_handling _switch () =
  let error = Error.Invalid_order "test error" in
  let error_str = Error.error_to_string error in
  Alcotest.(check string) "error message" "Invalid order: test error" error_str;
  Lwt.return ()

let tests = [
  "Types", [
    test_case "basic types" `Quick test_types;
  ];
  "Error", [
    test_case "error_to_string" `Quick test_error_handling;
  ];
]

let () =
  Lwt_main.run (run "Common Tests" tests)