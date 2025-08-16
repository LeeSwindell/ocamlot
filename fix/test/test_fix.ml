open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_common
open Ocamlot_fix

let test_fix_message_encoding _switch () =
  let msg = Protocol.{
    msg_type = NewOrderSingle;
    fields = [("11", "order-123"); ("55", "AAPL"); ("54", "1")];
  } in
  let encoded = Protocol.encode_message msg in
  Alcotest.(check bool) "contains message type" true (String.is_substring encoded ~substring:"35=D");
  Alcotest.(check bool) "contains order id" true (String.is_substring encoded ~substring:"11=order-123");
  Lwt.return ()

let test_fix_field_encoding _switch () =
  let field = ("35", "D") in
  let encoded = Protocol.encode_field field in
  Alcotest.(check string) "field encoding" ("35=D" ^ Protocol.fix_delimiter) encoded;
  Lwt.return ()

let tests = [
  "Protocol", [
    test_case "message encoding" `Quick test_fix_message_encoding;
    test_case "field encoding" `Quick test_fix_field_encoding;
  ];
]

let () =
  Lwt_main.run (run "FIX Tests" tests)