open Base
open Alcotest_lwt
open Ocamlot_nats

let test_protocol_messages _switch () =
  let connect_msg = Nats.Protocol.build_connect_message Nats.Protocol.default_connect_options in
  Alcotest.(check bool) "connect message has CONNECT" true 
    (String.is_prefix connect_msg ~prefix:"CONNECT ");
  
  let pub_msg = Nats.Protocol.build_pub_message ~subject:"test.subject" (Bytes.of_string "hello") in
  Alcotest.(check bool) "pub message has PUB" true
    (String.is_prefix pub_msg ~prefix:"PUB test.subject");
  
  Lwt.return_unit

let test_client_creation _switch () =
  let client = Nats.create () in
  Alcotest.(check bool) "client not connected initially" false (Nats.is_connected client);
  Lwt.return_unit

let test_message_parsing _switch () =
  let test_line = "MSG test.subject 1 5" in
  let (subject, sid, reply_to, size) = Nats.Protocol.parse_message_line test_line in
  Alcotest.(check string) "subject parsed" "test.subject" subject;
  Alcotest.(check string) "sid parsed" "1" sid;
  Alcotest.(check (option string)) "no reply_to" None reply_to;
  Alcotest.(check int) "size parsed" 5 size;
  Lwt.return_unit

let test_subscription_ids _switch () =
  let conn = Nats.Connection.create_connection () in
  let sid1 = Nats.Connection.generate_sid conn in
  let sid2 = Nats.Connection.generate_sid conn in
  Alcotest.(check bool) "different sids" true (not (String.equal sid1 sid2));
  Lwt.return_unit

(* Note: These tests don't require a real NATS server *)
let tests = [
  "Protocol", [
    test_case "message building" `Quick test_protocol_messages;
    test_case "message parsing" `Quick test_message_parsing;
  ];
  "Client", [
    test_case "client creation" `Quick test_client_creation;
    test_case "subscription IDs" `Quick test_subscription_ids;
  ];
]

let () =
  Lwt_main.run (run "NATS Client Tests" tests)