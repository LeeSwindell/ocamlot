open Base
open Alcotest_lwt
open Ocamlot_nats

let test_protocol_messages _switch () =
  Stdio.printf "Testing protocol message building...\n%!";
  let connect_msg = Nats.Protocol.build_connect_message Nats.Protocol.default_connect_options in
  Stdio.printf "  CONNECT message: %s\n%!" (String.escaped connect_msg);
  Alcotest.(check bool) "connect message has CONNECT" true 
    (String.is_prefix connect_msg ~prefix:"CONNECT ");
  
  let pub_msg = Nats.Protocol.build_pub_message ~subject:"test.subject" (Bytes.of_string "hello") in
  Stdio.printf "  PUB message: %s\n%!" (String.escaped pub_msg);
  Alcotest.(check bool) "pub message has PUB" true
    (String.is_prefix pub_msg ~prefix:"PUB test.subject");
  
  Lwt.return_unit

let test_client_creation _switch () =
  Stdio.printf "Testing client creation..\n%!";
  let client = Nats.create () in
  Stdio.printf "  Client created, connected: %b\n%!" (Nats.is_connected client);
  Alcotest.(check bool) "client not connected initially" false (Nats.is_connected client);
  Lwt.return_unit

let test_message_parsing _switch () =
  Stdio.printf "Testing message parsing...\n%!";
  let test_line = "MSG test.subject 1 5" in
  Stdio.printf "  Parsing: %s\n%!" test_line;
  let (subject, sid, reply_to, size) = Nats.Protocol.parse_message_line test_line in
  Stdio.printf "  Parsed - subject: %s, sid: %s, reply_to: %s, size: %d\n%!" 
    subject sid (Option.value reply_to ~default:"<none>") size;
  Alcotest.(check string) "subject parsed" "test.subject" subject;
  Alcotest.(check string) "sid parsed" "1" sid;
  Alcotest.(check (option string)) "no reply_to" None reply_to;
  Alcotest.(check int) "size parsed" 5 size;
  Lwt.return_unit

let test_subscription_ids _switch () =
  Stdio.printf "Testing subscription ID generation...\n%!";
  (* Test that SID generation works properly *)
  let conn = Nats.Connection.create () in
  let (sid1, conn') = Nats.Connection.generate_sid conn in
  let (sid2, _conn'') = Nats.Connection.generate_sid conn' in
  Stdio.printf "  Generated SIDs: %s, %s\n%!" sid1 sid2;
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
  Stdio.printf "\n=== Starting NATS Client Tests ===\n%!";
  let start_time = Unix.gettimeofday () in
  let result = Lwt_main.run (run "NATS Client Tests" tests) in
  let end_time = Unix.gettimeofday () in
  Stdio.printf "\n=== Test Summary ===\n%!";
  Stdio.printf "Total execution time: %.3fs\n%!" (end_time -. start_time);
  Stdio.printf "Test suite: %s\n%!" "NATS Client Tests";
  Stdio.printf "Total test cases: 4\n%!";
  result