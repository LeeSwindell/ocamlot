open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_nats

(* Test helpers *)
let _test_timeout = 5.0

let _with_timeout _t f =
  let timeout_promise = 
    let* () = Lwt_unix.sleep _test_timeout in
    Lwt.fail (Failure "Test timeout")
  in
  Lwt.pick [f (); timeout_promise]

(* Basic Connection Tests *)
let test_basic_connection _switch () =
  (* Tests basic connection creation and initial state *)
  let client = Nats.create () in
  Alcotest.(check bool) "client starts disconnected" false (Nats.is_connected client);
  Lwt.return_unit

let test_connection_options _switch () =
  (* Tests connection with custom configuration *)
  let config : Nats.connection_config = {
    host = "127.0.0.1";
    port = 4222;
    connect_timeout = 10.0;
    reconnect_attempts = 5;
    reconnect_delay = 2.0;
  } in
  let client = Nats.create ~config () in
  let client_config = Nats.get_connection_config client in
  Alcotest.(check string) "host configured" "127.0.0.1" client_config.host;
  Alcotest.(check int) "port configured" 4222 client_config.port;
  Lwt.return_unit

let test_connection_close _switch () =
  (* Tests connection cleanup and double close safety *)
  let client = Nats.create () in
  let* () = 
    try%lwt
      Nats.disconnect client
    with
    | _ -> Lwt.return_unit
  in
  (* Double close should be safe *)
  let* () = 
    try%lwt
      Nats.disconnect client
    with
    | _ -> Lwt.return_unit
  in
  Alcotest.(check bool) "client disconnected after close" false (Nats.is_connected client);
  Lwt.return_unit

(* Protocol Parsing Tests *)
let test_parse_ping_pong _switch () =
  (* Tests PING/PONG protocol message parsing and building *)
  let ping_msg = Nats.Protocol.ping_message in
  let pong_msg = Nats.Protocol.pong_message in
  Alcotest.(check string) "ping message format" "PING\r\n" ping_msg;
  Alcotest.(check string) "pong message format" "PONG\r\n" pong_msg;
  Lwt.return_unit

let test_parse_info _switch () =
  (* Tests server INFO message parsing *)
  let info_json = {|{"server_id":"test-server","version":"2.9.0","proto":1,"host":"localhost","port":4222,"max_payload":1048576}|} in
  let server_info = Nats.Protocol.parse_info_message info_json in
  Alcotest.(check string) "server_id parsed" "test-server" server_info.server_id;
  Alcotest.(check string) "version parsed" "2.9.0" server_info.version;
  Alcotest.(check int) "proto parsed" 1 server_info.proto;
  Alcotest.(check string) "host parsed" "localhost" server_info.host;
  Alcotest.(check int) "port parsed" 4222 server_info.port;
  Alcotest.(check int) "max_payload parsed" 1048576 server_info.max_payload;
  Lwt.return_unit

let test_parse_msg _switch () =
  (* Tests MSG protocol message parsing *)
  let msg_line_simple = "MSG test.subject 123 10" in
  let (subject, sid, reply_to, size) = Nats.Protocol.parse_message_line msg_line_simple in
  Alcotest.(check string) "subject parsed" "test.subject" subject;
  Alcotest.(check string) "sid parsed" "123" sid;
  Alcotest.(check (option string)) "no reply_to" None reply_to;
  Alcotest.(check int) "size parsed" 10 size;
  
  let msg_line_with_reply = "MSG test.subject 123 reply.inbox 15" in
  let (subject2, sid2, reply_to2, size2) = Nats.Protocol.parse_message_line msg_line_with_reply in
  Alcotest.(check string) "subject with reply" "test.subject" subject2;
  Alcotest.(check string) "sid with reply" "123" sid2;
  Alcotest.(check (option string)) "reply_to parsed" (Some "reply.inbox") reply_to2;
  Alcotest.(check int) "size with reply" 15 size2;
  Lwt.return_unit

let test_parse_error _switch () =
  (* Tests protocol error message handling *)
  try
    let _ = Nats.Protocol.parse_message_line "INVALID MSG FORMAT" in
    Alcotest.fail "Should have thrown Invalid_message exception"
  with
  | Nats.Invalid_message _ -> Lwt.return_unit
  | _ -> Alcotest.fail "Wrong exception type"

let test_invalid_protocol _switch () =
  (* Tests handling of invalid protocol messages *)
  try
    let _ = Nats.Protocol.parse_info_message "invalid json" in
    Alcotest.fail "Should have thrown Protocol_error exception"
  with
  | Nats.Protocol_error _ -> Lwt.return_unit
  | _ -> Alcotest.fail "Wrong exception type"

(* Basic Publishing Tests *)
let test_simple_publish _switch () =
  (* Tests basic publishing without server connection *)
  let client = Nats.create () in
  let* result = 
    try%lwt
      let* () = Nats.publish_string client ~subject:"test.subject" "hello world" in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  Alcotest.(check (result unit string)) "publish fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

let test_publish_message_building _switch () =
  (* Tests PUB message construction *)
  let payload = Bytes.of_string "test payload" in
  let pub_msg = Nats.Protocol.build_pub_message ~subject:"test.subject" payload in
  Alcotest.(check bool) "pub message has subject" true 
    (String.is_substring pub_msg ~substring:"test.subject");
  Alcotest.(check bool) "pub message has size" true 
    (String.is_substring pub_msg ~substring:"12");
  Alcotest.(check bool) "pub message starts with PUB" true 
    (String.is_prefix pub_msg ~prefix:"PUB");
  Lwt.return_unit

let test_publish_with_reply _switch () =
  (* Tests publishing with reply-to subject *)
  let payload = Bytes.of_string "test" in
  let pub_msg = Nats.Protocol.build_pub_message ~subject:"test.subject" 
    ~reply_to:"reply.inbox" payload in
  Alcotest.(check bool) "pub message has reply-to" true 
    (String.is_substring pub_msg ~substring:"reply.inbox");
  Lwt.return_unit

(* Basic Subscription Tests *)
let test_subscription_message_building _switch () =
  (* Tests SUB message construction *)
  let sub_msg = Nats.Protocol.build_sub_message ~subject:"test.subject" ~sid:"1" in
  Alcotest.(check string) "sub message format" "SUB test.subject 1\r\n" sub_msg;
  Lwt.return_unit

let test_unsubscribe_message_building _switch () =
  (* Tests UNSUB message construction *)
  let unsub_msg = Nats.Protocol.build_unsub_message ~sid:"1" () in
  Alcotest.(check string) "unsub message format" "UNSUB 1\r\n" unsub_msg;
  
  let unsub_msg_with_max = Nats.Protocol.build_unsub_message ~sid:"1" ~max_msgs:10 () in
  Alcotest.(check string) "unsub message with max" "UNSUB 1 10\r\n" unsub_msg_with_max;
  Lwt.return_unit

let test_simple_subscribe_async _switch () =
  (* Tests async subscription without server connection *)
  let client = Nats.create () in
  let callback _msg = Lwt.return_unit in
  let* result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.subject" ~callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  Alcotest.(check (result unit string)) "subscribe fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

(* Connection State Management Tests *)
let test_connection_status _switch () =
  (* Tests connection state tracking *)
  let client = Nats.create () in
  Alcotest.(check bool) "initial state disconnected" false (Nats.is_connected client);
  
  (* Test that operations fail when not connected *)
  let* pub_result = 
    try%lwt
      let* () = Nats.publish_string client ~subject:"test" "data" in
      Lwt.return false
    with
    | Nats.Not_connected -> Lwt.return true
    | _ -> Lwt.return false
  in
  Alcotest.(check bool) "publish fails when disconnected" true pub_result;
  Lwt.return_unit

(* Request-Reply Pattern Tests *)
let test_request_message_pattern _switch () =
  (* Tests request-reply message structure *)
  let client = Nats.create () in
  let payload = Bytes.of_string "request data" in
  let* result = 
    try%lwt
      let* _response = Nats.request client ~subject:"test.service" ~payload ~timeout:1.0 in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | Nats.Client_error msg -> Lwt.return (Error msg)
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  Alcotest.(check (result unit string)) "request fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

(* Flow Control Tests *)
let test_flush_without_connection _switch () =
  (* Tests flush behavior when not connected *)
  let client = Nats.create () in
  (* For now, we don't have a flush implementation, but we test the client state *)
  Alcotest.(check bool) "client not connected" false (Nats.is_connected client);
  Lwt.return_unit

(* Error Handling Tests *)
let test_bad_subject _switch () =
  (* Tests invalid subject handling *)
  let client = Nats.create () in
  let* result = 
    try%lwt
      let* () = Nats.publish_string client ~subject:"" "data" in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  Alcotest.(check (result unit string)) "empty subject handled" 
    (Error "not connected") result;
  Lwt.return_unit

let test_nil_connection _switch () =
  (* Tests null/invalid connection handling *)
  (* In OCaml, we can't have null connections, but we test uninitialized state *)
  let client = Nats.create () in
  Alcotest.(check bool) "client properly initialized" false (Nats.is_connected client);
  Lwt.return_unit

(* Advanced Protocol Tests *)
let test_connect_message_building _switch () =
  (* Tests CONNECT message construction with options *)
  let options = Nats.Protocol.default_connect_options in
  let connect_msg = Nats.Protocol.build_connect_message options in
  Alcotest.(check bool) "connect message format" true 
    (String.is_prefix connect_msg ~prefix:"CONNECT ");
  Alcotest.(check bool) "connect message has json" true 
    (String.is_substring connect_msg ~substring:"{");
  Alcotest.(check bool) "connect message ends with crlf" true 
    (String.is_suffix connect_msg ~suffix:"\r\n");
  Lwt.return_unit

let test_connect_message_required_fields _switch () =
  (* Tests that CONNECT message contains all required NATS protocol fields *)
  let options = Nats.Protocol.default_connect_options in
  let connect_msg = Nats.Protocol.build_connect_message options in
  
  (* Extract JSON part between CONNECT and \r\n *)
  let json_start = String.length "CONNECT " in
  let json_end = String.length connect_msg - 2 in
  let json_str = String.sub connect_msg ~pos:json_start ~len:(json_end - json_start) in
  
  (* Parse JSON and check required fields *)
  let json = Yojson.Safe.from_string json_str in
  
  (* Check required boolean fields are present and correct *)
  Alcotest.(check bool) "verbose field present" true 
    (match Yojson.Safe.Util.member "verbose" json with | `Bool false -> true | _ -> false);
  Alcotest.(check bool) "pedantic field present" true 
    (match Yojson.Safe.Util.member "pedantic" json with | `Bool false -> true | _ -> false);
  Alcotest.(check bool) "tls_required field present" true 
    (match Yojson.Safe.Util.member "tls_required" json with | `Bool false -> true | _ -> false);
  
  (* Check required string fields *)
  Alcotest.(check bool) "lang field present" true 
    (match Yojson.Safe.Util.member "lang" json with | `String "ocaml" -> true | _ -> false);
  Alcotest.(check bool) "version field present" true 
    (match Yojson.Safe.Util.member "version" json with | `String "1.0.0" -> true | _ -> false);
  
  (* Check optional fields with default values *)
  Alcotest.(check bool) "protocol field present" true 
    (match Yojson.Safe.Util.member "protocol" json with | `Int 1 -> true | _ -> false);
  Alcotest.(check bool) "echo field present" true 
    (match Yojson.Safe.Util.member "echo" json with | `Bool false -> true | _ -> false);
  Alcotest.(check bool) "headers field present" true 
    (match Yojson.Safe.Util.member "headers" json with | `Bool false -> true | _ -> false);
  Alcotest.(check bool) "no_responders field present" true 
    (match Yojson.Safe.Util.member "no_responders" json with | `Bool false -> true | _ -> false);
  
  Lwt.return_unit

let test_connect_message_auth_fields _switch () =
  (* Tests CONNECT message with authentication fields *)
  let auth_options = { 
    Nats.Protocol.default_connect_options with 
    auth_token = Some "test-token-123";
    user = Some "testuser";
    pass = Some "testpass";
  } in
  let connect_msg = Nats.Protocol.build_connect_message auth_options in
  
  (* Extract and parse JSON *)
  let json_start = String.length "CONNECT " in
  let json_end = String.length connect_msg - 2 in
  let json_str = String.sub connect_msg ~pos:json_start ~len:(json_end - json_start) in
  let json = Yojson.Safe.from_string json_str in
  
  (* Check auth fields are present *)
  Alcotest.(check bool) "auth_token field present" true 
    (match Yojson.Safe.Util.member "auth_token" json with | `String "test-token-123" -> true | _ -> false);
  Alcotest.(check bool) "user field present" true 
    (match Yojson.Safe.Util.member "user" json with | `String "testuser" -> true | _ -> false);
  Alcotest.(check bool) "pass field present" true 
    (match Yojson.Safe.Util.member "pass" json with | `String "testpass" -> true | _ -> false);
  
  Lwt.return_unit

let test_connect_message_tls_required _switch () =
  (* Tests CONNECT message with TLS required *)
  let tls_options = { 
    Nats.Protocol.default_connect_options with 
    tls_required = true;
  } in
  let connect_msg = Nats.Protocol.build_connect_message tls_options in
  
  (* Extract and parse JSON *)
  let json_start = String.length "CONNECT " in
  let json_end = String.length connect_msg - 2 in
  let json_str = String.sub connect_msg ~pos:json_start ~len:(json_end - json_start) in
  let json = Yojson.Safe.from_string json_str in
  
  (* Check TLS required field *)
  Alcotest.(check bool) "tls_required field is true" true 
    (match Yojson.Safe.Util.member "tls_required" json with | `Bool true -> true | _ -> false);
  
  Lwt.return_unit

let test_connect_message_nkey_jwt _switch () =
  (* Tests CONNECT message with NKey and JWT authentication *)
  let nkey_options = { 
    Nats.Protocol.default_connect_options with 
    nkey = Some "UAFK5VHZRDPNMHYWN6GDQZ5AKEYIJZJHBR7AIVHQHVW6TNIIDFJNQZMS";
    jwt = Some "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.test.jwt";
    signature = Some "test-signature-here";
  } in
  let connect_msg = Nats.Protocol.build_connect_message nkey_options in
  
  (* Extract and parse JSON *)
  let json_start = String.length "CONNECT " in
  let json_end = String.length connect_msg - 2 in
  let json_str = String.sub connect_msg ~pos:json_start ~len:(json_end - json_start) in
  let json = Yojson.Safe.from_string json_str in
  
  (* Check NKey and JWT fields *)
  Alcotest.(check bool) "nkey field present" true 
    (match Yojson.Safe.Util.member "nkey" json with | `String _ -> true | _ -> false);
  Alcotest.(check bool) "jwt field present" true 
    (match Yojson.Safe.Util.member "jwt" json with | `String _ -> true | _ -> false);
  Alcotest.(check bool) "signature field present" true 
    (match Yojson.Safe.Util.member "signature" json with | `String _ -> true | _ -> false);
  
  Lwt.return_unit

let test_server_info_extraction _switch () =
  (* Tests server information parsing and storage *)
  let client = Nats.create () in
  let server_info = Nats.get_server_info client in
  Alcotest.(check (option string)) "no server info when not connected" None
    (Option.map server_info ~f:(fun info -> info.server_id));
  Lwt.return_unit

(* Test suite definition *)
let tests = [
  "Connection Core", [
    test_case "basic connection" `Quick test_basic_connection;
    test_case "connection options" `Quick test_connection_options;
    test_case "connection close" `Quick test_connection_close;
    test_case "connection status" `Quick test_connection_status;
  ];
  "Protocol Parsing", [
    test_case "ping pong messages" `Quick test_parse_ping_pong;
    test_case "info message parsing" `Quick test_parse_info;
    test_case "msg message parsing" `Quick test_parse_msg;
    test_case "error message handling" `Quick test_parse_error;
    test_case "invalid protocol handling" `Quick test_invalid_protocol;
  ];
  "Basic Publishing", [
    test_case "simple publish" `Quick test_simple_publish;
    test_case "publish message building" `Quick test_publish_message_building;
    test_case "publish with reply" `Quick test_publish_with_reply;
  ];
  "Basic Subscribing", [
    test_case "subscription message building" `Quick test_subscription_message_building;
    test_case "unsubscribe message building" `Quick test_unsubscribe_message_building;
    test_case "simple async subscribe" `Quick test_simple_subscribe_async;
  ];
  "Request-Reply", [
    test_case "request message pattern" `Quick test_request_message_pattern;
  ];
  "Flow Control", [
    test_case "flush without connection" `Quick test_flush_without_connection;
  ];
  "Error Handling", [
    test_case "bad subject" `Quick test_bad_subject;
    test_case "nil connection" `Quick test_nil_connection;
  ];
  "Advanced Protocol", [
    test_case "connect message building" `Quick test_connect_message_building;
    test_case "connect message required fields" `Quick test_connect_message_required_fields;
    test_case "connect message auth fields" `Quick test_connect_message_auth_fields;
    test_case "connect message tls required" `Quick test_connect_message_tls_required;
    test_case "connect message nkey jwt" `Quick test_connect_message_nkey_jwt;
    test_case "server info extraction" `Quick test_server_info_extraction;
  ];
]

let () =
  Lwt_main.run (run "NATS Core Functionality Tests" tests)