open Base
open Alcotest_lwt
open Ocamlot_nats

(* CONNECT Protocol Compliance Tests - Focus on edge cases and protocol requirements *)

let test_connect_minimal_required_fields _switch () =
  (* Tests minimal CONNECT message with only required fields *)
  let minimal_options = {
    Nats.Protocol.verbose = false;
    pedantic = false;
    tls_required = false;
    auth_token = None;
    user = None;
    pass = None;
    name = None;
    lang = "ocaml";
    version = "1.0.0";
    protocol = None;
    echo = None;
    signature = None;
    jwt = None;
    no_responders = None;
    headers = None;
    nkey = None;
  } in
  let connect_msg = Nats.Protocol.build_connect_message minimal_options in
  
  (* Extract and parse JSON *)
  let json_start = String.length "CONNECT " in
  let json_end = String.length connect_msg - 2 in
  let json_str = String.sub connect_msg ~pos:json_start ~len:(json_end - json_start) in
  let json = Yojson.Safe.from_string json_str in
  
  (* Verify required fields are present *)
  Alcotest.(check bool) "verbose required field" true 
    (match Yojson.Safe.Util.member "verbose" json with | `Bool false -> true | _ -> false);
  Alcotest.(check bool) "pedantic required field" true 
    (match Yojson.Safe.Util.member "pedantic" json with | `Bool false -> true | _ -> false);
  Alcotest.(check bool) "tls_required required field" true 
    (match Yojson.Safe.Util.member "tls_required" json with | `Bool false -> true | _ -> false);
  Alcotest.(check bool) "lang required field" true 
    (match Yojson.Safe.Util.member "lang" json with | `String "ocaml" -> true | _ -> false);
  Alcotest.(check bool) "version required field" true 
    (match Yojson.Safe.Util.member "version" json with | `String "1.0.0" -> true | _ -> false);
  
  (* Verify optional fields are absent when None *)
  Alcotest.(check bool) "name field absent when None" true 
    (match Yojson.Safe.Util.member "name" json with | `Null -> true | _ -> false);
  
  Lwt.return_unit

let test_connect_auth_combinations _switch () =
  (* Tests different authentication combinations *)
  
  (* Test 1: Token auth only *)
  let token_options = { 
    Nats.Protocol.default_connect_options with 
    auth_token = Some "secret-token";
    user = None;
    pass = None;
  } in
  let token_msg = Nats.Protocol.build_connect_message token_options in
  let json_start = String.length "CONNECT " in
  let json_end = String.length token_msg - 2 in
  let json_str = String.sub token_msg ~pos:json_start ~len:(json_end - json_start) in
  let json = Yojson.Safe.from_string json_str in
  
  Alcotest.(check bool) "token auth present" true 
    (match Yojson.Safe.Util.member "auth_token" json with | `String "secret-token" -> true | _ -> false);
  Alcotest.(check bool) "user field null with token auth" true 
    (match Yojson.Safe.Util.member "user" json with | `Null -> true | _ -> false);
  
  (* Test 2: User/Pass auth *)
  let userpass_options = { 
    Nats.Protocol.default_connect_options with 
    auth_token = None;
    user = Some "admin";
    pass = Some "password123";
  } in
  let userpass_msg = Nats.Protocol.build_connect_message userpass_options in
  let json_start2 = String.length "CONNECT " in
  let json_end2 = String.length userpass_msg - 2 in
  let json_str2 = String.sub userpass_msg ~pos:json_start2 ~len:(json_end2 - json_start2) in
  let json2 = Yojson.Safe.from_string json_str2 in
  
  Alcotest.(check bool) "user field present" true 
    (match Yojson.Safe.Util.member "user" json2 with | `String "admin" -> true | _ -> false);
  Alcotest.(check bool) "pass field present" true 
    (match Yojson.Safe.Util.member "pass" json2 with | `String "password123" -> true | _ -> false);
  Alcotest.(check bool) "auth_token null with user/pass" true 
    (match Yojson.Safe.Util.member "auth_token" json2 with | `Null -> true | _ -> false);
  
  Lwt.return_unit

let test_connect_protocol_versions _switch () =
  (* Tests different protocol versions *)
  
  (* Test protocol version 0 (original) *)
  let proto0_options = { 
    Nats.Protocol.default_connect_options with 
    protocol = Some 0;
  } in
  let proto0_msg = Nats.Protocol.build_connect_message proto0_options in
  let json_start = String.length "CONNECT " in
  let json_end = String.length proto0_msg - 2 in
  let json_str = String.sub proto0_msg ~pos:json_start ~len:(json_end - json_start) in
  let json = Yojson.Safe.from_string json_str in
  
  Alcotest.(check bool) "protocol 0" true 
    (match Yojson.Safe.Util.member "protocol" json with | `Int 0 -> true | _ -> false);
  
  (* Test protocol version 1 (with cluster topology) *)
  let proto1_options = { 
    Nats.Protocol.default_connect_options with 
    protocol = Some 1;
  } in
  let proto1_msg = Nats.Protocol.build_connect_message proto1_options in
  let json_start2 = String.length "CONNECT " in
  let json_end2 = String.length proto1_msg - 2 in
  let json_str2 = String.sub proto1_msg ~pos:json_start2 ~len:(json_end2 - json_start2) in
  let json2 = Yojson.Safe.from_string json_str2 in
  
  Alcotest.(check bool) "protocol 1" true 
    (match Yojson.Safe.Util.member "protocol" json2 with | `Int 1 -> true | _ -> false);
  
  Lwt.return_unit

let test_connect_feature_flags _switch () =
  (* Tests various feature flag combinations *)
  let features_options = { 
    Nats.Protocol.default_connect_options with 
    verbose = true;
    pedantic = true;
    echo = Some true;
    headers = Some true;
    no_responders = Some true;
  } in
  let features_msg = Nats.Protocol.build_connect_message features_options in
  let json_start = String.length "CONNECT " in
  let json_end = String.length features_msg - 2 in
  let json_str = String.sub features_msg ~pos:json_start ~len:(json_end - json_start) in
  let json = Yojson.Safe.from_string json_str in
  
  (* Check all boolean flags are true *)
  Alcotest.(check bool) "verbose true" true 
    (match Yojson.Safe.Util.member "verbose" json with | `Bool true -> true | _ -> false);
  Alcotest.(check bool) "pedantic true" true 
    (match Yojson.Safe.Util.member "pedantic" json with | `Bool true -> true | _ -> false);
  Alcotest.(check bool) "echo true" true 
    (match Yojson.Safe.Util.member "echo" json with | `Bool true -> true | _ -> false);
  Alcotest.(check bool) "headers true" true 
    (match Yojson.Safe.Util.member "headers" json with | `Bool true -> true | _ -> false);
  Alcotest.(check bool) "no_responders true" true 
    (match Yojson.Safe.Util.member "no_responders" json with | `Bool true -> true | _ -> false);
  
  Lwt.return_unit

let test_connect_client_identification _switch () =
  (* Tests client identification fields *)
  let client_options = { 
    Nats.Protocol.default_connect_options with 
    name = Some "test-client-name";
    lang = "ocaml";
    version = "2.0.0-beta";
  } in
  let client_msg = Nats.Protocol.build_connect_message client_options in
  let json_start = String.length "CONNECT " in
  let json_end = String.length client_msg - 2 in
  let json_str = String.sub client_msg ~pos:json_start ~len:(json_end - json_start) in
  let json = Yojson.Safe.from_string json_str in
  
  Alcotest.(check bool) "client name" true 
    (match Yojson.Safe.Util.member "name" json with | `String "test-client-name" -> true | _ -> false);
  Alcotest.(check bool) "client lang" true 
    (match Yojson.Safe.Util.member "lang" json with | `String "ocaml" -> true | _ -> false);
  Alcotest.(check bool) "client version" true 
    (match Yojson.Safe.Util.member "version" json with | `String "2.0.0-beta" -> true | _ -> false);
  
  Lwt.return_unit

let test_connect_security_scenarios _switch () =
  (* Tests security-related scenarios *)
  
  (* Test TLS required with NKey authentication *)
  let secure_options = { 
    Nats.Protocol.default_connect_options with 
    tls_required = true;
    nkey = Some "UAFK5VHZRDPNMHYWN6GDQZ5AKEYIJZJHBR7AIVHQHVW6TNIIDFJNQZMS";
    jwt = Some "eyJ0eXAiOiJKV1QiLCJhbGciOiJFZDI1NTE5LW5rZXkifQ.eyJqdGkiOiJTWjZSTkY3VUhLWkw1R0Q3VTJWQzVZSlJKMkI0VUlEQkVaRllNTVdVTkVDNkNTRlMiLCJpYXQiOjE2MzE5ODQ2NTQsImlzcyI6IkFEWlRBUFNTQklCRFhOM0FKNjI1VkVZUkdWRktWUERNT1lOWUIzRjNUS1NPTTJYUU9JQ1VGWU5CIiwibmFtZSI6InRlc3QtdXNlciIsInN1YiI6IlVBRks1VkhackhQTk1IWVdONkdEUVo1QUtFWUlKWkpIQlI3QUlWSFFIVlc2VE5JSURGSk5RWk1TIiwidHlwZSI6InVzZXIifQ.test-signature";
    signature = Some "test-ed25519-signature";
  } in
  let secure_msg = Nats.Protocol.build_connect_message secure_options in
  let json_start = String.length "CONNECT " in
  let json_end = String.length secure_msg - 2 in
  let json_str = String.sub secure_msg ~pos:json_start ~len:(json_end - json_start) in
  let json = Yojson.Safe.from_string json_str in
  
  Alcotest.(check bool) "tls required with nkey" true 
    (match Yojson.Safe.Util.member "tls_required" json with | `Bool true -> true | _ -> false);
  Alcotest.(check bool) "nkey present" true 
    (match Yojson.Safe.Util.member "nkey" json with | `String _ -> true | _ -> false);
  Alcotest.(check bool) "jwt present" true 
    (match Yojson.Safe.Util.member "jwt" json with | `String _ -> true | _ -> false);
  Alcotest.(check bool) "signature present" true 
    (match Yojson.Safe.Util.member "signature" json with | `String _ -> true | _ -> false);
  
  Lwt.return_unit

let test_connect_message_format_compliance _switch () =
  (* Tests CONNECT message format compliance with NATS protocol *)
  let options = Nats.Protocol.default_connect_options in
  let connect_msg = Nats.Protocol.build_connect_message options in
  
  (* Test message structure *)
  Alcotest.(check bool) "starts with CONNECT" true 
    (String.is_prefix connect_msg ~prefix:"CONNECT ");
  Alcotest.(check bool) "ends with CRLF" true 
    (String.is_suffix connect_msg ~suffix:"\r\n");
  
  (* Test JSON is valid *)
  let json_start = String.length "CONNECT " in
  let json_end = String.length connect_msg - 2 in
  let json_str = String.sub connect_msg ~pos:json_start ~len:(json_end - json_start) in
  
  (* This should not throw an exception *)
  let _json = Yojson.Safe.from_string json_str in
  
  (* Test message length is reasonable (not empty, not too long) *)
  Alcotest.(check bool) "reasonable message length" true 
    (String.length connect_msg > 20 && String.length connect_msg < 2000);
  
  Lwt.return_unit

(* Test suite definition *)
let tests = [
  "CONNECT Compliance", [
    test_case "minimal required fields" `Quick test_connect_minimal_required_fields;
    test_case "auth combinations" `Quick test_connect_auth_combinations;
    test_case "protocol versions" `Quick test_connect_protocol_versions;
    test_case "feature flags" `Quick test_connect_feature_flags;
    test_case "client identification" `Quick test_connect_client_identification;
    test_case "security scenarios" `Quick test_connect_security_scenarios;
    test_case "message format compliance" `Quick test_connect_message_format_compliance;
  ];
]

let () =
  Lwt_main.run (run "NATS CONNECT Protocol Compliance Tests" tests)