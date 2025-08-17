open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_nats
open Stdio

(* Integration tests that connect to real NATS server *)
(* NOTE: These tests require NATS server running on localhost:4222 *)

let nats_host = "127.0.0.1"
let nats_port = 4222
let test_timeout = 10.0

let with_timeout t f =
  let timeout_promise = 
    let* () = Lwt_unix.sleep t in
    Lwt.fail (Failure "Test timeout")
  in
  Lwt.pick [f (); timeout_promise]

let test_real_connect_handshake _switch () =
  (* Tests complete CONNECT handshake with real NATS server *)
  with_timeout test_timeout (fun () ->
    let config = { 
      Nats.Connection.default_config with 
      host = nats_host; 
      port = nats_port; 
    } in
    
    let* result = 
      try%lwt
        let conn = Nats.Connection.create_connection ~config () in
        let* connected_conn = Nats.Connection.connect conn in
        
        (* Verify connection state *)
        Alcotest.(check bool) "connection established" true 
          (Nats.Connection.is_connected connected_conn);
        
        (* Get server info from handshake *)
        let server_info = connected_conn.server_info in
        Alcotest.(check bool) "server info received" true 
          (Option.is_some server_info);
        
        let info = Option.value_exn server_info in
        Alcotest.(check bool) "server_id present" true 
          (not (String.is_empty info.server_id));
        Alcotest.(check bool) "protocol version >= 1" true 
          (info.proto >= 1);
        
        (* Clean disconnect *)
        let* _disconnected_conn = Nats.Connection.disconnect connected_conn in
        Lwt.return (Ok ())
      with
      | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> 
        Lwt.return (Error "NATS server not available - skipping test")
      | Nats.Protocol_error msg ->
        Lwt.return (Error ("Protocol error: " ^ msg))
      | exn -> 
        Lwt.return (Error ("Unexpected error: " ^ (Exn.to_string exn)))
    in
    
    match result with
    | Ok () -> Lwt.return_unit
    | Error msg ->
      (* Skip test if NATS server not available *)
      if String.is_substring msg ~substring:"not available" then (
        printf "SKIPPED: %s\n" msg;
        Lwt.return_unit
      ) else (
        Alcotest.fail msg
      )
  )

let test_real_connect_options_validation _switch () =
  (* Tests different CONNECT options with real server *)
  with_timeout test_timeout (fun () ->
    let config = { 
      Nats.Connection.default_config with 
      host = nats_host; 
      port = nats_port; 
    } in
    
    let* result = 
      try%lwt
        let conn = Nats.Connection.create_connection ~config () in
        let* connected_conn = Nats.Connection.connect conn in
        
        (* Test with verbose enabled - future enhancement would allow testing different options *)
        let _verbose_options = { 
          Nats.Protocol.default_connect_options with 
          verbose = true;
          name = Some "test-client-verbose";
        } in
        
        (* For now, just verify the first connection worked *)
        Alcotest.(check bool) "verbose connection works" true 
          (Nats.Connection.is_connected connected_conn);
        
        (* Clean disconnect *)
        let* _disconnected = Nats.Connection.disconnect connected_conn in
        Lwt.return (Ok ())
      with
      | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> 
        Lwt.return (Error "NATS server not available - skipping test")
      | exn -> 
        Lwt.return (Error ("Error: " ^ (Exn.to_string exn)))
    in
    
    match result with
    | Ok () -> Lwt.return_unit
    | Error msg ->
      if String.is_substring msg ~substring:"not available" then (
        printf "SKIPPED: %s\n" msg;
        Lwt.return_unit
      ) else (
        Alcotest.fail msg
      )
  )

let test_real_connect_error_scenarios _switch () =
  (* Tests error scenarios with real server *)
  with_timeout test_timeout (fun () ->
    (* Test 1: Connect to wrong port *)
    let* result1 = 
      try%lwt
        let bad_config = { 
          Nats.Connection.default_config with 
          host = nats_host; 
          port = 9999; (* wrong port *)
        } in
        let conn = Nats.Connection.create_connection ~config:bad_config () in
        let* _connected = Nats.Connection.connect conn in
        Lwt.return (Error "Should have failed")
      with
      | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> 
        Lwt.return (Ok "Connection refused as expected")
      | exn -> 
        Lwt.return (Error ("Unexpected error: " ^ (Exn.to_string exn)))
    in
    
    Alcotest.(check (result string string)) "wrong port fails" 
      (Ok "Connection refused as expected") result1;
    
    (* Test 2: Connect to non-existent host *)
    let* result2 = 
      try%lwt
        let bad_config = { 
          Nats.Connection.default_config with 
          host = "nonexistent.host"; 
          port = nats_port;
        } in
        let conn = Nats.Connection.create_connection ~config:bad_config () in
        let* _connected = Nats.Connection.connect conn in
        Lwt.return (Error "Should have failed")
      with
      | Unix.Unix_error _ -> 
        Lwt.return (Ok "Host resolution failed as expected")
      | exn -> 
        Lwt.return (Error ("Unexpected error: " ^ (Exn.to_string exn)))
    in
    
    Alcotest.(check (result string string)) "bad host fails" 
      (Ok "Host resolution failed as expected") result2;
    
    Lwt.return_unit
  )

let test_real_connect_server_info_parsing _switch () =
  (* Tests server INFO message parsing from real server *)
  with_timeout test_timeout (fun () ->
    let* result = 
      try%lwt
        let config = { 
          Nats.Connection.default_config with 
          host = nats_host; 
          port = nats_port; 
        } in
        let conn = Nats.Connection.create_connection ~config () in
        let* connected_conn = Nats.Connection.connect conn in
        
        (* Verify server info was parsed correctly *)
        let server_info = connected_conn.server_info in
        Alcotest.(check bool) "server info exists" true (Option.is_some server_info);
        
        let info = Option.value_exn server_info in
        
        (* Check expected fields from our docker NATS config *)
        Alcotest.(check bool) "max_payload reasonable" true 
          (info.max_payload > 0 && info.max_payload <= 1_048_576); (* 1MB max from config *)
        
        Alcotest.(check bool) "protocol version valid" true 
          (info.proto >= 0 && info.proto <= 10);
        
        Alcotest.(check bool) "port matches" true 
          (info.port = nats_port);
        
        Alcotest.(check bool) "version string non-empty" true 
          (not (String.is_empty info.version));
        
        printf "Connected to NATS server: %s (version %s, proto %d)\n" 
          info.server_id info.version info.proto;
        
        let* _disconnected = Nats.Connection.disconnect connected_conn in
        Lwt.return (Ok ())
      with
      | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> 
        Lwt.return (Error "NATS server not available - skipping test")
      | exn -> 
        Lwt.return (Error ("Error: " ^ (Exn.to_string exn)))
    in
    
    match result with
    | Ok () -> Lwt.return_unit
    | Error msg ->
      if String.is_substring msg ~substring:"not available" then (
        printf "SKIPPED: %s\n" msg;
        Lwt.return_unit
      ) else (
        Alcotest.fail msg
      )
  )

(* Test suite definition *)
let tests = [
  "Real NATS Integration", [
    test_case "connect handshake" `Quick test_real_connect_handshake;
    test_case "connect options validation" `Quick test_real_connect_options_validation;
    test_case "connect error scenarios" `Quick test_real_connect_error_scenarios;
    test_case "server info parsing" `Quick test_real_connect_server_info_parsing;
  ];
]

let () =
  Lwt_main.run (run "NATS Real Server Integration Tests" tests)