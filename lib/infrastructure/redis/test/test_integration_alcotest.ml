open Lwt.Syntax
open Alcotest
open Alcotest_lwt

module Client = Ocamlot_infrastructure_redis.Client
module Connection = Ocamlot_infrastructure_redis.Connection

(* Helper to check if string contains substring *)
  let rec contains_s haystack needle =
    let len_h = String.length haystack in
    let len_n = String.length needle in
    if len_n > len_h then false
    else if String.sub haystack 0 len_n = needle then true
    else if len_h = len_n then false
    else contains_s (String.sub haystack 1 (len_h - 1)) needle
    
(* Helper to show client errors *)
let show_client_error = function
  | Client.Connection_error e -> "Connection: " ^ Connection.show_connection_error e
  | Client.Pool_exhausted -> "Pool exhausted"
  | Client.Redis_error msg -> "Redis error: " ^ msg
  | Client.Parse_error msg -> "Parse error: " ^ msg

let test_config = {
  Client.host = "127.0.0.1";
  port = 6379;
  pool_size = 5;
  connection_timeout = 5.0;
}

(* Custom testable for client results *)
let client_result_testable typ = 
  let pp_error ppf e = Format.fprintf ppf "Error: %s" (show_client_error e) in
  result typ (testable pp_error (=))

let string_option_testable = option string
let int_testable = int
let bool_testable = bool
let string_testable = string

(* =============================================================================
   INTEGRATION TEST CASES
   ============================================================================= *)

let test_ping _switch () =
  let* result = Client.with_client test_config (fun client ->
    Client.ping client
  ) in
  match result with
  | Ok response ->
      check string_testable "PING response should be PONG" "PONG" response;
      Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "PING command failed: %s" (show_client_error e))

let test_string_operations _switch () = 
  let* result = Client.with_client test_config (fun client ->
    (* Test SET *)
    let* set_result = Client.set client "alcotest_key" "alcotest_value" in
    (match set_result with
    | Error e -> Lwt.return (Error e)
    | Ok () ->
        (* Test GET existing key *)
        let* get_result = Client.get client "alcotest_key" in
        (match get_result with
        | Error e -> Lwt.return (Error e)
        | Ok value ->
            check string_option_testable "GET should return set value" (Some "alcotest_value") value;
            
            (* Test GET non-existent key *)
            let* get_none_result = Client.get client "non_existent_alcotest_key" in
            (match get_none_result with
            | Error e -> Lwt.return (Error e)
            | Ok none_value ->
                check string_option_testable "GET non-existent should return None" None none_value;
                
                (* Test DEL *)
                let* del_result = Client.del client ["alcotest_key"] in
                (match del_result with
                | Error e -> Lwt.return (Error e)
                | Ok count ->
                    check int_testable "DEL should delete 1 key" 1 count;
                    Lwt.return (Ok ())))))
  ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e -> fail (Printf.sprintf "String operations failed: %s" (show_client_error e))

let test_hash_operations _switch () =
  let* result = Client.with_client test_config (fun client ->
    (* Test HSET *)
    let* hset_result = Client.hset client "alcotest_hash" "field1" "value1" in
    (match hset_result with
    | Error e -> Lwt.return (Error e)
    | Ok is_new ->
        check bool_testable "HSET should create new field" true is_new;
        
        (* Test HGET *)
        let* hget_result = Client.hget client "alcotest_hash" "field1" in
        (match hget_result with
        | Error e -> Lwt.return (Error e)
        | Ok value ->
            check string_option_testable "HGET should return set value" (Some "value1") value;
            
            (* Test HGET non-existent field *)
            let* hget_none_result = Client.hget client "alcotest_hash" "non_existent_field" in
            (match hget_none_result with
            | Error e -> Lwt.return (Error e)
            | Ok none_value ->
                check string_option_testable "HGET non-existent field should return None" None none_value;
                
                (* Clean up *)
                let* _ = Client.del client ["alcotest_hash"] in
                Lwt.return (Ok ()))))
  ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e -> fail (Printf.sprintf "Hash operations failed: %s" (show_client_error e))

let test_list_operations _switch () =
  let* result = Client.with_client test_config (fun client ->
    (* Test LPUSH *)
    let* lpush_result = Client.lpush client "alcotest_list" ["item1"; "item2"; "item3"] in
    (match lpush_result with
    | Error e -> Lwt.return (Error e)
    | Ok count ->
        check int_testable "LPUSH should add 3 items" 3 count;
        
        (* Test RPOP *)
        let* rpop_result = Client.rpop client "alcotest_list" in
        (match rpop_result with
        | Error e -> Lwt.return (Error e)
        | Ok value ->
            check string_option_testable "RPOP should return first pushed item" (Some "item1") value;
            
            (* Test RPOP when list has remaining items *)
            let* rpop_result2 = Client.rpop client "alcotest_list" in
            (match rpop_result2 with
            | Error e -> Lwt.return (Error e)
            | Ok value2 ->
                check string_option_testable "Second RPOP should return second item" (Some "item2") value2;
                
                (* Clean up *)
                let* _ = Client.del client ["alcotest_list"] in
                Lwt.return (Ok ()))))
  ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e -> fail (Printf.sprintf "List operations failed: %s" (show_client_error e))

let test_info_command _switch () =
  let* result = Client.with_client test_config (fun client ->
    Client.info client
  ) in
  match result with
  | Ok info_text ->
      check bool_testable "INFO should return non-empty string" true (String.length info_text > 0);
      (* Check for common Redis INFO fields *)
      let contains_redis_version = contains_s info_text "redis_version" in
      check bool_testable "INFO should contain redis_version" true contains_redis_version;
      Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "INFO command failed: %s" (show_client_error e))


(* =============================================================================
   CONDITIONAL TEST SUITE DEFINITION
   ============================================================================= *)

let check_redis_available () =
  match Sys.getenv_opt "REDIS_URL" with
  | Some _ -> true
  | None ->
      (* Also check if Redis is available on default port *)
      try
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 6379) in
        Unix.connect sock addr;
        Unix.close sock;
        true
      with
      | _ -> false

let integration_ping_tests = 
  if check_redis_available () then [
    test_case "ping command" `Quick test_ping;
  ] else [
    test_case "ping command (skipped - no Redis)" `Quick (fun _switch () ->
      Printf.printf "Skipping integration test: Redis not available\n";
      Lwt.return_unit)
  ]

let integration_string_tests = 
  if check_redis_available () then [
    test_case "string operations" `Quick test_string_operations;
  ] else [
    test_case "string operations (skipped - no Redis)" `Quick (fun _switch () ->
      Printf.printf "Skipping integration test: Redis not available\n";
      Lwt.return_unit)
  ]

let integration_hash_tests = 
  if check_redis_available () then [
    test_case "hash operations" `Quick test_hash_operations;
  ] else [
    test_case "hash operations (skipped - no Redis)" `Quick (fun _switch () ->
      Printf.printf "Skipping integration test: Redis not available\n";  
      Lwt.return_unit)
  ]

let integration_list_tests = 
  if check_redis_available () then [
    test_case "list operations" `Quick test_list_operations;
  ] else [
    test_case "list operations (skipped - no Redis)" `Quick (fun _switch () ->
      Printf.printf "Skipping integration test: Redis not available\n";
      Lwt.return_unit)
  ]

let integration_info_tests = 
  if check_redis_available () then [
    test_case "info command" `Quick test_info_command;
  ] else [
    test_case "info command (skipped - no Redis)" `Quick (fun _switch () ->
      Printf.printf "Skipping integration test: Redis not available\n";
      Lwt.return_unit)
  ]

(* Combined integration tests *)
let all_integration_tests =
  integration_ping_tests @ integration_string_tests @ integration_hash_tests @ 
  integration_list_tests @ integration_info_tests