open Lwt.Syntax
open Alcotest
open Alcotest_lwt
module Client = Ocamlot_infrastructure_redis.Client
module Connection = Ocamlot_infrastructure_redis.Connection
module Resp3 = Ocamlot_infrastructure_redis.Resp3

(* Pipeline test helpers *)
type test_state = {
  client: Client.t;
  stream_ids: string list;
  entries: (string * (string * string) list) list;
}

type test_step = test_state -> (unit * test_state, Client.client_error) result Lwt.t

let (>>=?) m f =
  let* result = m in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (value, state) -> f value state

let return_ok value state = Lwt.return (Ok (value, state))

let run_test_pipeline client steps =
  let initial_state = {client; stream_ids = []; entries = []} in
  let rec run state = function
    | [] -> Lwt.return (Ok ((), state))
    | step :: rest ->
        let* result = step state in
        match result with
        | Error e -> Lwt.return (Error e)
        | Ok (_, next_state) -> run next_state rest
  in
  run initial_state steps

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
  | Client.Connection_error e ->
      "Connection: " ^ Connection.show_connection_error e
  | Client.Pool_exhausted -> "Pool exhausted"
  | Client.Redis_error msg -> "Redis error: " ^ msg
  | Client.Parse_error msg -> "Parse error: " ^ msg

(* Pipeline test step functions *)
let setup_clean_stream stream_name state =
  let* result = Client.del state.client [stream_name] in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok _ -> return_ok () state

let test_xlen_empty stream_name expected_count state =
  let* result = Client.xlen state.client stream_name in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok count ->
      check int ("XLEN on empty " ^ stream_name) expected_count count ;
      return_ok () state

let test_xrange_empty stream_name state =
  let* result = Client.xrange state.client stream_name "-" "+" () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok entries ->
      check (list (pair string (list (pair string string)))) 
        ("XRANGE on empty " ^ stream_name) [] entries ;
      return_ok () state

let add_stream_entry stream_name fields state =
  let xadd_cmd = Ocamlot_infrastructure_redis.Commands.xadd stream_name "*" fields () in
  let* result = Client.execute state.client xadd_cmd in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString (Some entry_id)) ->
      let new_entry = (entry_id, fields) in
      let updated_state = {state with entries = new_entry :: state.entries} in
      return_ok () updated_state
  | Ok _ -> Lwt.return (Error (Client.Parse_error "Expected BulkString ID from XADD"))

let test_xlen_populated stream_name expected_count state =
  let* result = Client.xlen state.client stream_name in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok count ->
      check int ("XLEN on populated " ^ stream_name) expected_count count ;
      return_ok () state

let test_xrange_all stream_name expected_count state =
  let* result = Client.xrange state.client stream_name "-" "+" () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok entries ->
      check int ("XRANGE entry count for " ^ stream_name) expected_count (List.length entries) ;
      return_ok () state

let test_xrange_with_count stream_name count expected_returned state =
  let* result = Client.xrange state.client stream_name "-" "+" ~count () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok entries ->
      check int ("XRANGE with COUNT " ^ string_of_int count) expected_returned (List.length entries) ;
      return_ok () state

let test_xrevrange_all stream_name expected_count state =
  let* result = Client.xrevrange state.client stream_name "+" "-" () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok entries ->
      check int ("XREVRANGE entry count for " ^ stream_name) expected_count (List.length entries) ;
      return_ok () state

let test_xrevrange_with_count stream_name count expected_returned state =
  let* result = Client.xrevrange state.client stream_name "+" "-" ~count () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok entries ->
      check int ("XREVRANGE with COUNT " ^ string_of_int count) expected_returned (List.length entries) ;
      return_ok () state

let cleanup_stream stream_name state =
  let* result = Client.del state.client [stream_name] in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok _ -> return_ok () state

(* XREAD-specific pipeline functions *)
let setup_multiple_streams stream_names state =
  let* result = Client.del state.client stream_names in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok _ -> return_ok () {state with stream_ids = stream_names}

let test_xread_empty streams expected_empty_result state =
  let* result = Client.xread state.client streams in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok xread_result ->
      check (list (pair string (list (pair string (list (pair string string))))))
        "XREAD on empty streams" expected_empty_result xread_result ;
      return_ok () state

let add_xread_entry stream_name fields state =
  let xadd_cmd = Ocamlot_infrastructure_redis.Commands.xadd stream_name "*" fields () in
  let* result = Client.execute state.client xadd_cmd in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString (Some _entry_id)) ->
      return_ok () state
  | Ok _ -> Lwt.return (Error (Client.Parse_error "Expected BulkString ID from XADD"))

let test_xread_basic streams expected_stream_count state =
  let* result = Client.xread state.client streams in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok xread_result ->
      check int ("XREAD should return " ^ string_of_int expected_stream_count ^ " streams") 
        expected_stream_count (List.length xread_result) ;
      return_ok () state

let test_xread_with_count streams count expected_entry_count state =
  let* result = Client.xread state.client ~count streams in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok xread_result ->
      (match xread_result with
       | (_, entries) :: _ ->
           check int ("XREAD with COUNT " ^ string_of_int count) 
             expected_entry_count (List.length entries) ;
           return_ok () state
       | [] -> return_ok () state)

let test_xread_continuation streams expected_new_entries state =
  let* result = Client.xread state.client streams in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok xread_result ->
      check int "XREAD continuation should return new entries"
        expected_new_entries (List.fold_left (fun acc (_, entries) -> acc + List.length entries) 0 xread_result) ;
      return_ok () state

let test_config =
  { Client.host= "127.0.0.1"
  ; port= 6379
  ; pool_size= 5
  ; connection_timeout= 5.0 }

(* Custom testable for client results *)
let client_result_testable typ =
  let pp_error ppf e =
    Format.fprintf ppf "Error: %s" (show_client_error e)
  in
  result typ (testable pp_error ( = ))

let string_option_testable = option string

let int_testable = int

let bool_testable = bool

let string_testable = string

(* =============================================================================
   INTEGRATION TEST CASES
   ============================================================================= *)

let test_ping _switch () =
  let* result =
    Client.with_client test_config (fun client -> Client.ping client)
  in
  match result with
  | Ok response ->
      check string_testable "PING response should be PONG" "PONG" response ;
      Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "PING command failed: %s" (show_client_error e))

let test_string_operations _switch () =
  let* result =
    Client.with_client test_config (fun client ->
        (* Test SET *)
        let* set_result =
          Client.set client "alcotest_key" "alcotest_value"
        in
        match set_result with
        | Error e -> Lwt.return (Error e)
        | Ok () -> (
            (* Test GET existing key *)
            let* get_result = Client.get client "alcotest_key" in
            match get_result with
            | Error e -> Lwt.return (Error e)
            | Ok value -> (
                check string_option_testable "GET should return set value"
                  (Some "alcotest_value") value ;
                (* Test GET non-existent key *)
                let* get_none_result =
                  Client.get client "non_existent_alcotest_key"
                in
                match get_none_result with
                | Error e -> Lwt.return (Error e)
                | Ok none_value -> (
                    check string_option_testable
                      "GET non-existent should return None" None none_value ;
                    (* Test DEL *)
                    let* del_result = Client.del client ["alcotest_key"] in
                    match del_result with
                    | Error e -> Lwt.return (Error e)
                    | Ok count ->
                        check int_testable "DEL should delete 1 key" 1 count ;
                        Lwt.return (Ok ()) ) ) ) )
  in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail
        (Printf.sprintf "String operations failed: %s" (show_client_error e))

let test_hash_operations _switch () =
  let* result =
    Client.with_client test_config (fun client ->
        (* Test HSET *)
        let* hset_result =
          Client.hset client "alcotest_hash" "field1" "value1"
        in
        match hset_result with
        | Error e -> Lwt.return (Error e)
        | Ok is_new -> (
            check bool_testable "HSET should create new field" true is_new ;
            (* Test HGET *)
            let* hget_result = Client.hget client "alcotest_hash" "field1" in
            match hget_result with
            | Error e -> Lwt.return (Error e)
            | Ok value -> (
                check string_option_testable "HGET should return set value"
                  (Some "value1") value ;
                (* Test HGET non-existent field *)
                let* hget_none_result =
                  Client.hget client "alcotest_hash" "non_existent_field"
                in
                match hget_none_result with
                | Error e -> Lwt.return (Error e)
                | Ok none_value ->
                    check string_option_testable
                      "HGET non-existent field should return None" None
                      none_value ;
                    (* Clean up *)
                    let* _ = Client.del client ["alcotest_hash"] in
                    Lwt.return (Ok ()) ) ) )
  in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail
        (Printf.sprintf "Hash operations failed: %s" (show_client_error e))

let test_list_operations _switch () =
  let* result =
    Client.with_client test_config (fun client ->
        (* Test LPUSH *)
        let* lpush_result =
          Client.lpush client "alcotest_list" ["item1"; "item2"; "item3"]
        in
        match lpush_result with
        | Error e -> Lwt.return (Error e)
        | Ok count -> (
            check int_testable "LPUSH should add 3 items" 3 count ;
            (* Test RPOP *)
            let* rpop_result = Client.rpop client "alcotest_list" in
            match rpop_result with
            | Error e -> Lwt.return (Error e)
            | Ok value -> (
                check string_option_testable
                  "RPOP should return first pushed item" (Some "item1") value ;
                (* Test RPOP when list has remaining items *)
                let* rpop_result2 = Client.rpop client "alcotest_list" in
                match rpop_result2 with
                | Error e -> Lwt.return (Error e)
                | Ok value2 ->
                    check string_option_testable
                      "Second RPOP should return second item" (Some "item2")
                      value2 ;
                    (* Clean up *)
                    let* _ = Client.del client ["alcotest_list"] in
                    Lwt.return (Ok ()) ) ) )
  in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail
        (Printf.sprintf "List operations failed: %s" (show_client_error e))

let test_info_command _switch () =
  let* result =
    Client.with_client test_config (fun client -> Client.info client)
  in
  match result with
  | Ok info_text ->
      check bool_testable "INFO should return non-empty string" true
        (String.length info_text > 0) ;
      (* Check for common Redis INFO fields *)
      let contains_redis_version = contains_s info_text "redis_version" in
      check bool_testable "INFO should contain redis_version" true
        contains_redis_version ;
      Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "INFO command failed: %s" (show_client_error e))

let test_stream_operations _switch () =
  let* result = Client.with_client test_config (fun client ->
      run_test_pipeline client [
        (fun state -> setup_clean_stream "alcotest_stream" state);
        (fun state -> test_xlen_empty "alcotest_stream" 0 state);
        (fun state -> test_xrange_empty "alcotest_stream" state);
        (fun state -> add_stream_entry "alcotest_stream" [("field1", "value1"); ("field2", "value2")] state);
        (fun state -> add_stream_entry "alcotest_stream" [("field3", "value3"); ("field4", "value4")] state);
        (fun state -> test_xlen_populated "alcotest_stream" 2 state);
        (fun state -> test_xrange_all "alcotest_stream" 2 state);
        (fun state -> test_xrange_with_count "alcotest_stream" 1 1 state);
        (fun state -> test_xrevrange_all "alcotest_stream" 2 state);
        (fun state -> test_xrevrange_with_count "alcotest_stream" 1 1 state);
        (fun state -> cleanup_stream "alcotest_stream" state);
      ] >>=? fun _ _ -> Lwt.return (Ok ())
    ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "Stream operations failed: %s" (show_client_error e))

let test_xread_operations_new _switch () =
  let* result = Client.with_client test_config (fun client ->
      run_test_pipeline client [
        (fun state -> setup_multiple_streams ["xread_stream1"; "xread_stream2"] state);
        (fun state -> test_xread_empty [Client.{key="xread_stream1"; id="0-0"}] [] state);
        (fun state -> add_xread_entry "xread_stream1" [("event", "pageview"); ("user", "john")] state);
        (fun state -> add_xread_entry "xread_stream2" [("event", "purchase"); ("user", "jane")] state);  
        (fun state -> add_xread_entry "xread_stream2" [("event", "login"); ("user", "alice")] state);
        (fun state -> test_xread_basic [Client.{key="xread_stream1"; id="0-0"}; Client.{key="xread_stream2"; id="0-0"}] 2 state);
        (fun state -> test_xread_with_count [Client.{key="xread_stream2"; id="0-0"}] 1 1 state);
        (fun state -> add_xread_entry "xread_stream1" [("event", "logout"); ("user", "john")] state);
        (fun state -> test_xread_continuation [Client.{key="xread_stream1"; id="$"}] 0 state);
        (fun state -> cleanup_stream "xread_stream1" state);
        (fun state -> cleanup_stream "xread_stream2" state);
      ] >>=? fun _ _ -> Lwt.return (Ok ())
    ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XREAD operations failed: %s" (show_client_error e))

(* =============================================================================
   CONDITIONAL TEST SUITE DEFINITION
   ============================================================================= *)

let check_redis_available () =
  match Sys.getenv_opt "REDIS_URL" with
  | Some _ -> true
  | None -> (
    (* Also check if Redis is available on default port *)
    try
      let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 6379) in
      Unix.connect sock addr ; Unix.close sock ; true
    with _ -> false )

let integration_ping_tests =
  if check_redis_available () then [test_case "ping command" `Quick test_ping]
  else
    [ test_case "ping command (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit ) ]

let integration_string_tests =
  if check_redis_available () then
    [test_case "string operations" `Quick test_string_operations]
  else
    [ test_case "string operations (skipped - no Redis)" `Quick
        (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit ) ]

let integration_hash_tests =
  if check_redis_available () then
    [test_case "hash operations" `Quick test_hash_operations]
  else
    [ test_case "hash operations (skipped - no Redis)" `Quick
        (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit ) ]

let integration_list_tests =
  if check_redis_available () then
    [test_case "list operations" `Quick test_list_operations]
  else
    [ test_case "list operations (skipped - no Redis)" `Quick
        (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit ) ]

let integration_info_tests =
  if check_redis_available () then
    [test_case "info command" `Quick test_info_command]
  else
    [ test_case "info command (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit ) ]

let integration_stream_tests =
  if check_redis_available () then
    [test_case "stream operations (XLEN, XRANGE, XREVRANGE)" `Quick test_stream_operations;
     test_case "xread operations" `Quick test_xread_operations_new]
  else
    [ test_case "stream operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xread operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit ) ]

(* Combined integration tests *)
let all_integration_tests =
  integration_ping_tests @ integration_string_tests @ integration_hash_tests
  @ integration_list_tests @ integration_info_tests @ integration_stream_tests
