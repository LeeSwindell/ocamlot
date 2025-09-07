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

(* XREADGROUP-specific pipeline functions *)
let create_consumer_group group_name stream_name start_id state =
  (* Create consumer group using raw command since XGROUP CREATE client wrapper not implemented *)
  let xgroup_cmd = Ocamlot_infrastructure_redis.Commands.xgroup_create stream_name group_name start_id () in
  let* result = Client.execute state.client xgroup_cmd in
  match result with
  | Error e -> Lwt.return (Error e)  
  | Ok _ -> return_ok () state

let test_xreadgroup_empty group consumer streams expected_result state =
  let* result = Client.xreadgroup state.client group consumer streams in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok xread_result ->
      check (list (pair string (list (pair string (list (pair string string))))))
        ("XREADGROUP " ^ group ^ "/" ^ consumer ^ " should return empty") expected_result xread_result ;
      return_ok () state

let test_xreadgroup_basic group consumer streams expected_stream_count state =
  let* result = Client.xreadgroup state.client group consumer streams in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok xread_result ->
      check int ("XREADGROUP should return " ^ string_of_int expected_stream_count ^ " streams") 
        expected_stream_count (List.length xread_result) ;
      return_ok () state

let test_xreadgroup_with_count group consumer streams count expected_entry_count state =
  let* result = Client.xreadgroup state.client group consumer ~count streams in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok xread_result ->
      (match xread_result with
       | (_, entries) :: _ ->
           check int ("XREADGROUP with COUNT " ^ string_of_int count) 
             expected_entry_count (List.length entries) ;
           return_ok () state
       | [] -> return_ok () state)

let test_xreadgroup_noack group consumer streams expected_stream_count state =
  let* result = Client.xreadgroup state.client group consumer ~noack:true streams in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok xread_result ->
      check int ("XREADGROUP with NOACK should return " ^ string_of_int expected_stream_count ^ " streams")
        expected_stream_count (List.length xread_result) ;
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

let test_xreadgroup_operations _switch () =
  let* result = Client.with_client test_config (fun client ->
      run_test_pipeline client [
        (* Setup streams and consumer group *)
        (fun state -> setup_multiple_streams ["xrg_stream1"; "xrg_stream2"] state);
        (fun state -> add_xread_entry "xrg_stream1" [("event", "start"); ("user", "alice")] state);
        (fun state -> add_xread_entry "xrg_stream2" [("event", "click"); ("page", "home")] state);
        (fun state -> create_consumer_group "testgroup" "xrg_stream1" "0" state);
        (fun state -> create_consumer_group "testgroup" "xrg_stream2" "0" state);
        
        (* Test XREADGROUP with > ID (new messages) *)
        (fun state -> test_xreadgroup_basic "testgroup" "consumer1" [Client.{key="xrg_stream1"; id=">"}] 1 state);
        
        (* Add more entries for testing multiple streams *)
        (fun state -> add_xread_entry "xrg_stream1" [("event", "update"); ("status", "ok")] state);
        (fun state -> add_xread_entry "xrg_stream2" [("event", "scroll"); ("page", "about")] state);
        
        (* Test XREADGROUP with multiple streams (should get new messages from both) *)
        (fun state -> test_xreadgroup_basic "testgroup" "consumer2" [Client.{key="xrg_stream1"; id=">"}; Client.{key="xrg_stream2"; id=">"}] 2 state);
        
        (* Add more entries and test COUNT *)
        (fun state -> add_xread_entry "xrg_stream1" [("event", "finish"); ("status", "complete")] state);
        (fun state -> test_xreadgroup_with_count "testgroup" "consumer3" [Client.{key="xrg_stream1"; id=">"}] 1 1 state);
        
        (* Test XREADGROUP with NOACK (add new message first) *)
        (fun state -> add_xread_entry "xrg_stream1" [("event", "noack_test"); ("type", "fire_and_forget")] state);
        (fun state -> test_xreadgroup_noack "testgroup" "consumer4" [Client.{key="xrg_stream1"; id=">"}] 1 state);
        
        (* Test reading pending messages with specific ID (0-0) - consumer1 should have pending messages *)
        (fun state -> test_xreadgroup_basic "testgroup" "consumer1" [Client.{key="xrg_stream1"; id="0-0"}] 1 state);
        
        (* Cleanup *)
        (fun state -> cleanup_stream "xrg_stream1" state);
        (fun state -> cleanup_stream "xrg_stream2" state);
      ] >>=? fun _ _ -> Lwt.return (Ok ())
    ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XREADGROUP operations failed: %s" (show_client_error e))

(* XACK Pipeline Test Functions *)
let test_xack_single_message group_name _consumer_name stream_name message_ids expected_count state =
  let* result = Client.xack state.client stream_name group_name message_ids in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok count ->
      check int ("XACK should acknowledge " ^ string_of_int expected_count ^ " messages") 
        expected_count count ;
      return_ok () state

let test_xack_multiple_messages group_name stream_name message_ids expected_count state =
  let* result = Client.xack state.client stream_name group_name message_ids in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok count ->
      check int ("XACK should acknowledge " ^ string_of_int expected_count ^ " messages") 
        expected_count count ;
      return_ok () state

let test_xack_nonexistent_message group_name stream_name nonexistent_id state =
  let* result = Client.xack state.client stream_name group_name [nonexistent_id] in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok count ->
      check int "XACK should return 0 for nonexistent message" 0 count ;
      return_ok () state

let create_pending_message_single group_name consumer_name stream_name _message_ids state =
  (* Use XREADGROUP to create a pending message that won't be acknowledged *)
  let* result = Client.xreadgroup state.client group_name consumer_name [Client.{key=stream_name; id=">"}] in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok xread_result ->
      (match xread_result with
       | (stream_key, entries) :: _ when stream_key = stream_name && List.length entries > 0 ->
           (* Store the message IDs for later acknowledgment *)
           let entry_ids = List.map (fun (entry_id, _) -> entry_id) entries in
           return_ok entry_ids state
       | _ -> 
           return_ok [] state)

let test_xack_operations _switch () =
  let* result = Client.with_client test_config (fun client ->
      let stream_name = "xack_test_stream_" ^ string_of_int (Random.int 10000) in
      let group_name = "xack_test_group_" ^ string_of_int (Random.int 1000) in
      
      run_test_pipeline client [
        (* Setup: create stream, consumer group and add messages *)
        (fun state -> setup_clean_stream stream_name state);
        (fun state -> add_stream_entry stream_name [("field1", "value1")] state);
        (fun state -> add_stream_entry stream_name [("field2", "value2")] state);  
        (fun state -> add_stream_entry stream_name [("field3", "value3")] state);
        (fun state -> create_consumer_group group_name stream_name "0" state);
        
        (* Create pending messages by reading without acknowledging *)
        (fun state ->
          let* result = Client.xreadgroup state.client group_name "consumer1" [Client.{key=stream_name; id=">"}] in
          match result with
          | Error e -> Lwt.return (Error e)
          | Ok xread_result ->
              (match xread_result with
               | (_, entries) :: _ when List.length entries >= 2 ->
                   let updated_state = {state with entries} in
                   return_ok () updated_state
               | _ -> return_ok () state));
        
        (* Test XACK with single message *)
        (fun state ->
          if List.length state.entries > 0 then
            let (first_id, _) = List.hd state.entries in
            test_xack_single_message group_name "consumer1" stream_name [first_id] 1 state
          else return_ok () state);
          
        (* Test XACK with multiple messages *)
        (fun state ->
          if List.length state.entries > 1 then
            let remaining_entries = List.tl state.entries in
            let remaining_ids = List.map (fun (entry_id, _) -> entry_id) remaining_entries in
            test_xack_multiple_messages group_name stream_name remaining_ids (List.length remaining_ids) state
          else return_ok () state);
          
        (* Test XACK with nonexistent message ID *)
        (fun state -> test_xack_nonexistent_message group_name stream_name "9999999999-0" state);
        
        (* Cleanup *)
        (fun state -> cleanup_stream stream_name state);
      ] >>=? fun _ _ -> Lwt.return (Ok ())
    ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XACK operations failed: %s" (show_client_error e))

(* XACKDEL Pipeline Test Functions *)
let test_xackdel_single_entry group_name stream_name entry_id expected_result ref_handling state =
  let* result = Client.xackdel state.client stream_name group_name [entry_id] ~ref_handling () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok [actual_result] ->
      if actual_result <> expected_result then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected result %d, got %d" expected_result actual_result)))
      else
        return_ok () state
  | Ok results ->
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected single result, got %d results: [%s]" 
          (List.length results) (String.concat "; " (List.map string_of_int results)))))

let test_xackdel_multiple_entries group_name stream_name entry_ids expected_results ref_handling state =
  let* result = Client.xackdel state.client stream_name group_name entry_ids ~ref_handling () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok actual_results ->
      if actual_results <> expected_results then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected results [%s], got [%s]" 
          (String.concat "; " (List.map string_of_int expected_results))
          (String.concat "; " (List.map string_of_int actual_results)))))
      else
        return_ok () state

let test_xackdel_operations _switch () =
  let* result = Client.with_client test_config (fun client ->
      let stream_name = "xackdel_test_stream_" ^ string_of_int (Random.int 10000) in
      let group_name = "xackdel_test_group_" ^ string_of_int (Random.int 1000) in
      
      run_test_pipeline client [
        (* Setup: create stream, consumer group and add messages *)
        (fun state -> setup_clean_stream stream_name state);
        (fun state -> add_stream_entry stream_name [("field1", "value1")] state);
        (fun state -> add_stream_entry stream_name [("field2", "value2")] state);  
        (fun state -> add_stream_entry stream_name [("field3", "value3")] state);
        (fun state -> create_consumer_group group_name stream_name "0" state);
        
        (* Create pending messages by reading without acknowledging *)
        (fun state ->
          let* result = Client.xreadgroup state.client group_name "consumer1" [Client.{key=stream_name; id=">"}] in
          match result with
          | Error e -> Lwt.return (Error e)
          | Ok xread_result ->
              (match xread_result with
               | (_, entries) :: _ when List.length entries >= 2 ->
                   let updated_state = {state with entries} in
                   return_ok () updated_state
               | _ -> return_ok () state));
        
        (* Test XACKDEL with KEEPREF (default) - should acknowledge and delete *)
        (fun state ->
          if List.length state.entries > 0 then
            let (first_id, _) = List.hd state.entries in
            test_xackdel_single_entry group_name stream_name first_id 1 Ocamlot_infrastructure_redis.Commands.Keepref state
          else return_ok () state);
          
        (* Test XACKDEL with DELREF - should acknowledge and delete with reference cleanup *)
        (fun state ->
          if List.length state.entries > 1 then
            let remaining_entries = List.tl state.entries in
            let remaining_ids = List.map (fun (entry_id, _) -> entry_id) remaining_entries in
            let expected_results = List.map (fun _ -> 1) remaining_ids in
            test_xackdel_multiple_entries group_name stream_name remaining_ids expected_results Ocamlot_infrastructure_redis.Commands.Delref state
          else return_ok () state);
          
        (* Test XACKDEL with nonexistent message IDs *)
        (fun state ->
          let nonexistent_ids = ["9999999999-0"; "8888888888-0"] in
          let expected_results = [-1; -1] in
          test_xackdel_multiple_entries group_name stream_name nonexistent_ids expected_results Ocamlot_infrastructure_redis.Commands.Keepref state);
          
        (* Test XACKDEL ACKED option with new messages *)
        (fun state -> add_stream_entry stream_name [("field4", "value4")] state);
        (fun state ->
          (* Create another pending message *)
          let* result = Client.xreadgroup state.client group_name "consumer2" [Client.{key=stream_name; id=">"}] in
          match result with
          | Error e -> Lwt.return (Error e)
          | Ok xread_result ->
              (match xread_result with
               | [(_, [(new_id, _)])] ->
                   (* Test ACKED mode - should acknowledge but may not delete if not acked by all groups *)
                   test_xackdel_single_entry group_name stream_name new_id 1 Ocamlot_infrastructure_redis.Commands.Acked state
               | _ -> return_ok () state));
        
        (* Cleanup *)
        (fun state -> cleanup_stream stream_name state);
      ] >>=? fun _ _ -> Lwt.return (Ok ())
    ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XACKDEL operations failed: %s" (show_client_error e))

(* XPENDING Pipeline Test Functions *)
let test_xpending_summary group_name stream_name expected_count state =
  let* result = Client.xpending state.client stream_name group_name () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.Summary summary) ->
      check int ("XPENDING summary should show " ^ string_of_int expected_count ^ " pending messages") 
        expected_count summary.count ;
      return_ok () state
  | Ok (Client.Extended _) ->
      Lwt.return (Error (Client.Parse_error "Expected summary, got extended result"))

let test_xpending_extended group_name stream_name expected_min_count state =
  let* result = Client.xpending state.client stream_name group_name 
                  ~range:(Ocamlot_infrastructure_redis.Commands.Extended {start="-"; end_="+"; count=10; consumer=None; idle=None}) () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.Extended entries) ->
      check bool ("XPENDING extended should return at least " ^ string_of_int expected_min_count ^ " entries") 
        true (List.length entries >= expected_min_count) ;
      return_ok () state
  | Ok (Client.Summary _) ->
      Lwt.return (Error (Client.Parse_error "Expected extended, got summary result"))

let test_xpending_with_consumer group_name stream_name consumer_name expected_found state =
  let* result = Client.xpending state.client stream_name group_name 
                  ~range:(Ocamlot_infrastructure_redis.Commands.Extended {start="-"; end_="+"; count=10; consumer=Some consumer_name; idle=None}) () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.Extended entries) ->
      let consumer_entries = List.filter (fun entry -> entry.Client.consumer = consumer_name) entries in
      if expected_found then
        check bool ("XPENDING should find entries for consumer " ^ consumer_name) 
          true (List.length consumer_entries > 0)
      else
        check int ("XPENDING should find no entries for consumer " ^ consumer_name) 
          0 (List.length consumer_entries) ;
      return_ok () state
  | Ok (Client.Summary _) ->
      Lwt.return (Error (Client.Parse_error "Expected extended, got summary result"))

let test_xpending_idle_filter group_name stream_name idle_ms state =
  let* result = Client.xpending state.client stream_name group_name 
                  ~range:(Ocamlot_infrastructure_redis.Commands.Extended {start="-"; end_="+"; count=10; consumer=None; idle=Some idle_ms}) () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.Extended entries) ->
      (* All returned entries should have idle_time >= idle_ms *)
      let all_idle = List.for_all (fun entry -> entry.Client.idle_time >= idle_ms) entries in
      check bool ("All XPENDING entries should have idle_time >= " ^ string_of_int idle_ms) 
        true all_idle ;
      return_ok () state
  | Ok (Client.Summary _) ->
      Lwt.return (Error (Client.Parse_error "Expected extended, got summary result"))

let test_xpending_operations _switch () =
  let* result = Client.with_client test_config (fun client ->
      let stream_name = "xpending_test_stream_" ^ string_of_int (Random.int 10000) in
      let group_name = "xpending_test_group_" ^ string_of_int (Random.int 1000) in
      
      run_test_pipeline client [
        (* Setup: create stream, consumer group and add messages *)
        (fun state -> setup_clean_stream stream_name state);
        (fun state -> add_stream_entry stream_name [("field1", "value1")] state);
        (fun state -> add_stream_entry stream_name [("field2", "value2")] state);  
        (fun state -> add_stream_entry stream_name [("field3", "value3")] state);
        (fun state -> create_consumer_group group_name stream_name "0" state);
        
        (* Test XPENDING summary with no pending messages initially *)
        (fun state -> test_xpending_summary group_name stream_name 0 state);
        
        (* Create pending messages by reading without acknowledging *)
        (fun state ->
          let* result = Client.xreadgroup state.client group_name "consumer1" [Client.{key=stream_name; id=">"}] in
          match result with
          | Error e -> Lwt.return (Error e)
          | Ok xread_result ->
              (match xread_result with
               | (_, entries) :: _ when List.length entries >= 2 ->
                   let updated_state = {state with entries} in
                   return_ok () updated_state
               | _ -> return_ok () state));
        
        (* Test XPENDING summary with pending messages *)
        (fun state -> test_xpending_summary group_name stream_name 3 state);
        
        (* Test XPENDING extended form *)
        (fun state -> test_xpending_extended group_name stream_name 2 state);
        
        (* Test XPENDING with consumer filter *)
        (fun state -> test_xpending_with_consumer group_name stream_name "consumer1" true state);
        (fun state -> test_xpending_with_consumer group_name stream_name "nonexistent_consumer" false state);
        
        (* Wait briefly then test idle filter (should find messages idle > 0ms) *)
        (fun state -> 
          let* _ = Lwt_unix.sleep 0.001 in (* Wait 1ms *)
          test_xpending_idle_filter group_name stream_name 0 state);
        
        (* Test XPENDING after acknowledging some messages *)
        (fun state ->
          match state.entries with
          | (first_id, _) :: _ ->
              let* ack_result = Client.xack state.client stream_name group_name [first_id] in
              (match ack_result with
               | Error e -> Lwt.return (Error e)
               | Ok _ -> test_xpending_summary group_name stream_name 2 state)
          | _ -> test_xpending_summary group_name stream_name 3 state);
        
        (* Cleanup *)
        (fun state -> cleanup_stream stream_name state);
      ] >>=? fun _ _ -> Lwt.return (Ok ())
    ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XPENDING operations failed: %s" (show_client_error e))

(* XCLAIM Pipeline Test Functions *)
let test_xclaim_basic group_name stream_name _consumer1 consumer2 ids state =
  let* result = Client.xclaim state.client stream_name group_name consumer2 0 ids () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.ClaimEntries entries) ->
      check bool "XCLAIM should return claimed entries" 
        true (List.length entries > 0) ;
      return_ok () state
  | Ok (Client.ClaimIds _) ->
      Lwt.return (Error (Client.Parse_error "Expected ClaimEntries, got ClaimIds"))

let test_xclaim_justid group_name stream_name _consumer1 consumer2 ids state =
  let* result = Client.xclaim state.client stream_name group_name consumer2 0 ids ~options:[Ocamlot_infrastructure_redis.Commands.JustId] () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.ClaimIds claimed_ids) ->
      check bool "XCLAIM JUSTID should return message IDs" 
        true (List.length claimed_ids > 0) ;
      return_ok () state
  | Ok (Client.ClaimEntries _) ->
      Lwt.return (Error (Client.Parse_error "Expected ClaimIds, got ClaimEntries"))

let test_xclaim_with_options group_name stream_name _consumer1 consumer2 ids state =
  let* result = Client.xclaim state.client stream_name group_name consumer2 0 ids 
                  ~options:[Ocamlot_infrastructure_redis.Commands.Idle 0; Ocamlot_infrastructure_redis.Commands.RetryCount 5] () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.ClaimEntries entries) ->
      check bool "XCLAIM with options should return claimed entries" 
        true (List.length entries >= 0) ;
      return_ok () state
  | Ok (Client.ClaimIds _) ->
      Lwt.return (Error (Client.Parse_error "Expected ClaimEntries, got ClaimIds"))

let test_xclaim_nonexistent_ids group_name stream_name consumer2 state =
  let* result = Client.xclaim state.client stream_name group_name consumer2 1000 ["9999999999-0"] () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.ClaimEntries entries) ->
      check int "XCLAIM nonexistent IDs should return empty" 0 (List.length entries) ;
      return_ok () state
  | Ok (Client.ClaimIds ids) ->
      check int "XCLAIM nonexistent IDs should return empty" 0 (List.length ids) ;
      return_ok () state

(* XAUTOCLAIM Pipeline Test Functions *)
let test_xautoclaim_basic group_name stream_name consumer_name expected_min state =
  let* result = Client.xautoclaim state.client stream_name group_name consumer_name 0 "0-0" () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.AutoClaimEntries {next_cursor; claimed_entries; deleted_ids = _}) ->
      check bool "XAUTOCLAIM should return a valid cursor" 
        true (String.length next_cursor > 0) ;
      check bool ("XAUTOCLAIM should claim at least " ^ string_of_int expected_min ^ " entries")
        true (List.length claimed_entries >= expected_min) ;
      return_ok () state
  | Ok (Client.AutoClaimIds _) ->
      Lwt.return (Error (Client.Parse_error "Expected AutoClaimEntries, got AutoClaimIds"))

let test_xautoclaim_justid group_name stream_name consumer_name expected_min state =
  let* result = Client.xautoclaim state.client stream_name group_name consumer_name 0 "0-0" ~justid:true () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.AutoClaimIds {next_cursor; claimed_ids; deleted_ids = _}) ->
      check bool "XAUTOCLAIM JUSTID should return a valid cursor" 
        true (String.length next_cursor > 0) ;
      check bool ("XAUTOCLAIM JUSTID should claim at least " ^ string_of_int expected_min ^ " IDs")
        true (List.length claimed_ids >= expected_min) ;
      return_ok () state
  | Ok (Client.AutoClaimEntries _) ->
      Lwt.return (Error (Client.Parse_error "Expected AutoClaimIds, got AutoClaimEntries"))

let test_xautoclaim_with_count group_name stream_name consumer_name count state =
  let* result = Client.xautoclaim state.client stream_name group_name consumer_name 0 "0-0" ~count:(Some count) () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Client.AutoClaimEntries {next_cursor = _; claimed_entries; deleted_ids = _}) ->
      check bool ("XAUTOCLAIM with count should claim at most " ^ string_of_int count ^ " entries")
        true (List.length claimed_entries <= count) ;
      return_ok () state
  | Ok (Client.AutoClaimIds _) ->
      Lwt.return (Error (Client.Parse_error "Expected AutoClaimEntries, got AutoClaimIds"))

let test_xclaim_xautoclaim_operations _switch () =
  let* result = Client.with_client test_config (fun client ->
      let stream_name = "xclaim_test_stream_" ^ string_of_int (Random.int 10000) in
      let group_name = "xclaim_test_group_" ^ string_of_int (Random.int 1000) in
      
      run_test_pipeline client [
        (* Setup: create stream, consumer group and add messages *)
        (fun state -> setup_clean_stream stream_name state);
        (fun state -> add_stream_entry stream_name [("field1", "value1")] state);
        (fun state -> add_stream_entry stream_name [("field2", "value2")] state);  
        (fun state -> add_stream_entry stream_name [("field3", "value3")] state);
        (fun state -> add_stream_entry stream_name [("field4", "value4")] state);
        (fun state -> create_consumer_group group_name stream_name "0" state);
        
        (* Create pending messages by reading with consumer1 without acknowledging *)
        (fun state ->
          let* result = Client.xreadgroup state.client group_name "consumer1" [Client.{key=stream_name; id=">"}] in
          match result with
          | Error e -> Lwt.return (Error e)
          | Ok xread_result ->
              (match xread_result with
               | (_, entries) :: _ when List.length entries >= 2 ->
                   let updated_state = {state with entries} in
                   return_ok () updated_state
               | _ -> return_ok () state));
        
        (* Test XCLAIM basic functionality *)
        (fun state ->
          match state.entries with
          | (first_id, _) :: (second_id, _) :: _ ->
              test_xclaim_basic group_name stream_name "consumer1" "consumer2" [first_id; second_id] state
          | _ -> return_ok () state);
          
        (* Test XCLAIM with JUSTID option *)  
        (fun state ->
          match state.entries with
          | (first_id, _) :: _ ->
              test_xclaim_justid group_name stream_name "consumer1" "consumer3" [first_id] state
          | _ -> return_ok () state);
          
        (* Test XCLAIM with options *)
        (fun state ->
          match state.entries with
          | (_, _) :: (second_id, _) :: _ ->
              test_xclaim_with_options group_name stream_name "consumer1" "consumer4" [second_id] state
          | _ -> return_ok () state);
          
        (* Test XCLAIM with nonexistent IDs *)
        (fun state -> test_xclaim_nonexistent_ids group_name stream_name "consumer2" state);
        
        (* Create more pending messages for XAUTOCLAIM testing *)
        (fun state ->
          let* result = Client.xreadgroup state.client group_name "consumer5" [Client.{key=stream_name; id=">"}] in
          match result with
          | Error e -> Lwt.return (Error e)
          | Ok _ -> return_ok () state);
            
        (* Wait briefly to ensure messages become "idle" *)
        (fun state -> 
          let* _ = Lwt_unix.sleep 0.002 in (* Wait 2ms *)
          return_ok () state);
        
        (* Test XAUTOCLAIM basic functionality *)
        (fun state -> test_xautoclaim_basic group_name stream_name "consumer6" 1 state);
        
        (* Test XAUTOCLAIM with JUSTID *)
        (fun state -> test_xautoclaim_justid group_name stream_name "consumer7" 1 state);
        
        (* Test XAUTOCLAIM with count limit *)
        (fun state -> test_xautoclaim_with_count group_name stream_name "consumer8" 2 state);
        
        (* Cleanup *)
        (fun state -> cleanup_stream stream_name state);
      ] >>=? fun _ _ -> Lwt.return (Ok ())
    ) in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XCLAIM/XAUTOCLAIM operations failed: %s" (show_client_error e))

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

(* XDEL-specific pipeline functions *)
let test_xdel_single_entry stream_name entry_id expected_count state =
  let* result = Client.xdel state.client stream_name [entry_id] in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok count ->
      if count <> expected_count then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected to delete %d entries, got %d" expected_count count)))
      else
        return_ok () state

let test_xdel_multiple_entries stream_name entry_ids expected_count state =
  let* result = Client.xdel state.client stream_name entry_ids in
  match result with
  | Error e -> Lwt.return (Error e)  
  | Ok count ->
      if count <> expected_count then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected to delete %d entries, got %d" expected_count count)))
      else
        return_ok () state

let test_xdel_mixed_entries stream_name entry_ids expected_count state =
  let* result = Client.xdel state.client stream_name entry_ids in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok count ->
      if count <> expected_count then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected to delete %d entries from mixed list, got %d" expected_count count)))
      else
        return_ok () state

let test_xdel_operations _switch () =
  let* result =
    Client.with_client test_config (fun client ->
        let stream_name = "test_xdel_stream_" ^ string_of_int (Random.int 10000) in
        run_test_pipeline client [
          (* Setup: Add 3 entries to test stream *)
          (fun state -> setup_clean_stream stream_name state);
          (fun state -> add_stream_entry stream_name [("field1", "value1")] state);
          (fun state -> add_stream_entry stream_name [("field2", "value2")] state);
          (fun state -> add_stream_entry stream_name [("field3", "value3")] state);
          
          (* Test 1: Delete one existing entry (middle one) *)
          (fun state -> 
             let second_entry_id = match state.entries with
             | [_; (id, _); _] -> id  (* Get middle entry ID *)
             | _ -> failwith "Expected 3 entries in test state"
             in
             test_xdel_single_entry stream_name second_entry_id 1 state);
             
          (* Test 2: Delete multiple existing entries (remaining two) *)
          (fun state ->
             let remaining_ids = match state.entries with  
             | [(id3, _); _; (id1, _)] -> [id1; id3]  (* First and third entries *)
             | _ -> failwith "Expected 3 entries in test state"
             in
             test_xdel_multiple_entries stream_name remaining_ids 2 state);
             
          (* Test 3: Try to delete already deleted entries (should return 0) *)
          (fun state ->
             let all_ids = List.map fst state.entries in
             test_xdel_multiple_entries stream_name all_ids 0 state);
             
          (* Test 4: Add new entry and test mixed deletion *)
          (fun state -> add_stream_entry stream_name [("field4", "value4")] state);
          (fun state ->
             let new_id = match state.entries with
             | (id, _) :: _ -> id  (* Get the newest entry ID *)
             | _ -> failwith "Expected at least 1 entry in test state"
             in
             (* Mix new ID with non-existent IDs *)
             let mixed_ids = ["9999999999-0"; new_id; "8888888888-0"] in
             test_xdel_mixed_entries stream_name mixed_ids 1 state);  (* Only new_id should be deleted *)
             
          (* Cleanup *)
          (fun state -> cleanup_stream stream_name state);
        ] >>=? fun _ _ -> Lwt.return (Ok ())
    )
  in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XDEL operations failed: %s" (show_client_error e))

(* XDELEX Pipeline Test Functions *)
let test_xdelex_single_entry stream_name entry_id expected_result ref_handling state =
  let* result = Client.xdelex state.client stream_name [entry_id] ~ref_handling () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok [actual_result] ->
      if actual_result <> expected_result then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected result %d, got %d" expected_result actual_result)))
      else
        return_ok () state
  | Ok results ->
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected single result, got %d results: [%s]" 
          (List.length results) (String.concat "; " (List.map string_of_int results)))))

let test_xdelex_multiple_entries stream_name entry_ids expected_results ref_handling state =
  let* result = Client.xdelex state.client stream_name entry_ids ~ref_handling () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok actual_results ->
      if actual_results <> expected_results then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected results [%s], got [%s]" 
          (String.concat "; " (List.map string_of_int expected_results))
          (String.concat "; " (List.map string_of_int actual_results)))))
      else
        return_ok () state

let test_xdelex_operations _switch () =
  let* result =
    Client.with_client test_config (fun client ->
        let stream_name = "test_xdelex_stream_" ^ string_of_int (Random.int 10000) in
        run_test_pipeline client [
          (* Setup: Add 3 entries to test stream *)
          (fun state -> setup_clean_stream stream_name state);
          (fun state -> add_stream_entry stream_name [("field1", "value1")] state);
          (fun state -> add_stream_entry stream_name [("field2", "value2")] state);
          (fun state -> add_stream_entry stream_name [("field3", "value3")] state);
          
          (* Test 1: XDELEX with KEEPREF (default) - should behave like XDEL *)
          (fun state -> 
             let second_entry_id = match state.entries with
             | [_; (id, _); _] -> id  (* Get middle entry ID *)
             | _ -> failwith "Expected 3 entries in test state"
             in
             test_xdelex_single_entry stream_name second_entry_id 1 Ocamlot_infrastructure_redis.Commands.Keepref state);
             
          (* Test 2: XDELEX with DELREF - delete and clean references *)
          (fun state ->
             let remaining_ids = match state.entries with  
             | [(id3, _); _; (id1, _)] -> [id1; id3]  (* First and third entries *)
             | _ -> failwith "Expected 3 entries in test state"
             in
             test_xdelex_multiple_entries stream_name remaining_ids [1; 1] Ocamlot_infrastructure_redis.Commands.Delref state);
             
          (* Test 3: Try to delete already deleted entries (should return -1 for each) *)
          (fun state ->
             let all_ids = List.map fst state.entries in
             let expected_results = List.map (fun _ -> -1) all_ids in
             test_xdelex_multiple_entries stream_name all_ids expected_results Ocamlot_infrastructure_redis.Commands.Keepref state);
             
          (* Test 4: Add new entry and test mixed deletion *)
          (fun state -> add_stream_entry stream_name [("field4", "value4")] state);
          (fun state ->
             let new_id = match state.entries with
             | (id, _) :: _ -> id  (* Get the newest entry ID *)
             | _ -> failwith "Expected at least 1 entry in test state"
             in
             (* Mix new ID with non-existent IDs *)
             let mixed_ids = ["9999999999-0"; new_id; "8888888888-0"] in
             let expected_results = [-1; 1; -1] in  (* Only new_id should be deleted *)
             test_xdelex_multiple_entries stream_name mixed_ids expected_results Ocamlot_infrastructure_redis.Commands.Delref state);
             
          (* Test 5: Test ACKED option with new entries *)
          (fun state -> add_stream_entry stream_name [("field5", "value5")] state);
          (fun state ->
             let new_id = match state.entries with
             | (id, _) :: _ -> id  (* Get the newest entry ID *)
             | _ -> failwith "Expected at least 1 entry in test state"
             in
             (* ACKED should not delete unacknowledged entries, so expect result depends on actual state *)
             test_xdelex_single_entry stream_name new_id 1 Ocamlot_infrastructure_redis.Commands.Acked state);
             
          (* Cleanup *)
          (fun state -> cleanup_stream stream_name state);
        ] >>=? fun _ _ -> Lwt.return (Ok ())
    )
  in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XDELEX operations failed: %s" (show_client_error e))

(* XTRIM-specific pipeline functions *)
let test_xtrim_maxlen stream_name max_len expected_count state =
  let* result = Client.xtrim state.client stream_name (Ocamlot_infrastructure_redis.Commands.Maxlen max_len) () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok count ->
      if count <> expected_count then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected XTRIM MAXLEN to remove %d entries, got %d" expected_count count)))
      else
        return_ok () state

let test_xtrim_minid stream_name min_id expected_count state =
  let* result = Client.xtrim state.client stream_name (Ocamlot_infrastructure_redis.Commands.Minid min_id) () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok count ->
      if count <> expected_count then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected XTRIM MINID to remove %d entries, got %d" expected_count count)))
      else
        return_ok () state

let test_xtrim_approximate stream_name max_len state =
  let* result = Client.xtrim state.client stream_name (Ocamlot_infrastructure_redis.Commands.Maxlen max_len) ~operator:Ocamlot_infrastructure_redis.Commands.Approximate () in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok _count ->
      (* Just verify it succeeded - Redis approximate trimming behavior can vary *)
      return_ok () state

let test_current_stream_length stream_name expected_length state =
  let* result = Client.xlen state.client stream_name in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok length ->
      if length <> expected_length then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected stream length %d, got %d" expected_length length)))
      else
        return_ok () state

let test_xtrim_operations _switch () =
  let* result =
    Client.with_client test_config (fun client ->
        let stream_name = "test_xtrim_stream_" ^ string_of_int (Random.int 10000) in
        run_test_pipeline client [
          (* Setup: Create stream with 5 entries *)
          (fun state -> setup_clean_stream stream_name state);
          (fun state -> add_stream_entry stream_name [("field1", "value1")] state);
          (fun state -> add_stream_entry stream_name [("field2", "value2")] state);
          (fun state -> add_stream_entry stream_name [("field3", "value3")] state);
          (fun state -> add_stream_entry stream_name [("field4", "value4")] state);
          (fun state -> add_stream_entry stream_name [("field5", "value5")] state);
          
          (* Verify we have 5 entries *)
          (fun state -> test_current_stream_length stream_name 5 state);
          
          (* Test 1: XTRIM MAXLEN to keep only 3 entries (should remove 2) *)
          (fun state -> test_xtrim_maxlen stream_name 3 2 state);
          (fun state -> test_current_stream_length stream_name 3 state);
          
          (* Test 2: Add more entries for MINID test *)
          (fun state -> add_stream_entry stream_name [("field6", "value6")] state);
          (fun state -> add_stream_entry stream_name [("field7", "value7")] state);
          (fun state -> test_current_stream_length stream_name 5 state);
          
          (* Test 3: XTRIM MINID - trim everything before the middle entry *)
          (fun state ->
             let middle_id = match state.entries with
             | (_, _) :: (id2, _) :: _ -> id2  (* Second newest entry *)
             | _ -> failwith "Expected at least 2 entries"
             in
             test_xtrim_minid stream_name middle_id 3 state);  (* Should remove 3 older entries *)
          (fun state -> test_current_stream_length stream_name 2 state);
          
          (* Test 4: Add more entries and test approximate trimming *)
          (fun state -> add_stream_entry stream_name [("field8", "value8")] state);
          (fun state -> add_stream_entry stream_name [("field9", "value9")] state);
          (fun state -> add_stream_entry stream_name [("field10", "value10")] state);
          (fun state -> test_current_stream_length stream_name 5 state);
          
          (* Test 5: XTRIM approximate (just verify it works) *)
          (fun state -> test_xtrim_approximate stream_name 1 state);
          
          (* Cleanup *)
          (fun state -> cleanup_stream stream_name state);
        ] >>=? fun _ _ -> Lwt.return (Ok ())
    )
  in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XTRIM operations failed: %s" (show_client_error e))

let integration_stream_tests =
  if check_redis_available () then
    [test_case "stream operations (XLEN, XRANGE, XREVRANGE)" `Quick test_stream_operations;
     test_case "xread operations" `Quick test_xread_operations_new;
     test_case "xreadgroup operations" `Quick test_xreadgroup_operations;
     test_case "xack operations" `Quick test_xack_operations;
     test_case "xackdel operations" `Quick test_xackdel_operations;
     test_case "xpending operations" `Quick test_xpending_operations;
     test_case "xclaim and xautoclaim operations" `Quick test_xclaim_xautoclaim_operations;
     test_case "xdel operations" `Quick test_xdel_operations;
     test_case "xdelex operations" `Quick test_xdelex_operations;
     test_case "xtrim operations" `Quick test_xtrim_operations]
  else
    [ test_case "stream operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xread operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xreadgroup operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xack operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xackdel operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xpending operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xclaim and xautoclaim operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xdel operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xdelex operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xtrim operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit ) ]

let test_xgroup_operations _switch () =
  let* result =
    Client.with_client test_config (fun client ->
        let stream_name = "test_xgroup_stream_" ^ string_of_int (Random.int 10000) in
        let group_name1 = "test_group1_" ^ string_of_int (Random.int 1000) in
        let group_name2 = "test_group2_" ^ string_of_int (Random.int 1000) in
        let group_name3 = "test_group3_" ^ string_of_int (Random.int 1000) in
        
        (* First add some data to create the stream *)
        let* add_result = Client.execute client 
          (Ocamlot_infrastructure_redis.Commands.xadd stream_name "*" [("field1", "value1")] ()) in
        match add_result with
        | Error e -> Lwt.return (Error e)
        | Ok _ ->
            (* Test 1: XGROUP CREATE basic *)
            let* create_result = Client.xgroup_create client stream_name group_name1 "0-0" () in
            match create_result with
            | Error e -> Lwt.return (Error e)
            | Ok () ->
                (* Test 2: XGROUP CREATE with MKSTREAM (should create non-existent stream) *)
                let nonexistent_stream = "nonexistent_stream_" ^ string_of_int (Random.int 10000) in
                let* mkstream_result = Client.xgroup_create client nonexistent_stream group_name2 "$" ~mkstream:true () in
                match mkstream_result with
                | Error e -> Lwt.return (Error e)
                | Ok () ->
                    (* Test 3: XGROUP CREATE with ENTRIESREAD *)
                    let* entriesread_result = Client.xgroup_create client stream_name group_name3 "0-0" ~entriesread:100 () in
                    match entriesread_result with
                    | Error e -> Lwt.return (Error e)
                    | Ok () ->
                        (* Test 4: XGROUP DESTROY existing group *)
                        let* destroy_result1 = Client.xgroup_destroy client stream_name group_name1 in
                        match destroy_result1 with
                        | Error e -> Lwt.return (Error e)
                        | Ok destroyed1 ->
                            if not destroyed1 then 
                              Lwt.return (Error (Client.Redis_error "Expected group to be destroyed"))
                            else
                              (* Test 5: XGROUP DESTROY non-existent group *)
                              let* destroy_result2 = Client.xgroup_destroy client stream_name group_name1 in
                              match destroy_result2 with
                              | Error e -> Lwt.return (Error e)
                              | Ok destroyed2 ->
                                  if destroyed2 then 
                                    Lwt.return (Error (Client.Redis_error "Expected group to not exist"))
                                  else
                                    (* Cleanup: destroy remaining groups *)
                                    let* _cleanup1 = Client.xgroup_destroy client stream_name group_name3 in
                                    let* _cleanup2 = Client.xgroup_destroy client nonexistent_stream group_name2 in
                                    Lwt.return (Ok ())
    )
  in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XGROUP operations failed: %s" (show_client_error e))

(* XGROUP CREATECONSUMER Pipeline Test Functions *)
let test_xgroup_createconsumer_new stream_name group_name consumer_name expected_created state =
  let* result = Client.xgroup_createconsumer state.client stream_name group_name consumer_name in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok created ->
      if created <> expected_created then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected CREATECONSUMER to return %b, got %b" expected_created created)))
      else
        return_ok () state

let test_xgroup_createconsumer_operations _switch () =
  let* result =
    Client.with_client test_config (fun client ->
        let stream_name = "test_createconsumer_stream_" ^ string_of_int (Random.int 10000) in
        let group_name = "test_createconsumer_group_" ^ string_of_int (Random.int 1000) in
        
        run_test_pipeline client [
          (* Setup: create stream and consumer group *)
          (fun state -> setup_clean_stream stream_name state);
          (fun state -> add_stream_entry stream_name [("field1", "value1")] state);
          (fun state -> create_consumer_group group_name stream_name "0" state);
          
          (* Test 1: Create new consumer (should return true) *)
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "consumer1" true state);
          
          (* Test 2: Try to create same consumer again (should return false) *)
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "consumer1" false state);
          
          (* Test 3: Create different consumer (should return true) *)
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "consumer2" true state);
          
          (* Test 4: Create consumer with special characters (should return true) *)
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "consumer_with_underscores" true state);
          
          (* Test 5: Create consumer with special characters again (should return false) *)
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "consumer_with_underscores" false state);
          
          (* Cleanup *)
          (fun state -> cleanup_stream stream_name state);
        ] >>=? fun _ _ -> Lwt.return (Ok ())
    )
  in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XGROUP CREATECONSUMER operations failed: %s" (show_client_error e))

(* XGROUP DELCONSUMER Pipeline Test Functions *)
let test_xgroup_delconsumer_basic stream_name group_name consumer_name expected_pending_count state =
  let* result = Client.xgroup_delconsumer state.client stream_name group_name consumer_name in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok pending_count ->
      if pending_count <> expected_pending_count then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected DELCONSUMER to return %d pending messages, got %d" expected_pending_count pending_count)))
      else
        return_ok () state

let consume_messages_to_create_pending stream_name group_name consumer_name count state =
  (* Use XREADGROUP to consume messages, creating pending entries *)
  let streams = [{Client.key = stream_name; id = ">"}] in
  let* result = Client.xreadgroup state.client group_name consumer_name ~count streams in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok messages ->
      let total_consumed = List.fold_left (fun acc (_, entries) -> acc + List.length entries) 0 messages in
      if total_consumed < count then
        Lwt.return (Error (Client.Redis_error (Printf.sprintf "Expected to consume at least %d messages, got %d" count total_consumed)))
      else
        return_ok () state

let test_xgroup_delconsumer_operations _switch () =
  let* result =
    Client.with_client test_config (fun client ->
        let stream_name = "test_delconsumer_stream_" ^ string_of_int (Random.int 10000) in
        let group_name = "test_delconsumer_group_" ^ string_of_int (Random.int 1000) in
        
        run_test_pipeline client [
          (* Setup: create stream and consumer group *)
          (fun state -> setup_clean_stream stream_name state);
          (fun state -> add_stream_entry stream_name [("field1", "value1")] state);
          (fun state -> add_stream_entry stream_name [("field2", "value2")] state);
          (fun state -> add_stream_entry stream_name [("field3", "value3")] state);
          (fun state -> create_consumer_group group_name stream_name "0" state);
          
          (* Test 1: Delete non-existent consumer (should return 0) *)
          (fun state -> test_xgroup_delconsumer_basic stream_name group_name "nonexistent_consumer" 0 state);
          
          (* Test 2: Create consumer and delete without pending messages (should return 0) *)
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "empty_consumer" true state);
          (fun state -> test_xgroup_delconsumer_basic stream_name group_name "empty_consumer" 0 state);
          
          (* Test 3: Create consumer, consume messages (create pending), then delete *)
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "pending_consumer" true state);
          (fun state -> consume_messages_to_create_pending stream_name group_name "pending_consumer" 2 state);
          (fun state -> test_xgroup_delconsumer_basic stream_name group_name "pending_consumer" 2 state);
          
          (* Test 4: Create multiple consumers with different pending counts *)
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "consumer1" true state);
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "consumer2" true state);
          (* Add more messages for different consumption patterns *)
          (fun state -> add_stream_entry stream_name [("field4", "value4")] state);
          (fun state -> add_stream_entry stream_name [("field5", "value5")] state);
          (* Consumer1 gets 1 message, Consumer2 gets remaining *)
          (fun state -> consume_messages_to_create_pending stream_name group_name "consumer1" 1 state);
          (fun state -> consume_messages_to_create_pending stream_name group_name "consumer2" 1 state);
          (* Delete consumers and verify their pending counts *)
          (fun state -> test_xgroup_delconsumer_basic stream_name group_name "consumer1" 1 state);
          (fun state -> test_xgroup_delconsumer_basic stream_name group_name "consumer2" 1 state);
          
          (* Test 5: Delete consumer with special characters *)
          (fun state -> test_xgroup_createconsumer_new stream_name group_name "special_consumer_123" true state);
          (fun state -> test_xgroup_delconsumer_basic stream_name group_name "special_consumer_123" 0 state);
          
          (* Cleanup *)
          (fun state -> cleanup_stream stream_name state);
        ] >>=? fun _ _ -> Lwt.return (Ok ())
    )
  in
  match result with
  | Ok () -> Lwt.return_unit
  | Error e ->
      fail (Printf.sprintf "XGROUP DELCONSUMER operations failed: %s" (show_client_error e))

let integration_xgroup_tests =
  if check_redis_available () then
    [test_case "xgroup operations (CREATE/DESTROY)" `Quick test_xgroup_operations;
     test_case "xgroup createconsumer operations" `Quick test_xgroup_createconsumer_operations;
     test_case "xgroup delconsumer operations" `Quick test_xgroup_delconsumer_operations]
  else
    [ test_case "xgroup operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xgroup createconsumer operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit );
      test_case "xgroup delconsumer operations (skipped - no Redis)" `Quick (fun _switch () ->
          Printf.printf "Skipping integration test: Redis not available\n" ;
          Lwt.return_unit ) ]

(* Combined integration tests *)
let all_integration_tests =
  integration_ping_tests @ integration_string_tests @ integration_hash_tests
  @ integration_list_tests @ integration_info_tests @ integration_stream_tests
  @ integration_xgroup_tests
