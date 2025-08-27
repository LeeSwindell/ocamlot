open Alcotest
module Redis = Ocamlot_infrastructure_redis
open Redis.Resp3
open Redis.Commands

(* Reuse the testable from parser tests *)
let resp_value_testable = Alcotest.testable pp_resp_value equal_resp_value

(* Round-trip test helper for command testing
   This function:
   1. Constructs a command using the Commands module
   2. Serializes it to RESP3 wire format
   3. Parses it back to verify round-trip correctness
   4. Compares with expected structure *)
let test_command_roundtrip name command_fn expected_structure =
  test_case name `Quick (fun () ->
    let command = command_fn () in
    let serialized = Redis.Resp3.serialize_resp3 command in
    let parsed = Redis.Resp3.parse_string serialized in
    match parsed with
    | Ok actual -> 
        check resp_value_testable name expected_structure actual
    | Error msg -> 
        Alcotest.fail (Printf.sprintf "Parse failed for '%s': %s\nSerialized: %S" 
                      name msg serialized)
  )

(* Additional helper for checking that serialization produces expected string *)
let test_command_serialization name command_fn expected_serialized =
  test_case name `Quick (fun () ->
    let command = command_fn () in
    let serialized = Redis.Resp3.serialize_resp3 command in
    check string name expected_serialized serialized
  )

(* =============================================================================
   PING COMMAND TESTS
   ============================================================================= *)

let ping_command_tests = [
  (* Basic PING without message *)
  test_command_roundtrip 
    "ping without message" 
    (fun () -> ping ())
    (Array (Some [BulkString (Some "PING")]));
    
  (* PING with various message types *)
  test_command_roundtrip 
    "ping with simple message" 
    (fun () -> ping ~message:"hello" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "hello")]));
    
  test_command_roundtrip 
    "ping with empty message" 
    (fun () -> ping ~message:"" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "")]));
    
  test_command_roundtrip 
    "ping with space in message" 
    (fun () -> ping ~message:"hello world" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "hello world")]));
    
  test_command_roundtrip 
    "ping with newline in message" 
    (fun () -> ping ~message:"hello\nworld" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "hello\nworld")]));
    
  test_command_roundtrip 
    "ping with carriage return and newline" 
    (fun () -> ping ~message:"hello\r\nworld" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "hello\r\nworld")]));
    
  test_command_roundtrip 
    "ping with tab character" 
    (fun () -> ping ~message:"hello\tworld" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "hello\tworld")]));
    
  test_command_roundtrip 
    "ping with special characters" 
    (fun () -> ping ~message:"!@#$%^&*()_+-=[]{}|;':\",./<>?" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "!@#$%^&*()_+-=[]{}|;':\",./<>?")]));
    
  test_command_roundtrip 
    "ping with unicode emoji" 
    (fun () -> ping ~message:"Hello 👋 World 🌍" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "Hello 👋 World 🌍")]));
    
  test_command_roundtrip 
    "ping with unicode characters" 
    (fun () -> ping ~message:"你好世界" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "你好世界")]));
    
  test_command_roundtrip 
    "ping with very long message" 
    (fun () -> ping ~message:(String.make 1000 'x') ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some (String.make 1000 'x'))]));
    
  test_command_roundtrip 
    "ping with null bytes in message" 
    (fun () -> ping ~message:"hello\x00world" ())
    (Array (Some [BulkString (Some "PING"); BulkString (Some "hello\x00world")]));
]

(* Serialization format tests - verify the exact wire format *)
let ping_serialization_tests = [
  test_command_serialization
    "ping wire format without message"
    (fun () -> ping ())
    "*1\r\n$4\r\nPING\r\n";
    
  test_command_serialization
    "ping wire format with message"
    (fun () -> ping ~message:"PONG" ())
    "*2\r\n$4\r\nPING\r\n$4\r\nPONG\r\n";
    
  test_command_serialization
    "ping wire format with empty message"
    (fun () -> ping ~message:"" ())
    "*2\r\n$4\r\nPING\r\n$0\r\n\r\n";
]

(* Combine all PING tests *)
let all_ping_tests = 
  ping_command_tests @ ping_serialization_tests
