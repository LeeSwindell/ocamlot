open Test_resp3_parsers
open Test_resp3_serializers
open Test_connection
open Test_commands
open Test_integration_alcotest

(* Convert sync tests to lwt tests *)
let sync_to_lwt test_list =
  List.map (fun (name, speed, test_fn) -> 
    (name, speed, fun _switch -> 
      test_fn (); 
      Lwt.return_unit)) test_list

let () =
  Lwt_main.run (Alcotest_lwt.run "RESP3 and Connection Tests" [
    (* Parser tests *)
    "parser: simple string", sync_to_lwt simple_string_parser_tests;
    "parser: simple error", sync_to_lwt simple_error_parser_tests;
    "parser: bulk string", sync_to_lwt bulk_string_parser_tests;
    "parser: integer", sync_to_lwt integer_parser_tests;
    "parser: array", sync_to_lwt array_parser_tests;
    "parser: null", sync_to_lwt null_parser_tests;
    "parser: boolean", sync_to_lwt boolean_parser_tests;
    "parser: double", sync_to_lwt double_parser_tests;
    "parser: big number", sync_to_lwt big_number_parser_tests;
    "parser: bulk error", sync_to_lwt bulk_error_parser_tests;
    "parser: verbatim string", sync_to_lwt verbatim_string_parser_tests;
    "parser: map", sync_to_lwt map_parser_tests;
    "parser: attribute", sync_to_lwt attribute_parser_tests;
    "parser: set", sync_to_lwt set_parser_tests;
    "parser: push", sync_to_lwt push_parser_tests;
    "parser: invalid", sync_to_lwt invalid_parser_tests;
    "buffered parser: all", sync_to_lwt buffered_parser_tests;
    
    (* Serializer tests *)
    "serializer: simple string", sync_to_lwt simple_string_serializer_tests;
    "serializer: simple error", sync_to_lwt simple_error_serializer_tests;
    "serializer: integer", sync_to_lwt integer_serializer_tests;
    "serializer: bulk string", sync_to_lwt bulk_string_serializer_tests;
    "serializer: array", sync_to_lwt array_serializer_tests;
    "serializer: null", sync_to_lwt null_serializer_tests;
    "serializer: boolean", sync_to_lwt boolean_serializer_tests;
    "serializer: double", sync_to_lwt double_serializer_tests;
    "serializer: big number", sync_to_lwt big_number_serializer_tests;
    "serializer: bulk error", sync_to_lwt bulk_error_serializer_tests;
    "serializer: verbatim string", sync_to_lwt verbatim_string_serializer_tests;
    "serializer: map", sync_to_lwt map_serializer_tests;
    "serializer: attribute", sync_to_lwt attribute_serializer_tests;
    "serializer: set", sync_to_lwt set_serializer_tests;
    "serializer: push", sync_to_lwt push_serializer_tests;
    
    (* Connection tests *)
    "connection: basic", sync_to_lwt connection_tests;
    "connection: advanced", sync_to_lwt advanced_connection_tests;
    
    (* Command tests *)
    "commands: ping", sync_to_lwt all_ping_tests;
    "commands: get", sync_to_lwt get_command_tests;
    "commands: set", sync_to_lwt set_command_tests;
    "commands: hget", sync_to_lwt hget_command_tests;
    "commands: hset", sync_to_lwt hset_command_tests;
    "commands: incr", sync_to_lwt incr_command_tests;
    "commands: del", sync_to_lwt del_command_tests;
    "commands: edge cases", sync_to_lwt edge_case_tests;
    
    (* RESP3 data type tests *)
    "resp3: boolean responses", sync_to_lwt boolean_response_tests;
    "resp3: double responses", sync_to_lwt double_response_tests;
    "resp3: map responses", sync_to_lwt map_response_tests;
    "resp3: set responses", sync_to_lwt set_response_tests;
    "resp3: integer responses", sync_to_lwt integer_response_tests;
    "resp3: null responses", sync_to_lwt null_response_tests;
    "resp3: mixed arrays", sync_to_lwt mixed_array_tests;
    "resp3: protocol compliance", sync_to_lwt resp3_compliance_tests;
    
    (* Comprehensive command category tests *)
    "commands: connection & server", sync_to_lwt connection_server_tests;
    "commands: key management", sync_to_lwt key_management_tests;
    "commands: strings extended", sync_to_lwt string_command_tests;
    "commands: hashes extended", sync_to_lwt hash_command_tests;
    "commands: lists", sync_to_lwt list_command_tests;
    "commands: sets", sync_to_lwt redis_set_command_tests;
    "commands: sorted sets", sync_to_lwt sorted_set_command_tests;
    "commands: pub/sub", sync_to_lwt pubsub_command_tests;
    "commands: transactions", sync_to_lwt transaction_command_tests;
    
    (* Integration tests (conditional on Redis availability) *)
    "integration: ping", integration_ping_tests;
    "integration: strings", integration_string_tests;
    "integration: hashes", integration_hash_tests;
    "integration: lists", integration_list_tests;
    "integration: info", integration_info_tests;
  ])