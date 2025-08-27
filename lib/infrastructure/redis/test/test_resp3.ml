open Alcotest
open Test_resp3_parsers
open Test_resp3_serializers
open Test_connection
open Test_commands

let () =
  run "RESP3 and Connection Tests" [
    (* Parser tests *)
    "parser: simple string", simple_string_parser_tests;
    "parser: simple error", simple_error_parser_tests;
    "parser: bulk string", bulk_string_parser_tests;
    "parser: integer", integer_parser_tests;
    "parser: array", array_parser_tests;
    "parser: null", null_parser_tests;
    "parser: boolean", boolean_parser_tests;
    "parser: double", double_parser_tests;
    "parser: big number", big_number_parser_tests;
    "parser: bulk error", bulk_error_parser_tests;
    "parser: verbatim string", verbatim_string_parser_tests;
    "parser: map", map_parser_tests;
    "parser: attribute", attribute_parser_tests;
    "parser: set", set_parser_tests;
    "parser: push", push_parser_tests;
    "parser: invalid", invalid_parser_tests;
    "buffered parser: all", buffered_parser_tests;
    
    (* Serializer tests *)
    "serializer: simple string", simple_string_serializer_tests;
    "serializer: simple error", simple_error_serializer_tests;
    "serializer: integer", integer_serializer_tests;
    "serializer: bulk string", bulk_string_serializer_tests;
    "serializer: array", array_serializer_tests;
    "serializer: null", null_serializer_tests;
    "serializer: boolean", boolean_serializer_tests;
    "serializer: double", double_serializer_tests;
    "serializer: big number", big_number_serializer_tests;
    "serializer: bulk error", bulk_error_serializer_tests;
    "serializer: verbatim string", verbatim_string_serializer_tests;
    "serializer: map", map_serializer_tests;
    "serializer: attribute", attribute_serializer_tests;
    "serializer: set", set_serializer_tests;
    "serializer: push", push_serializer_tests;
    
    (* Connection tests *)
    "connection: basic", connection_tests;
    "connection: advanced", advanced_connection_tests;
    
    (* Command tests *)
    "commands: ping", all_ping_tests;
    "commands: get", get_command_tests;
    "commands: set", set_command_tests;
    "commands: hget", hget_command_tests;
    "commands: hset", hset_command_tests;
    "commands: incr", incr_command_tests;
    "commands: del", del_command_tests;
    "commands: edge cases", edge_case_tests;
    
    (* RESP3 data type tests *)
    "resp3: boolean responses", boolean_response_tests;
    "resp3: double responses", double_response_tests;
    "resp3: map responses", map_response_tests;
    "resp3: set responses", set_response_tests;
    "resp3: integer responses", integer_response_tests;
    "resp3: null responses", null_response_tests;
    "resp3: mixed arrays", mixed_array_tests;
    "resp3: protocol compliance", resp3_compliance_tests;
    
    (* Comprehensive command category tests *)
    "commands: connection & server", connection_server_tests;
    "commands: key management", key_management_tests;
    "commands: strings extended", string_command_tests;
    "commands: hashes extended", hash_command_tests;
    "commands: lists", list_command_tests;
    "commands: sets", redis_set_command_tests;
    "commands: sorted sets", sorted_set_command_tests;
    "commands: pub/sub", pubsub_command_tests;
    "commands: transactions", transaction_command_tests;
  ]