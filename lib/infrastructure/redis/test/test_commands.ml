open Alcotest
module Redis = Ocamlot_infrastructure_redis
open Redis.Resp3
open Redis.Commands

(* Reuse the testable from parser tests *)
let resp_value_testable = Alcotest.testable pp_resp_value equal_resp_value

(* Round-trip test helper for command testing This function: 1. Constructs a
   command using the Commands module 2. Serializes it to RESP3 wire format 3.
   Parses it back to verify round-trip correctness 4. Compares with expected
   structure *)
let test_command_roundtrip name command_fn expected_structure =
  test_case name `Quick (fun () ->
      let command = command_fn () in
      let serialized = Redis.Resp3.serialize_resp3 command in
      let parsed = Redis.Resp3.parse_string serialized in
      match parsed with
      | Ok actual -> check resp_value_testable name expected_structure actual
      | Error msg ->
          Alcotest.fail
            (Printf.sprintf "Parse failed for '%s': %s\nSerialized: %S" name
               msg serialized ) )

(* Additional helper for checking that serialization produces expected
   string *)
let test_command_serialization name command_fn expected_serialized =
  test_case name `Quick (fun () ->
      let command = command_fn () in
      let serialized = Redis.Resp3.serialize_resp3 command in
      check string name expected_serialized serialized )

(* =============================================================================
   PING COMMAND TESTS
   ============================================================================= *)

let ping_command_tests =
  [ (* Basic PING without message *)
    test_command_roundtrip "ping without message"
      (fun () -> ping ())
      (ping ())
  ; (* PING with various message types *)
    test_command_roundtrip "ping with simple message"
      (fun () -> ping ~message:"hello" ())
      (ping ~message:"hello" ())
  ; test_command_roundtrip "ping with empty message"
      (fun () -> ping ~message:"" ())
      (ping ~message:"" ())
  ; test_command_roundtrip "ping with space in message"
      (fun () -> ping ~message:"hello world" ())
      (ping ~message:"hello world" ())
  ; test_command_roundtrip "ping with newline in message"
      (fun () -> ping ~message:"hello\nworld" ())
      (ping ~message:"hello\nworld" ())
  ; test_command_roundtrip "ping with carriage return and newline"
      (fun () -> ping ~message:"hello\r\nworld" ())
      (ping ~message:"hello\r\nworld" ())
  ; test_command_roundtrip "ping with tab character"
      (fun () -> ping ~message:"hello\tworld" ())
      (ping ~message:"hello\tworld" ())
  ; test_command_roundtrip "ping with special characters"
      (fun () -> ping ~message:"!@#$%^&*()_+-=[]{}|;':\",./<>?" ())
      (ping ~message:"!@#$%^&*()_+-=[]{}|;':\",./<>?" ())
  ; test_command_roundtrip "ping with unicode emoji"
      (fun () -> ping ~message:"Hello 👋 World 🌍" ())
      (ping ~message:"Hello 👋 World 🌍" ())
  ; test_command_roundtrip "ping with unicode characters"
      (fun () -> ping ~message:"你好世界" ())
      (ping ~message:"你好世界" ())
  ; test_command_roundtrip "ping with very long message"
      (fun () -> ping ~message:(String.make 1000 'x') ())
      (ping ~message:(String.make 1000 'x') ())
  ; test_command_roundtrip "ping with null bytes in message"
      (fun () -> ping ~message:"hello\x00world" ())
      (ping ~message:"hello\x00world" ()) ]

(* Serialization format tests - verify the exact wire format *)
let ping_serialization_tests =
  [ test_command_serialization "ping wire format without message"
      (fun () -> ping ())
      "*1\r\n$4\r\nPING\r\n"
  ; test_command_serialization "ping wire format with message"
      (fun () -> ping ~message:"PONG" ())
      "*2\r\n$4\r\nPING\r\n$4\r\nPONG\r\n"
  ; test_command_serialization "ping wire format with empty message"
      (fun () -> ping ~message:"" ())
      "*2\r\n$4\r\nPING\r\n$0\r\n\r\n" ]

(* Combine all PING tests *)
let all_ping_tests = ping_command_tests @ ping_serialization_tests

(* =============================================================================
   CANONICAL REDIS COMMAND TESTS (from Official Documentation)
   ============================================================================= *)

(* These tests are based on the official Redis protocol documentation
   examples *)

(* GET Command Tests - Canonical Examples *)
let get_command_tests =
  [ test_command_roundtrip "get command structure"
      (fun () -> get "key")
      (get "key")
  ; test_command_serialization "get canonical wire format"
      (fun () -> get "key")
      "*2\r\n$3\r\nGET\r\n$3\r\nkey\r\n"
  ; test_command_serialization "get with longer key"
      (fun () -> get "mykey")
      "*2\r\n$3\r\nGET\r\n$5\r\nmykey\r\n" ]

(* SET Command Tests - Canonical Examples *)
let set_command_tests =
  [ test_command_roundtrip "set command structure"
      (fun () -> set "key" "value" ())
      (set "key" "value" ())
  ; test_command_serialization "set canonical wire format"
      (fun () -> set "key" "value" ())
      "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$5\r\nvalue\r\n"
  ; test_command_serialization "set with empty value"
      (fun () -> set "key" "" ())
      "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$0\r\n\r\n" ]

(* HGET Command Tests - Canonical Examples *)
let hget_command_tests =
  [ test_command_roundtrip "hget command structure"
      (fun () -> hget "myhash" "field")
      (hget "myhash" "field")
  ; test_command_serialization "hget canonical wire format"
      (fun () -> hget "myhash" "field")
      "*3\r\n$4\r\nHGET\r\n$6\r\nmyhash\r\n$5\r\nfield\r\n" ]

(* HSET Command Tests - Canonical Examples *)
let hset_command_tests =
  [ test_command_roundtrip "hset single field command structure"
      (fun () -> hset "myhash" [("field", "value")])
      (hset "myhash" [("field", "value")])
  ; test_command_serialization "hset single field canonical wire format"
      (fun () -> hset "myhash" [("field", "value")])
      "*4\r\n$4\r\nHSET\r\n$6\r\nmyhash\r\n$5\r\nfield\r\n$5\r\nvalue\r\n"
  ; test_command_roundtrip "hset multiple fields command structure"
      (fun () -> hset "myhash" [("field1", "value1"); ("field2", "value2")])
      (hset "myhash" [("field1", "value1"); ("field2", "value2")]) ]

(* INCR Command Tests - Canonical Examples *)
let incr_command_tests =
  [ test_command_roundtrip "incr command structure"
      (fun () -> incr "mycounter")
      (incr "mycounter")
  ; test_command_serialization "incr canonical wire format"
      (fun () -> incr "mycounter")
      "*2\r\n$4\r\nINCR\r\n$9\r\nmycounter\r\n" ]

(* DEL Command Tests - Canonical Examples *)
let del_command_tests =
  [ test_command_roundtrip "del single key command structure"
      (fun () -> del ["key"])
      (del ["key"])
  ; test_command_serialization "del single key canonical wire format"
      (fun () -> del ["key"])
      "*2\r\n$3\r\nDEL\r\n$3\r\nkey\r\n"
  ; test_command_roundtrip "del multiple keys command structure"
      (fun () -> del ["key1"; "key2"; "key3"])
      (del ["key1"; "key2"; "key3"])
  ; test_command_serialization "del multiple keys canonical wire format"
      (fun () -> del ["key1"; "key2"])
      "*3\r\n$3\r\nDEL\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n" ]

(* Edge Cases and Boundary Conditions *)
let edge_case_tests =
  [ (* Empty keys/values *)
    test_command_serialization "get with empty key"
      (fun () -> get "")
      "*2\r\n$3\r\nGET\r\n$0\r\n\r\n"
  ; test_command_serialization "set with empty key and value"
      (fun () -> set "" "" ())
      "*3\r\n$3\r\nSET\r\n$0\r\n\r\n$0\r\n\r\n"
  ; (* Keys with special characters *)
    test_command_serialization "get with key containing colon"
      (fun () -> get "user:1001")
      "*2\r\n$3\r\nGET\r\n$9\r\nuser:1001\r\n"
  ; test_command_serialization "set with key containing spaces"
      (fun () -> set "my key" "my value" ())
      "*3\r\n$3\r\nSET\r\n$6\r\nmy key\r\n$8\r\nmy value\r\n"
  ; (* Values with newlines and special chars *)
    test_command_serialization "set with value containing newlines"
      (fun () -> set "key" "line1\nline2" ())
      "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$11\r\nline1\nline2\r\n"
  ; test_command_serialization "set with value containing CRLF"
      (fun () -> set "key" "line1\r\nline2" ())
      "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$12\r\nline1\r\nline2\r\n"
  ; (* Binary data *)
    test_command_serialization "set with binary data (null bytes)"
      (fun () -> set "key" "hello\x00world" ())
      "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$11\r\nhello\x00world\r\n" ]

(* =============================================================================
   RESP3 DATA TYPE TESTS
   ============================================================================= *)

(* These tests verify that our commands can handle RESP3-specific
   responses *)

(* Commands that would return Boolean responses *)
let boolean_response_tests =
  [ (* SISMEMBER returns boolean in RESP3 *)
    test_command_roundtrip "sismember command structure"
      (fun () -> sismember "myset" "member")
      (sismember "myset" "member")
  ; test_command_serialization "sismember wire format"
      (fun () -> sismember "myset" "member")
      "*3\r\n$9\r\nSISMEMBER\r\n$5\r\nmyset\r\n$6\r\nmember\r\n" ]

(* Commands that would return Double responses *)
let double_response_tests =
  [ (* ZSCORE returns double in RESP3 *)
    test_command_roundtrip "zscore command structure"
      (fun () -> zscore "myzset" "member")
      (zscore "myzset" "member")
  ; test_command_serialization "zscore wire format"
      (fun () -> zscore "myzset" "member")
      "*3\r\n$6\r\nZSCORE\r\n$6\r\nmyzset\r\n$6\r\nmember\r\n" ]

(* Commands that would return Map responses *)
let map_response_tests =
  [ (* HGETALL returns map in RESP3 *)
    test_command_roundtrip "hgetall command structure"
      (fun () -> hgetall "myhash")
      (hgetall "myhash")
  ; test_command_serialization "hgetall wire format"
      (fun () -> hgetall "myhash")
      "*2\r\n$7\r\nHGETALL\r\n$6\r\nmyhash\r\n" ]

(* Commands that would return Set responses *)
let set_response_tests =
  [ (* SMEMBERS returns set in RESP3 *)
    test_command_roundtrip "smembers command structure"
      (fun () -> smembers "myset")
      (smembers "myset")
  ; test_command_serialization "smembers wire format"
      (fun () -> smembers "myset")
      "*2\r\n$8\r\nSMEMBERS\r\n$5\r\nmyset\r\n" ]

(* Commands demonstrating various integer responses *)
let integer_response_tests =
  [ (* INCR returns integer *)
    test_command_roundtrip "incr returns integer response"
      (fun () -> incr "counter")
      (incr "counter")
  ; (* DEL returns integer count *)
    test_command_roundtrip "del returns integer count"
      (fun () -> del ["key1"; "key2"])
      (del ["key1"; "key2"])
  ; (* These commands would return various integer values *)
    test_command_serialization "strlen wire format"
      (fun () -> strlen "mykey")
      "*2\r\n$6\r\nSTRLEN\r\n$5\r\nmykey\r\n"
  ; test_command_serialization "llen wire format"
      (fun () -> llen "mylist")
      "*2\r\n$4\r\nLLEN\r\n$6\r\nmylist\r\n"
  ; test_command_serialization "scard wire format"
      (fun () -> scard "myset")
      "*2\r\n$5\r\nSCARD\r\n$5\r\nmyset\r\n" ]

(* Commands demonstrating null responses *)
let null_response_tests =
  [ (* GET on non-existent key returns null *)
    test_command_roundtrip "get non-existent key"
      (fun () -> get "nonexistent")
      (get "nonexistent")
  ; (* HGET on non-existent field returns null *)
    test_command_roundtrip "hget non-existent field"
      (fun () -> hget "myhash" "nonexistent")
      (hget "myhash" "nonexistent") ]

(* Commands demonstrating arrays with mixed types *)
let mixed_array_tests =
  [ (* MGET returns array of bulk strings (some may be null) *)
    test_command_serialization "mget multiple keys wire format"
      (fun () -> mget ["key1"; "key2"; "key3"])
      "*4\r\n$4\r\nMGET\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n$4\r\nkey3\r\n"
  ; (* HMGET returns array of bulk strings *)
    test_command_serialization "hmget multiple fields wire format"
      (fun () ->
        Array
          (Some
             [ BulkString (Some "HMGET")
             ; BulkString (Some "myhash")
             ; BulkString (Some "field1")
             ; BulkString (Some "field2") ] ) )
      "*4\r\n$5\r\nHMGET\r\n$6\r\nmyhash\r\n$6\r\nfield1\r\n$6\r\nfield2\r\n"
  ; (* LRANGE returns array of bulk strings *)
    test_command_serialization "lrange wire format"
      (fun () -> lrange "mylist" 0 (-1))
      "*4\r\n$6\r\nLRANGE\r\n$6\r\nmylist\r\n$1\r\n0\r\n$2\r\n-1\r\n" ]

(* RESP3 Protocol Compliance Tests *)
let resp3_compliance_tests =
  [ (* Commands with large arguments to test bulk string size handling *)
    test_command_serialization "command with large argument"
      (fun () -> set "key" (String.make 1000 'x') ())
      ( "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$1000\r\n" ^ String.make 1000 'x'
      ^ "\r\n" )
  ; (* Commands with UTF-8 content *)
    test_command_serialization "command with unicode key and value"
      (fun () -> set "键" "值" ())
      "*3\r\n$3\r\nSET\r\n$3\r\n键\r\n$3\r\n值\r\n"
  ; (* Commands with binary data *)
    test_command_serialization "command with binary data"
      (fun () -> set "binary_key" "\x00\x01\x02\xFF" ())
      "*3\r\n$3\r\nSET\r\n$10\r\nbinary_key\r\n$4\r\n\x00\x01\x02\xFF\r\n" ]

(* Combined RESP3 data type tests *)
let all_resp3_tests =
  boolean_response_tests @ double_response_tests @ map_response_tests
  @ set_response_tests @ integer_response_tests @ null_response_tests
  @ mixed_array_tests @ resp3_compliance_tests

(* =============================================================================
   COMPREHENSIVE REDIS COMMAND CATEGORIES
   ============================================================================= *)

(* Connection and Server Commands *)
let connection_server_tests =
  [ (* PING - already tested above *)
    test_command_serialization "echo command"
      (fun () -> echo "Hello World")
      "*2\r\n$4\r\nECHO\r\n$11\r\nHello World\r\n"
  ; test_command_serialization "select database"
      (fun () -> select 1)
      "*2\r\n$6\r\nSELECT\r\n$1\r\n1\r\n"
  ; test_command_serialization "quit connection"
      (fun () -> quit ())
      "*1\r\n$4\r\nQUIT\r\n"
  ; test_command_serialization "info server"
      (fun () -> info ())
      "*1\r\n$4\r\nINFO\r\n" ]

(* Key Management Commands *)
let key_management_tests =
  [ (* DEL - already tested above *)
    test_command_serialization "exists single key"
      (fun () -> exists ["mykey"])
      "*2\r\n$6\r\nEXISTS\r\n$5\r\nmykey\r\n"
  ; test_command_serialization "exists multiple keys"
      (fun () -> exists ["key1"; "key2"])
      "*3\r\n$6\r\nEXISTS\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n"
  ; test_command_serialization "type command"
      (fun () -> type_of_key "mykey")
      "*2\r\n$4\r\nTYPE\r\n$5\r\nmykey\r\n"
  ; test_command_serialization "expire with seconds"
      (fun () -> expire "mykey" 300)
      "*3\r\n$6\r\nEXPIRE\r\n$5\r\nmykey\r\n$3\r\n300\r\n"
  ; test_command_serialization "ttl command"
      (fun () -> ttl "mykey")
      "*2\r\n$3\r\nTTL\r\n$5\r\nmykey\r\n"
  ; test_command_serialization "rename command"
      (fun () -> rename "oldkey" "newkey")
      "*3\r\n$6\r\nRENAME\r\n$6\r\noldkey\r\n$6\r\nnewkey\r\n"
  ; (* Additional key management commands *)
    test_command_serialization "expireat command"
      (fun () -> expireat "mykey" 1640995200L)
      "*3\r\n$8\r\nEXPIREAT\r\n$5\r\nmykey\r\n$10\r\n1640995200\r\n"
  ; test_command_serialization "persist command"
      (fun () -> persist "mykey")
      "*2\r\n$7\r\nPERSIST\r\n$5\r\nmykey\r\n"
  ; test_command_serialization "renamenx command"
      (fun () -> renamenx "oldkey" "newkey")
      "*3\r\n$8\r\nRENAMENX\r\n$6\r\noldkey\r\n$6\r\nnewkey\r\n"
  ; test_command_serialization "dump command"
      (fun () -> dump "mykey")
      "*2\r\n$4\r\nDUMP\r\n$5\r\nmykey\r\n"
  ; test_command_serialization "restore command"
      (fun () ->
        restore "mykey" 0
          "\x00\x05hello\x09\x00\x87\x2c\x97\x0b\x13\xfe\x40\x9e" () )
      "*4\r\n\
       $7\r\n\
       RESTORE\r\n\
       $5\r\n\
       mykey\r\n\
       $1\r\n\
       0\r\n\
       $17\r\n\
       \x00\x05hello\x09\x00\x87\x2c\x97\x0b\x13\xfe\x40\x9e\r\n"
  ; test_command_serialization "restore command with replace"
      (fun () ->
        restore "mykey" 0
          "\x00\x05hello\x09\x00\x87\x2c\x97\x0b\x13\xfe\x40\x9e"
          ~replace:true () )
      "*5\r\n\
       $7\r\n\
       RESTORE\r\n\
       $5\r\n\
       mykey\r\n\
       $1\r\n\
       0\r\n\
       $17\r\n\
       \x00\x05hello\x09\x00\x87\x2c\x97\x0b\x13\xfe\x40\x9e\r\n\
       $7\r\n\
       REPLACE\r\n"
  ; test_command_serialization "keys command"
      (fun () -> keys "*")
      "*2\r\n$4\r\nKEYS\r\n$1\r\n*\r\n"
  ; test_command_serialization "keys command with pattern"
      (fun () -> keys "user:*")
      "*2\r\n$4\r\nKEYS\r\n$6\r\nuser:*\r\n"
  ; test_command_serialization "scan command basic"
      (fun () -> scan 0 ())
      "*2\r\n$4\r\nSCAN\r\n$1\r\n0\r\n"
  ; test_command_serialization "scan command with match"
      (fun () -> scan 0 ~pattern:"user:*" ())
      "*4\r\n$4\r\nSCAN\r\n$1\r\n0\r\n$5\r\nMATCH\r\n$6\r\nuser:*\r\n"
  ; test_command_serialization "scan command with count"
      (fun () -> scan 0 ~count:10 ())
      "*4\r\n$4\r\nSCAN\r\n$1\r\n0\r\n$5\r\nCOUNT\r\n$2\r\n10\r\n"
  ; test_command_serialization "scan command with match and count"
      (fun () -> scan 0 ~pattern:"user:*" ~count:10 ())
      "*6\r\n\
       $4\r\n\
       SCAN\r\n\
       $1\r\n\
       0\r\n\
       $5\r\n\
       MATCH\r\n\
       $6\r\n\
       user:*\r\n\
       $5\r\n\
       COUNT\r\n\
       $2\r\n\
       10\r\n"
  ; test_command_serialization "randomkey command"
      (fun () -> randomkey ())
      "*1\r\n$9\r\nRANDOMKEY\r\n"
  ; test_command_serialization "move command"
      (fun () -> move "mykey" 1)
      "*3\r\n$4\r\nMOVE\r\n$5\r\nmykey\r\n$1\r\n1\r\n"
  ; (* Test round-trip serialization for key commands *)
    test_command_roundtrip "exists keys roundtrip"
      (fun () -> exists ["key1"; "key2"; "key3"])
      (exists ["key1"; "key2"; "key3"])
  ; test_command_roundtrip "type_of_key roundtrip"
      (fun () -> type_of_key "mykey")
      (type_of_key "mykey")
  ; test_command_roundtrip "expire roundtrip"
      (fun () -> expire "mykey" 3600)
      (expire "mykey" 3600)
  ; test_command_roundtrip "expireat roundtrip"
      (fun () -> expireat "mykey" 1640995200L)
      (expireat "mykey" 1640995200L)
  ; test_command_roundtrip "ttl roundtrip"
      (fun () -> ttl "mykey")
      (ttl "mykey")
  ; test_command_roundtrip "persist roundtrip"
      (fun () -> persist "mykey")
      (persist "mykey")
  ; test_command_roundtrip "rename roundtrip"
      (fun () -> rename "oldkey" "newkey")
      (rename "oldkey" "newkey")
  ; test_command_roundtrip "renamenx roundtrip"
      (fun () -> renamenx "oldkey" "newkey")
      (renamenx "oldkey" "newkey")
  ; test_command_roundtrip "dump roundtrip"
      (fun () -> dump "mykey")
      (dump "mykey")
  ; test_command_roundtrip "keys pattern roundtrip"
      (fun () -> keys "user:*:active")
      (keys "user:*:active")
  ; test_command_roundtrip "scan with options roundtrip"
      (fun () -> scan 42 ~pattern:"prefix:*" ~count:100 ())
      (scan 42 ~pattern:"prefix:*" ~count:100 ())
  ; test_command_roundtrip "randomkey roundtrip"
      (fun () -> randomkey ())
      (randomkey ())
  ; test_command_roundtrip "move roundtrip"
      (fun () -> move "mykey" 5)
      (move "mykey" 5) ]

(* String Commands (beyond GET/SET/INCR) *)
let string_command_tests =
  [ test_command_serialization "mget multiple keys"
      (fun () -> mget ["key1"; "key2"; "key3"])
      "*4\r\n$4\r\nMGET\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n$4\r\nkey3\r\n"
  ; test_command_serialization "mset multiple key-values"
      (fun () -> mset [("key1", "val1"); ("key2", "val2")])
      "*5\r\n\
       $4\r\n\
       MSET\r\n\
       $4\r\n\
       key1\r\n\
       $4\r\n\
       val1\r\n\
       $4\r\n\
       key2\r\n\
       $4\r\n\
       val2\r\n"
  ; test_command_serialization "incrby with amount"
      (fun () -> incrby "mycounter" 5)
      "*3\r\n$6\r\nINCRBY\r\n$9\r\nmycounter\r\n$1\r\n5\r\n"
  ; test_command_serialization "decr command"
      (fun () -> decr "mycounter")
      "*2\r\n$4\r\nDECR\r\n$9\r\nmycounter\r\n"
  ; test_command_serialization "append command"
      (fun () -> append "mykey" "suffix")
      "*3\r\n$6\r\nAPPEND\r\n$5\r\nmykey\r\n$6\r\nsuffix\r\n"
  ; test_command_serialization "strlen command"
      (fun () -> strlen "mykey")
      "*2\r\n$6\r\nSTRLEN\r\n$5\r\nmykey\r\n"
  ; (* Additional string commands *)
    test_command_serialization "set with expiration (EX)"
      (fun () -> set "mykey" "myvalue" ~ex:3600 ())
      "*5\r\n\
       $3\r\n\
       SET\r\n\
       $5\r\n\
       mykey\r\n\
       $7\r\n\
       myvalue\r\n\
       $2\r\n\
       EX\r\n\
       $4\r\n\
       3600\r\n"
  ; test_command_serialization "set with expiration (PX)"
      (fun () -> set "mykey" "myvalue" ~px:60000 ())
      "*5\r\n\
       $3\r\n\
       SET\r\n\
       $5\r\n\
       mykey\r\n\
       $7\r\n\
       myvalue\r\n\
       $2\r\n\
       PX\r\n\
       $5\r\n\
       60000\r\n"
  ; test_command_serialization "set with NX flag"
      (fun () -> set "mykey" "myvalue" ~nx:true ())
      "*4\r\n$3\r\nSET\r\n$5\r\nmykey\r\n$7\r\nmyvalue\r\n$2\r\nNX\r\n"
  ; test_command_serialization "set with XX flag"
      (fun () -> set "mykey" "myvalue" ~xx:true ())
      "*4\r\n$3\r\nSET\r\n$5\r\nmykey\r\n$7\r\nmyvalue\r\n$2\r\nXX\r\n"
  ; test_command_serialization "set with all options"
      (fun () -> set "mykey" "myvalue" ~ex:3600 ~nx:true ())
      "*6\r\n\
       $3\r\n\
       SET\r\n\
       $5\r\n\
       mykey\r\n\
       $7\r\n\
       myvalue\r\n\
       $2\r\n\
       EX\r\n\
       $4\r\n\
       3600\r\n\
       $2\r\n\
       NX\r\n"
  ; test_command_serialization "msetnx command"
      (fun () -> msetnx [("key1", "val1"); ("key2", "val2")])
      "*5\r\n\
       $6\r\n\
       MSETNX\r\n\
       $4\r\n\
       key1\r\n\
       $4\r\n\
       val1\r\n\
       $4\r\n\
       key2\r\n\
       $4\r\n\
       val2\r\n"
  ; test_command_serialization "getset command"
      (fun () -> getset "mykey" "newvalue")
      "*3\r\n$6\r\nGETSET\r\n$5\r\nmykey\r\n$8\r\nnewvalue\r\n"
  ; test_command_serialization "setnx command"
      (fun () -> setnx "mykey" "myvalue")
      "*3\r\n$5\r\nSETNX\r\n$5\r\nmykey\r\n$7\r\nmyvalue\r\n"
  ; test_command_serialization "setex command"
      (fun () -> setex "mykey" 3600 "myvalue")
      "*4\r\n$5\r\nSETEX\r\n$5\r\nmykey\r\n$4\r\n3600\r\n$7\r\nmyvalue\r\n"
  ; test_command_serialization "psetex command"
      (fun () -> psetex "mykey" 60000 "myvalue")
      "*4\r\n$6\r\nPSETEX\r\n$5\r\nmykey\r\n$5\r\n60000\r\n$7\r\nmyvalue\r\n"
  ; test_command_serialization "incrbyfloat command"
      (fun () -> incrbyfloat "mykey" 3.14)
      "*3\r\n$11\r\nINCRBYFLOAT\r\n$5\r\nmykey\r\n$4\r\n3.14\r\n"
  ; test_command_serialization "decrby command"
      (fun () -> decrby "mycounter" 5)
      "*3\r\n$6\r\nDECRBY\r\n$9\r\nmycounter\r\n$1\r\n5\r\n"
  ; test_command_serialization "getrange command"
      (fun () -> getrange "mykey" 0 4)
      "*4\r\n$8\r\nGETRANGE\r\n$5\r\nmykey\r\n$1\r\n0\r\n$1\r\n4\r\n"
  ; test_command_serialization "setrange command"
      (fun () -> setrange "mykey" 6 "world")
      "*4\r\n$8\r\nSETRANGE\r\n$5\r\nmykey\r\n$1\r\n6\r\n$5\r\nworld\r\n"
  ; (* Test round-trip serialization for string commands *)
    test_command_roundtrip "mget roundtrip"
      (fun () -> mget ["key1"; "key2"; "key3"])
      (mget ["key1"; "key2"; "key3"])
  ; test_command_roundtrip "mset roundtrip"
      (fun () -> mset [("key1", "val1"); ("key2", "val2")])
      (mset [("key1", "val1"); ("key2", "val2")])
  ; test_command_roundtrip "set with options roundtrip"
      (fun () -> set "mykey" "myvalue" ~ex:3600 ~nx:true ())
      (set "mykey" "myvalue" ~ex:3600 ~nx:true ())
  ; test_command_roundtrip "msetnx roundtrip"
      (fun () -> msetnx [("key1", "val1"); ("key2", "val2")])
      (msetnx [("key1", "val1"); ("key2", "val2")])
  ; test_command_roundtrip "getset roundtrip"
      (fun () -> getset "mykey" "newvalue")
      (getset "mykey" "newvalue")
  ; test_command_roundtrip "setnx roundtrip"
      (fun () -> setnx "mykey" "myvalue")
      (setnx "mykey" "myvalue")
  ; test_command_roundtrip "setex roundtrip"
      (fun () -> setex "mykey" 3600 "myvalue")
      (setex "mykey" 3600 "myvalue")
  ; test_command_roundtrip "psetex roundtrip"
      (fun () -> psetex "mykey" 60000 "myvalue")
      (psetex "mykey" 60000 "myvalue")
  ; test_command_roundtrip "incrbyfloat roundtrip"
      (fun () -> incrbyfloat "mykey" 2.718)
      (incrbyfloat "mykey" 2.718)
  ; test_command_roundtrip "decrby roundtrip"
      (fun () -> decrby "mycounter" 10)
      (decrby "mycounter" 10)
  ; test_command_roundtrip "getrange roundtrip"
      (fun () -> getrange "mykey" 5 10)
      (getrange "mykey" 5 10)
  ; test_command_roundtrip "setrange roundtrip"
      (fun () -> setrange "mykey" 3 "hello")
      (setrange "mykey" 3 "hello") ]

(* Hash Commands (beyond HGET/HSET) *)
let hash_command_tests =
  [ test_command_serialization "hmget multiple fields"
      (fun () -> hmget "myhash" ["field1"; "field2"])
      "*4\r\n$5\r\nHMGET\r\n$6\r\nmyhash\r\n$6\r\nfield1\r\n$6\r\nfield2\r\n"
  ; test_command_serialization "hgetall command"
      (fun () -> hgetall "myhash")
      "*2\r\n$7\r\nHGETALL\r\n$6\r\nmyhash\r\n"
  ; test_command_serialization "hkeys command"
      (fun () -> hkeys "myhash")
      "*2\r\n$5\r\nHKEYS\r\n$6\r\nmyhash\r\n"
  ; test_command_serialization "hvals command"
      (fun () -> hvals "myhash")
      "*2\r\n$5\r\nHVALS\r\n$6\r\nmyhash\r\n"
  ; test_command_serialization "hlen command"
      (fun () -> hlen "myhash")
      "*2\r\n$4\r\nHLEN\r\n$6\r\nmyhash\r\n"
  ; test_command_serialization "hdel command"
      (fun () -> hdel "myhash" ["field1"; "field2"])
      "*4\r\n$4\r\nHDEL\r\n$6\r\nmyhash\r\n$6\r\nfield1\r\n$6\r\nfield2\r\n"
  ; test_command_serialization "hexists command"
      (fun () -> hexists "myhash" "field")
      "*3\r\n$7\r\nHEXISTS\r\n$6\r\nmyhash\r\n$5\r\nfield\r\n"
  ; (* Additional hash commands *)
    test_command_serialization "hmset command"
      (fun () -> hmset "myhash" [("field1", "value1"); ("field2", "value2")])
      "*6\r\n\
       $5\r\n\
       HMSET\r\n\
       $6\r\n\
       myhash\r\n\
       $6\r\n\
       field1\r\n\
       $6\r\n\
       value1\r\n\
       $6\r\n\
       field2\r\n\
       $6\r\n\
       value2\r\n"
  ; test_command_serialization "hincrby command"
      (fun () -> hincrby "myhash" "counter" 10)
      "*4\r\n$7\r\nHINCRBY\r\n$6\r\nmyhash\r\n$7\r\ncounter\r\n$2\r\n10\r\n"
  ; test_command_serialization "hincrbyfloat command"
      (fun () -> hincrbyfloat "myhash" "score" 2.5)
      "*4\r\n\
       $12\r\n\
       HINCRBYFLOAT\r\n\
       $6\r\n\
       myhash\r\n\
       $5\r\n\
       score\r\n\
       $3\r\n\
       2.5\r\n"
  ; test_command_serialization "hscan command basic"
      (fun () -> hscan "myhash" 0 ())
      "*3\r\n$5\r\nHSCAN\r\n$6\r\nmyhash\r\n$1\r\n0\r\n"
  ; test_command_serialization "hscan command with match"
      (fun () -> hscan "myhash" 0 ~pattern:"field*" ())
      "*5\r\n\
       $5\r\n\
       HSCAN\r\n\
       $6\r\n\
       myhash\r\n\
       $1\r\n\
       0\r\n\
       $5\r\n\
       MATCH\r\n\
       $6\r\n\
       field*\r\n"
  ; test_command_serialization "hscan command with count"
      (fun () -> hscan "myhash" 0 ~count:5 ())
      "*5\r\n\
       $5\r\n\
       HSCAN\r\n\
       $6\r\n\
       myhash\r\n\
       $1\r\n\
       0\r\n\
       $5\r\n\
       COUNT\r\n\
       $1\r\n\
       5\r\n"
  ; test_command_serialization "hscan command with match and count"
      (fun () -> hscan "myhash" 0 ~pattern:"field*" ~count:10 ())
      "*7\r\n\
       $5\r\n\
       HSCAN\r\n\
       $6\r\n\
       myhash\r\n\
       $1\r\n\
       0\r\n\
       $5\r\n\
       MATCH\r\n\
       $6\r\n\
       field*\r\n\
       $5\r\n\
       COUNT\r\n\
       $2\r\n\
       10\r\n"
  ; test_command_serialization "hstrlen command"
      (fun () -> hstrlen "myhash" "field")
      "*3\r\n$7\r\nHSTRLEN\r\n$6\r\nmyhash\r\n$5\r\nfield\r\n"
  ; test_command_serialization "hsetnx command"
      (fun () -> hsetnx "myhash" "newfield" "newvalue")
      "*4\r\n\
       $6\r\n\
       HSETNX\r\n\
       $6\r\n\
       myhash\r\n\
       $8\r\n\
       newfield\r\n\
       $8\r\n\
       newvalue\r\n"
  ; (* Test round-trip serialization for hash commands *)
    test_command_roundtrip "hmget roundtrip"
      (fun () -> hmget "myhash" ["field1"; "field2"; "field3"])
      (hmget "myhash" ["field1"; "field2"; "field3"])
  ; test_command_roundtrip "hgetall roundtrip"
      (fun () -> hgetall "myhash")
      (hgetall "myhash")
  ; test_command_roundtrip "hkeys roundtrip"
      (fun () -> hkeys "myhash")
      (hkeys "myhash")
  ; test_command_roundtrip "hvals roundtrip"
      (fun () -> hvals "myhash")
      (hvals "myhash")
  ; test_command_roundtrip "hlen roundtrip"
      (fun () -> hlen "myhash")
      (hlen "myhash")
  ; test_command_roundtrip "hdel roundtrip"
      (fun () -> hdel "myhash" ["field1"; "field2"])
      (hdel "myhash" ["field1"; "field2"])
  ; test_command_roundtrip "hexists roundtrip"
      (fun () -> hexists "myhash" "field")
      (hexists "myhash" "field")
  ; test_command_roundtrip "hmset roundtrip"
      (fun () -> hmset "myhash" [("field1", "value1"); ("field2", "value2")])
      (hmset "myhash" [("field1", "value1"); ("field2", "value2")])
  ; test_command_roundtrip "hincrby roundtrip"
      (fun () -> hincrby "myhash" "counter" 5)
      (hincrby "myhash" "counter" 5)
  ; test_command_roundtrip "hincrbyfloat roundtrip"
      (fun () -> hincrbyfloat "myhash" "score" 1.5)
      (hincrbyfloat "myhash" "score" 1.5)
  ; test_command_roundtrip "hscan with options roundtrip"
      (fun () -> hscan "myhash" 42 ~pattern:"field*" ~count:20 ())
      (hscan "myhash" 42 ~pattern:"field*" ~count:20 ())
  ; test_command_roundtrip "hstrlen roundtrip"
      (fun () -> hstrlen "myhash" "longfield")
      (hstrlen "myhash" "longfield")
  ; test_command_roundtrip "hsetnx roundtrip"
      (fun () -> hsetnx "myhash" "conditionalfield" "conditionalvalue")
      (hsetnx "myhash" "conditionalfield" "conditionalvalue") ]

(* List Commands *)
let list_command_tests =
  [ test_command_serialization "lpush single value"
      (fun () -> lpush "mylist" ["value"])
      "*3\r\n$5\r\nLPUSH\r\n$6\r\nmylist\r\n$5\r\nvalue\r\n"
  ; test_command_serialization "lpush multiple values"
      (fun () -> lpush "mylist" ["val1"; "val2"])
      "*4\r\n$5\r\nLPUSH\r\n$6\r\nmylist\r\n$4\r\nval1\r\n$4\r\nval2\r\n"
  ; test_command_serialization "rpush command"
      (fun () -> rpush "mylist" ["value"])
      "*3\r\n$5\r\nRPUSH\r\n$6\r\nmylist\r\n$5\r\nvalue\r\n"
  ; test_command_serialization "lpop command"
      (fun () -> lpop "mylist")
      "*2\r\n$4\r\nLPOP\r\n$6\r\nmylist\r\n"
  ; test_command_serialization "rpop command"
      (fun () -> rpop "mylist")
      "*2\r\n$4\r\nRPOP\r\n$6\r\nmylist\r\n"
  ; test_command_serialization "llen command"
      (fun () -> llen "mylist")
      "*2\r\n$4\r\nLLEN\r\n$6\r\nmylist\r\n"
  ; test_command_serialization "lrange command"
      (fun () -> lrange "mylist" 0 (-1))
      "*4\r\n$6\r\nLRANGE\r\n$6\r\nmylist\r\n$1\r\n0\r\n$2\r\n-1\r\n"
  ; test_command_serialization "lindex command"
      (fun () -> lindex "mylist" 0)
      "*3\r\n$6\r\nLINDEX\r\n$6\r\nmylist\r\n$1\r\n0\r\n"
  ; (* Additional list commands *)
    test_command_serialization "ltrim command"
      (fun () -> ltrim "mylist" 1 5)
      "*4\r\n$5\r\nLTRIM\r\n$6\r\nmylist\r\n$1\r\n1\r\n$1\r\n5\r\n"
  ; test_command_serialization "lset command"
      (fun () -> lset "mylist" 2 "newvalue")
      "*4\r\n$4\r\nLSET\r\n$6\r\nmylist\r\n$1\r\n2\r\n$8\r\nnewvalue\r\n"
  ; test_command_serialization "lrem command positive count"
      (fun () -> lrem "mylist" 2 "value")
      "*4\r\n$4\r\nLREM\r\n$6\r\nmylist\r\n$1\r\n2\r\n$5\r\nvalue\r\n"
  ; test_command_serialization "lrem command negative count"
      (fun () -> lrem "mylist" (-1) "value")
      "*4\r\n$4\r\nLREM\r\n$6\r\nmylist\r\n$2\r\n-1\r\n$5\r\nvalue\r\n"
  ; test_command_serialization "lrem command zero count"
      (fun () -> lrem "mylist" 0 "value")
      "*4\r\n$4\r\nLREM\r\n$6\r\nmylist\r\n$1\r\n0\r\n$5\r\nvalue\r\n"
  ; test_command_serialization "linsert before"
      (fun () -> linsert "mylist" ~before:true "pivot" "newvalue")
      "*5\r\n\
       $7\r\n\
       LINSERT\r\n\
       $6\r\n\
       mylist\r\n\
       $6\r\n\
       BEFORE\r\n\
       $5\r\n\
       pivot\r\n\
       $8\r\n\
       newvalue\r\n"
  ; test_command_serialization "linsert after"
      (fun () -> linsert "mylist" ~before:false "pivot" "newvalue")
      "*5\r\n\
       $7\r\n\
       LINSERT\r\n\
       $6\r\n\
       mylist\r\n\
       $5\r\n\
       AFTER\r\n\
       $5\r\n\
       pivot\r\n\
       $8\r\n\
       newvalue\r\n"
  ; test_command_serialization "blpop single key"
      (fun () -> blpop ["mylist"] 10.0)
      "*3\r\n$5\r\nBLPOP\r\n$6\r\nmylist\r\n$2\r\n10\r\n"
  ; test_command_serialization "blpop multiple keys"
      (fun () -> blpop ["list1"; "list2"; "list3"] 5.5)
      "*5\r\n\
       $5\r\n\
       BLPOP\r\n\
       $5\r\n\
       list1\r\n\
       $5\r\n\
       list2\r\n\
       $5\r\n\
       list3\r\n\
       $3\r\n\
       5.5\r\n"
  ; test_command_serialization "brpop single key"
      (fun () -> brpop ["mylist"] 0.0)
      "*3\r\n$5\r\nBRPOP\r\n$6\r\nmylist\r\n$1\r\n0\r\n"
  ; test_command_serialization "brpop multiple keys"
      (fun () -> brpop ["list1"; "list2"] 30.0)
      "*4\r\n$5\r\nBRPOP\r\n$5\r\nlist1\r\n$5\r\nlist2\r\n$2\r\n30\r\n"
  ; test_command_serialization "brpoplpush command"
      (fun () -> brpoplpush "source" "dest" 15.0)
      "*4\r\n$10\r\nBRPOPLPUSH\r\n$6\r\nsource\r\n$4\r\ndest\r\n$2\r\n15\r\n"
  ; test_command_serialization "rpoplpush command"
      (fun () -> rpoplpush "source" "dest")
      "*3\r\n$9\r\nRPOPLPUSH\r\n$6\r\nsource\r\n$4\r\ndest\r\n"
  ; (* Test round-trip serialization for list commands *)
    test_command_roundtrip "lpush roundtrip"
      (fun () -> lpush "mylist" ["val1"; "val2"; "val3"])
      (lpush "mylist" ["val1"; "val2"; "val3"])
  ; test_command_roundtrip "rpush roundtrip"
      (fun () -> rpush "mylist" ["val1"; "val2"])
      (rpush "mylist" ["val1"; "val2"])
  ; test_command_roundtrip "lpop roundtrip"
      (fun () -> lpop "mylist")
      (lpop "mylist")
  ; test_command_roundtrip "rpop roundtrip"
      (fun () -> rpop "mylist")
      (rpop "mylist")
  ; test_command_roundtrip "llen roundtrip"
      (fun () -> llen "mylist")
      (llen "mylist")
  ; test_command_roundtrip "lrange roundtrip"
      (fun () -> lrange "mylist" 1 10)
      (lrange "mylist" 1 10)
  ; test_command_roundtrip "lindex roundtrip"
      (fun () -> lindex "mylist" 5)
      (lindex "mylist" 5)
  ; test_command_roundtrip "ltrim roundtrip"
      (fun () -> ltrim "mylist" 0 99)
      (ltrim "mylist" 0 99)
  ; test_command_roundtrip "lset roundtrip"
      (fun () -> lset "mylist" 3 "replacement")
      (lset "mylist" 3 "replacement")
  ; test_command_roundtrip "lrem roundtrip"
      (fun () -> lrem "mylist" (-2) "target")
      (lrem "mylist" (-2) "target")
  ; test_command_roundtrip "linsert roundtrip"
      (fun () -> linsert "mylist" ~before:true "anchor" "insertion")
      (linsert "mylist" ~before:true "anchor" "insertion")
  ; test_command_roundtrip "blpop roundtrip"
      (fun () -> blpop ["queue1"; "queue2"] 60.0)
      (blpop ["queue1"; "queue2"] 60.0)
  ; test_command_roundtrip "brpop roundtrip"
      (fun () -> brpop ["queue1"; "queue2"; "queue3"] 120.5)
      (brpop ["queue1"; "queue2"; "queue3"] 120.5)
  ; test_command_roundtrip "brpoplpush roundtrip"
      (fun () -> brpoplpush "workqueue" "processing" 45.0)
      (brpoplpush "workqueue" "processing" 45.0)
  ; test_command_roundtrip "rpoplpush roundtrip"
      (fun () -> rpoplpush "source_list" "dest_list")
      (rpoplpush "source_list" "dest_list") ]

(* Set Commands *)
let redis_set_command_tests =
  [ test_command_serialization "sadd single member"
      (fun () -> sadd "myset" ["member"])
      "*3\r\n$4\r\nSADD\r\n$5\r\nmyset\r\n$6\r\nmember\r\n"
  ; test_command_serialization "sadd multiple members"
      (fun () -> sadd "myset" ["member1"; "member2"])
      "*4\r\n$4\r\nSADD\r\n$5\r\nmyset\r\n$7\r\nmember1\r\n$7\r\nmember2\r\n"
  ; test_command_serialization "srem command"
      (fun () -> srem "myset" ["member"])
      "*3\r\n$4\r\nSREM\r\n$5\r\nmyset\r\n$6\r\nmember\r\n"
  ; test_command_serialization "scard command"
      (fun () -> scard "myset")
      "*2\r\n$5\r\nSCARD\r\n$5\r\nmyset\r\n"
  ; test_command_serialization "sismember command"
      (fun () -> sismember "myset" "member")
      "*3\r\n$9\r\nSISMEMBER\r\n$5\r\nmyset\r\n$6\r\nmember\r\n"
  ; test_command_serialization "smembers command"
      (fun () -> smembers "myset")
      "*2\r\n$8\r\nSMEMBERS\r\n$5\r\nmyset\r\n"
  ; test_command_serialization "sinter command"
      (fun () -> sinter ["set1"; "set2"])
      "*3\r\n$6\r\nSINTER\r\n$4\r\nset1\r\n$4\r\nset2\r\n"
  ; (* Additional set commands *)
    test_command_serialization "srandmember without count"
      (fun () -> srandmember "myset" ())
      "*2\r\n$11\r\nSRANDMEMBER\r\n$5\r\nmyset\r\n"
  ; test_command_serialization "srandmember with count"
      (fun () -> srandmember "myset" ~count:3 ())
      "*3\r\n$11\r\nSRANDMEMBER\r\n$5\r\nmyset\r\n$1\r\n3\r\n"
  ; test_command_serialization "srandmember with negative count"
      (fun () -> srandmember "myset" ~count:(-2) ())
      "*3\r\n$11\r\nSRANDMEMBER\r\n$5\r\nmyset\r\n$2\r\n-2\r\n"
  ; test_command_serialization "spop without count"
      (fun () -> spop "myset" ())
      "*2\r\n$4\r\nSPOP\r\n$5\r\nmyset\r\n"
  ; test_command_serialization "spop with count"
      (fun () -> spop "myset" ~count:5 ())
      "*3\r\n$4\r\nSPOP\r\n$5\r\nmyset\r\n$1\r\n5\r\n"
  ; test_command_serialization "smove command"
      (fun () -> smove "source" "dest" "member")
      "*4\r\n$5\r\nSMOVE\r\n$6\r\nsource\r\n$4\r\ndest\r\n$6\r\nmember\r\n"
  ; test_command_serialization "sinterstore command"
      (fun () -> sinterstore "result" ["set1"; "set2"; "set3"])
      "*5\r\n\
       $11\r\n\
       SINTERSTORE\r\n\
       $6\r\n\
       result\r\n\
       $4\r\n\
       set1\r\n\
       $4\r\n\
       set2\r\n\
       $4\r\n\
       set3\r\n"
  ; test_command_serialization "sunion command"
      (fun () -> sunion ["set1"; "set2"])
      "*3\r\n$6\r\nSUNION\r\n$4\r\nset1\r\n$4\r\nset2\r\n"
  ; test_command_serialization "sunionstore command"
      (fun () -> sunionstore "result" ["set1"; "set2"])
      "*4\r\n\
       $11\r\n\
       SUNIONSTORE\r\n\
       $6\r\n\
       result\r\n\
       $4\r\n\
       set1\r\n\
       $4\r\n\
       set2\r\n"
  ; test_command_serialization "sdiff command"
      (fun () -> sdiff ["set1"; "set2"; "set3"])
      "*4\r\n$5\r\nSDIFF\r\n$4\r\nset1\r\n$4\r\nset2\r\n$4\r\nset3\r\n"
  ; test_command_serialization "sdiffstore command"
      (fun () -> sdiffstore "result" ["set1"; "set2"])
      "*4\r\n\
       $10\r\n\
       SDIFFSTORE\r\n\
       $6\r\n\
       result\r\n\
       $4\r\n\
       set1\r\n\
       $4\r\n\
       set2\r\n"
  ; test_command_serialization "sscan command basic"
      (fun () -> sscan "myset" 0 ())
      "*3\r\n$5\r\nSSCAN\r\n$5\r\nmyset\r\n$1\r\n0\r\n"
  ; test_command_serialization "sscan command with match"
      (fun () -> sscan "myset" 0 ~pattern:"prefix*" ())
      "*5\r\n\
       $5\r\n\
       SSCAN\r\n\
       $5\r\n\
       myset\r\n\
       $1\r\n\
       0\r\n\
       $5\r\n\
       MATCH\r\n\
       $7\r\n\
       prefix*\r\n"
  ; test_command_serialization "sscan command with count"
      (fun () -> sscan "myset" 0 ~count:10 ())
      "*5\r\n\
       $5\r\n\
       SSCAN\r\n\
       $5\r\n\
       myset\r\n\
       $1\r\n\
       0\r\n\
       $5\r\n\
       COUNT\r\n\
       $2\r\n\
       10\r\n"
  ; test_command_serialization "sscan command with match and count"
      (fun () -> sscan "myset" 42 ~pattern:"item:*" ~count:20 ())
      "*7\r\n\
       $5\r\n\
       SSCAN\r\n\
       $5\r\n\
       myset\r\n\
       $2\r\n\
       42\r\n\
       $5\r\n\
       MATCH\r\n\
       $6\r\n\
       item:*\r\n\
       $5\r\n\
       COUNT\r\n\
       $2\r\n\
       20\r\n"
  ; (* Test round-trip serialization for set commands *)
    test_command_roundtrip "sadd roundtrip"
      (fun () -> sadd "myset" ["member1"; "member2"; "member3"])
      (sadd "myset" ["member1"; "member2"; "member3"])
  ; test_command_roundtrip "srem roundtrip"
      (fun () -> srem "myset" ["member1"; "member2"])
      (srem "myset" ["member1"; "member2"])
  ; test_command_roundtrip "scard roundtrip"
      (fun () -> scard "myset")
      (scard "myset")
  ; test_command_roundtrip "sismember roundtrip"
      (fun () -> sismember "myset" "member")
      (sismember "myset" "member")
  ; test_command_roundtrip "smembers roundtrip"
      (fun () -> smembers "myset")
      (smembers "myset")
  ; test_command_roundtrip "srandmember with count roundtrip"
      (fun () -> srandmember "myset" ~count:5 ())
      (srandmember "myset" ~count:5 ())
  ; test_command_roundtrip "spop with count roundtrip"
      (fun () -> spop "myset" ~count:3 ())
      (spop "myset" ~count:3 ())
  ; test_command_roundtrip "smove roundtrip"
      (fun () -> smove "source_set" "dest_set" "element")
      (smove "source_set" "dest_set" "element")
  ; test_command_roundtrip "sinter roundtrip"
      (fun () -> sinter ["set1"; "set2"; "set3"])
      (sinter ["set1"; "set2"; "set3"])
  ; test_command_roundtrip "sinterstore roundtrip"
      (fun () -> sinterstore "result_set" ["set1"; "set2"])
      (sinterstore "result_set" ["set1"; "set2"])
  ; test_command_roundtrip "sunion roundtrip"
      (fun () -> sunion ["set1"; "set2"])
      (sunion ["set1"; "set2"])
  ; test_command_roundtrip "sunionstore roundtrip"
      (fun () -> sunionstore "union_result" ["set1"; "set2"; "set3"])
      (sunionstore "union_result" ["set1"; "set2"; "set3"])
  ; test_command_roundtrip "sdiff roundtrip"
      (fun () -> sdiff ["set1"; "set2"])
      (sdiff ["set1"; "set2"])
  ; test_command_roundtrip "sdiffstore roundtrip"
      (fun () -> sdiffstore "diff_result" ["set1"; "set2"])
      (sdiffstore "diff_result" ["set1"; "set2"])
  ; test_command_roundtrip "sscan with options roundtrip"
      (fun () -> sscan "large_set" 100 ~pattern:"user:*" ~count:50 ())
      (sscan "large_set" 100 ~pattern:"user:*" ~count:50 ()) ]

(* Sorted Set Commands *)
let sorted_set_command_tests =
  [ test_command_serialization "zadd single score-member"
      (fun () -> zadd "myzset" [(100.0, "member")])
      "*4\r\n$4\r\nZADD\r\n$6\r\nmyzset\r\n$3\r\n100\r\n$6\r\nmember\r\n"
  ; test_command_serialization "zadd multiple score-members"
      (fun () -> zadd "myzset" [(100.0, "member1"); (200.0, "member2")])
      "*6\r\n\
       $4\r\n\
       ZADD\r\n\
       $6\r\n\
       myzset\r\n\
       $3\r\n\
       100\r\n\
       $7\r\n\
       member1\r\n\
       $3\r\n\
       200\r\n\
       $7\r\n\
       member2\r\n"
  ; test_command_serialization "zrem command"
      (fun () -> zrem "myzset" ["member"])
      "*3\r\n$4\r\nZREM\r\n$6\r\nmyzset\r\n$6\r\nmember\r\n"
  ; test_command_serialization "zcard command"
      (fun () -> zcard "myzset")
      "*2\r\n$5\r\nZCARD\r\n$6\r\nmyzset\r\n"
  ; test_command_serialization "zscore command"
      (fun () -> zscore "myzset" "member")
      "*3\r\n$6\r\nZSCORE\r\n$6\r\nmyzset\r\n$6\r\nmember\r\n"
  ; test_command_serialization "zrange command"
      (fun () -> zrange "myzset" 0 (-1) ())
      "*4\r\n$6\r\nZRANGE\r\n$6\r\nmyzset\r\n$1\r\n0\r\n$2\r\n-1\r\n"
  ; test_command_serialization "zrank command"
      (fun () -> zrank "myzset" "member")
      "*3\r\n$5\r\nZRANK\r\n$6\r\nmyzset\r\n$6\r\nmember\r\n"
  ; (* Additional sorted set commands *)
    test_command_serialization "zcount command"
      (fun () -> zcount "myzset" 10.0 100.0)
      "*4\r\n$6\r\nZCOUNT\r\n$6\r\nmyzset\r\n$2\r\n10\r\n$3\r\n100\r\n"
  ; test_command_serialization "zrevrank command"
      (fun () -> zrevrank "myzset" "member")
      "*3\r\n$8\r\nZREVRANK\r\n$6\r\nmyzset\r\n$6\r\nmember\r\n"
  ; test_command_serialization "zrevrange command"
      (fun () -> zrevrange "myzset" 0 (-1) ())
      "*4\r\n$9\r\nZREVRANGE\r\n$6\r\nmyzset\r\n$1\r\n0\r\n$2\r\n-1\r\n"
  ; test_command_serialization "zrevrange with withscores"
      (fun () -> zrevrange "myzset" 0 5 ~withscores:true ())
      "*5\r\n\
       $9\r\n\
       ZREVRANGE\r\n\
       $6\r\n\
       myzset\r\n\
       $1\r\n\
       0\r\n\
       $1\r\n\
       5\r\n\
       $10\r\n\
       WITHSCORES\r\n"
  ; test_command_serialization "zrangebyscore basic"
      (fun () -> zrangebyscore "myzset" 10.0 100.0 ())
      "*4\r\n\
       $13\r\n\
       ZRANGEBYSCORE\r\n\
       $6\r\n\
       myzset\r\n\
       $2\r\n\
       10\r\n\
       $3\r\n\
       100\r\n"
  ; test_command_serialization "zrangebyscore with withscores"
      (fun () -> zrangebyscore "myzset" 10.0 100.0 ~withscores:true ())
      "*5\r\n\
       $13\r\n\
       ZRANGEBYSCORE\r\n\
       $6\r\n\
       myzset\r\n\
       $2\r\n\
       10\r\n\
       $3\r\n\
       100\r\n\
       $10\r\n\
       WITHSCORES\r\n"
  ; test_command_serialization "zrangebyscore with limit"
      (fun () -> zrangebyscore "myzset" 10.0 100.0 ~limit:(5, 10) ())
      "*7\r\n\
       $13\r\n\
       ZRANGEBYSCORE\r\n\
       $6\r\n\
       myzset\r\n\
       $2\r\n\
       10\r\n\
       $3\r\n\
       100\r\n\
       $5\r\n\
       LIMIT\r\n\
       $1\r\n\
       5\r\n\
       $2\r\n\
       10\r\n"
  ; test_command_serialization "zrevrangebyscore basic"
      (fun () -> zrevrangebyscore "myzset" 100.0 10.0 ())
      "*4\r\n\
       $16\r\n\
       ZREVRANGEBYSCORE\r\n\
       $6\r\n\
       myzset\r\n\
       $3\r\n\
       100\r\n\
       $2\r\n\
       10\r\n"
  ; test_command_serialization "zrevrangebyscore with withscores and limit"
      (fun () ->
        zrevrangebyscore "myzset" 100.0 10.0 ~withscores:true ~limit:(0, 5)
          () )
      "*8\r\n\
       $16\r\n\
       ZREVRANGEBYSCORE\r\n\
       $6\r\n\
       myzset\r\n\
       $3\r\n\
       100\r\n\
       $2\r\n\
       10\r\n\
       $10\r\n\
       WITHSCORES\r\n\
       $5\r\n\
       LIMIT\r\n\
       $1\r\n\
       0\r\n\
       $1\r\n\
       5\r\n"
  ; test_command_serialization "zremrangebyrank command"
      (fun () -> zremrangebyrank "myzset" 0 10)
      "*4\r\n\
       $15\r\n\
       ZREMRANGEBYRANK\r\n\
       $6\r\n\
       myzset\r\n\
       $1\r\n\
       0\r\n\
       $2\r\n\
       10\r\n"
  ; test_command_serialization "zremrangebyscore command"
      (fun () -> zremrangebyscore "myzset" 10.0 100.0)
      "*4\r\n\
       $16\r\n\
       ZREMRANGEBYSCORE\r\n\
       $6\r\n\
       myzset\r\n\
       $2\r\n\
       10\r\n\
       $3\r\n\
       100\r\n"
  ; test_command_serialization "zincrby command"
      (fun () -> zincrby "myzset" 10.5 "member")
      "*4\r\n$7\r\nZINCRBY\r\n$6\r\nmyzset\r\n$4\r\n10.5\r\n$6\r\nmember\r\n"
  ; test_command_serialization "zinterstore basic"
      (fun () -> zinterstore "result" ["set1"; "set2"] ())
      "*5\r\n\
       $11\r\n\
       ZINTERSTORE\r\n\
       $6\r\n\
       result\r\n\
       $1\r\n\
       2\r\n\
       $4\r\n\
       set1\r\n\
       $4\r\n\
       set2\r\n"
  ; test_command_serialization "zinterstore with weights"
      (fun () -> zinterstore "result" ["set1"; "set2"] ~weights:[2.0; 0.5] ())
      "*8\r\n\
       $11\r\n\
       ZINTERSTORE\r\n\
       $6\r\n\
       result\r\n\
       $1\r\n\
       2\r\n\
       $4\r\n\
       set1\r\n\
       $4\r\n\
       set2\r\n\
       $7\r\n\
       WEIGHTS\r\n\
       $1\r\n\
       2\r\n\
       $3\r\n\
       0.5\r\n"
  ; test_command_serialization "zinterstore with aggregate"
      (fun () -> zinterstore "result" ["set1"; "set2"] ~aggregate:`MAX ())
      "*7\r\n\
       $11\r\n\
       ZINTERSTORE\r\n\
       $6\r\n\
       result\r\n\
       $1\r\n\
       2\r\n\
       $4\r\n\
       set1\r\n\
       $4\r\n\
       set2\r\n\
       $9\r\n\
       AGGREGATE\r\n\
       $3\r\n\
       MAX\r\n"
  ; test_command_serialization "zunionstore basic"
      (fun () -> zunionstore "result" ["set1"; "set2"; "set3"] ())
      "*6\r\n\
       $11\r\n\
       ZUNIONSTORE\r\n\
       $6\r\n\
       result\r\n\
       $1\r\n\
       3\r\n\
       $4\r\n\
       set1\r\n\
       $4\r\n\
       set2\r\n\
       $4\r\n\
       set3\r\n"
  ; test_command_serialization "zunionstore with weights and aggregate"
      (fun () ->
        zunionstore "result" ["set1"; "set2"] ~weights:[1.0; 2.0]
          ~aggregate:`MIN () )
      "*10\r\n\
       $11\r\n\
       ZUNIONSTORE\r\n\
       $6\r\n\
       result\r\n\
       $1\r\n\
       2\r\n\
       $4\r\n\
       set1\r\n\
       $4\r\n\
       set2\r\n\
       $7\r\n\
       WEIGHTS\r\n\
       $1\r\n\
       1\r\n\
       $1\r\n\
       2\r\n\
       $9\r\n\
       AGGREGATE\r\n\
       $3\r\n\
       MIN\r\n"
  ; test_command_serialization "zscan basic"
      (fun () -> zscan "myzset" 0 ())
      "*3\r\n$5\r\nZSCAN\r\n$6\r\nmyzset\r\n$1\r\n0\r\n"
  ; test_command_serialization "zscan with match"
      (fun () -> zscan "myzset" 0 ~pattern:"member:*" ())
      "*5\r\n\
       $5\r\n\
       ZSCAN\r\n\
       $6\r\n\
       myzset\r\n\
       $1\r\n\
       0\r\n\
       $5\r\n\
       MATCH\r\n\
       $8\r\n\
       member:*\r\n"
  ; test_command_serialization "zscan with count"
      (fun () -> zscan "myzset" 42 ~count:20 ())
      "*5\r\n\
       $5\r\n\
       ZSCAN\r\n\
       $6\r\n\
       myzset\r\n\
       $2\r\n\
       42\r\n\
       $5\r\n\
       COUNT\r\n\
       $2\r\n\
       20\r\n"
  ; test_command_serialization "zscan with match and count"
      (fun () -> zscan "myzset" 100 ~pattern:"user:*" ~count:50 ())
      "*7\r\n\
       $5\r\n\
       ZSCAN\r\n\
       $6\r\n\
       myzset\r\n\
       $3\r\n\
       100\r\n\
       $5\r\n\
       MATCH\r\n\
       $6\r\n\
       user:*\r\n\
       $5\r\n\
       COUNT\r\n\
       $2\r\n\
       50\r\n"
  ; (* Test round-trip serialization for sorted set commands *)
    test_command_roundtrip "zadd roundtrip"
      (fun () -> zadd "myzset" [(100.5, "member1"); (200.0, "member2")])
      (zadd "myzset" [(100.5, "member1"); (200.0, "member2")])
  ; test_command_roundtrip "zrem roundtrip"
      (fun () -> zrem "myzset" ["member1"; "member2"])
      (zrem "myzset" ["member1"; "member2"])
  ; test_command_roundtrip "zcard roundtrip"
      (fun () -> zcard "myzset")
      (zcard "myzset")
  ; test_command_roundtrip "zcount roundtrip"
      (fun () -> zcount "myzset" 50.0 150.0)
      (zcount "myzset" 50.0 150.0)
  ; test_command_roundtrip "zscore roundtrip"
      (fun () -> zscore "myzset" "member")
      (zscore "myzset" "member")
  ; test_command_roundtrip "zrank roundtrip"
      (fun () -> zrank "myzset" "member")
      (zrank "myzset" "member")
  ; test_command_roundtrip "zrevrank roundtrip"
      (fun () -> zrevrank "myzset" "member")
      (zrevrank "myzset" "member")
  ; test_command_roundtrip "zrange with withscores roundtrip"
      (fun () -> zrange "myzset" 0 10 ~withscores:true ())
      (zrange "myzset" 0 10 ~withscores:true ())
  ; test_command_roundtrip "zrevrange roundtrip"
      (fun () -> zrevrange "myzset" 0 (-1) ())
      (zrevrange "myzset" 0 (-1) ())
  ; test_command_roundtrip "zrangebyscore with options roundtrip"
      (fun () ->
        zrangebyscore "myzset" 10.0 100.0 ~withscores:true ~limit:(5, 15) () )
      (zrangebyscore "myzset" 10.0 100.0 ~withscores:true ~limit:(5, 15) ())
  ; test_command_roundtrip "zrevrangebyscore roundtrip"
      (fun () -> zrevrangebyscore "myzset" 100.0 10.0 ())
      (zrevrangebyscore "myzset" 100.0 10.0 ())
  ; test_command_roundtrip "zremrangebyrank roundtrip"
      (fun () -> zremrangebyrank "myzset" 5 15)
      (zremrangebyrank "myzset" 5 15)
  ; test_command_roundtrip "zremrangebyscore roundtrip"
      (fun () -> zremrangebyscore "myzset" 50.0 150.0)
      (zremrangebyscore "myzset" 50.0 150.0)
  ; test_command_roundtrip "zincrby roundtrip"
      (fun () -> zincrby "myzset" 25.5 "member")
      (zincrby "myzset" 25.5 "member")
  ; test_command_roundtrip "zinterstore with all options roundtrip"
      (fun () ->
        zinterstore "result" ["set1"; "set2"] ~weights:[1.5; 2.0]
          ~aggregate:`SUM () )
      (zinterstore "result" ["set1"; "set2"] ~weights:[1.5; 2.0]
         ~aggregate:`SUM () )
  ; test_command_roundtrip "zunionstore roundtrip"
      (fun () -> zunionstore "union_result" ["zset1"; "zset2"; "zset3"] ())
      (zunionstore "union_result" ["zset1"; "zset2"; "zset3"] ())
  ; test_command_roundtrip "zscan with options roundtrip"
      (fun () -> zscan "large_zset" 200 ~pattern:"item:*" ~count:100 ())
      (zscan "large_zset" 200 ~pattern:"item:*" ~count:100 ()) ]

(* Bitmap Commands *)
let bitmap_command_tests =
  [ (* SETBIT command tests *)
    test_command_serialization "setbit set bit to 1"
      (fun () -> setbit "mykey" 7 true)
      "*4\r\n$6\r\nSETBIT\r\n$5\r\nmykey\r\n$1\r\n7\r\n$1\r\n1\r\n"
  ; test_command_serialization "setbit set bit to 0"
      (fun () -> setbit "mykey" 100 false)
      "*4\r\n$6\r\nSETBIT\r\n$5\r\nmykey\r\n$3\r\n100\r\n$1\r\n0\r\n"
  ; test_command_serialization "setbit large offset"
      (fun () -> setbit "bitmap" 999999 true)
      "*4\r\n$6\r\nSETBIT\r\n$6\r\nbitmap\r\n$6\r\n999999\r\n$1\r\n1\r\n"
  ; (* GETBIT command tests *)
    test_command_serialization "getbit command"
      (fun () -> getbit "mykey" 7)
      "*3\r\n$6\r\nGETBIT\r\n$5\r\nmykey\r\n$1\r\n7\r\n"
  ; test_command_serialization "getbit large offset"
      (fun () -> getbit "bitmap" 100000)
      "*3\r\n$6\r\nGETBIT\r\n$6\r\nbitmap\r\n$6\r\n100000\r\n"
  ; (* BITCOUNT command tests *)
    test_command_serialization "bitcount full key"
      (fun () -> bitcount "mykey" ())
      "*2\r\n$8\r\nBITCOUNT\r\n$5\r\nmykey\r\n"
  ; test_command_serialization "bitcount with start"
      (fun () -> bitcount "mykey" ~start:1 ())
      "*3\r\n$8\r\nBITCOUNT\r\n$5\r\nmykey\r\n$1\r\n1\r\n"
  ; test_command_serialization "bitcount with start and end"
      (fun () -> bitcount "mykey" ~start:1 ~end_pos:100 ())
      "*4\r\n$8\r\nBITCOUNT\r\n$5\r\nmykey\r\n$1\r\n1\r\n$3\r\n100\r\n"
  ; (* BITOP command tests *)
    test_command_serialization "bitop AND operation"
      (fun () -> bitop `AND "result" ["key1"; "key2"])
      "*5\r\n\
       $5\r\n\
       BITOP\r\n\
       $3\r\n\
       AND\r\n\
       $6\r\n\
       result\r\n\
       $4\r\n\
       key1\r\n\
       $4\r\n\
       key2\r\n"
  ; test_command_serialization "bitop OR operation"
      (fun () -> bitop `OR "dest" ["source1"; "source2"; "source3"])
      "*6\r\n\
       $5\r\n\
       BITOP\r\n\
       $2\r\n\
       OR\r\n\
       $4\r\n\
       dest\r\n\
       $7\r\n\
       source1\r\n\
       $7\r\n\
       source2\r\n\
       $7\r\n\
       source3\r\n"
  ; test_command_serialization "bitop XOR operation"
      (fun () -> bitop `XOR "result" ["a"; "b"])
      "*5\r\n\
       $5\r\n\
       BITOP\r\n\
       $3\r\n\
       XOR\r\n\
       $6\r\n\
       result\r\n\
       $1\r\n\
       a\r\n\
       $1\r\n\
       b\r\n"
  ; test_command_serialization "bitop NOT operation"
      (fun () -> bitop `NOT "inverted" ["original"])
      "*4\r\n\
       $5\r\n\
       BITOP\r\n\
       $3\r\n\
       NOT\r\n\
       $8\r\n\
       inverted\r\n\
       $8\r\n\
       original\r\n"
  ; (* BITPOS command tests *)
    test_command_serialization "bitpos find 1 bit"
      (fun () -> bitpos "mykey" true ())
      "*3\r\n$6\r\nBITPOS\r\n$5\r\nmykey\r\n$1\r\n1\r\n"
  ; test_command_serialization "bitpos find 0 bit"
      (fun () -> bitpos "mykey" false ())
      "*3\r\n$6\r\nBITPOS\r\n$5\r\nmykey\r\n$1\r\n0\r\n"
  ; test_command_serialization "bitpos with start"
      (fun () -> bitpos "mykey" true ~start:10 ())
      "*4\r\n$6\r\nBITPOS\r\n$5\r\nmykey\r\n$1\r\n1\r\n$2\r\n10\r\n"
  ; test_command_serialization "bitpos with start and end"
      (fun () -> bitpos "mykey" false ~start:0 ~end_pos:50 ())
      "*5\r\n\
       $6\r\n\
       BITPOS\r\n\
       $5\r\n\
       mykey\r\n\
       $1\r\n\
       0\r\n\
       $1\r\n\
       0\r\n\
       $2\r\n\
       50\r\n"
  ; (* BITFIELD command tests *)
    test_command_serialization "bitfield single GET"
      (fun () -> bitfield "mykey" [Get ("i8", 0)])
      "*5\r\n\
       $8\r\n\
       BITFIELD\r\n\
       $5\r\n\
       mykey\r\n\
       $3\r\n\
       GET\r\n\
       $2\r\n\
       i8\r\n\
       $1\r\n\
       0\r\n"
  ; test_command_serialization "bitfield single SET"
      (fun () -> bitfield "mykey" [Set ("u4", 4, 15)])
      "*6\r\n\
       $8\r\n\
       BITFIELD\r\n\
       $5\r\n\
       mykey\r\n\
       $3\r\n\
       SET\r\n\
       $2\r\n\
       u4\r\n\
       $1\r\n\
       4\r\n\
       $2\r\n\
       15\r\n"
  ; test_command_serialization "bitfield single INCRBY"
      (fun () -> bitfield "mykey" [Incrby ("i16", 8, 100)])
      "*6\r\n\
       $8\r\n\
       BITFIELD\r\n\
       $5\r\n\
       mykey\r\n\
       $6\r\n\
       INCRBY\r\n\
       $3\r\n\
       i16\r\n\
       $1\r\n\
       8\r\n\
       $3\r\n\
       100\r\n"
  ; test_command_serialization "bitfield multiple operations"
      (fun () ->
        bitfield "mykey"
          [Get ("i8", 0); Set ("u4", 4, 15); Incrby ("i16", 8, 1)] )
      "*13\r\n\
       $8\r\n\
       BITFIELD\r\n\
       $5\r\n\
       mykey\r\n\
       $3\r\n\
       GET\r\n\
       $2\r\n\
       i8\r\n\
       $1\r\n\
       0\r\n\
       $3\r\n\
       SET\r\n\
       $2\r\n\
       u4\r\n\
       $1\r\n\
       4\r\n\
       $2\r\n\
       15\r\n\
       $6\r\n\
       INCRBY\r\n\
       $3\r\n\
       i16\r\n\
       $1\r\n\
       8\r\n\
       $1\r\n\
       1\r\n"
  ; (* Round-trip serialization tests *)
    test_command_roundtrip "setbit roundtrip"
      (fun () -> setbit "test_key" 42 true)
      (setbit "test_key" 42 true)
  ; test_command_roundtrip "getbit roundtrip"
      (fun () -> getbit "test_key" 42)
      (getbit "test_key" 42)
  ; test_command_roundtrip "bitcount with range roundtrip"
      (fun () -> bitcount "test_key" ~start:0 ~end_pos:100 ())
      (bitcount "test_key" ~start:0 ~end_pos:100 ())
  ; test_command_roundtrip "bitop AND roundtrip"
      (fun () -> bitop `AND "result" ["key1"; "key2"])
      (bitop `AND "result" ["key1"; "key2"])
  ; test_command_roundtrip "bitpos with range roundtrip"
      (fun () -> bitpos "test_key" true ~start:0 ~end_pos:100 ())
      (bitpos "test_key" true ~start:0 ~end_pos:100 ())
  ; test_command_roundtrip "bitfield complex operations roundtrip"
      (fun () ->
        bitfield "test_key"
          [Get ("i8", 0); Set ("u4", 4, 15); Incrby ("i16", 8, 1)] )
      (bitfield "test_key"
         [Get ("i8", 0); Set ("u4", 4, 15); Incrby ("i16", 8, 1)] ) ]

(* HyperLogLog Commands *)
let hyperloglog_command_tests =
  [ (* PFADD command tests *)
    test_command_serialization "pfadd single element"
      (fun () -> pfadd "hll_key" ["element1"])
      "*3\r\n$5\r\nPFADD\r\n$7\r\nhll_key\r\n$8\r\nelement1\r\n"
  ; test_command_serialization "pfadd multiple elements"
      (fun () -> pfadd "hll_key" ["elem1"; "elem2"; "elem3"])
      "*5\r\n\
       $5\r\n\
       PFADD\r\n\
       $7\r\n\
       hll_key\r\n\
       $5\r\n\
       elem1\r\n\
       $5\r\n\
       elem2\r\n\
       $5\r\n\
       elem3\r\n"
  ; test_command_serialization "pfadd with empty string element"
      (fun () -> pfadd "hll_key" [""])
      "*3\r\n$5\r\nPFADD\r\n$7\r\nhll_key\r\n$0\r\n\r\n"
  ; test_command_serialization "pfadd with long element"
      (fun () -> pfadd "hll_key" ["very_long_element_name_for_testing"])
      "*3\r\n\
       $5\r\n\
       PFADD\r\n\
       $7\r\n\
       hll_key\r\n\
       $34\r\n\
       very_long_element_name_for_testing\r\n"
  ; test_command_serialization "pfadd with special characters"
      (fun () ->
        pfadd "hll_key"
          ["elem:with:colons"; "elem with spaces"; "elem\nwith\nnewlines"] )
      "*5\r\n\
       $5\r\n\
       PFADD\r\n\
       $7\r\n\
       hll_key\r\n\
       $16\r\n\
       elem:with:colons\r\n\
       $16\r\n\
       elem with spaces\r\n\
       $18\r\n\
       elem\n\
       with\n\
       newlines\r\n"
  ; (* PFCOUNT command tests *)
    test_command_serialization "pfcount single key"
      (fun () -> pfcount ["hll_key"])
      "*2\r\n$7\r\nPFCOUNT\r\n$7\r\nhll_key\r\n"
  ; test_command_serialization "pfcount multiple keys"
      (fun () -> pfcount ["hll1"; "hll2"; "hll3"])
      "*4\r\n$7\r\nPFCOUNT\r\n$4\r\nhll1\r\n$4\r\nhll2\r\n$4\r\nhll3\r\n"
  ; test_command_serialization "pfcount with long key names"
      (fun () -> pfcount ["hyperloglog_key_with_very_long_name"])
      "*2\r\n$7\r\nPFCOUNT\r\n$35\r\nhyperloglog_key_with_very_long_name\r\n"
  ; (* PFMERGE command tests *)
    test_command_serialization "pfmerge single source"
      (fun () -> pfmerge "dest_hll" ["source_hll"])
      "*3\r\n$7\r\nPFMERGE\r\n$8\r\ndest_hll\r\n$10\r\nsource_hll\r\n"
  ; test_command_serialization "pfmerge multiple sources"
      (fun () -> pfmerge "destination" ["source1"; "source2"; "source3"])
      "*5\r\n\
       $7\r\n\
       PFMERGE\r\n\
       $11\r\n\
       destination\r\n\
       $7\r\n\
       source1\r\n\
       $7\r\n\
       source2\r\n\
       $7\r\n\
       source3\r\n"
  ; test_command_serialization "pfmerge with many sources"
      (fun () ->
        pfmerge "merged" ["hll_a"; "hll_b"; "hll_c"; "hll_d"; "hll_e"] )
      "*7\r\n\
       $7\r\n\
       PFMERGE\r\n\
       $6\r\n\
       merged\r\n\
       $5\r\n\
       hll_a\r\n\
       $5\r\n\
       hll_b\r\n\
       $5\r\n\
       hll_c\r\n\
       $5\r\n\
       hll_d\r\n\
       $5\r\n\
       hll_e\r\n"
  ; (* Edge cases *)
    test_command_serialization "pfadd with numeric-like elements"
      (fun () -> pfadd "numbers" ["123"; "456"; "789"])
      "*5\r\n\
       $5\r\n\
       PFADD\r\n\
       $7\r\n\
       numbers\r\n\
       $3\r\n\
       123\r\n\
       $3\r\n\
       456\r\n\
       $3\r\n\
       789\r\n"
  ; test_command_serialization "pfadd with binary data"
      (fun () -> pfadd "binary" ["\x00\x01\x02\xFF"])
      "*3\r\n$5\r\nPFADD\r\n$6\r\nbinary\r\n$4\r\n\x00\x01\x02\xFF\r\n"
  ; (* Round-trip serialization tests *)
    test_command_roundtrip "pfadd roundtrip"
      (fun () -> pfadd "test_hll" ["elem1"; "elem2"])
      (pfadd "test_hll" ["elem1"; "elem2"])
  ; test_command_roundtrip "pfcount roundtrip"
      (fun () -> pfcount ["hll1"; "hll2"])
      (pfcount ["hll1"; "hll2"])
  ; test_command_roundtrip "pfmerge roundtrip"
      (fun () -> pfmerge "dest" ["src1"; "src2"; "src3"])
      (pfmerge "dest" ["src1"; "src2"; "src3"])
  ; test_command_roundtrip "pfadd single element roundtrip"
      (fun () -> pfadd "single_hll" ["single_element"])
      (pfadd "single_hll" ["single_element"])
  ; test_command_roundtrip "pfcount single key roundtrip"
      (fun () -> pfcount ["single_key"])
      (pfcount ["single_key"])
  ; test_command_roundtrip "pfmerge with many sources roundtrip"
      (fun () -> pfmerge "dest" ["a"; "b"; "c"; "d"; "e"; "f"])
      (pfmerge "dest" ["a"; "b"; "c"; "d"; "e"; "f"]) ]

(* Stream Commands *)
let stream_command_tests =
  [ (* Basic XADD tests *)
    test_command_serialization "xadd with auto-generated ID"
      (fun () -> xadd "mystream" "*" [("field1", "value1")] ())
      "*5\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $1\r\n\
       *\r\n\
       $6\r\n\
       field1\r\n\
       $6\r\n\
       value1\r\n"
  ; test_command_serialization "xadd with explicit ID"
      (fun () -> xadd "mystream" "1526919030474-55" [("field1", "value1")] ())
      "*5\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $16\r\n\
       1526919030474-55\r\n\
       $6\r\n\
       field1\r\n\
       $6\r\n\
       value1\r\n"
  ; test_command_serialization "xadd with multiple fields"
      (fun () ->
        xadd "mystream" "*"
          [("field1", "value1"); ("field2", "value2"); ("field3", "value3")]
          () )
      "*9\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $1\r\n\
       *\r\n\
       $6\r\n\
       field1\r\n\
       $6\r\n\
       value1\r\n\
       $6\r\n\
       field2\r\n\
       $6\r\n\
       value2\r\n\
       $6\r\n\
       field3\r\n\
       $6\r\n\
       value3\r\n"
  ; test_command_serialization "xadd with incomplete ID"
      (fun () -> xadd "mystream" "1526919030474-*" [("message", "Hello")] ())
      "*5\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $15\r\n\
       1526919030474-*\r\n\
       $7\r\n\
       message\r\n\
       $5\r\n\
       Hello\r\n"
  ; (* XADD with NOMKSTREAM option *)
    test_command_serialization "xadd with nomkstream"
      (fun () -> xadd "mystream" "*" [("field", "value")] ~nomkstream:true ())
      "*6\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $10\r\n\
       NOMKSTREAM\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; (* XADD with reference handling options *)
    test_command_serialization "xadd with keepref"
      (fun () ->
        xadd "mystream" "*" [("field", "value")] ~ref_handling:KeepRef () )
      "*6\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $7\r\n\
       KEEPREF\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; test_command_serialization "xadd with delref"
      (fun () ->
        xadd "mystream" "*" [("field", "value")] ~ref_handling:DelRef () )
      "*6\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $6\r\n\
       DELREF\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; test_command_serialization "xadd with acked"
      (fun () ->
        xadd "mystream" "*" [("field", "value")] ~ref_handling:Acked () )
      "*6\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $5\r\n\
       ACKED\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; (* XADD with MAXLEN trimming *)
    test_command_serialization "xadd with maxlen exact"
      (fun () ->
        xadd "mystream" "*"
          [("field", "value")]
          ~trim_strategy:(MaxLen (1000, false))
          () )
      "*8\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $6\r\n\
       MAXLEN\r\n\
       $1\r\n\
       =\r\n\
       $4\r\n\
       1000\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; test_command_serialization "xadd with maxlen approximate"
      (fun () ->
        xadd "mystream" "*"
          [("field", "value")]
          ~trim_strategy:(MaxLen (1000, true))
          () )
      "*8\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $6\r\n\
       MAXLEN\r\n\
       $1\r\n\
       ~\r\n\
       $4\r\n\
       1000\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; test_command_serialization "xadd with maxlen and limit"
      (fun () ->
        xadd "mystream" "*"
          [("field", "value")]
          ~trim_strategy:(MaxLen (1000, true))
          ~limit:10 () )
      "*10\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $6\r\n\
       MAXLEN\r\n\
       $1\r\n\
       ~\r\n\
       $4\r\n\
       1000\r\n\
       $5\r\n\
       LIMIT\r\n\
       $2\r\n\
       10\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; (* XADD with MINID trimming *)
    test_command_serialization "xadd with minid exact"
      (fun () ->
        xadd "mystream" "*"
          [("field", "value")]
          ~trim_strategy:(MinId ("1526919030474-0", false))
          () )
      "*8\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $5\r\n\
       MINID\r\n\
       $1\r\n\
       =\r\n\
       $15\r\n\
       1526919030474-0\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; test_command_serialization "xadd with minid approximate"
      (fun () ->
        xadd "mystream" "*"
          [("field", "value")]
          ~trim_strategy:(MinId ("1526919030474-0", true))
          () )
      "*8\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $5\r\n\
       MINID\r\n\
       $1\r\n\
       ~\r\n\
       $15\r\n\
       1526919030474-0\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; (* Complex XADD with multiple options *)
    test_command_serialization "xadd with nomkstream and maxlen"
      (fun () ->
        xadd "mystream" "*"
          [("field", "value")]
          ~nomkstream:true
          ~trim_strategy:(MaxLen (100, true))
          () )
      "*9\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $10\r\n\
       NOMKSTREAM\r\n\
       $6\r\n\
       MAXLEN\r\n\
       $1\r\n\
       ~\r\n\
       $3\r\n\
       100\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; test_command_serialization "xadd with keepref and maxlen with limit"
      (fun () ->
        xadd "mystream" "*"
          [("field", "value")]
          ~ref_handling:KeepRef
          ~trim_strategy:(MaxLen (500, true))
          ~limit:5 () )
      "*11\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $7\r\n\
       KEEPREF\r\n\
       $6\r\n\
       MAXLEN\r\n\
       $1\r\n\
       ~\r\n\
       $3\r\n\
       500\r\n\
       $5\r\n\
       LIMIT\r\n\
       $1\r\n\
       5\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $5\r\n\
       value\r\n"
  ; (* Edge cases *)
    test_command_serialization "xadd with empty field value"
      (fun () -> xadd "mystream" "*" [("field", "")] ())
      "*5\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $1\r\n\
       *\r\n\
       $5\r\n\
       field\r\n\
       $0\r\n\
       \r\n"
  ; test_command_serialization "xadd with special characters in field/value"
      (fun () ->
        xadd "mystream" "*" [("field:1", "value\nwith\nnewlines")] () )
      "*5\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $1\r\n\
       *\r\n\
       $7\r\n\
       field:1\r\n\
       $19\r\n\
       value\n\
       with\n\
       newlines\r\n"
  ; test_command_serialization "xadd with unicode"
      (fun () -> xadd "mystream" "*" [("名前", "値")] ())
      "*5\r\n\
       $4\r\n\
       XADD\r\n\
       $8\r\n\
       mystream\r\n\
       $1\r\n\
       *\r\n\
       $6\r\n\
       名前\r\n\
       $3\r\n\
       値\r\n"
  ; (* Round-trip tests *)
    test_command_roundtrip "xadd basic roundtrip"
      (fun () -> xadd "mystream" "*" [("field", "value")] ())
      (xadd "mystream" "*" [("field", "value")] ())
  ; test_command_roundtrip "xadd with options roundtrip"
      (fun () ->
        xadd "mystream" "*"
          [("f1", "v1"); ("f2", "v2")]
          ~nomkstream:true
          ~trim_strategy:(MaxLen (100, false))
          () )
      (xadd "mystream" "*"
         [("f1", "v1"); ("f2", "v2")]
         ~nomkstream:true
         ~trim_strategy:(MaxLen (100, false))
         () )
  ; test_command_roundtrip "xadd complex roundtrip"
      (fun () ->
        xadd "stream" "1-0"
          [("a", "1"); ("b", "2"); ("c", "3")]
          ~ref_handling:DelRef
          ~trim_strategy:(MinId ("0-0", true))
          ~limit:20 () )
      (xadd "stream" "1-0"
         [("a", "1"); ("b", "2"); ("c", "3")]
         ~ref_handling:DelRef
         ~trim_strategy:(MinId ("0-0", true))
         ~limit:20 () )
  
  (* XLEN tests *)
  ; test_command_serialization "xlen basic"
      (fun () -> xlen "mystream")
      "*2\r\n$4\r\nXLEN\r\n$8\r\nmystream\r\n"
  
  ; test_command_serialization "xlen with special characters in key"
      (fun () -> xlen "stream:123")
      "*2\r\n$4\r\nXLEN\r\n$10\r\nstream:123\r\n"
  
  ; test_command_serialization "xlen with unicode key"
      (fun () -> xlen "ストリーム")
      "*2\r\n$4\r\nXLEN\r\n$15\r\nストリーム\r\n"
  
  ; test_command_roundtrip "xlen command roundtrip"
      (fun () -> xlen "test-stream")
      (xlen "test-stream") ]

(* Pub/Sub Commands *)
let pubsub_command_tests =
  [ test_command_serialization "publish command"
      (fun () ->
        Array
          (Some
             [ BulkString (Some "PUBLISH")
             ; BulkString (Some "channel")
             ; BulkString (Some "message") ] ) )
      "*3\r\n$7\r\nPUBLISH\r\n$7\r\nchannel\r\n$7\r\nmessage\r\n"
  ; test_command_serialization "subscribe command"
      (fun () ->
        Array
          (Some [BulkString (Some "SUBSCRIBE"); BulkString (Some "channel")]) )
      "*2\r\n$9\r\nSUBSCRIBE\r\n$7\r\nchannel\r\n"
  ; test_command_serialization "unsubscribe command"
      (fun () ->
        Array
          (Some [BulkString (Some "UNSUBSCRIBE"); BulkString (Some "channel")]
          ) )
      "*2\r\n$11\r\nUNSUBSCRIBE\r\n$7\r\nchannel\r\n"
  ; test_command_serialization "psubscribe pattern command"
      (fun () ->
        Array
          (Some
             [BulkString (Some "PSUBSCRIBE"); BulkString (Some "channel.*")]
          ) )
      "*2\r\n$10\r\nPSUBSCRIBE\r\n$9\r\nchannel.*\r\n" ]

(* Transaction Commands *)
let transaction_command_tests =
  [ test_command_serialization "multi command"
      (fun () -> Array (Some [BulkString (Some "MULTI")]))
      "*1\r\n$5\r\nMULTI\r\n"
  ; test_command_serialization "exec command"
      (fun () -> Array (Some [BulkString (Some "EXEC")]))
      "*1\r\n$4\r\nEXEC\r\n"
  ; test_command_serialization "discard command"
      (fun () -> Array (Some [BulkString (Some "DISCARD")]))
      "*1\r\n$7\r\nDISCARD\r\n"
  ; test_command_serialization "watch command"
      (fun () ->
        Array
          (Some
             [ BulkString (Some "WATCH")
             ; BulkString (Some "key1")
             ; BulkString (Some "key2") ] ) )
      "*3\r\n$5\r\nWATCH\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n"
  ; test_command_serialization "unwatch command"
      (fun () -> Array (Some [BulkString (Some "UNWATCH")]))
      "*1\r\n$7\r\nUNWATCH\r\n" ]

(* Combined comprehensive command tests *)
let all_comprehensive_tests =
  connection_server_tests @ key_management_tests @ string_command_tests
  @ hash_command_tests @ list_command_tests @ redis_set_command_tests
  @ sorted_set_command_tests @ bitmap_command_tests
  @ hyperloglog_command_tests @ stream_command_tests @ pubsub_command_tests
  @ transaction_command_tests

(* Combined canonical tests *)
let all_canonical_tests =
  get_command_tests @ set_command_tests @ hget_command_tests
  @ hset_command_tests @ incr_command_tests @ del_command_tests
  @ edge_case_tests @ all_resp3_tests @ all_comprehensive_tests
