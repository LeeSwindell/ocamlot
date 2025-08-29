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
    (ping ());
    
  (* PING with various message types *)
  test_command_roundtrip 
    "ping with simple message" 
    (fun () -> ping ~message:"hello" ())
    (ping ~message:"hello" ());
    
  test_command_roundtrip 
    "ping with empty message" 
    (fun () -> ping ~message:"" ())
    (ping ~message:"" ());
    
  test_command_roundtrip 
    "ping with space in message" 
    (fun () -> ping ~message:"hello world" ())
    (ping ~message:"hello world" ());
    
  test_command_roundtrip 
    "ping with newline in message" 
    (fun () -> ping ~message:"hello\nworld" ())
    (ping ~message:"hello\nworld" ());
    
  test_command_roundtrip 
    "ping with carriage return and newline" 
    (fun () -> ping ~message:"hello\r\nworld" ())
    (ping ~message:"hello\r\nworld" ());
    
  test_command_roundtrip 
    "ping with tab character" 
    (fun () -> ping ~message:"hello\tworld" ())
    (ping ~message:"hello\tworld" ());
    
  test_command_roundtrip 
    "ping with special characters" 
    (fun () -> ping ~message:"!@#$%^&*()_+-=[]{}|;':\",./<>?" ())
    (ping ~message:"!@#$%^&*()_+-=[]{}|;':\",./<>?" ());
    
  test_command_roundtrip 
    "ping with unicode emoji" 
    (fun () -> ping ~message:"Hello 👋 World 🌍" ())
    (ping ~message:"Hello 👋 World 🌍" ());
    
  test_command_roundtrip 
    "ping with unicode characters" 
    (fun () -> ping ~message:"你好世界" ())
    (ping ~message:"你好世界" ());
    
  test_command_roundtrip 
    "ping with very long message" 
    (fun () -> ping ~message:(String.make 1000 'x') ())
    (ping ~message:(String.make 1000 'x') ());
    
  test_command_roundtrip 
    "ping with null bytes in message" 
    (fun () -> ping ~message:"hello\x00world" ())
    (ping ~message:"hello\x00world" ());
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

(* =============================================================================
   CANONICAL REDIS COMMAND TESTS (from Official Documentation)
   ============================================================================= *)

(* These tests are based on the official Redis protocol documentation examples *)

(* GET Command Tests - Canonical Examples *)
let get_command_tests = [
  test_command_roundtrip
    "get command structure"
    (fun () -> get "key")
    (get "key");
    
  test_command_serialization
    "get canonical wire format"
    (fun () -> get "key")
    "*2\r\n$3\r\nGET\r\n$3\r\nkey\r\n";
    
  test_command_serialization
    "get with longer key"
    (fun () -> get "mykey")
    "*2\r\n$3\r\nGET\r\n$5\r\nmykey\r\n";
]

(* SET Command Tests - Canonical Examples *)
let set_command_tests = [
  test_command_roundtrip
    "set command structure"
    (fun () -> set "key" "value" ())
    (set "key" "value" ());
    
  test_command_serialization
    "set canonical wire format"
    (fun () -> set "key" "value" ())
    "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$5\r\nvalue\r\n";
    
  test_command_serialization
    "set with empty value"
    (fun () -> set "key" "" ())
    "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$0\r\n\r\n";
]

(* HGET Command Tests - Canonical Examples *)
let hget_command_tests = [
  test_command_roundtrip
    "hget command structure"
    (fun () -> hget "myhash" "field")
    (hget "myhash" "field");
    
  test_command_serialization
    "hget canonical wire format"
    (fun () -> hget "myhash" "field")
    "*3\r\n$4\r\nHGET\r\n$6\r\nmyhash\r\n$5\r\nfield\r\n";
]

(* HSET Command Tests - Canonical Examples *)
let hset_command_tests = [
  test_command_roundtrip
    "hset single field command structure"
    (fun () -> hset "myhash" [("field", "value")])
    (hset "myhash" [("field", "value")]);
    
  test_command_serialization
    "hset single field canonical wire format"
    (fun () -> hset "myhash" [("field", "value")])
    "*4\r\n$4\r\nHSET\r\n$6\r\nmyhash\r\n$5\r\nfield\r\n$5\r\nvalue\r\n";
    
  test_command_roundtrip
    "hset multiple fields command structure"
    (fun () -> hset "myhash" [("field1", "value1"); ("field2", "value2")])
    (hset "myhash" [("field1", "value1"); ("field2", "value2")]);
]

(* INCR Command Tests - Canonical Examples *)
let incr_command_tests = [
  test_command_roundtrip
    "incr command structure"
    (fun () -> incr "mycounter")
    (incr "mycounter");
    
  test_command_serialization
    "incr canonical wire format"
    (fun () -> incr "mycounter")
    "*2\r\n$4\r\nINCR\r\n$9\r\nmycounter\r\n";
]

(* DEL Command Tests - Canonical Examples *)
let del_command_tests = [
  test_command_roundtrip
    "del single key command structure"
    (fun () -> del ["key"])
    (del ["key"]);
    
  test_command_serialization
    "del single key canonical wire format"
    (fun () -> del ["key"])
    "*2\r\n$3\r\nDEL\r\n$3\r\nkey\r\n";
    
  test_command_roundtrip
    "del multiple keys command structure"
    (fun () -> del ["key1"; "key2"; "key3"])
    (del ["key1"; "key2"; "key3"]);
    
  test_command_serialization
    "del multiple keys canonical wire format"
    (fun () -> del ["key1"; "key2"])
    "*3\r\n$3\r\nDEL\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n";
]

(* Edge Cases and Boundary Conditions *)
let edge_case_tests = [
  (* Empty keys/values *)
  test_command_serialization
    "get with empty key"
    (fun () -> get "")
    "*2\r\n$3\r\nGET\r\n$0\r\n\r\n";
    
  test_command_serialization
    "set with empty key and value"
    (fun () -> set "" "" ())
    "*3\r\n$3\r\nSET\r\n$0\r\n\r\n$0\r\n\r\n";
    
  (* Keys with special characters *)
  test_command_serialization
    "get with key containing colon"
    (fun () -> get "user:1001")
    "*2\r\n$3\r\nGET\r\n$9\r\nuser:1001\r\n";
    
  test_command_serialization
    "set with key containing spaces"
    (fun () -> set "my key" "my value" ())
    "*3\r\n$3\r\nSET\r\n$6\r\nmy key\r\n$8\r\nmy value\r\n";
    
  (* Values with newlines and special chars *)
  test_command_serialization
    "set with value containing newlines"
    (fun () -> set "key" "line1\nline2" ())
    "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$11\r\nline1\nline2\r\n";
    
  test_command_serialization
    "set with value containing CRLF"
    (fun () -> set "key" "line1\r\nline2" ())
    "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$12\r\nline1\r\nline2\r\n";
    
  (* Binary data *)
  test_command_serialization
    "set with binary data (null bytes)"
    (fun () -> set "key" "hello\x00world" ())
    "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$11\r\nhello\x00world\r\n";
]

(* =============================================================================
   RESP3 DATA TYPE TESTS
   ============================================================================= *)

(* These tests verify that our commands can handle RESP3-specific responses *)

(* Commands that would return Boolean responses *)
let boolean_response_tests = [
  (* SISMEMBER returns boolean in RESP3 *)
  test_command_roundtrip
    "sismember command structure"
    (fun () -> sismember "myset" "member")
    (sismember "myset" "member");
    
  test_command_serialization
    "sismember wire format"
    (fun () -> sismember "myset" "member")
    "*3\r\n$9\r\nSISMEMBER\r\n$5\r\nmyset\r\n$6\r\nmember\r\n";
]

(* Commands that would return Double responses *)
let double_response_tests = [
  (* ZSCORE returns double in RESP3 *)
  test_command_roundtrip
    "zscore command structure"
    (fun () -> zscore "myzset" "member")
    (zscore "myzset" "member");
    
  test_command_serialization
    "zscore wire format"
    (fun () -> zscore "myzset" "member")
    "*3\r\n$6\r\nZSCORE\r\n$6\r\nmyzset\r\n$6\r\nmember\r\n";
]

(* Commands that would return Map responses *)
let map_response_tests = [
  (* HGETALL returns map in RESP3 *)
  test_command_roundtrip
    "hgetall command structure"
    (fun () -> hgetall "myhash")
    (hgetall "myhash");
    
  test_command_serialization
    "hgetall wire format"
    (fun () -> hgetall "myhash")
    "*2\r\n$7\r\nHGETALL\r\n$6\r\nmyhash\r\n";
]

(* Commands that would return Set responses *)
let set_response_tests = [
  (* SMEMBERS returns set in RESP3 *)
  test_command_roundtrip
    "smembers command structure"
    (fun () -> smembers "myset")
    (smembers "myset");
    
  test_command_serialization
    "smembers wire format"
    (fun () -> smembers "myset")
    "*2\r\n$8\r\nSMEMBERS\r\n$5\r\nmyset\r\n";
]

(* Commands demonstrating various integer responses *)
let integer_response_tests = [
  (* INCR returns integer *)
  test_command_roundtrip
    "incr returns integer response"
    (fun () -> incr "counter")
    (incr "counter");
    
  (* DEL returns integer count *)
  test_command_roundtrip
    "del returns integer count"
    (fun () -> del ["key1"; "key2"])
    (del ["key1"; "key2"]);
    
  (* These commands would return various integer values *)
  test_command_serialization
    "strlen wire format"
    (fun () -> strlen "mykey")
    "*2\r\n$6\r\nSTRLEN\r\n$5\r\nmykey\r\n";
    
  test_command_serialization
    "llen wire format"
    (fun () -> llen "mylist")
    "*2\r\n$4\r\nLLEN\r\n$6\r\nmylist\r\n";
    
  test_command_serialization
    "scard wire format"
    (fun () -> scard "myset")
    "*2\r\n$5\r\nSCARD\r\n$5\r\nmyset\r\n";
]

(* Commands demonstrating null responses *)
let null_response_tests = [
  (* GET on non-existent key returns null *)
  test_command_roundtrip
    "get non-existent key"
    (fun () -> get "nonexistent")
    (get "nonexistent");
    
  (* HGET on non-existent field returns null *)
  test_command_roundtrip
    "hget non-existent field"
    (fun () -> hget "myhash" "nonexistent")
    (hget "myhash" "nonexistent");
]

(* Commands demonstrating arrays with mixed types *)
let mixed_array_tests = [
  (* MGET returns array of bulk strings (some may be null) *)
  test_command_serialization
    "mget multiple keys wire format"
    (fun () -> mget ["key1"; "key2"; "key3"])
    "*4\r\n$4\r\nMGET\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n$4\r\nkey3\r\n";
    
  (* HMGET returns array of bulk strings *)
  test_command_serialization
    "hmget multiple fields wire format"
    (fun () -> 
      Array (Some [BulkString (Some "HMGET"); BulkString (Some "myhash"); 
                   BulkString (Some "field1"); BulkString (Some "field2")]))
    "*4\r\n$5\r\nHMGET\r\n$6\r\nmyhash\r\n$6\r\nfield1\r\n$6\r\nfield2\r\n";
    
  (* LRANGE returns array of bulk strings *)
  test_command_serialization
    "lrange wire format"
    (fun () -> lrange "mylist" 0 (-1))
    "*4\r\n$6\r\nLRANGE\r\n$6\r\nmylist\r\n$1\r\n0\r\n$2\r\n-1\r\n";
]

(* RESP3 Protocol Compliance Tests *)
let resp3_compliance_tests = [
  (* Commands with large arguments to test bulk string size handling *)
  test_command_serialization
    "command with large argument"
    (fun () -> set "key" (String.make 1000 'x') ())
    ("*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$1000\r\n" ^ String.make 1000 'x' ^ "\r\n");
    
  (* Commands with UTF-8 content *)
  test_command_serialization
    "command with unicode key and value"
    (fun () -> set "键" "值" ())
    "*3\r\n$3\r\nSET\r\n$3\r\n键\r\n$3\r\n值\r\n";
    
  (* Commands with binary data *)
  test_command_serialization
    "command with binary data"
    (fun () -> set "binary_key" "\x00\x01\x02\xFF" ())
    "*3\r\n$3\r\nSET\r\n$10\r\nbinary_key\r\n$4\r\n\x00\x01\x02\xFF\r\n";
]

(* Combined RESP3 data type tests *)
let all_resp3_tests = 
  boolean_response_tests @ double_response_tests @ map_response_tests @ 
  set_response_tests @ integer_response_tests @ null_response_tests @ 
  mixed_array_tests @ resp3_compliance_tests

(* =============================================================================
   COMPREHENSIVE REDIS COMMAND CATEGORIES
   ============================================================================= *)

(* Connection and Server Commands *)
let connection_server_tests = [
  (* PING - already tested above *)
  test_command_serialization
    "echo command"
    (fun () -> echo "Hello World")
    "*2\r\n$4\r\nECHO\r\n$11\r\nHello World\r\n";
    
  test_command_serialization
    "select database"
    (fun () -> select 1)
    "*2\r\n$6\r\nSELECT\r\n$1\r\n1\r\n";
    
  test_command_serialization
    "quit connection"
    (fun () -> quit ())
    "*1\r\n$4\r\nQUIT\r\n";
    
  test_command_serialization
    "info server"
    (fun () -> info ())
    "*1\r\n$4\r\nINFO\r\n";
]

(* Key Management Commands *)
let key_management_tests = [
  (* DEL - already tested above *)
  test_command_serialization
    "exists single key"
    (fun () -> exists ["mykey"])
    "*2\r\n$6\r\nEXISTS\r\n$5\r\nmykey\r\n";
    
  test_command_serialization
    "exists multiple keys"
    (fun () -> exists ["key1"; "key2"])
    "*3\r\n$6\r\nEXISTS\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n";
    
  test_command_serialization
    "type command"
    (fun () -> type_of_key "mykey")
    "*2\r\n$4\r\nTYPE\r\n$5\r\nmykey\r\n";
    
  test_command_serialization
    "expire with seconds"
    (fun () -> expire "mykey" 300)
    "*3\r\n$6\r\nEXPIRE\r\n$5\r\nmykey\r\n$3\r\n300\r\n";
    
  test_command_serialization
    "ttl command"
    (fun () -> ttl "mykey")
    "*2\r\n$3\r\nTTL\r\n$5\r\nmykey\r\n";
    
  test_command_serialization
    "rename command"
    (fun () -> rename "oldkey" "newkey")
    "*3\r\n$6\r\nRENAME\r\n$6\r\noldkey\r\n$6\r\nnewkey\r\n";
]

(* String Commands (beyond GET/SET/INCR) *)
let string_command_tests = [
  test_command_serialization
    "mget multiple keys"
    (fun () -> mget ["key1"; "key2"; "key3"])
    "*4\r\n$4\r\nMGET\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n$4\r\nkey3\r\n";
    
  test_command_serialization
    "mset multiple key-values"
    (fun () -> Array (Some [BulkString (Some "MSET"); BulkString (Some "key1"); BulkString (Some "val1"); 
                           BulkString (Some "key2"); BulkString (Some "val2")]))
    "*5\r\n$4\r\nMSET\r\n$4\r\nkey1\r\n$4\r\nval1\r\n$4\r\nkey2\r\n$4\r\nval2\r\n";
    
  test_command_serialization
    "incrby with amount"
    (fun () -> incrby "mycounter" 5)
    "*3\r\n$6\r\nINCRBY\r\n$9\r\nmycounter\r\n$1\r\n5\r\n";
    
  test_command_serialization
    "decr command"
    (fun () -> decr "mycounter")
    "*2\r\n$4\r\nDECR\r\n$9\r\nmycounter\r\n";
    
  test_command_serialization
    "append command"
    (fun () -> append "mykey" "suffix")
    "*3\r\n$6\r\nAPPEND\r\n$5\r\nmykey\r\n$6\r\nsuffix\r\n";
    
  test_command_serialization
    "strlen command"
    (fun () -> strlen "mykey")
    "*2\r\n$6\r\nSTRLEN\r\n$5\r\nmykey\r\n";
]

(* Hash Commands (beyond HGET/HSET) *)
let hash_command_tests = [
  test_command_serialization
    "hmget multiple fields"
    (fun () -> Array (Some [BulkString (Some "HMGET"); BulkString (Some "myhash"); 
                           BulkString (Some "field1"); BulkString (Some "field2")]))
    "*4\r\n$5\r\nHMGET\r\n$6\r\nmyhash\r\n$6\r\nfield1\r\n$6\r\nfield2\r\n";
    
  test_command_serialization
    "hgetall command"
    (fun () -> Array (Some [BulkString (Some "HGETALL"); BulkString (Some "myhash")]))
    "*2\r\n$7\r\nHGETALL\r\n$6\r\nmyhash\r\n";
    
  test_command_serialization
    "hkeys command"
    (fun () -> Array (Some [BulkString (Some "HKEYS"); BulkString (Some "myhash")]))
    "*2\r\n$5\r\nHKEYS\r\n$6\r\nmyhash\r\n";
    
  test_command_serialization
    "hvals command"
    (fun () -> Array (Some [BulkString (Some "HVALS"); BulkString (Some "myhash")]))
    "*2\r\n$5\r\nHVALS\r\n$6\r\nmyhash\r\n";
    
  test_command_serialization
    "hlen command"
    (fun () -> Array (Some [BulkString (Some "HLEN"); BulkString (Some "myhash")]))
    "*2\r\n$4\r\nHLEN\r\n$6\r\nmyhash\r\n";
    
  test_command_serialization
    "hdel command"
    (fun () -> Array (Some [BulkString (Some "HDEL"); BulkString (Some "myhash"); 
                           BulkString (Some "field1"); BulkString (Some "field2")]))
    "*4\r\n$4\r\nHDEL\r\n$6\r\nmyhash\r\n$6\r\nfield1\r\n$6\r\nfield2\r\n";
    
  test_command_serialization
    "hexists command"
    (fun () -> Array (Some [BulkString (Some "HEXISTS"); BulkString (Some "myhash"); BulkString (Some "field")]))
    "*3\r\n$7\r\nHEXISTS\r\n$6\r\nmyhash\r\n$5\r\nfield\r\n";
]

(* List Commands *)
let list_command_tests = [
  test_command_serialization
    "lpush single value"
    (fun () -> Array (Some [BulkString (Some "LPUSH"); BulkString (Some "mylist"); BulkString (Some "value")]))
    "*3\r\n$5\r\nLPUSH\r\n$6\r\nmylist\r\n$5\r\nvalue\r\n";
    
  test_command_serialization
    "lpush multiple values"
    (fun () -> Array (Some [BulkString (Some "LPUSH"); BulkString (Some "mylist"); 
                           BulkString (Some "val1"); BulkString (Some "val2")]))
    "*4\r\n$5\r\nLPUSH\r\n$6\r\nmylist\r\n$4\r\nval1\r\n$4\r\nval2\r\n";
    
  test_command_serialization
    "rpush command"
    (fun () -> Array (Some [BulkString (Some "RPUSH"); BulkString (Some "mylist"); BulkString (Some "value")]))
    "*3\r\n$5\r\nRPUSH\r\n$6\r\nmylist\r\n$5\r\nvalue\r\n";
    
  test_command_serialization
    "lpop command"
    (fun () -> Array (Some [BulkString (Some "LPOP"); BulkString (Some "mylist")]))
    "*2\r\n$4\r\nLPOP\r\n$6\r\nmylist\r\n";
    
  test_command_serialization
    "rpop command"
    (fun () -> Array (Some [BulkString (Some "RPOP"); BulkString (Some "mylist")]))
    "*2\r\n$4\r\nRPOP\r\n$6\r\nmylist\r\n";
    
  test_command_serialization
    "llen command"
    (fun () -> Array (Some [BulkString (Some "LLEN"); BulkString (Some "mylist")]))
    "*2\r\n$4\r\nLLEN\r\n$6\r\nmylist\r\n";
    
  test_command_serialization
    "lrange command"
    (fun () -> Array (Some [BulkString (Some "LRANGE"); BulkString (Some "mylist"); 
                           BulkString (Some "0"); BulkString (Some "-1")]))
    "*4\r\n$6\r\nLRANGE\r\n$6\r\nmylist\r\n$1\r\n0\r\n$2\r\n-1\r\n";
    
  test_command_serialization
    "lindex command"
    (fun () -> Array (Some [BulkString (Some "LINDEX"); BulkString (Some "mylist"); BulkString (Some "0")]))
    "*3\r\n$6\r\nLINDEX\r\n$6\r\nmylist\r\n$1\r\n0\r\n";
]

(* Set Commands *)
let redis_set_command_tests = [
  test_command_serialization
    "sadd single member"
    (fun () -> Array (Some [BulkString (Some "SADD"); BulkString (Some "myset"); BulkString (Some "member")]))
    "*3\r\n$4\r\nSADD\r\n$5\r\nmyset\r\n$6\r\nmember\r\n";
    
  test_command_serialization
    "sadd multiple members"
    (fun () -> Array (Some [BulkString (Some "SADD"); BulkString (Some "myset"); 
                           BulkString (Some "member1"); BulkString (Some "member2")]))
    "*4\r\n$4\r\nSADD\r\n$5\r\nmyset\r\n$7\r\nmember1\r\n$7\r\nmember2\r\n";
    
  test_command_serialization
    "srem command"
    (fun () -> Array (Some [BulkString (Some "SREM"); BulkString (Some "myset"); BulkString (Some "member")]))
    "*3\r\n$4\r\nSREM\r\n$5\r\nmyset\r\n$6\r\nmember\r\n";
    
  test_command_serialization
    "scard command"
    (fun () -> Array (Some [BulkString (Some "SCARD"); BulkString (Some "myset")]))
    "*2\r\n$5\r\nSCARD\r\n$5\r\nmyset\r\n";
    
  test_command_serialization
    "sismember command"
    (fun () -> Array (Some [BulkString (Some "SISMEMBER"); BulkString (Some "myset"); BulkString (Some "member")]))
    "*3\r\n$9\r\nSISMEMBER\r\n$5\r\nmyset\r\n$6\r\nmember\r\n";
    
  test_command_serialization
    "smembers command"
    (fun () -> Array (Some [BulkString (Some "SMEMBERS"); BulkString (Some "myset")]))
    "*2\r\n$8\r\nSMEMBERS\r\n$5\r\nmyset\r\n";
    
  test_command_serialization
    "sinter command"
    (fun () -> Array (Some [BulkString (Some "SINTER"); BulkString (Some "set1"); BulkString (Some "set2")]))
    "*3\r\n$6\r\nSINTER\r\n$4\r\nset1\r\n$4\r\nset2\r\n";
]

(* Sorted Set Commands *)
let sorted_set_command_tests = [
  test_command_serialization
    "zadd single score-member"
    (fun () -> Array (Some [BulkString (Some "ZADD"); BulkString (Some "myzset"); 
                           BulkString (Some "100"); BulkString (Some "member")]))
    "*4\r\n$4\r\nZADD\r\n$6\r\nmyzset\r\n$3\r\n100\r\n$6\r\nmember\r\n";
    
  test_command_serialization
    "zadd multiple score-members"
    (fun () -> Array (Some [BulkString (Some "ZADD"); BulkString (Some "myzset"); 
                           BulkString (Some "100"); BulkString (Some "member1");
                           BulkString (Some "200"); BulkString (Some "member2")]))
    "*6\r\n$4\r\nZADD\r\n$6\r\nmyzset\r\n$3\r\n100\r\n$7\r\nmember1\r\n$3\r\n200\r\n$7\r\nmember2\r\n";
    
  test_command_serialization
    "zrem command"
    (fun () -> Array (Some [BulkString (Some "ZREM"); BulkString (Some "myzset"); BulkString (Some "member")]))
    "*3\r\n$4\r\nZREM\r\n$6\r\nmyzset\r\n$6\r\nmember\r\n";
    
  test_command_serialization
    "zcard command"
    (fun () -> Array (Some [BulkString (Some "ZCARD"); BulkString (Some "myzset")]))
    "*2\r\n$5\r\nZCARD\r\n$6\r\nmyzset\r\n";
    
  test_command_serialization
    "zscore command"
    (fun () -> Array (Some [BulkString (Some "ZSCORE"); BulkString (Some "myzset"); BulkString (Some "member")]))
    "*3\r\n$6\r\nZSCORE\r\n$6\r\nmyzset\r\n$6\r\nmember\r\n";
    
  test_command_serialization
    "zrange command"
    (fun () -> Array (Some [BulkString (Some "ZRANGE"); BulkString (Some "myzset"); 
                           BulkString (Some "0"); BulkString (Some "-1")]))
    "*4\r\n$6\r\nZRANGE\r\n$6\r\nmyzset\r\n$1\r\n0\r\n$2\r\n-1\r\n";
    
  test_command_serialization
    "zrank command"
    (fun () -> Array (Some [BulkString (Some "ZRANK"); BulkString (Some "myzset"); BulkString (Some "member")]))
    "*3\r\n$5\r\nZRANK\r\n$6\r\nmyzset\r\n$6\r\nmember\r\n";
]

(* Pub/Sub Commands *)
let pubsub_command_tests = [
  test_command_serialization
    "publish command"
    (fun () -> Array (Some [BulkString (Some "PUBLISH"); BulkString (Some "channel"); BulkString (Some "message")]))
    "*3\r\n$7\r\nPUBLISH\r\n$7\r\nchannel\r\n$7\r\nmessage\r\n";
    
  test_command_serialization
    "subscribe command"
    (fun () -> Array (Some [BulkString (Some "SUBSCRIBE"); BulkString (Some "channel")]))
    "*2\r\n$9\r\nSUBSCRIBE\r\n$7\r\nchannel\r\n";
    
  test_command_serialization
    "unsubscribe command"
    (fun () -> Array (Some [BulkString (Some "UNSUBSCRIBE"); BulkString (Some "channel")]))
    "*2\r\n$11\r\nUNSUBSCRIBE\r\n$7\r\nchannel\r\n";
    
  test_command_serialization
    "psubscribe pattern command"
    (fun () -> Array (Some [BulkString (Some "PSUBSCRIBE"); BulkString (Some "channel.*")]))
    "*2\r\n$10\r\nPSUBSCRIBE\r\n$9\r\nchannel.*\r\n";
]

(* Transaction Commands *)
let transaction_command_tests = [
  test_command_serialization
    "multi command"
    (fun () -> Array (Some [BulkString (Some "MULTI")]))
    "*1\r\n$5\r\nMULTI\r\n";
    
  test_command_serialization
    "exec command"
    (fun () -> Array (Some [BulkString (Some "EXEC")]))
    "*1\r\n$4\r\nEXEC\r\n";
    
  test_command_serialization
    "discard command"
    (fun () -> Array (Some [BulkString (Some "DISCARD")]))
    "*1\r\n$7\r\nDISCARD\r\n";
    
  test_command_serialization
    "watch command"
    (fun () -> Array (Some [BulkString (Some "WATCH"); BulkString (Some "key1"); BulkString (Some "key2")]))
    "*3\r\n$5\r\nWATCH\r\n$4\r\nkey1\r\n$4\r\nkey2\r\n";
    
  test_command_serialization
    "unwatch command"
    (fun () -> Array (Some [BulkString (Some "UNWATCH")]))
    "*1\r\n$7\r\nUNWATCH\r\n";
]

(* Combined comprehensive command tests *)
let all_comprehensive_tests = 
  connection_server_tests @ key_management_tests @ string_command_tests @
  hash_command_tests @ list_command_tests @ redis_set_command_tests @ 
  sorted_set_command_tests @ pubsub_command_tests @ transaction_command_tests

(* Combined canonical tests *)
let all_canonical_tests = 
  get_command_tests @ set_command_tests @ hget_command_tests @ 
  hset_command_tests @ incr_command_tests @ del_command_tests @ 
  edge_case_tests @ all_resp3_tests @ all_comprehensive_tests
