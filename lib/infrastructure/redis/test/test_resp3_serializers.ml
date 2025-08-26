open Alcotest
module Redis =  Ocamlot_infrastructure_redis
open Redis.Resp3

let resp_value_testable = Alcotest.testable pp_resp_value equal_resp_value

(* Universal test helper for serialization *)
let test_serialize name input expected =
  test_case name `Quick (fun () ->
    let actual = serialize_resp3 input in
    check string name expected actual
  )

(* Roundtrip test helper - serialize then parse back *)
let test_roundtrip name input =
  test_case name `Quick (fun () ->
    let serialized = serialize_resp3 input in
    match parse_string serialized with
    | Ok parsed -> check resp_value_testable name input parsed
    | Error msg -> fail ("Roundtrip failed: " ^ msg)
  )

let simple_string_serializer_tests = [
  test_serialize "simple string empty" (SimpleString "") "+\r\n";
  test_serialize "simple string single char" (SimpleString "a") "+a\r\n";
  test_serialize "simple string word" (SimpleString "OK") "+OK\r\n";
  test_serialize "simple string with spaces" (SimpleString "hello world") "+hello world\r\n";
  test_serialize "simple string long" (SimpleString (String.make 500 'x')) ("+" ^ (String.make 500 'x') ^ "\r\n");
  test_roundtrip "simple string roundtrip" (SimpleString "test");
]

let simple_error_serializer_tests = [
  test_serialize "simple error empty" (SimpleError "") "-\r\n";
  test_serialize "simple error single word" (SimpleError "ERR") "-ERR\r\n";
  test_serialize "simple error with message" (SimpleError "ERR unknown command") "-ERR unknown command\r\n";
  test_serialize "simple error with special chars" (SimpleError "ERR invalid!@#$%") "-ERR invalid!@#$%\r\n";
  test_serialize "simple error long" (SimpleError (String.make 500 'e')) ("-" ^ String.make 500 'e' ^ "\r\n");
  test_roundtrip "simple error roundtrip" (SimpleError "test error");
]

let integer_serializer_tests = [
  test_serialize "integer zero" (Integer 0L) ":0\r\n";
  test_serialize "integer positive single" (Integer 5L) ":5\r\n";
  test_serialize "integer negative" (Integer (-42L)) ":-42\r\n";
  test_serialize "integer large positive" (Integer 9223372036854775807L) ":9223372036854775807\r\n";
  test_serialize "integer large negative" (Integer (-9223372036854775808L)) ":-9223372036854775808\r\n";
  test_roundtrip "integer roundtrip" (Integer 12345L);
]

let bulk_string_serializer_tests = [
  test_serialize "bulk string null" (BulkString None) "$-1\r\n";
  test_serialize "bulk string empty" (BulkString (Some "")) "$0\r\n\r\n";
  test_serialize "bulk string single char" (BulkString (Some "a")) "$1\r\na\r\n";
  test_serialize "bulk string word" (BulkString (Some "hello")) "$5\r\nhello\r\n";
  test_serialize "bulk string with CRLF inside" (BulkString (Some "he\r\nllo")) "$7\r\nhe\r\nllo\r\n";
  test_serialize "bulk string binary" (BulkString (Some "\x00\x01\x02\x03")) "$4\r\n\x00\x01\x02\x03\r\n";
  test_serialize "bulk string large" (BulkString (Some (String.make 1000 'x'))) (Printf.sprintf "$%d\r\n%s\r\n" 1000 (String.make 1000 'x'));
  test_roundtrip "bulk string roundtrip" (BulkString (Some "test"));
]

let array_serializer_tests = [
  test_serialize "array null" (Array None) "*-1\r\n";
  test_serialize "array empty" (Array (Some [])) "*0\r\n";
  test_serialize "array single element" (Array (Some [SimpleString "OK"])) "*1\r\n+OK\r\n";
  test_serialize "array two elements" (Array (Some [SimpleString "first"; SimpleString "second"])) "*2\r\n+first\r\n+second\r\n";
  test_serialize "array mixed types" (Array (Some [Integer 42L; SimpleString "OK"; BulkString None])) "*3\r\n:42\r\n+OK\r\n$-1\r\n";
  test_serialize "array nested" (Array (Some [
    Array (Some [SimpleString "a"; SimpleString "b"]);
    Array (Some [SimpleString "c"; SimpleString "d"])
  ])) "*2\r\n*2\r\n+a\r\n+b\r\n*2\r\n+c\r\n+d\r\n";
  test_roundtrip "array roundtrip" (Array (Some [SimpleString "test"; Integer 123L]));
]

let null_serializer_tests = [
  test_serialize "null value" Null "_\r\n";
  test_roundtrip "null roundtrip" Null;
]

let boolean_serializer_tests = [
  test_serialize "boolean true" (Boolean true) "#t\r\n";
  test_serialize "boolean false" (Boolean false) "#f\r\n";
  test_roundtrip "boolean true roundtrip" (Boolean true);
  test_roundtrip "boolean false roundtrip" (Boolean false);
]

let double_serializer_tests = [
  test_serialize "double zero" (Double 0.0) ",0\r\n";
  test_serialize "double whole number" (Double 20.) ",20\r\n";
  test_serialize "double positive" (Double 3.14159) ",3.14159\r\n";
  test_serialize "double negative" (Double (-2.5)) ",-2.5\r\n";
  test_serialize "double scientific" (Double 1.5e10) ",15000000000\r\n";
  test_serialize "double infinity" (Double infinity) ",inf\r\n";
  test_serialize "double neg infinity" (Double neg_infinity) ",-inf\r\n";
  test_roundtrip "double roundtrip" (Double 3.14);
  (* Special case for nan - can't do direct comparison *)
  test_case "double nan serialization" `Quick (fun () ->
    let actual = serialize_resp3 (Double nan) in
    check string "nan serialization" ",nan\r\n" actual
  );
]

let big_number_serializer_tests = [
  test_serialize "big number zero" (BigNumber Z.zero) "(0\r\n";
  test_serialize "big number small positive" (BigNumber (Z.of_int 123)) "(123\r\n";
  test_serialize "big number small negative" (BigNumber (Z.of_int (-456))) "(-456\r\n";
  test_serialize "big number beyond int64" (BigNumber (Z.of_string "92233720368547758089999999")) "(92233720368547758089999999\r\n";
  test_serialize "big number very large" (BigNumber (Z.of_string (String.make 100 '9'))) ("(" ^ String.make 100 '9' ^ "\r\n");
  test_roundtrip "big number roundtrip" (BigNumber (Z.of_string "123456789012345678901234567890"));
]

let bulk_error_serializer_tests = [
  test_serialize "bulk error empty" (BulkError "") "!0\r\n\r\n";
  test_serialize "bulk error simple" (BulkError "ERR") "!3\r\nERR\r\n";
  test_serialize "bulk error with message" (BulkError "SYNTAX invalid syntax") "!21\r\nSYNTAX invalid syntax\r\n";
  test_serialize "bulk error with newline" (BulkError "ERR\r\nline 2") "!11\r\nERR\r\nline 2\r\n";
  test_roundtrip "bulk error roundtrip" (BulkError "test error");
]

let verbatim_string_serializer_tests = [
  test_serialize "verbatim string empty txt" (VerbatimString {format="txt"; content=""}) "=4\r\ntxt:\r\n";
  test_serialize "verbatim string simple txt" (VerbatimString {format="txt"; content="example"}) "=11\r\ntxt:example\r\n";
  test_serialize "verbatim string markdown" (VerbatimString {format="mkd"; content="# Hello World"}) "=17\r\nmkd:# Hello World\r\n";
  test_serialize "verbatim string with newlines" (VerbatimString {format="txt"; content="line1\r\nline2"}) "=16\r\ntxt:line1\r\nline2\r\n";
  test_roundtrip "verbatim string roundtrip" (VerbatimString {format="txt"; content="test"});
]

let map_serializer_tests = [
  test_serialize "map empty" (Map []) "%0\r\n";
  test_serialize "map single pair" (Map [(SimpleString "key", SimpleString "value")]) "%1\r\n+key\r\n+value\r\n";
  test_serialize "map two pairs" (Map [(SimpleString "a", Integer 1L); (SimpleString "b", Integer 2L)]) "%2\r\n+a\r\n:1\r\n+b\r\n:2\r\n";
  test_serialize "map mixed types" (Map [(Integer 1L, SimpleString "one"); (BulkString (Some "two"), Integer 2L)]) "%2\r\n:1\r\n+one\r\n$3\r\ntwo\r\n:2\r\n";
  test_serialize "map nested" (Map [(SimpleString "outer", Map [(SimpleString "inner", SimpleString "value")])]) "%1\r\n+outer\r\n%1\r\n+inner\r\n+value\r\n";
  test_roundtrip "map roundtrip" (Map [(SimpleString "key", SimpleString "value")]);
]

let attribute_serializer_tests = [
  test_serialize "attribute no attrs" (Attribute {attrs=[]; value=SimpleString "value"}) "|0\r\n+value\r\n";
  test_serialize "attribute single attr" (Attribute {attrs=[(SimpleString "key", SimpleString "val")]; value=SimpleString "OK"}) "|1\r\n+key\r\n+val\r\n+OK\r\n";
  test_serialize "attribute multiple attrs" (Attribute {
    attrs=[(SimpleString "ttl", Integer 3600L); (SimpleString "type", SimpleString "string")];
    value=Integer 42L
  }) "|2\r\n+ttl\r\n:3600\r\n+type\r\n+string\r\n:42\r\n";
  test_roundtrip "attribute roundtrip" (Attribute {attrs=[(SimpleString "key", SimpleString "val")]; value=SimpleString "OK"});
]

let set_serializer_tests = [
  test_serialize "set empty" (Set []) "~0\r\n";
  test_serialize "set single element" (Set [SimpleString "element"]) "~1\r\n+element\r\n";
  test_serialize "set multiple elements" (Set [SimpleString "a"; SimpleString "b"; SimpleString "c"]) "~3\r\n+a\r\n+b\r\n+c\r\n";
  test_serialize "set mixed types" (Set [Integer 1L; Integer 2L; Integer 3L]) "~3\r\n:1\r\n:2\r\n:3\r\n";
  test_roundtrip "set roundtrip" (Set [SimpleString "a"; SimpleString "b"]);
]

let push_serializer_tests = [
  test_serialize "push minimal" (Push {kind="pubsub"; data=[SimpleString "message"]}) ">2\r\n$6\r\npubsub\r\n+message\r\n";
  test_serialize "push with multiple data" (Push {kind="monitor"; data=[Integer 123L; SimpleString "client"; SimpleString "GET key"]}) ">4\r\n$7\r\nmonitor\r\n:123\r\n+client\r\n+GET key\r\n";
  test_serialize "push with bulk string kind" (Push {kind="message"; data=[SimpleString "channel"; SimpleString "data"]}) ">3\r\n$7\r\nmessage\r\n+channel\r\n+data\r\n";
  test_roundtrip "push roundtrip" (Push {kind="test"; data=[SimpleString "data"]});
]
