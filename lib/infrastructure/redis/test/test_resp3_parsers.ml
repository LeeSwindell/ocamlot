open Alcotest
module Redis =  Ocamlot_infrastructure_redis
open Redis.Resp3

let resp_value_testable = Alcotest.testable pp_resp_value equal_resp_value

(* Universal test helper for parsing RESP strings *)
let test name input expected =
  test_case name `Quick (fun () ->
    let actual = parse_string input in
    check (result resp_value_testable string) name expected actual
  )

let simple_string_parser_tests = [
  test "simple string empty" "+\r\n" (Ok (SimpleString ""));
  test "simple string single char" "+a\r\n" (Ok (SimpleString "a"));
  test "simple string word" "+OK\r\n" (Ok (SimpleString "OK"));
  test "simple string with spaces" "+hello world\r\n" (Ok (SimpleString "hello world"));
  test "simple string long" ("+" ^ (String.make 500 'x') ^ "\r\n") (Ok (SimpleString (String.make 500 'x')));
]

let simple_error_parser_tests = [
  test "simple error empty" "-\r\n" (Ok (SimpleError ""));
  test "simple error single word" "-ERR\r\n" (Ok (SimpleError "ERR"));
  test "simple error with message" "-ERR unknown command\r\n" (Ok (SimpleError "ERR unknown command"));
  test "simple error with special chars" "-ERR invalid!@#$%\r\n" (Ok (SimpleError "ERR invalid!@#$%"));
  test "simple error long" ("-" ^ String.make 500 'e' ^ "\r\n") (Ok (SimpleError (String.make 500 'e')));
]

let integer_parser_tests = [
  test "integer zero" ":0\r\n" (Ok (Integer 0L));
  test "integer positive single" ":5\r\n" (Ok (Integer 5L));
  test "integer negative" ":-42\r\n" (Ok (Integer (-42L)));
  test "integer large positive" ":9223372036854775807\r\n" (Ok (Integer 9223372036854775807L));
  test "integer large negative" ":-9223372036854775808\r\n" (Ok (Integer (-9223372036854775808L)));
]

let bulk_string_parser_tests = [
  test "bulk string null" "$-1\r\n" (Ok (BulkString None));
  test "bulk string empty" "$0\r\n\r\n" (Ok (BulkString (Some "")));
  test "bulk string single char" "$1\r\na\r\n" (Ok (BulkString (Some "a")));
  test "bulk string word" "$5\r\nhello\r\n" (Ok (BulkString (Some "hello")));
  test "bulk string with CRLF inside" "$7\r\nhe\r\nllo\r\n" (Ok (BulkString (Some "he\r\nllo")));
  test "bulk string binary" "$4\r\n\x00\x01\x02\x03\r\n" (Ok (BulkString (Some "\x00\x01\x02\x03")));
  test "bulk string large" (let s = String.make 1000 'x' in 
    Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s) 
    (Ok (BulkString (Some (String.make 1000 'x'))));
]

let array_parser_tests = [
  test "array null" "*-1\r\n" (Ok (Array None));
  test "array empty" "*0\r\n" (Ok (Array (Some [])));
  test "array single element" "*1\r\n+OK\r\n" (Ok (Array (Some [SimpleString "OK"])));
  test "array two elements" "*2\r\n+first\r\n+second\r\n" 
    (Ok (Array (Some [SimpleString "first"; SimpleString "second"])));
  test "array mixed types" "*3\r\n:42\r\n+OK\r\n$-1\r\n" 
    (Ok (Array (Some [Integer 42L; SimpleString "OK"; BulkString None])));
  test "array nested" "*2\r\n*2\r\n+a\r\n+b\r\n*2\r\n+c\r\n+d\r\n"
    (Ok (Array (Some [
      Array (Some [SimpleString "a"; SimpleString "b"]);
      Array (Some [SimpleString "c"; SimpleString "d"])
    ])));
]

let null_parser_tests = [
  test "null value" "_\r\n" (Ok Null);
]

let boolean_parser_tests = [
  test "boolean true" "#t\r\n" (Ok (Boolean true));
  test "boolean false" "#f\r\n" (Ok (Boolean false));
]

let double_parser_tests = [
  test "double zero" ",0\r\n" (Ok (Double 0.0));
  test "double positive" ",3.14159\r\n" (Ok (Double 3.14159));
  test "double negative" ",-2.5\r\n" (Ok (Double (-2.5)));
  test "double scientific" ",1.5e10\r\n" (Ok (Double 1.5e10));
  test "double infinity" ",inf\r\n" (Ok (Double infinity));
  test "double neg infinity" ",-inf\r\n" (Ok (Double neg_infinity));
  (* Special case for nan, since in OCaml nan isn't equal to itself *)
  test_case "double nan" `Quick (fun () ->
    match parse_string ",nan\r\n" with
    | Ok (Double x) when Float.is_nan x -> ()
    | _ -> fail "Expected NaN"
  )
]

let big_number_parser_tests = [
  test "big number zero" "(0\r\n" (Ok (BigNumber Z.zero));
  test "big number small positive" "(123\r\n" (Ok (BigNumber (Z.of_int 123)));
  test "big number small negative" "(-456\r\n" (Ok (BigNumber (Z.of_int (-456))));
  test "big number beyond int64" "(92233720368547758089999999\r\n" 
    (Ok (BigNumber (Z.of_string "92233720368547758089999999")));
  test "big number very large" 
    ("(" ^ String.make 100 '9' ^ "\r\n")
    (Ok (BigNumber (Z.of_string (String.make 100 '9'))));
]

let bulk_error_parser_tests = [
  test "bulk error empty" "!0\r\n\r\n" (Ok (BulkError ""));
  test "bulk error simple" "!3\r\nERR\r\n" (Ok (BulkError "ERR"));
  test "bulk error with message" "!21\r\nSYNTAX invalid syntax\r\n" 
    (Ok (BulkError "SYNTAX invalid syntax"));
  test "bulk error with newline" "!11\r\nERR\r\nline 2\r\n" 
    (Ok (BulkError "ERR\r\nline 2"));
]

let verbatim_string_parser_tests = [
  test "verbatim string empty txt" "=4\r\ntxt:\r\n" 
    (Ok (VerbatimString {format="txt"; content=""}));
  test "verbatim string simple txt" "=11\r\ntxt:example\r\n" 
    (Ok (VerbatimString {format="txt"; content="example"}));
  test "verbatim string markdown" "=17\r\nmkd:# Hello World\r\n" 
    (Ok (VerbatimString {format="mkd"; content="# Hello World"}));
  test "verbatim string with newlines" "=16\r\ntxt:line1\r\nline2\r\n" 
    (Ok (VerbatimString {format="txt"; content="line1\r\nline2"}));
]

let map_parser_tests = [
  test "map empty" "%0\r\n" (Ok (Map []));
  test "map single pair" "%1\r\n+key\r\n+value\r\n" 
    (Ok (Map [(SimpleString "key", SimpleString "value")]));
  test "map two pairs" "%2\r\n+a\r\n:1\r\n+b\r\n:2\r\n"
    (Ok (Map [(SimpleString "a", Integer 1L); (SimpleString "b", Integer 2L)]));
  test "map mixed types" "%2\r\n:1\r\n+one\r\n$3\r\ntwo\r\n:2\r\n"
    (Ok (Map [(Integer 1L, SimpleString "one"); (BulkString (Some "two"), Integer 2L)]));
  test "map nested" "%1\r\n+outer\r\n%1\r\n+inner\r\n+value\r\n"
    (Ok (Map [(SimpleString "outer", Map [(SimpleString "inner", SimpleString "value")])]));
]

let attribute_parser_tests = [
  test "attribute no attrs" "|0\r\n+value\r\n" 
    (Ok (Attribute {attrs=[]; value=SimpleString "value"}));
  test "attribute single attr" "|1\r\n+key\r\n+val\r\n+OK\r\n"
    (Ok (Attribute {attrs=[(SimpleString "key", SimpleString "val")]; value=SimpleString "OK"}));
  test "attribute multiple attrs" "|2\r\n+ttl\r\n:3600\r\n+type\r\n+string\r\n:42\r\n"
    (Ok (Attribute {
      attrs=[(SimpleString "ttl", Integer 3600L); (SimpleString "type", SimpleString "string")];
      value=Integer 42L
    }));
]

let set_parser_tests = [
  test "set empty" "~0\r\n" (Ok (Set []));
  test "set single element" "~1\r\n+element\r\n" (Ok (Set [SimpleString "element"]));
  test "set multiple elements" "~3\r\n+a\r\n+b\r\n+c\r\n" 
    (Ok (Set [SimpleString "a"; SimpleString "b"; SimpleString "c"]));
  test "set mixed types" "~3\r\n:1\r\n:2\r\n:3\r\n"
    (Ok (Set [Integer 1L; Integer 2L; Integer 3L]));
]

let push_parser_tests = [
  test "push minimal" ">2\r\n+pubsub\r\n+message\r\n" 
    (Ok (Push {kind="pubsub"; data=[SimpleString "message"]}));
  test "push with multiple data" ">4\r\n+monitor\r\n:123\r\n+client\r\n+GET key\r\n"
    (Ok (Push {kind="monitor"; data=[Integer 123L; SimpleString "client"; SimpleString "GET key"]}));
  test "push with bulk string kind" ">3\r\n$7\r\nmessage\r\n+channel\r\n+data\r\n"
    (Ok (Push {kind="message"; data=[SimpleString "channel"; SimpleString "data"]}));
]

let invalid_parser_tests = [
  (* Empty message *)
  test "empty message" "" (Error "empty: Invalid RESP format - Empty message");

  (* Invalid prefix *)
  test "no prefix char" "fdsa\r\n" (Error "invalid prefix: Invalid RESP format - Unknown prefix: f");
  test "invalid prefix x" "x\r\n" (Error "invalid prefix: Invalid RESP format - Unknown prefix: x");
  test "invalid prefix @" "@test\r\n" (Error "invalid prefix: Invalid RESP format - Unknown prefix: @");
  
  (* Simple String (+) failures *)
  test "+ no CRLF" "+OK" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "+ only LF" "+OK\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "+ only CR" "+OK\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "+ wrong order LFCR" "+OK\n\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  
  (* Simple Error (-) failures *)
  test "- no CRLF" "-Error" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "- only LF" "-Error\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "- only CR" "-Error\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "- wrong order LFCR" "-Error\n\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  
  (* Integer (:) failures *)
  test ": no CRLF" ":42" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test ": only LF" ":42\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test ": only CR" ":42\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test ": wrong order LFCR" ":42\n\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test ": invalid integer" ":abc\r\n" (Error "integer: Invalid integer: abc");
  test ": invalid integer no CRLF" ":abc" (Error "Incomplete message - expected more data (may be missing a prefix)");
  
  (* Bulk String ($) failures *)
  test "$ no CRLF length" "$5" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "$ only LF length" "$5\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "$ only CR length" "$5\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "$ no CRLF content" "$5\r\nhello" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "$ only LF content" "$5\r\nhello\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "$ only CR content" "$5\r\nhello\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "$ wrong length" "$3\r\nhello\r\n" (Error "bulk string > CRLF: string");
  
  (* Array '*' failures *)
  test "* no CRLF" "*2" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "* only LF" "*2\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "* only CR" "*2\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "* incomplete array" "*2\r\n+OK\r\n" (Error "array > array elements > empty: Invalid RESP format - Empty message");
  
  (* Null (_) failures *)
  test "_ no CRLF" "_" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "_ only LF" "_\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "_ only CR" "_\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "_ extra chars" "_null\r\n" (Error "null > CRLF: string");
  
  (* Boolean (#) failures *)
  test "# no CRLF true" "#t" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "# only LF true" "#t\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "# only CR true" "#t\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "# no CRLF false" "#f" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "# only LF false" "#f\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "# only CR false" "#f\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "# invalid boolean" "#x\r\n" (Error "boolean: Invalid boolean value: x");
  (* This should fail for the invalid bool before the CRLF *)
  test "# invalid boolean no CRLF" "#x" (Error "boolean: Invalid boolean value: x");
  
  (* Double (,) failures *)
  test ", no CRLF" ",3.14" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test ", only LF" ",3.14\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test ", only CR" ",3.14\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test ", invalid double" ",abc\r\n" (Error "double: Invalid double: abc");
  test ", invalid double no CRLF" ",abc" (Error "Incomplete message - expected more data (may be missing a prefix)");
  
  (* Big Number (() failures *)
  test "( no CRLF" "(123456789" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "( only LF" "(123456789\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "( only CR" "(123456789\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "( invalid number" "(notanumber\r\n" (Error "big number: Invalid big number: notanumber");
  
  (* Bulk Error (!) failures *)
  test "! no CRLF length" "!21" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "! only LF length" "!21\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "! only CR length" "!21\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "! no CRLF content" "!21\r\nSYNTAX invalid syntax" (Error "Incomplete message - expected more data (may be missing a prefix)");
  
  (* Verbatim String (=) failures *)
  test "= no CRLF length" "=15" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "= only LF length" "=15\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "= only CR length" "=15\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "= no format separator" "=10\r\nnoformat\r\n" (Error "verbatim string > verbatim colon: char ':'");
  
  (* Map (%) failures *)
  test "% no CRLF" "%2" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "% only LF" "%2\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "% only CR" "%2\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "% incomplete map" "%1\r\n+key\r\n" (Error "map > map key-value pairs > empty: Invalid RESP format - Empty message");
  
  (* Set (~) failures *)
  test "~ no CRLF" "~3" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "~ only LF" "~3\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "~ only CR" "~3\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "~ incomplete set" "~2\r\n+elem1\r\n" (Error "set > set elements > empty: Invalid RESP format - Empty message");
  
  (* Push (>) failures *)
  test "> no CRLF" ">2" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "> only LF" ">2\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "> only CR" ">2\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "> too few elements" ">1\r\n+pubsub\r\n" (Error "push: Push must have at least kind and one element");
  
  (* Attribute (|) failures *)
  test "| no CRLF" "|1" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "| only LF" "|1\n" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "| only CR" "|1\r" (Error "Incomplete message - expected more data (may be missing a prefix)");
  test "| missing value" "|1\r\n+key\r\n+val\r\n" (Error "attribute > attribute value > empty: Invalid RESP format - Empty message");
  
  (* Edge cases *)
  test "only CRLF" "\r\n" (Error "invalid prefix: Invalid RESP format - Unknown prefix: \r");
  test "only CR" "\r" (Error "invalid prefix: Invalid RESP format - Unknown prefix: \r");
  test "only LF" "\n" (Error "invalid prefix: Invalid RESP format - Unknown prefix: \n");
]
