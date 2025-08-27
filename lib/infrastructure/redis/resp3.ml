open Angstrom
open Lwt.Syntax
module B = Base

let pp_z fmt z = Stdlib.Format.fprintf fmt "%s" (Z.to_string z)

type resp_value =
  | SimpleString of string                    (* +OK\r\n *)
  | SimpleError of string                     (* -ERR message\r\n *)
  | Integer of int64                          (* :42\r\n - use int64 for Redis compatibility *)
  | BulkString of string option                (* $6\r\nfoobar\r\n or $-1\r\n *)
  | Array of resp_value list option            (* *2\r\n... or *-1\r\n for null array *)
  | Null                                       (* _\r\n *)
  | Boolean of bool                            (* #t\r\n or #f\r\n *)
  | Double of float                            (* ,3.14\r\n *)
  | BigNumber of Z.t [@printer pp_z]           (* (3492890328409238509324850943850943850\r\n *)
  | BulkError of string                        (* !21\r\nSYNTAX invalid syntax\r\n *)
  | VerbatimString of {format: string; content: string} (* =15\r\ntxt:Some string\r\n *)
  | Map of (resp_value * resp_value) list      (* %2\r\n... *)
  | Attribute of {attrs: (resp_value * resp_value) list; value: resp_value} (* |1\r\n... *)
  | Set of resp_value list                     (* ~5\r\n... *)
  | Push of {kind: string; data: resp_value list} (* >4\r\n... *)
[@@deriving show, eq]


let crlf = string "\r\n" <?> "CRLF"
let till_crlf = take_till (fun c -> c = '\r') <* crlf

(* For array/bulk string lengths *)
let is_length_char = function '0'..'9' | '-' -> true | _ -> false

let parse_length = 
  take_while1 is_length_char <* crlf >>| int_of_string
  <?> "length field"

(* Parse bulk string body after the $ *)
let parse_bulk_string_body =
  parse_length >>= fun len ->
  if len < 0 then 
    return (BulkString None)
  else
    (take len <?> "bulk String Data") <* crlf >>| fun s -> BulkString (Some s)

(* Parse array body after the * *)
let parse_array_body self =
  parse_length >>= fun num_items ->
  if num_items < 0 then
    return (Array None)
  else
    (count num_items self <?> "array elements") >>| fun items -> Array (Some items)

(* Parse bulk error body after the ! *)
let parse_bulk_error_body =
  parse_length >>= fun len ->
  (take len <?> "bulk error message") <* crlf >>| fun s -> BulkError s

(* Parse verbatim string body after the = *)
let parse_verbatim_string_body =
  parse_length >>= fun len ->
  (* Parse exactly 3 chars for format, then colon, then the rest *)
  (take 3) <?> "verbatim format" >>= fun format ->
  (char ':') <?> "verbatim colon" >>= fun _ ->
  (take (len - 4)) <?> "verbatim string data" <* crlf >>| fun content ->
  VerbatimString {format; content}

(* Parse map body after the % *)
let parse_map_body self =
  parse_length >>= fun num_pairs ->
  (* Maps have num_pairs * 2 elements (key, value, key, value...) *)
  (count (num_pairs * 2) self <?> "map key-value pairs") >>= fun elements ->
  (* Pair them up *)
  let rec make_pairs = function
    | [] -> []
    | k::v::rest -> (k, v) :: make_pairs rest
    | [_] -> failwith "Odd number of elements in map"
  in
  return (Map (make_pairs elements))

(* Parse attribute body after the | *)
let parse_attribute_body self =
  parse_length >>= fun num_attrs ->
  (* First, parse num_attrs * 2 elements for the attributes *)
  (count (num_attrs * 2) self <?> "attribute key-value pairs") >>= fun attr_elements ->
  (* Pair them up *)
  let rec make_pairs = function
    | [] -> []
    | k::v::rest -> (k, v) :: make_pairs rest
    | [_] -> failwith "Odd number of elements in attributes"
  in
  let attrs = make_pairs attr_elements in
  (* Then parse the actual value *)
  (self <?> "attribute value") >>| fun value ->
  Attribute {attrs; value}

(* Parse set body after the ~ *)
let parse_set_body self =
  parse_length >>= fun len ->
  (count len self <?> "set elements") >>| fun items -> Set items

(* Parse push body after the > *)
let parse_push_body self =
  parse_length >>= fun length ->
  if length < 2 then
    fail "Push must have at least kind and one element"
  else
    (self <?> "push type") >>= fun first ->
    (* First element should be the push kind *)
    match first with
    | SimpleString kind | BulkString (Some kind) ->
        (count (length - 1) self <?> "push data") >>| fun data ->
        Push {kind; data}
    | _ -> fail "Push kind must be a string"

(* Main parser, matching on the first char *)
let parse_resp =
  fix (fun self ->
    peek_char >>= function
    | None -> (fail "Invalid RESP format - Empty message") <?> "empty"
    | Some c ->
        match c with
        (* Simple string *)
        | '+' -> (char '+' *> till_crlf >>| fun s -> SimpleString s) 
          <?> "simple string"
        
        (* Simple error *)
        | '-' -> (char '-' *> till_crlf >>| fun s -> SimpleError s)
          <?> "simple error"
        
        (* Integer *)
        | ':' -> (char ':' *> till_crlf >>= fun s ->
            (match Int64.of_string_opt s with
             | Some i -> return (Integer i)
             | None -> fail (Printf.sprintf "Invalid integer: %s" s)))
             <?> "integer"
        
        (* Null *)
        | '_' -> (char '_' *> crlf *> return Null)
          <?> "null"
        
        (* Boolean *)
        | '#' -> (char '#' *> any_char >>= (function
            | 't' -> crlf *> return (Boolean true)
            | 'f' -> crlf *> return (Boolean false)
            | c -> fail (Printf.sprintf "Invalid boolean value: %c" c)))
            <?> "boolean"
        
        (* Double *)
        | ',' -> (char ',' *> till_crlf >>= fun s ->
            (match float_of_string_opt s with
             | Some f -> return (Double f)
             | None -> fail (Printf.sprintf "Invalid double: %s" s)))
             <?> "double"
        
        (* Big number *)
        | '(' -> (char '(' *> till_crlf >>= fun s ->
            (try return (BigNumber (Z.of_string s))
             with _ -> fail (Printf.sprintf "Invalid big number: %s" s)))
             <?> "big number"
        
        (* Bulk string *)
        | '$' -> (char '$' *> parse_bulk_string_body)
          <?> "bulk string"
        
        (* Array *)
        | '*' -> (char '*' *> parse_array_body self)
          <?> "array"
        
        (* Bulk error *)
        | '!' -> (char '!' *> parse_bulk_error_body)
          <?> "bulk error"
        
        (* Verbatim string *)
        | '=' -> (char '=' *> parse_verbatim_string_body)
          <?> "verbatim string"
        
        (* Map *)
        | '%' -> (char '%' *> parse_map_body self)
          <?> "map"
        
        (* Attribute *)
        | '|' -> (char '|' *> parse_attribute_body self)
          <?> "attribute"
        
        (* Set *)
        | '~' -> (char '~' *> parse_set_body self)
          <?> "set"
        
        (* Push *)
        | '>' -> (char '>' *> parse_push_body self)
          <?> "push"
        
        (* Invalid prefix *)
        | c -> (fail (Printf.sprintf "Invalid RESP format - Unknown prefix: %c" c))
          <?> "invalid prefix"
  )

let parse_resp_buffered (reader : unit -> string Lwt.t) : (resp_value, string) result Lwt.t =
  (* This is a recursive function that handles the parsing state machine *)
  let rec parse_with_state state =
    match state with
    
    (* CASE 1: Parser successfully completed parsing a value *)
    | Buffered.Done (_unconsumed, value) ->
        (* _unconsumed: any leftover bytes after parsing this value *)
        (* value: the successfully parsed RESP value *)
        Lwt.return_ok value  (* Wrap in Ok and return as Lwt promise *)
    
    (* CASE 2: Parser needs more data to continue *)
    | Buffered.Partial continue ->
        (* continue: a function that accepts more data and returns new state *)
        (* We wrap in try-catch to handle IO errors *)
        Lwt.catch
          (fun () ->
            (* Call the reader function to get more bytes *)
            let* chunk = reader () in  (* This blocks until data arrives *)
            
            (* Check if we got EOF (empty string) *)
            if String.length chunk = 0 then
              (* Tell parser there's no more data coming *)
              match continue `Eof with
              | Buffered.Done (_, value) -> 
                  (* Parser had enough data to complete *)
                  Lwt.return_ok value
              | _ -> 
                  (* Parser still needed more data - protocol error *)
                  Lwt.return_error "Incomplete RESP message"
            else
              (* We got data, feed it to parser and recurse with new state *)
              parse_with_state (continue (`String chunk))
          )
          (* Convert any exceptions to error strings *)
          (fun exn -> Lwt.return_error (Printexc.to_string exn))
    
    (* CASE 3: Parser encountered an error *)
    | Buffered.Fail (_unconsumed, _, msg) ->
        (* _unconsumed: bytes that couldn't be parsed *)
        (* msg: error message from the parser *)
        Lwt.return_error msg
  in
  
  (* Start the state machine with initial parser state *)
  let initial_state = Buffered.parse parse_resp in
  parse_with_state initial_state

let parse_resp_from_channel (input : Lwt_io.input_channel) : (resp_value, string) result Lwt.t =
  let reader () = Lwt_io.read ~count:4096 input in
  parse_resp_buffered reader

(* Convenience functions *)
let parse_string str =
  match parse_string ~consume:All parse_resp str with
  | Ok v -> Ok v
  | Error msg when B.String.is_substring ~substring: "end_of_input" msg ->
      Error "Incomplete message - missing CRLF"
  | Error msg when B.String.is_substring ~substring:"not enough input" msg ->
      Error "Incomplete message - expected more data (may be missing a prefix)"
  | Error msg when B.String.is_substring ~substring:"expected string \"\\r\\n\"" msg ->
      Error "Invalid line ending - expected CRLF (\\r\\n)"
  | Error msg -> Error msg

(* The construct below uses mutual recursion among the helper functions. This is needed since an Arry may need to be serialized for each element, which could contain maps, that in turn have their own elements, and so on. *)

(* Serializer to RESP3 formatted strings. *)
let rec serialize_resp3 = function
  | SimpleString s -> "+" ^ s ^ "\r\n"
  | SimpleError s -> "-" ^ s ^ "\r\n"
  | Integer i -> ":" ^ Int64.to_string i ^ "\r\n"
  | BulkString (Some s) -> 
      "$" ^ string_of_int (String.length s) ^ "\r\n" ^ s ^ "\r\n"
  | BulkString None -> "$-1\r\n"
  | Array (Some items) -> serialize_array items
  | Array None -> "*-1\r\n"
  | Null -> "_\r\n"
  | Boolean true -> "#t\r\n"
  | Boolean false -> "#f\r\n"
  | Double f -> serialize_double f
  | BigNumber z -> "(" ^ Z.to_string z ^ "\r\n"
  | BulkError s -> 
      "!" ^ string_of_int (String.length s) ^ "\r\n" ^ s ^ "\r\n"
  | VerbatimString {format; content} ->
      let total_len = String.length format + 1 + String.length content in
      "=" ^ string_of_int total_len ^ "\r\n" ^ format ^ ":" ^ content ^ "\r\n"
  | Map pairs -> serialize_map pairs
  | Attribute {attrs; value} -> serialize_attribute attrs value
  | Set items -> serialize_set items
  | Push {kind; data} -> serialize_push kind data

and serialize_double f =
  if Float.is_integer f then
    "," ^ string_of_int (int_of_float f) ^ "\r\n"
  else
    "," ^ string_of_float f ^ "\r\n"

and serialize_array items = 
  "*" ^ string_of_int (List.length items) ^ "\r\n" ^
  String.concat "" (List.map serialize_resp3 items)

and serialize_map pairs =
  "%" ^ string_of_int (List.length pairs) ^ "\r\n" ^
  String.concat "" (List.fold_left (fun acc (k, v) -> acc @ [k; v]) [] pairs |> List.map serialize_resp3)

and serialize_attribute attrs value =
  "|" ^ string_of_int (List.length attrs) ^ "\r\n" ^
  String.concat "" (List.fold_left (fun acc (k, v) -> acc @ [k; v]) [] attrs |> List.map serialize_resp3) ^
  serialize_resp3 value

and serialize_set items =
  "~" ^ string_of_int (List.length items) ^ "\r\n" ^
  String.concat "" (List.map serialize_resp3 items)

and serialize_push kind data =
  ">" ^ string_of_int (1 + List.length data) ^ "\r\n" ^
  serialize_resp3 (BulkString (Some kind)) ^
  String.concat "" (List.map serialize_resp3 data)
