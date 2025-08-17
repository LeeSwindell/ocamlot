open Base

(* NATS Protocol Messages *)
type connect_options = {
  verbose: bool;
  pedantic: bool;
  tls_required: bool;
  auth_token: string option;
  user: string option;
  pass: string option;
  name: string option;
  lang: string;
  version: string;
  protocol: int option;
  echo: bool option;
  signature: string option;
  jwt: string option;
  no_responders: bool option;
  headers: bool option;
  nkey: string option;
} [@@deriving yojson]

type server_info = {
  server_id: string;
  server_name: string option;
  version: string;
  proto: int;
  git_commit: string option;
  go: string option;
  host: string;
  port: int;
  headers: bool option;
  max_payload: int;
  jetstream: bool option;
  client_id: int option;
  client_ip: string option;
  xkey: string option;
} [@@deriving yojson]

type message = {
  subject: string;
  sid: string;
  reply_to: string option;
  payload: bytes;
}

(* Protocol errors *)
exception Protocol_error of string
exception Connection_closed
exception Invalid_message of string

(* Message parsing *)
let parse_info_message data =
  try
    let json = Yojson.Safe.from_string data in
    match server_info_of_yojson json with
    | Ok info -> info
    | Error msg -> raise (Protocol_error ("Invalid INFO: " ^ msg))
  with
  | Yojson.Json_error msg -> raise (Protocol_error ("JSON parse error: " ^ msg))

let build_connect_message options =
  let json = connect_options_to_yojson options in
  "CONNECT " ^ (Yojson.Safe.to_string json) ^ "\r\n"

let build_pub_message ~subject ?reply_to payload =
  let reply_part = match reply_to with
    | Some r -> " " ^ r
    | None -> ""
  in
  let size = Bytes.length payload in
  Printf.sprintf "PUB %s%s %d\r\n" subject reply_part size

let build_sub_message ~subject ~sid =
  Printf.sprintf "SUB %s %s\r\n" subject sid

let build_unsub_message ~sid ?max_msgs () =
  match max_msgs with
  | Some max -> Printf.sprintf "UNSUB %s %d\r\n" sid max
  | None -> Printf.sprintf "UNSUB %s\r\n" sid

let ping_message = "PING\r\n"
let pong_message = "PONG\r\n"

(* Message parsing from server *)
let parse_message_line line =
  match String.split line ~on:' ' with
  | "MSG" :: subject :: sid :: size_str :: [] ->
    (subject, sid, None, Int.of_string size_str)
  | "MSG" :: subject :: sid :: reply_to :: size_str :: [] ->
    (subject, sid, Some reply_to, Int.of_string size_str)
  | _ ->
    raise (Invalid_message ("Invalid MSG line: " ^ line))

(* Protocol state machine *)
type protocol_state = 
  | WaitingInfo
  | Connected
  | Closed

let default_connect_options = {
  verbose = false;
  pedantic = false;
  tls_required = false;
  auth_token = None;
  user = None;
  pass = None;
  name = Some "ocamlot-nats";
  lang = "ocaml";
  version = "1.0.0";
  protocol = Some 1;
  echo = Some false;
  signature = None;
  jwt = None;
  no_responders = Some false;
  headers = Some false;
  nkey = None;
}