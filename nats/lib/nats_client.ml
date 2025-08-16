open Base
open Lwt.Syntax
open Nats_connection
open Nats_protocol

(* Client state *)
type client = {
  connection: connection_state ref;
  message_loop: unit Lwt.t option ref;
  is_running: bool ref;
}

type subscription = {
  client: client;
  sid: string;
}

exception Client_error of string
exception Not_connected

(* Client creation and connection *)
let create ?(config = default_config) () = {
  connection = ref (create_connection ~config ());
  message_loop = ref None;
  is_running = ref false;
}

let connect client =
  let* conn = connect !(client.connection) in
  client.connection := conn;
  client.is_running := true;
  
  (* Start message processing loop in background *)
  let loop = start_message_loop conn in
  client.message_loop := Some loop;
  
  Lwt.return ()

let disconnect client =
  client.is_running := false;
  
  (* Cancel message loop if running *)
  (match !(client.message_loop) with
  | Some loop -> Lwt.cancel loop
  | None -> ());
  
  let* conn = disconnect !(client.connection) in
  client.connection := conn;
  client.message_loop := None;
  Lwt.return ()

let is_connected client =
  is_connected !(client.connection)

(* Publishing *)
let publish client ~subject ?reply_to payload =
  if not (is_connected client) then
    Lwt.fail Not_connected
  else
    publish !(client.connection) ~subject ?reply_to payload

let publish_string client ~subject ?reply_to data =
  let payload = Bytes.of_string data in
  publish client ~subject ?reply_to payload

(* Subscribing *)
let subscribe client ~subject ~callback =
  if not (is_connected client) then
    Lwt.fail Not_connected
  else
    let* sid = subscribe !(client.connection) ~subject ~callback in
    Lwt.return { client; sid }

let subscribe_string client ~subject ~callback =
  let string_callback msg = 
    let data = Bytes.to_string msg.payload in
    callback { msg with payload = Bytes.of_string data } data
  in
  subscribe client ~subject ~callback:string_callback

let unsubscribe subscription =
  if not (is_connected subscription.client) then
    Lwt.fail Not_connected
  else
    unsubscribe !(subscription.client.connection) ~sid:subscription.sid

(* Utility functions *)
let get_server_info client =
  !(client.connection).server_info

let get_connection_config client =
  !(client.connection).config

(* Request-reply pattern helper *)
let request client ~subject ~payload ~timeout =
  let reply_subject = "reply." ^ (Int.to_string (Random.int 1000000)) in
  let result_promise, result_resolver = Lwt.wait () in
  
  let reply_handler msg =
    Lwt.wakeup result_resolver msg.payload;
    Lwt.return ()
  in
  
  let* reply_sub = subscribe client ~subject:reply_subject ~callback:reply_handler in
  let* () = publish client ~subject ~reply_to:reply_subject payload in
  
  let timeout_promise = 
    let* () = Lwt_unix.sleep timeout in
    Lwt.fail (Client_error "Request timeout")
  in
  
  let* result = Lwt.pick [result_promise; timeout_promise] in
  let* () = unsubscribe reply_sub in
  Lwt.return result