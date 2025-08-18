open Base
open Lwt.Syntax
open Nats_protocol
open Stdio

type connection_config = {
  host: string;
  port: int;
  connect_timeout: float;
  reconnect_attempts: int;
  reconnect_delay: float;
}

type connection_state = {
  config: connection_config;
  socket: Lwt_unix.file_descr option;
  input_channel: Lwt_io.input_channel option;
  output_channel: Lwt_io.output_channel option;
  protocol_state: protocol_state;
  server_info: server_info option;
  next_sid: int ref;
  subscriptions: (string, string * (message -> unit Lwt.t)) Hashtbl.t;
}

let default_config = {
  host = "localhost";
  port = 4222;
  connect_timeout = 5.0;
  reconnect_attempts = 3;
  reconnect_delay = 1.0;
}

let create_connection ?(config = default_config) () = {
  config;
  socket = None;
  input_channel = None;
  output_channel = None;
  protocol_state = WaitingInfo;
  server_info = None;
  next_sid = ref 1;
  subscriptions = Hashtbl.create (module String);
}

let generate_sid conn =
  let sid = Int.to_string !(conn.next_sid) in
  Int.incr conn.next_sid;
  sid

let send_command conn command =
  match conn.output_channel with
  | None -> Lwt.fail Connection_closed
  | Some oc ->
    let* () = Lwt_io.write oc command in
    Lwt_io.flush oc

let read_line conn =
  match conn.input_channel with
  | None -> Lwt.fail Connection_closed
  | Some ic ->
    Lwt_io.read_line ic

let connect conn =
  let* () = Lwt.return () in
  try
    printf "[NATS_CONNECTION] Connecting to %s:%d...\n%!" conn.config.host conn.config.port;
    
    (* Create TCP connection *)
    let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
    printf "[NATS_CONNECTION] Socket created, resolving address: %s\n%!" conn.config.host;
    
    (* Resolve hostname to IP address *)
    let addr = 
      try 
        Unix.inet_addr_of_string conn.config.host
      with _ ->
        (* If not an IP address, resolve hostname *)
        let hostent = Unix.gethostbyname conn.config.host in
        hostent.h_addr_list.(0)
    in
    let sockaddr = Unix.ADDR_INET (addr, conn.config.port) in
    printf "[NATS_CONNECTION] Address resolved, connecting...\n%!";
    
    let* () = Lwt_unix.connect socket sockaddr in
    
    (* Create I/O channels *)
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
    
    (* Update connection state *)
    let conn = { conn with 
      socket = Some socket;
      input_channel = Some ic;
      output_channel = Some oc;
      protocol_state = WaitingInfo;
    } in
    
    (* Read INFO message from server *)
    let* info_line = read_line conn in
    if not (String.is_prefix info_line ~prefix:"INFO ") then
      raise (Protocol_error "Expected INFO message");
    
    let info_json = String.drop_prefix info_line 5 in
    let server_info = parse_info_message info_json in
    
    (* Send CONNECT message *)
    let connect_msg = build_connect_message default_connect_options in
    let* () = send_command conn connect_msg in
    
    (* Wait for +OK, -ERR, or PING (server may send PING immediately) *)
    let* response = read_line conn in
    let* () = 
      if String.is_prefix response ~prefix:"-ERR" then
        Lwt.fail (Protocol_error ("Server error: " ^ response))
      else if String.equal response "PING" then
        (* Server sent PING immediately - respond with PONG and connection is established *)
        send_command conn pong_message
      else if not (String.is_prefix response ~prefix:"+OK") then
        Lwt.fail (Protocol_error ("Unexpected response: " ^ response))
      else
        Lwt.return_unit
    in
    
    let connected_conn = { conn with 
      protocol_state = Connected;
      server_info = Some server_info;
    } in
    
    Lwt.return connected_conn
    
  with
  | Unix.Unix_error (errno, func, arg) ->
    let error_msg = Printf.sprintf "%s(%s): %s" func arg (Unix.error_message errno) in
    Lwt.fail (Protocol_error error_msg)
  | exn ->
    Lwt.fail exn

let disconnect conn =
  let* () = match conn.input_channel with
    | Some ic -> Lwt_io.close ic
    | None -> Lwt.return ()
  in
  let* () = match conn.output_channel with
    | Some oc -> Lwt_io.close oc
    | None -> Lwt.return ()
  in
  let* () = match conn.socket with
    | Some socket -> Lwt_unix.close socket
    | None -> Lwt.return ()
  in
  Lwt.return { conn with 
    socket = None;
    input_channel = None;
    output_channel = None;
    protocol_state = Closed;
  }

let is_connected conn =
  match conn.protocol_state with
  | Connected -> true
  | _ -> false

let publish conn ~subject ?reply_to payload =
  if not (is_connected conn) then
    Lwt.fail Connection_closed
  else
    let pub_msg = build_pub_message ~subject ?reply_to payload in
    let* () = send_command conn pub_msg in
    let* () = send_command conn (Bytes.to_string payload ^ "\r\n") in
    Lwt.return ()

let subscribe conn ~subject ~callback =
  if not (is_connected conn) then
    Lwt.fail Connection_closed
  else
    let sid = generate_sid conn in
    let sub_msg = build_sub_message ~subject ~sid in
    Hashtbl.set conn.subscriptions ~key:sid ~data:(subject, callback);
    let* () = send_command conn sub_msg in
    Lwt.return sid

let unsubscribe conn ~sid =
  if not (is_connected conn) then
    Lwt.fail Connection_closed
  else
    let* () = Lwt.return () in
    Hashtbl.remove conn.subscriptions sid;
    let unsub_msg = build_unsub_message ~sid () in
    send_command conn unsub_msg

let handle_incoming_message conn subject sid reply_to payload =
  match Hashtbl.find conn.subscriptions sid with
  | Some (_, callback) ->
    let message = { subject; sid; reply_to; payload } in
    callback message
  | None ->
    (* Subscription not found, possibly already unsubscribed *)
    Lwt.return ()

let start_message_loop conn =
  let rec loop () =
    if is_connected conn then
      try%lwt
        let* line = read_line conn in
        if String.is_prefix line ~prefix:"MSG " then (
          let (subject, sid, reply_to, _size) = parse_message_line line in
          let* payload_data = match conn.input_channel with
            | Some ic -> Lwt_io.read_value ic
            | None -> Lwt.fail Connection_closed
          in
          let payload = Bytes.of_string payload_data in
          let* () = handle_incoming_message conn subject sid reply_to payload in
          let* _crlf = read_line conn in (* consume \r\n after payload *)
          loop ()
        ) else if String.equal line "PING" then (
          let* () = send_command conn pong_message in
          loop ()
        ) else if String.is_prefix line ~prefix:"+OK" || String.is_prefix line ~prefix:"-ERR" then (
          (* Server responses, log and continue *)
          loop ()
        ) else (
          (* Unknown message, log and continue *)
          loop ()
        )
      with
      | End_of_file -> Lwt.return ()
      | exn -> Lwt.fail exn
    else
      Lwt.return ()
  in
  loop ()