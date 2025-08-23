open Base
open Lwt.Syntax
open Nats_protocol

type connection_config = {
  host: string;
  port: int;
  connect_timeout: float;
  reconnect_attempts: int;
  reconnect_delay: float;
}

(* Immutable connection state - functional core *)
type t = {
  config: connection_config;
  socket: Lwt_unix.file_descr option;
  input_channel: Lwt_io.input_channel option;
  output_channel: Lwt_io.output_channel option;
  protocol_state: protocol_state;
  server_info: server_info option;
  next_sid: int;
  subscriptions: (string, (string * (message -> unit Lwt.t)), String.comparator_witness) Map.t;
}

let default_config = {
  host = "localhost";
  port = 4222;
  connect_timeout = 5.0;
  reconnect_attempts = 3;
  reconnect_delay = 1.0;
}

let create ?(config = default_config) () = {
  config;
  socket = None;
  input_channel = None;
  output_channel = None;
  protocol_state = WaitingInfo;
  server_info = None;
  next_sid = 1;
  subscriptions = Map.empty (module String);
}

(* Pure functional SID generation *)
let generate_sid conn =
  let sid = Int.to_string conn.next_sid in
  let conn' = { conn with next_sid = conn.next_sid + 1 } in
  (sid, conn')

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

let read_bytes conn size =
  match conn.input_channel with
  | None -> Lwt.fail Connection_closed
  | Some ic ->
    let buffer = Bytes.create size in
    let* () = Lwt_io.read_into_exactly ic buffer 0 size in
    Lwt.return buffer

let connect config =
  try%lwt
    (* Create TCP connection *)
    let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
    
    (* Resolve hostname to IP address *)
    let addr = 
      try 
        Unix.inet_addr_of_string config.host
      with _ ->
        (* If not an IP address, resolve hostname *)
        let hostent = Unix.gethostbyname config.host in
        hostent.h_addr_list.(0)
    in
    let sockaddr = Unix.ADDR_INET (addr, config.port) in
    
    let* () = Lwt_unix.connect socket sockaddr in
    
    (* Create I/O channels - these share the same FD *)
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
    
    (* Initial connection state *)
    let conn = {
      config;
      socket = Some socket;
      input_channel = Some ic;
      output_channel = Some oc;
      protocol_state = WaitingInfo;
      server_info = None;
      next_sid = 1;
      subscriptions = Map.empty (module String);
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
    
    (* Wait for +OK, -ERR, or PING *)
    let* response = read_line conn in
    let* () = 
      if String.is_prefix response ~prefix:"-ERR" then
        Lwt.fail (Protocol_error ("Server error: " ^ response))
      else if String.equal response "PING" then
        (* Server sent PING immediately - respond with PONG *)
        send_command conn pong_message
      else if not (String.is_prefix response ~prefix:"+OK") then
        Lwt.fail (Protocol_error ("Unexpected response: " ^ response))
      else
        Lwt.return_unit
    in
    
    Lwt.return { conn with 
      protocol_state = Connected;
      server_info = Some server_info;
    }
    
  with
  | Unix.Unix_error (errno, func, arg) ->
    let error_msg = Printf.sprintf "%s(%s): %s" func arg (Unix.error_message errno) in
    Lwt.fail (Protocol_error error_msg)

let disconnect conn =
  (* FIXED: Only close the input channel which will close the underlying FD *)
  (* Since both channels share the same FD, closing one closes the socket *)
  let close_channels () =
    match conn.input_channel with
    | Some ic ->
      (* Close input channel, which closes the FD *)
      Lwt.catch
        (fun () -> Lwt_io.close ic)
        (fun _ -> Lwt.return ())
    | None ->
      (* If no input channel, try output channel *)
      match conn.output_channel with
      | Some oc ->
        Lwt.catch
          (fun () -> Lwt_io.close oc)
          (fun _ -> Lwt.return ())
      | None ->
        Lwt.return ()
  in
  
  let* () = close_channels () in
  
  (* Don't try to close socket - channels handle it *)
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
    Lwt.return conn

let subscribe conn ~subject ~callback =
  if not (is_connected conn) then
    Lwt.fail Connection_closed
  else
    let (sid, conn') = generate_sid conn in
    let sub_msg = build_sub_message ~subject ~sid in
    let conn'' = { 
      conn' with 
      subscriptions = Map.add_exn conn'.subscriptions ~key:sid ~data:(subject, callback) 
    } in
    let* () = send_command conn'' sub_msg in
    Lwt.return (sid, conn'')

let unsubscribe conn ~sid =
  if not (is_connected conn) then
    Lwt.fail Connection_closed
  else
    let conn' = { 
      conn with 
      subscriptions = Map.remove conn.subscriptions sid 
    } in
    let unsub_msg = build_unsub_message ~sid () in
    let* () = send_command conn' unsub_msg in
    Lwt.return conn'

let handle_incoming_message conn subject sid reply_to payload =
  match Map.find conn.subscriptions sid with
  | Some (_, callback) ->
    let message = { subject; sid; reply_to; payload } in
    callback message
  | None ->
    (* Subscription not found, possibly already unsubscribed *)
    Lwt.return ()

let process_message_line conn line =
  if String.is_prefix line ~prefix:"MSG " then
    let (subject, sid, reply_to, size) = parse_message_line line in
    (* FIXED: Read exact bytes, not marshalled value *)
    let* payload = read_bytes conn size in
    let* () = handle_incoming_message conn subject sid reply_to payload in
    let* _crlf = read_line conn in (* consume \r\n after payload *)
    Lwt.return conn
  else if String.equal line "PING" then
    let* () = send_command conn pong_message in
    Lwt.return conn
  else if String.is_prefix line ~prefix:"+OK" || String.is_prefix line ~prefix:"-ERR" then
    (* Server responses, continue *)
    Lwt.return conn
  else
    (* Unknown message, continue *)
    Lwt.return conn

(* Message loop for handling incoming messages *)
let rec message_loop conn =
  if is_connected conn then
    try%lwt
      let* line = read_line conn in
      let* conn' = process_message_line conn line in
      message_loop conn'
    with
    | End_of_file -> Lwt.return conn
    | exn -> Lwt.fail exn
  else
    Lwt.return conn