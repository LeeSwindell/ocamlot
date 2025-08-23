open Base
open Lwt.Syntax

(* Module with encapsulated mutable state for clean API *)
module Client = struct
  type t = {
    mutable conn: Nats_connection.t option;
    mutable message_loop: unit Lwt.t option;
    config: Nats_connection.connection_config;
  }
  
  type subscription = {
    client: t;
    sid: string;
  }
  
  exception Client_error of string
  exception Not_connected
  
  let create ?(config = Nats_connection.default_config) () = {
    conn = None;
    message_loop = None;
    config;
  }
  
  let connect t =
    let* conn = Nats_connection.connect t.config in
    t.conn <- Some conn;
    
    (* Start message loop in background *)
    let loop = 
      Lwt.async (fun () ->
        try%lwt
          match t.conn with
          | Some c -> 
            let* _final_conn = Nats_connection.message_loop c in
            Lwt.return_unit
          | None -> Lwt.return_unit
        with
        | _ -> 
          (* Log error but don't crash *)
          Lwt.return_unit
      );
      Lwt.return_unit
    in
    t.message_loop <- Some loop;
    Lwt.return_unit
  
  let disconnect t =
    (* Cancel message loop *)
    (match t.message_loop with
    | Some loop -> Lwt.cancel loop
    | None -> ());
    
    (* Disconnect connection *)
    let* () = match t.conn with
    | Some conn -> 
      let* disconnected = Nats_connection.disconnect conn in
      t.conn <- Some disconnected;
      Lwt.return_unit
    | None -> Lwt.return_unit
    in
    
    t.conn <- None;
    t.message_loop <- None;
    Lwt.return_unit
  
  let is_connected t =
    match t.conn with
    | Some conn -> Nats_connection.is_connected conn
    | None -> false
  
  let publish t ~subject ?reply_to payload =
    match t.conn with
    | None -> Lwt.fail Not_connected
    | Some conn ->
      if not (Nats_connection.is_connected conn) then
        Lwt.fail Not_connected
      else
        let* conn' = Nats_connection.publish conn ~subject ?reply_to payload in
        t.conn <- Some conn';
        Lwt.return_unit
  
  let publish_string t ~subject ?reply_to data =
    let payload = Bytes.of_string data in
    publish t ~subject ?reply_to payload
  
  let subscribe t ~subject ~callback =
    match t.conn with
    | None -> Lwt.fail Not_connected
    | Some conn ->
      if not (Nats_connection.is_connected conn) then
        Lwt.fail Not_connected
      else
        let* (sid, conn') = Nats_connection.subscribe conn ~subject ~callback in
        t.conn <- Some conn';
        Lwt.return { client = t; sid }
  
  let subscribe_string t ~subject ~callback =
    let string_callback msg = 
      let data = Bytes.to_string msg.Nats_protocol.payload in
      callback msg data
    in
    subscribe t ~subject ~callback:string_callback
  
  let unsubscribe subscription =
    let t = subscription.client in
    match t.conn with
    | None -> Lwt.fail Not_connected
    | Some conn ->
      if not (Nats_connection.is_connected conn) then
        Lwt.fail Not_connected
      else
        let* conn' = Nats_connection.unsubscribe conn ~sid:subscription.sid in
        t.conn <- Some conn';
        Lwt.return_unit
  
  let get_server_info t =
    match t.conn with
    | Some conn -> conn.server_info
    | None -> None
  
  let get_connection_config t =
    t.config
  
  let request t ~subject ~payload ~timeout =
    let reply_subject = "reply." ^ (Int.to_string (Random.int 1000000)) in
    let result_promise, result_resolver = Lwt.wait () in
    
    let reply_handler msg =
      Lwt.wakeup result_resolver msg.Nats_protocol.payload;
      Lwt.return_unit
    in
    
    let* reply_sub = subscribe t ~subject:reply_subject ~callback:reply_handler in
    let* () = publish t ~subject ~reply_to:reply_subject payload in
    
    let timeout_promise = 
      let* () = Lwt_unix.sleep timeout in
      Lwt.fail (Client_error "Request timeout")
    in
    
    let* result = Lwt.pick [result_promise; timeout_promise] in
    let* () = unsubscribe reply_sub in
    Lwt.return result
end

(* Re-export types and functions at module level for compatibility *)
type client = Client.t
type subscription = Client.subscription

exception Client_error = Client.Client_error
exception Not_connected = Client.Not_connected

let create = Client.create
let connect = Client.connect
let disconnect = Client.disconnect
let is_connected = Client.is_connected
let publish = Client.publish
let publish_string = Client.publish_string
let subscribe = Client.subscribe
let subscribe_string = Client.subscribe_string
let unsubscribe = Client.unsubscribe
let get_server_info = Client.get_server_info
let get_connection_config = Client.get_connection_config
let request = Client.request