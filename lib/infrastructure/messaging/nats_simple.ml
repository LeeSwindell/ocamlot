open Base

(* Simplified NATS client without ctypes dependency *)
(* This is a placeholder implementation for development *)

exception Nats_error of string

(* Connection state *)
type connection = {
  url: string;
  connected: bool ref;
  id: int;
}

type subscription = {
  subject: string;
  connection: connection;
  active: bool ref;
  id: int;
}

(* Global state for connections and subscriptions *)
let connection_counter = ref 0
let subscription_counter = ref 0
let active_connections = ref []
let active_subscriptions = ref []

(* Connection management *)
let connect url =
  Int.incr connection_counter;
  let conn = {
    url;
    connected = ref true;
    id = !connection_counter;
  } in
  active_connections := conn :: !active_connections;
  Stdio.printf "NATS [SIMPLE]: Connected to %s (id: %d)\n" url conn.id;
  conn

let close conn =
  conn.connected := false;
  active_connections := List.filter !active_connections ~f:(fun c -> not (phys_equal c conn));
  Stdio.printf "NATS [SIMPLE]: Closed connection %d\n" conn.id

let is_connected conn =
  !(conn.connected)

(* Publishing *)
let publish conn subject data =
  if not (is_connected conn) then
    raise (Nats_error "Connection is closed")
  else (
    Stdio.printf "NATS [SIMPLE]: Published to %s on connection %d: %s\n" 
      subject conn.id (String.prefix data 50);
    (* In real implementation, this would send to NATS server *)
    ()
  )

(* Subscription *)
let subscribe conn subject =
  if not (is_connected conn) then
    raise (Nats_error "Connection is closed")
  else (
    Int.incr subscription_counter;
    let sub = {
      subject;
      connection = conn;
      active = ref true;
      id = !subscription_counter;
    } in
    active_subscriptions := sub :: !active_subscriptions;
    Stdio.printf "NATS [SIMPLE]: Subscribed to %s (sub id: %d)\n" subject sub.id;
    sub
  )

let unsubscribe sub =
  sub.active := false;
  active_subscriptions := List.filter !active_subscriptions ~f:(fun s -> not (phys_equal s sub));
  Stdio.printf "NATS [SIMPLE]: Unsubscribed from %s (sub id: %d)\n" sub.subject sub.id

(* Utility functions *)
let get_connection_count () =
  List.length !active_connections

let get_subscription_count () =
  List.length !active_subscriptions

let get_active_subjects () =
  List.map !active_subscriptions ~f:(fun sub -> sub.subject)
  |> List.dedup_and_sort ~compare:String.compare