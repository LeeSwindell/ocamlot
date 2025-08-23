open Base
open Lwt.Syntax
open Event_types
open Event_serialization
open Ocamlot_infrastructure_nats

(* Simple print-based logging for now *)
let log_info msg = Stdio.printf "[INFO] %s\n%!" msg
let log_error msg = Stdio.eprintf "[ERROR] %s\n%!" msg
let log_debug msg = Stdio.printf "[DEBUG] %s\n%!" msg

(* Connection management *)
type connection_config = {
  nats_host: string;
  nats_port: int;
  reconnect_attempts: int;
  reconnect_delay: float;
} [@@deriving show]

type connection_state = {
  config: connection_config;
  nats_client: Nats.client option;
  is_connected: bool;
  subscriptions: (string, subscription_info) Hashtbl.t;
}

and subscription_info = {
  subject: string;
  nats_sub: Nats.subscription;
  handler: event_payload -> unit Lwt.t;
  active: bool;
}

(* Global connection state *)
let connection_state = ref None

(* Connection management *)
let create_connection config =
  {
    config;
    nats_client = None;
    is_connected = false;
    subscriptions = Hashtbl.create (module String);
  }

let connect ?(config = {nats_host = "localhost"; nats_port = 4222; reconnect_attempts = 3; reconnect_delay = 1.0}) () =
  let* () = Lwt.return () in
  try
    let nats_config = Nats.Connection.{
      host = config.nats_host;
      port = config.nats_port;
      connect_timeout = 5.0;
      reconnect_attempts = config.reconnect_attempts;
      reconnect_delay = config.reconnect_delay;
    } in
    let nats_client = Nats.create ~config:nats_config () in
    let* () = Nats.connect nats_client in
    let state = {
      config;
      nats_client = Some nats_client;
      is_connected = true;
      subscriptions = Hashtbl.create (module String);
    } in
    connection_state := Some state;
    log_info (Printf.sprintf "Connected to NATS server: %s:%d" config.nats_host config.nats_port);
    Lwt.return (Ok ())
  with
  | Nats.Client_error msg | Nats.Protocol_error msg ->
    log_error (Printf.sprintf "Failed to connect to NATS: %s" msg);
    Lwt.return (Error msg)
  | exn ->
    let error_msg = "Unexpected connection error: " ^ (Exn.to_string exn) in
    log_error (Printf.sprintf "%s" error_msg);
    Lwt.return (Error error_msg)

let disconnect () =
  let* () = Lwt.return () in
  match !connection_state with
  | None -> 
    Lwt.return ()
  | Some state ->
    let* () = match state.nats_client with
      | None -> Lwt.return ()
      | Some client -> Nats.disconnect client
    in
    connection_state := None;
    log_info (Printf.sprintf "Disconnected from NATS server");
    Lwt.return ()

let is_connected () =
  match !connection_state with
  | None -> false
  | Some state -> 
    state.is_connected && 
    (match state.nats_client with
    | None -> false
    | Some client -> Nats.is_connected client)

(* Event publishing *)
let publish_event (event : event_payload event) =
  let* () = Lwt.return () in
  match !connection_state with
  | None -> 
    Lwt.return (Error "Not connected to NATS server")
  | Some state ->
    (match state.nats_client with
    | None ->
      Lwt.return (Error "No active NATS connection")
    | Some client ->
      try
        let serialized_event = serialize event in
        let* () = Nats.publish_string client ~subject:event.subject serialized_event in
        log_debug (Printf.sprintf "Published event to subject %s" event.subject);
        Lwt.return (Ok ())
      with
      | Nats.Client_error msg | Nats.Protocol_error msg ->
        log_error (Printf.sprintf "Failed to publish event: %s" msg);
        Lwt.return (Error msg)
      | Serialization_error msg ->
        log_error (Printf.sprintf "Failed to serialize event: %s" msg);
        Lwt.return (Error msg)
      | exn ->
        let error_msg = "Unexpected publish error: " ^ (Exn.to_string exn) in
        log_error (Printf.sprintf "%s" error_msg);
        Lwt.return (Error error_msg))

let publish_payload payload =
  let subject = get_subject_for_event payload in
  let event = create_event ~subject ~payload () in
  publish_event event

(* Event subscription *)
let subscribe subject handler =
  let* () = Lwt.return () in
  match !connection_state with
  | None ->
    Lwt.return (Error "Not connected to NATS server")
  | Some state ->
    (match state.nats_client with
    | None ->
      Lwt.return (Error "No active NATS connection")
    | Some client ->
      try
        let message_handler _msg data =
          try
            let event = deserialize data in
            handler event.payload
          with
          | Deserialization_error err ->
            log_error (Printf.sprintf "Failed to deserialize message: %s" err);
            Lwt.return ()
          | exn ->
            log_error (Printf.sprintf "Error handling message: %s" (Exn.to_string exn));
            Lwt.return ()
        in
        let* nats_sub = Nats.subscribe_string client ~subject ~callback:message_handler in
        let sub_info = {
          subject;
          nats_sub;
          handler;
          active = true;
        } in
        Hashtbl.set state.subscriptions ~key:subject ~data:sub_info;
        log_info (Printf.sprintf "Subscribed to subject: %s" subject);
        Lwt.return (Ok sub_info)
      with
      | Nats.Client_error msg | Nats.Protocol_error msg ->
        log_error (Printf.sprintf "Failed to subscribe to %s: %s" subject msg);
        Lwt.return (Error msg)
      | exn ->
        let error_msg = "Unexpected subscription error: " ^ (Exn.to_string exn) in
        log_error (Printf.sprintf "%s" error_msg);
        Lwt.return (Error error_msg))

(* Convenience functions for specific event types *)
let publish_order_event payload =
  publish_payload (OrderEvent payload)

let publish_market_event payload =
  publish_payload (MarketEvent payload)

let publish_risk_event payload =
  publish_payload (RiskEvent payload)

let publish_system_event payload =
  publish_payload (SystemEvent payload)

let subscribe_to_orders handler =
  subscribe "orders.>" (function
    | OrderEvent payload -> handler payload
    | _ -> Lwt.return ())

let subscribe_to_market_data handler =
  subscribe "market.>" (function
    | MarketEvent payload -> handler payload
    | _ -> Lwt.return ())

let subscribe_to_risk_events handler =
  subscribe "risk.>" (function
    | RiskEvent payload -> handler payload
    | _ -> Lwt.return ())

let subscribe_to_system_events handler =
  subscribe "system.>" (function
    | SystemEvent payload -> handler payload
    | _ -> Lwt.return ())

(* Utility functions *)
let get_active_subscriptions () =
  match !connection_state with
  | None -> []
  | Some state ->
    Hashtbl.to_alist state.subscriptions
    |> List.filter ~f:(fun (_, info) -> info.active)
    |> List.map ~f:(fun (subject, _) -> subject)

let get_connection_status () =
  match !connection_state with
  | None -> "disconnected"
  | Some state ->
    if state.is_connected then "connected" else "disconnected"