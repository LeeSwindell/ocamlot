open Base
open Lwt.Syntax
open Stdio

(* NATS Bridge Service - Connects NATS to Dream WebSocket *)

(* Configuration for NATS bridge *)
type nats_bridge_config = {
  nats_host: string;
  nats_port: int;
  reconnect_attempts: int;
  reconnect_delay_ms: int;
  subjects: string list;
  connection_timeout_ms: int;
}

(* Connection state tracking *)
type connection_state = {
  mutable client: Ocamlot_nats.Nats.client option;
  is_connected: bool ref;
  last_connection_attempt: float ref;
  connection_attempts: int ref;
  subscriptions: (string * Ocamlot_nats.Nats.subscription) list ref;
}

(* Bridge service state *)
type bridge_state = {
  config: nats_bridge_config;
  connection: connection_state;
  message_callback: Web_types.websocket_message -> unit Lwt.t;
  error_callback: string -> unit Lwt.t;
  mutable is_running: bool;
}

(* Convert OHLCV bar from market data to web format *)
let ohlcv_bar_to_web (bar : Ocamlot_market_data.Conflation.ohlcv_bar) : Web_types.web_ohlcv_bar =
  {
    instrument_id = bar.instrument_id;
    interval = Ocamlot_market_data.Conflation.interval_to_string bar.interval;
    open_price = bar.open_price;
    high_price = bar.high_price;
    low_price = bar.low_price;
    close_price = bar.close_price;
    volume = bar.volume;
    vwap = bar.vwap;
    trade_count = bar.trade_count;
    open_timestamp = bar.open_timestamp;
    close_timestamp = bar.close_timestamp;
  }

(* Convert analytics from market data to web format *)
let analytics_to_web (analytics : Ocamlot_market_data.Analytics.symbol_analytics) : Web_types.web_analytics =
  {
    instrument_id = analytics.instrument_id;
    timestamp = analytics.timestamp;
    sma_20 = analytics.sma_20;
    ema_20 = analytics.ema_20;
    rsi_14 = (match analytics.rsi_14 with
      | Some rsi -> Some rsi.value
      | None -> None);
    volume_ratio = analytics.volume_ratio;
    vwap = analytics.vwap_daily;
  }

(* Parse NATS message based on subject *)
let parse_nats_message subject payload =
  try
    let json = Yojson.Safe.from_string payload in
    
    (* Determine message type based on subject pattern *)
    if String.is_substring subject ~substring:"market.bars" then
      (* Parse conflated bar message *)
      match Ocamlot_market_data.Conflation.ohlcv_bar_of_yojson json with
      | Ok bar -> 
        let web_bar = ohlcv_bar_to_web bar in
        Ok (Web_types.ConflatedBar web_bar)
      | Error err -> Error ("Failed to parse OHLCV bar: " ^ err)
    
    else if String.is_substring subject ~substring:"market.analytics" then
      (* Parse analytics message *)
      match Ocamlot_market_data.Analytics.symbol_analytics_of_yojson json with
      | Ok analytics ->
        let web_analytics = analytics_to_web analytics in
        Ok (Web_types.Analytics web_analytics)
      | Error err -> Error ("Failed to parse analytics: " ^ err)
    
    else
      Error ("Unknown subject pattern: " ^ subject)
      
  with
  | Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)
  | exn -> Error ("Parse exception: " ^ (Exn.to_string exn))

(* NATS message handler *)
let handle_nats_message bridge_state subject message =
  let payload = Bytes.to_string message.Ocamlot_nats.Nats.Protocol.payload in
  
  match parse_nats_message subject payload with
  | Ok websocket_msg ->
    bridge_state.message_callback websocket_msg
  | Error err ->
    let error_msg = Printf.sprintf "NATS message parse error on %s: %s" subject err in
    bridge_state.error_callback error_msg

(* Create NATS client and establish connection *)
let create_connection config =
  try%lwt
    printf "[NATS_BRIDGE] Attempting connection to %s:%d with timeout %.2fs\n%!" 
      config.nats_host config.nats_port (Float.of_int config.connection_timeout_ms /. 1000.0);
    
    let nats_config : Ocamlot_nats.Nats.connection_config = {
      host = config.nats_host;
      port = config.nats_port;
      connect_timeout = Float.of_int config.connection_timeout_ms /. 1000.0;
      reconnect_attempts = config.reconnect_attempts;
      reconnect_delay = Float.of_int config.reconnect_delay_ms /. 1000.0;
    } in
    
    printf "[NATS_BRIDGE] NATS config created: host=%s, port=%d\n%!" 
      nats_config.host nats_config.port;
    
    let client = Ocamlot_nats.Nats.create ~config:nats_config () in
    printf "[NATS_BRIDGE] NATS client created, attempting connection...\n%!";
    
    let* () = Ocamlot_nats.Nats.connect client in
    printf "[NATS_BRIDGE] NATS connection successful!\n%!";
    
    Lwt.return (Ok client)
  with
  | exn -> 
    printf "[NATS_BRIDGE] NATS connection failed with exception: %s\n%!" (Exn.to_string exn);
    Lwt.return (Result.Error (Exn.to_string exn))

(* Subscribe to NATS subjects *)
let subscribe_to_subjects bridge_state client =
  Lwt_list.fold_left_s (fun acc subject ->
    try%lwt
      let callback msg = handle_nats_message bridge_state subject msg in
      let* subscription = Ocamlot_nats.Nats.subscribe client ~subject ~callback in
      
      (* Store subscription for cleanup *)
      bridge_state.connection.subscriptions := (subject, subscription) :: !(bridge_state.connection.subscriptions);
      
      Lwt.return (acc @ [subscription])
    with
    | exn ->
      let* () = bridge_state.error_callback (Printf.sprintf "Failed to subscribe to %s: %s" subject (Exn.to_string exn)) in
      Lwt.return acc
  ) [] bridge_state.config.subjects

(* Connection management with retry logic *)
let manage_connection bridge_state =
  let rec connection_loop () =
    if bridge_state.is_running then
      let now = Unix.time () in
      let last_attempt = !(bridge_state.connection.last_connection_attempt) in
      let attempts = !(bridge_state.connection.connection_attempts) in
      
      (* Check if we should attempt connection *)
      let should_connect = 
        not !(bridge_state.connection.is_connected) &&
        (Float.( >= ) (now -. last_attempt) (Float.of_int bridge_state.config.reconnect_delay_ms /. 1000.0)) &&
        attempts < bridge_state.config.reconnect_attempts
      in
      
      if should_connect then (
        bridge_state.connection.last_connection_attempt := now;
        bridge_state.connection.connection_attempts := attempts + 1;
        
        let* result = create_connection bridge_state.config in
        match result with
        | Ok client ->
          bridge_state.connection.client <- Some client;
          bridge_state.connection.is_connected := true;
          bridge_state.connection.connection_attempts := 0;
          
          let* _subscriptions = subscribe_to_subjects bridge_state client in
          let* () = bridge_state.message_callback (Web_types.SystemStatus { 
            status = "nats_connected"; 
            message = "NATS bridge connected successfully" 
          }) in
          
          (* Monitor connection and retry if it fails *)
          let* () = Lwt_unix.sleep (Float.of_int bridge_state.config.reconnect_delay_ms /. 1000.0) in
          connection_loop ()
          
        | Error err ->
          let* () = bridge_state.error_callback (Printf.sprintf "NATS connection failed (attempt %d): %s" attempts err) in
          let* () = Lwt_unix.sleep (Float.of_int bridge_state.config.reconnect_delay_ms /. 1000.0) in
          connection_loop ()
      ) else (
        (* Wait and check again *)
        let* () = Lwt_unix.sleep 1.0 in
        connection_loop ()
      )
    else
      Lwt.return_unit
  in
  connection_loop ()

(* Cleanup subscriptions and disconnect *)
let cleanup_connection bridge_state =
  try%lwt
    (* Unsubscribe from all subjects *)
    let* () = Lwt_list.iter_s (fun (_subject, subscription) ->
      try%lwt
        Ocamlot_nats.Nats.unsubscribe subscription
      with
      | _ -> Lwt.return_unit
    ) !(bridge_state.connection.subscriptions) in
    
    (* Disconnect client *)
    let* () = match bridge_state.connection.client with
      | Some client -> Ocamlot_nats.Nats.disconnect client
      | None -> Lwt.return_unit
    in
    
    (* Reset state *)
    bridge_state.connection.client <- None;
    bridge_state.connection.is_connected := false;
    bridge_state.connection.subscriptions := [];
    
    Lwt.return_unit
  with
  | _ -> Lwt.return_unit

(* Create and start NATS bridge service *)
let create_bridge ~config ~message_callback ~error_callback =
  let connection = {
    client = None;
    is_connected = ref false;
    last_connection_attempt = ref 0.0;
    connection_attempts = ref 0;
    subscriptions = ref [];
  } in
  
  {
    config;
    connection;
    message_callback;
    error_callback;
    is_running = false;
  }

(* Start the bridge service *)
let start_bridge bridge_state =
  if not bridge_state.is_running then (
    bridge_state.is_running <- true;
    Lwt.async (fun () -> manage_connection bridge_state);
    Lwt.return_unit
  ) else
    Lwt.return_unit

(* Stop the bridge service *)
let stop_bridge bridge_state =
  if bridge_state.is_running then (
    bridge_state.is_running <- false;
    cleanup_connection bridge_state
  ) else
    Lwt.return_unit

(* Get bridge status *)
let get_bridge_status bridge_state =
  let is_connected = !(bridge_state.connection.is_connected) in
  let attempts = !(bridge_state.connection.connection_attempts) in
  let subscription_count = List.length !(bridge_state.connection.subscriptions) in
  
  ({
    is_running = bridge_state.is_running;
    is_connected;
    connection_attempts = attempts;
    active_subscriptions = subscription_count;
    subjects = bridge_state.config.subjects;
  } : Web_types.bridge_status)

(* Default configuration *)
let default_config = {
  nats_host = Option.value (Sys.getenv "NATS_HOST") ~default:"127.0.0.1";
  nats_port = Option.value_map (Sys.getenv "NATS_PORT") ~default:4222 ~f:Int.of_string;
  reconnect_attempts = 5;
  reconnect_delay_ms = 2000;
  subjects = [
    "market.bars.*";
    "market.analytics.*";
  ];
  connection_timeout_ms = 5000;
}