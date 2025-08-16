open Lwt.Syntax
module Web_types = Ocamlot_web.Web_types
open Web_types

(* Helper to create WebSocket error messages *)
let websocket_error ~error ~details : websocket_message = Error { error; details }

(* Global state for the web server *)
module State = struct
  let active_connections = ref []
  let simulation_running = ref false
  let market_data_stats = ref {
    total_ticks = 0;
    ticks_per_second = 0.0;
    active_symbols = 0;
    price_ranges = [];
    last_update = Unix.time ();
  }
  
  let add_connection ws = 
    active_connections := ws :: !active_connections
    
  let remove_connection ws =
    active_connections := List.filter (fun c -> c != ws) !active_connections
    
  let broadcast_to_all message =
    let json_msg = message_to_json message in
    List.iter (fun ws ->
      try
        Dream.send ws json_msg |> ignore
      with
      | _ -> () (* Connection might be closed *)
    ) !active_connections
end

(* Market data generation using unified library *)
module Feed = Ocamlot_market_data.Feed

let market_profiles = ref Feed.default_profiles
let market_initialized = ref false

let initialize_server_market_data () =
  if not !market_initialized then (
    Feed.set_random_seed 123; (* Different seed from client for variety *)
    Feed.initialize_market_state !market_profiles;
    market_initialized := true
  )

let generate_sample_market_data () =
  initialize_server_market_data ();
  let market_snapshot = Feed.generate_market_snapshot !market_profiles in
  List.map Web_types.market_data_to_web market_snapshot

(* WebSocket handler *)
let websocket_handler _request =
  Dream.websocket (fun websocket ->
    State.add_connection websocket;
  
  (* Send initial status *)
  let status_msg = SystemStatus { 
    status = "connected"; 
    message = "WebSocket connection established" 
  } in
  let* () = Dream.send websocket (message_to_json status_msg) in
  
  (* Handle incoming messages *)
  let rec handle_messages () =
    match%lwt Dream.receive websocket with
    | Some message_text ->
      (match message_from_json message_text with
      | Ok (SimulationControl { action; parameters = _ }) ->
        (match action with
        | "start" ->
          State.simulation_running := true;
          let response = SystemStatus { 
            status = "simulation_started"; 
            message = "Market data simulation started" 
          } in
          let* () = Dream.send websocket (message_to_json response) in
          handle_messages ()
        | "stop" ->
          State.simulation_running := false;
          let response = SystemStatus { 
            status = "simulation_stopped"; 
            message = "Market data simulation stopped" 
          } in
          let* () = Dream.send websocket (message_to_json response) in
          handle_messages ()
        | _ ->
          let error_msg = websocket_error 
            ~error:"unknown_action" 
            ~details:("Unknown simulation action: " ^ action) in
          let* () = Dream.send websocket (message_to_json error_msg) in
          handle_messages ())
      | Ok _ ->
        (* Handle other message types *)
        handle_messages ()
      | Error err ->
        let error_msg = websocket_error 
          ~error:"parse_error" 
          ~details:err in
        let* () = Dream.send websocket (message_to_json error_msg) in
        handle_messages ())
    | None ->
      (* WebSocket closed *)
      State.remove_connection websocket;
      Lwt.return ()
  in
  handle_messages ())

(* Market data simulation loop *)
let start_market_data_loop () =
  let rec loop () =
    if !State.simulation_running then
      let market_data_list = generate_sample_market_data () in
      List.iter (fun data ->
        let message = MarketDataTick data in
        State.broadcast_to_all message
      ) market_data_list;
      let* () = Lwt_unix.sleep 0.1 in (* 100ms interval *)
      loop ()
    else
      let* () = Lwt_unix.sleep 1.0 in (* Check every second when stopped *)
      loop ()
  in
  loop ()

(* HTTP API endpoints *)
let api_status _request =
  let status = {
    is_running = !State.simulation_running;
    uptime_ms = 0; (* TODO: implement uptime tracking *)
    ticks_generated = !State.market_data_stats.total_ticks;
    active_instruments = ["AAPL"; "GOOGL"; "MSFT"; "TSLA"; "AMZN"];
  } in
  let response = success_response 
    ~data:(simulation_status_to_yojson status) 
    ~message:"Simulation status retrieved" in
  Dream.respond (response_to_json response)
    ~headers:[("Content-Type", "application/json")]

let api_start_simulation _request =
  State.simulation_running := true;
  let response = success_response 
    ~data:(`String "started") 
    ~message:"Simulation started successfully" in
  Dream.respond (response_to_json response)
    ~headers:[("Content-Type", "application/json")]

let api_stop_simulation _request =
  State.simulation_running := false;
  let response = success_response 
    ~data:(`String "stopped") 
    ~message:"Simulation stopped successfully" in
  Dream.respond (response_to_json response)
    ~headers:[("Content-Type", "application/json")]

(* Static file serving *)
let serve_static request =
  let path = Dream.target request in
  let file_path = match path with
    | "/" -> "web/static/index.html"
    | "/style.css" -> "web/static/style.css"
    | "/app.js" -> "web/static/app.js"
    | _ -> "web/static" ^ path
  in
  Dream.from_filesystem "." file_path request

(* Main server setup *)
let () =
  (* Start the market data simulation loop in background *)
  Lwt.async start_market_data_loop;
  
  Dream.run
    ~interface:"0.0.0.0"
    ~port:8080
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/ws" websocket_handler;
    Dream.get "/api/status" api_status;
    Dream.post "/api/start" api_start_simulation;
    Dream.post "/api/stop" api_stop_simulation;
    Dream.get "/**" serve_static;
  ]