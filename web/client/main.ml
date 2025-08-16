open Js_of_ocaml
open Browser_types
(* Use Firebug console for logging - suppress deprecation warnings *)
[@@@warning "-3"]
let console_log msg = Firebug.console##log (Js.string msg)
let console_error msg = Firebug.console##error (Js.string msg)
[@@@warning "+3"]

(* DOM utilities *)
let get_element_by_id id =
  Js.Opt.to_option (Dom_html.document##getElementById (Js.string id))

let create_element tag =
  Dom_html.document##createElement (Js.string tag)

let append_child parent child =
  ignore (parent##appendChild (child :> Dom.node Js.t))

let _set_inner_html element html =
  element##.innerHTML := Js.string html

let set_text_content element text =
  element##.textContent := Js.some (Js.string text)

(* Global state for the client *)
module ClientState = struct
  let websocket = ref None
  let market_data_history = ref []
  let max_history_length = 100
  let is_connected = ref false
  let auto_scroll = ref true
  let simulation_running = ref false
  
  (* Helper function to take first n elements from a list *)
  let rec take n lst =
    if n <= 0 then []
    else match lst with
      | [] -> []
      | x :: xs -> x :: take (n - 1) xs
  
  let add_market_data (data : web_market_data) =
    market_data_history := data :: !market_data_history;
    if List.length !market_data_history > max_history_length then
      market_data_history := take max_history_length !market_data_history
      
  let get_latest_data limit =
    take (min limit (List.length !market_data_history)) !market_data_history
    
  let start_market_data () =
    simulation_running := true;
    console_log "Starting market data via server"
    
  let stop_market_data () =
    simulation_running := false;
    console_log "Stopping market data"
end

(* Market data display functions *)
let format_price price =
  Printf.sprintf "%.2f" price

let format_timestamp timestamp =
  let date = new%js Js.date_fromTimeValue (Js.number_of_float (timestamp *. 1000.0)) in
  let hours = date##getHours in
  let minutes = date##getMinutes in
  let seconds = date##getSeconds in
  let milliseconds = date##getMilliseconds in
  Printf.sprintf "%02d:%02d:%02d.%03d" hours minutes seconds milliseconds

let get_price_trend_class prev_price current_price =
  if current_price > prev_price then "price-up"
  else if current_price < prev_price then "price-down"
  else "price-unchanged"

let create_market_data_row (data : web_market_data) trend_class =
  let row = create_element "tr" in
  row##.className := Js.string ("market-data-row " ^ trend_class);
  
  let cells = [
    ("symbol", data.instrument_id);
    ("bid", format_price data.bid);
    ("ask", format_price data.ask);
    ("last", format_price data.last_price);
    ("volume", Printf.sprintf "%.0f" data.volume);
    ("time", format_timestamp data.timestamp);
  ] in
  
  List.iter (fun (class_name, content) ->
    let cell = create_element "td" in
    cell##.className := Js.string class_name;
    set_text_content cell content;
    append_child row cell
  ) cells;
  
  row

let update_market_data_display () =
  match get_element_by_id "market-data-table-body" with
  | Some tbody ->
    (* Clear existing rows *)
    tbody##.innerHTML := Js.string "";
    
    (* Get latest data and display *)
    let latest_data : web_market_data list = ClientState.get_latest_data 50 in
    List.iteri (fun index (data : web_market_data) ->
      let trend_class = 
        if index > 0 then
          try
            let prev_data = List.nth latest_data (index - 1) in
            if String.equal prev_data.instrument_id data.instrument_id then
              get_price_trend_class prev_data.last_price data.last_price
            else "price-unchanged"
          with
          | _ -> "price-unchanged"
        else "price-unchanged"
      in
      let row = create_market_data_row data trend_class in
      append_child tbody row
    ) latest_data;
    
    (* Auto-scroll to top if enabled *)
    if !ClientState.auto_scroll then (
      match get_element_by_id "market-data-container" with
      | Some container -> container##.scrollTop := Js.number_of_float 0.0
      | None -> ()
    )
  | None ->
    console_log "Market data table body not found"

(* WebSocket connection and message handling *)
let handle_websocket_message message_text =
  match message_from_json (Js.to_string message_text) with
  | Ok (MarketDataTick data) ->
    ClientState.add_market_data data;
    update_market_data_display ()
  | Ok (ConflatedBar bar) ->
    console_log ("Received conflated bar for " ^ bar.instrument_id ^ " (" ^ bar.interval ^ "): OHLCV=" ^ 
                 (Printf.sprintf "%.2f/%.2f/%.2f/%.2f/%.0f" bar.open_price bar.high_price bar.low_price bar.close_price bar.volume));
    (* Convert conflated bar to market data tick for display *)
    let market_tick = {
      instrument_id = bar.instrument_id;
      bid = bar.close_price -. 0.01;  (* Approximate bid from close price *)
      ask = bar.close_price +. 0.01;  (* Approximate ask from close price *)
      last_price = bar.close_price;
      volume = bar.volume;
      timestamp = bar.close_timestamp;
    } in
    ClientState.add_market_data market_tick;
    update_market_data_display ()
  | Ok (Analytics analytics) ->
    console_log ("Received analytics for " ^ analytics.instrument_id ^ " at " ^ (string_of_float analytics.timestamp));
    (* Analytics messages are informational - log but don't add to market data history *)
    ()
  | Ok (SystemStatus { status; message }) ->
    console_log ("System status: " ^ status ^ " - " ^ message);
    (* Update connection status indicator *)
    (match get_element_by_id "connection-status" with
    | Some element ->
      element##.className := Js.string ("status " ^ status);
      set_text_content element (String.capitalize_ascii status)
    | None -> ())
  | Ok (SimulationControl _) ->
    console_log "Received simulation control message"
  | Ok (Error { error; details }) ->
    console_error ("WebSocket error: " ^ error ^ " - " ^ details)
  | Error err ->
    console_error ("Failed to parse WebSocket message: " ^ err)

(* WebSocket control functions *)
let send_websocket_message msg =
  match !ClientState.websocket with
  | Some ws -> 
    let json_msg = message_to_json msg in
    ws##send (Js.string json_msg)
  | None -> 
    console_error "WebSocket not connected"

let start_market_data_via_websocket () =
  let control_msg = SimulationControl { 
    action = "start"; 
    parameters = [] 
  } in
  send_websocket_message control_msg;
  ClientState.start_market_data ()

let stop_market_data_via_websocket () =
  let control_msg = SimulationControl { 
    action = "stop"; 
    parameters = [] 
  } in
  send_websocket_message control_msg;
  ClientState.stop_market_data ()

let connect_websocket () =
  let protocol = if Js.to_string Dom_html.window##.location##.protocol = "https:" then "wss" else "ws" in
  let host = Js.to_string Dom_html.window##.location##.host in
  let ws_url = Printf.sprintf "%s://%s/ws" protocol host in
  
  let ws = new%js WebSockets.webSocket (Js.string ws_url) in
  
  ws##.onopen := Dom.handler (fun _event ->
    console_log "WebSocket connected";
    ClientState.is_connected := true;
    ClientState.websocket := Some ws;
    Js._true
  );
  
  ws##.onmessage := Dom.handler (fun event ->
    let message_text = event##.data in
    handle_websocket_message message_text;
    Js._true
  );
  
  ws##.onclose := Dom.handler (fun _event ->
    console_log "WebSocket disconnected";
    ClientState.is_connected := false;
    ClientState.websocket := None;
    Js._true
  );
  
  ws##.onerror := Dom.handler (fun _event ->
    console_error "WebSocket error";
    Js._true
  )

(* Control functions - WebSocket-based market data control *)

let setup_controls () =
  (* Start market data button *)
  (match get_element_by_id "start-simulation" with
  | Some button ->
    button##.onclick := Dom.handler (fun _event ->
      start_market_data_via_websocket ();
      (* Update connection status to show market data starting *)
      (match get_element_by_id "connection-status" with
      | Some element ->
        element##.className := Js.string "status starting";
        set_text_content element "Starting Market Data"
      | None -> ());
      Js._true
    )
  | None -> ());
  
  (* Stop market data button *)
  (match get_element_by_id "stop-simulation" with
  | Some button ->
    button##.onclick := Dom.handler (fun _event ->
      stop_market_data_via_websocket ();
      (* Update connection status to show market data stopping *)
      (match get_element_by_id "connection-status" with
      | Some element ->
        element##.className := Js.string "status stopping";
        set_text_content element "Stopping Market Data"
      | None -> ());
      Js._true
    )
  | None -> ());
  
  (* Auto-scroll toggle *)
  (match get_element_by_id "auto-scroll-toggle" with
  | Some element ->
    let checkbox = (Js.Unsafe.coerce element : Dom_html.inputElement Js.t) in
    checkbox##.onchange := Dom.handler (fun _event ->
      ClientState.auto_scroll := Js.to_bool checkbox##.checked;
      Js._true
    )
  | None -> ())

(* Initialize the application *)
let init () =
  console_log "OCamlot Market Data Dashboard initializing...";
  
  (* Setup initial UI state *)
  (match get_element_by_id "connection-status" with
  | Some element -> 
    set_text_content element "Ready";
    element##.className := Js.string "status connected"
  | None -> ());
  
  (* Setup controls *)
  setup_controls ();
  
  (* Connect to WebSocket for market data *)
  connect_websocket ();
  
  console_log "OCamlot Market Data Dashboard initialized (WebSocket Mode)"

(* Start the application when the DOM is ready *)
let () =
  Dom_html.window##.onload := Dom.handler (fun _event ->
    init ();
    Js._true
  )