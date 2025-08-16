open Js_of_ocaml
open Js_of_ocaml_lwt
open Lwt.Syntax
open Ocamlot_web.Web_types

(* DOM utilities *)
let get_element_by_id id =
  Js.Opt.to_option (Dom_html.document##getElementById (Js.string id))

let create_element tag =
  Dom_html.document##createElement (Js.string tag)

let append_child parent child =
  ignore (parent##appendChild child)

let set_inner_html element html =
  element##.innerHTML := Js.string html

let set_text_content element text =
  element##.textContent := Some (Js.string text)

(* Global state for the client *)
module ClientState = struct
  let websocket = ref None
  let market_data_history = ref []
  let max_history_length = 100
  let is_connected = ref false
  let auto_scroll = ref true
  
  (* Helper function to take first n elements from a list *)
  let rec take n lst =
    if n <= 0 then []
    else match lst with
      | [] -> []
      | x :: xs -> x :: take (n - 1) xs
  
  let add_market_data data =
    market_data_history := data :: !market_data_history;
    if List.length !market_data_history > max_history_length then
      market_data_history := take max_history_length !market_data_history
      
  let get_latest_data limit =
    take (min limit (List.length !market_data_history)) !market_data_history
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
  if Float.(current_price > prev_price) then "price-up"
  else if Float.(current_price < prev_price) then "price-down"
  else "price-unchanged"

let create_market_data_row data trend_class =
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
    let latest_data = ClientState.get_latest_data 50 in
    List.iteri (fun index data ->
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
    if !ClientState.auto_scroll then
      match get_element_by_id "market-data-container" with
      | Some container -> container##.scrollTop := 0
      | None -> ()
  | None ->
    Firebug.console##log (Js.string "Market data table body not found")

(* WebSocket connection and message handling *)
let handle_websocket_message message_text =
  match message_from_json (Js.to_string message_text) with
  | Ok (MarketDataTick data) ->
    ClientState.add_market_data data;
    update_market_data_display ()
  | Ok (SystemStatus { status; message }) ->
    Firebug.console##log (Js.string ("System status: " ^ status ^ " - " ^ message));
    (* Update connection status indicator *)
    (match get_element_by_id "connection-status" with
    | Some element ->
      element##.className := Js.string ("status " ^ status);
      set_text_content element (String.capitalize status)
    | None -> ())
  | Ok (Error { error; details }) ->
    Firebug.console##error (Js.string ("WebSocket error: " ^ error ^ " - " ^ details))
  | Error err ->
    Firebug.console##error (Js.string ("Failed to parse WebSocket message: " ^ err))
  | _ ->
    Firebug.console##log (Js.string "Received unknown message type")

let connect_websocket () =
  let protocol = if Js.to_string Dom_html.window##.location##.protocol = "https:" then "wss" else "ws" in
  let host = Js.to_string Dom_html.window##.location##.host in
  let ws_url = Printf.sprintf "%s://%s/ws" protocol host in
  
  let ws = new%js WebSockets.webSocket (Js.string ws_url) in
  
  ws##.onopen := Dom.handler (fun _event ->
    Firebug.console##log (Js.string "WebSocket connected");
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
    Firebug.console##log (Js.string "WebSocket disconnected");
    ClientState.is_connected := false;
    ClientState.websocket := None;
    Js._true
  );
  
  ws##.onerror := Dom.handler (fun _event ->
    Firebug.console##error (Js.string "WebSocket error");
    Js._true
  )

(* Control functions *)
let send_simulation_control action =
  match !ClientState.websocket with
  | Some ws when !ClientState.is_connected ->
    let message = SimulationControl { action; parameters = [] } in
    let json_message = message_to_json message in
    ws##send (Js.string json_message)
  | _ ->
    Firebug.console##error (Js.string "WebSocket not connected")

let setup_controls () =
  (* Start simulation button *)
  (match get_element_by_id "start-simulation" with
  | Some button ->
    button##.onclick := Dom.handler (fun _event ->
      send_simulation_control "start";
      Js._true
    )
  | None -> ());
  
  (* Stop simulation button *)
  (match get_element_by_id "stop-simulation" with
  | Some button ->
    button##.onclick := Dom.handler (fun _event ->
      send_simulation_control "stop";
      Js._true
    )
  | None -> ());
  
  (* Auto-scroll toggle *)
  (match get_element_by_id "auto-scroll-toggle" with
  | Some checkbox ->
    checkbox##.onchange := Dom.handler (fun _event ->
      ClientState.auto_scroll := Js.to_bool checkbox##.checked;
      Js._true
    )
  | None -> ())

(* Initialize the application *)
let init () =
  Firebug.console##log (Js.string "OCamlot Market Data Dashboard initializing...");
  
  (* Setup initial UI state *)
  (match get_element_by_id "connection-status" with
  | Some element -> 
    set_text_content element "Disconnected";
    element##.className := Js.string "status disconnected"
  | None -> ());
  
  (* Setup controls *)
  setup_controls ();
  
  (* Connect to WebSocket *)
  connect_websocket ();
  
  Firebug.console##log (Js.string "OCamlot Market Data Dashboard initialized")

(* Start the application when the DOM is ready *)
let () =
  Dom_html.window##.onload := Dom.handler (fun _event ->
    init ();
    Js._true
  )