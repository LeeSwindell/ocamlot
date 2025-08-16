open Base

(* Web-safe versions of core types that can be serialized to JSON *)

(* Market data types for web interface *)
type web_market_data = {
  instrument_id: string;
  bid: float;
  ask: float;
  last_price: float;
  volume: float;
  timestamp: float;
} [@@deriving yojson, show]

(* Helper function to create market data - we'll add conversion functions later *)
let create_market_data ~instrument_id ~bid ~ask ~last_price ~volume ~timestamp =
  { instrument_id; bid; ask; last_price; volume; timestamp }

(* WebSocket message types *)
type websocket_message = 
  | MarketDataTick of web_market_data
  | SystemStatus of { status: string; message: string }
  | SimulationControl of { action: string; parameters: (string * string) list }
  | Error of { error: string; details: string }
  [@@deriving yojson, show]

(* UI state types *)
type symbol_filter = {
  enabled_symbols: string list;
  display_limit: int;
  auto_scroll: bool;
} [@@deriving yojson, show]

type dashboard_config = {
  update_frequency_ms: int;
  symbol_filter: symbol_filter;
  chart_type: string; (* "table" | "line" | "candlestick" *)
  theme: string; (* "light" | "dark" *)
} [@@deriving yojson, show]

(* Simulation control types *)
type simulation_config = {
  instruments: string list;
  tick_interval_ms: int;
  volatility: float;
  initial_prices: (string * float) list;
} [@@deriving yojson, show]

type simulation_status = {
  is_running: bool;
  uptime_ms: int;
  ticks_generated: int;
  active_instruments: string list;
} [@@deriving yojson, show]

(* HTTP API response types *)
type api_response = 
  | Success of { data: Yojson.Safe.t; message: string }
  | Error of { error: string; details: string }
  [@@deriving yojson, show]

(* Market data statistics for monitoring *)
type market_stats = {
  total_ticks: int;
  ticks_per_second: float;
  active_symbols: int;
  price_ranges: (string * float * float) list; (* symbol, min, max *)
  last_update: float;
} [@@deriving yojson, show]

(* Real-time data structure for frontend state *)
type live_market_data = {
  data: web_market_data;
  price_change: float; (* percentage change from previous *)
  trend: string; (* "up" | "down" | "unchanged" *)
  sequence_number: int;
} [@@deriving yojson, show]

(* WebSocket protocol helpers *)
let message_to_json (msg : websocket_message) : string =
  websocket_message_to_yojson msg |> Yojson.Safe.to_string

let message_from_json (json_str : string) : (websocket_message, string) Result.t =
  try
    let json = Yojson.Safe.from_string json_str in
    match websocket_message_of_yojson json with
    | Ok msg -> Ok msg
    | Error err -> Error ("JSON parse error: " ^ err)
  with
  | exn -> Error ("JSON exception: " ^ (Exn.to_string exn))

(* API response helpers *)
let success_response ~data ~message =
  Success { data; message }

let error_response ~error ~details =
  Error { error; details }

let response_to_json (response : api_response) : string =
  api_response_to_yojson response |> Yojson.Safe.to_string

(* Configuration defaults *)
let default_dashboard_config = {
  update_frequency_ms = 100;
  symbol_filter = {
    enabled_symbols = ["AAPL"; "GOOGL"; "MSFT"; "TSLA"];
    display_limit = 100;
    auto_scroll = true;
  };
  chart_type = "table";
  theme = "light";
}

let default_simulation_config = {
  instruments = ["AAPL"; "GOOGL"; "MSFT"; "TSLA"; "AMZN"];
  tick_interval_ms = 100;
  volatility = 0.02;
  initial_prices = [
    ("AAPL", 150.0);
    ("GOOGL", 2800.0);
    ("MSFT", 300.0);
    ("TSLA", 800.0);
    ("AMZN", 3200.0);
  ];
}