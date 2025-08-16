open Base

(* Browser-safe web types - no server dependencies *)
(* This module contains only the types and functions needed by the JavaScript client *)

(* Market data types for web interface *)
type web_market_data = {
  instrument_id: string;
  bid: float;
  ask: float;
  last_price: float;
  volume: float;
  timestamp: float;
} [@@deriving yojson, show]

(* OHLCV bar data for web interface *)
type web_ohlcv_bar = {
  instrument_id: string;
  interval: string;  (* "1s", "1m", "5m", etc. *)
  open_price: float;
  high_price: float;
  low_price: float;
  close_price: float;
  volume: float;
  vwap: float;
  trade_count: int;
  open_timestamp: float;
  close_timestamp: float;
} [@@deriving yojson, show]

(* Analytics data for web interface *)
type web_analytics = {
  instrument_id: string;
  timestamp: float;
  sma_20: float option;
  ema_20: float option;
  rsi_14: float option;
  volume_ratio: float option;
  vwap: float option;
} [@@deriving yojson, show]

(* WebSocket message types *)
type websocket_message = 
  | MarketDataTick of web_market_data
  | ConflatedBar of web_ohlcv_bar
  | Analytics of web_analytics
  | SystemStatus of { status: string; message: string }
  | SimulationControl of { action: string; parameters: (string * string) list }
  | Error of { error: string; details: string }
  [@@deriving yojson, show]

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

(* Helper function to create market data *)
let create_market_data ~instrument_id ~bid ~ask ~last_price ~volume ~timestamp =
  { instrument_id; bid; ask; last_price; volume; timestamp }

(* Browser-compatible market data generation - no Lwt dependencies *)
type instrument_profile = {
  symbol: string;
  base_price: float;
  volatility: float;        (* Daily volatility as decimal (0.02 = 2%) *)
  min_spread: float;        (* Minimum bid-ask spread *)
  max_spread: float;        (* Maximum bid-ask spread *)
  avg_volume: float;        (* Average volume per tick *)
  volume_variance: float;   (* Volume variance factor *)
  momentum_factor: float;   (* Tendency to continue price direction *)
  mean_reversion: float;    (* Tendency to return to base price *)
} [@@deriving show]

type market_state = {
  current_prices: (string * float) list;
  price_directions: (string * float) list; (* -1.0 to 1.0, momentum *)
  volumes: (string * float) list;
  last_update: float;
} [@@deriving show]

(* Default instrument profiles *)
let default_profiles = [
  {
    symbol = "AAPL";
    base_price = 150.0;
    volatility = 0.025;
    min_spread = 0.01;
    max_spread = 0.05;
    avg_volume = 1000.0;
    volume_variance = 0.5;
    momentum_factor = 0.1;
    mean_reversion = 0.05;
  };
  {
    symbol = "GOOGL";
    base_price = 2800.0;
    volatility = 0.03;
    min_spread = 0.10;
    max_spread = 1.0;
    avg_volume = 500.0;
    volume_variance = 0.6;
    momentum_factor = 0.15;
    mean_reversion = 0.03;
  };
  {
    symbol = "MSFT";
    base_price = 300.0;
    volatility = 0.022;
    min_spread = 0.01;
    max_spread = 0.10;
    avg_volume = 800.0;
    volume_variance = 0.4;
    momentum_factor = 0.08;
    mean_reversion = 0.06;
  };
  {
    symbol = "TSLA";
    base_price = 800.0;
    volatility = 0.045;
    min_spread = 0.05;
    max_spread = 0.50;
    avg_volume = 1500.0;
    volume_variance = 0.8;
    momentum_factor = 0.25;
    mean_reversion = 0.02;
  };
  {
    symbol = "AMZN";
    base_price = 3200.0;
    volatility = 0.028;
    min_spread = 0.10;
    max_spread = 1.50;
    avg_volume = 600.0;
    volume_variance = 0.5;
    momentum_factor = 0.12;
    mean_reversion = 0.04;
  };
]

(* Note: Browser-side market data generation removed as per architecture cleanup *)
(* All market data now comes from server via WebSocket *)