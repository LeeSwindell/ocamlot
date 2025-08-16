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

(* Synthetic data generation state *)
let market_state = ref {
  current_prices = [];
  price_directions = [];
  volumes = [];
  last_update = 0.0;
}

let initialize_market_state profiles =
  let current_prices = List.map profiles ~f:(fun p -> (p.symbol, p.base_price)) in
  let price_directions = List.map profiles ~f:(fun p -> (p.symbol, 0.0)) in
  let volumes = List.map profiles ~f:(fun p -> (p.symbol, p.avg_volume)) in
  market_state := {
    current_prices;
    price_directions;
    volumes;
    last_update = Unix.time ();
  }

(* Utility functions *)
let find_profile profiles symbol =
  List.find profiles ~f:(fun p -> String.equal p.symbol symbol)

let get_current_price symbol =
  List.Assoc.find !market_state.current_prices symbol ~equal:String.equal

let get_price_direction symbol =
  List.Assoc.find !market_state.price_directions symbol ~equal:String.equal |> Option.value ~default:0.0

let update_price symbol new_price =
  market_state := {
    !market_state with
    current_prices = List.Assoc.add !market_state.current_prices symbol new_price ~equal:String.equal;
  }

let update_direction symbol new_direction =
  market_state := {
    !market_state with
    price_directions = List.Assoc.add !market_state.price_directions symbol new_direction ~equal:String.equal;
  }

(* Advanced price generation with momentum and mean reversion *)
let generate_price_movement profile current_time =
  let current_price = get_current_price profile.symbol |> Option.value ~default:profile.base_price in
  let current_direction = get_price_direction profile.symbol in
  
  (* Time-based volatility scaling *)
  let time_factor = 
    if profile.symbol |> String.is_prefix ~prefix:"crypto" then 1.0
    else 
      (* Simulate market hours effect - higher volatility during market hours *)
      let hour = (current_time |> Unix.gmtime).tm_hour in
      if hour >= 9 && hour <= 16 then 1.2 else 0.6
  in
  
  (* Random component *)
  let random_shock = (Random.float 2.0) -. 1.0 in (* -1 to 1 *)
  
  (* Momentum component *)
  let momentum_contribution = current_direction *. profile.momentum_factor in
  
  (* Mean reversion component *)
  let distance_from_base = (current_price -. profile.base_price) /. profile.base_price in
  let mean_reversion_contribution = -.distance_from_base *. profile.mean_reversion in
  
  (* Combine all factors *)
  let total_direction = random_shock +. momentum_contribution +. mean_reversion_contribution in
  let price_change = total_direction *. profile.volatility *. time_factor *. current_price in
  
  (* Update direction with decay *)
  let new_direction = total_direction *. 0.7 +. current_direction *. 0.3 in
  update_direction profile.symbol (Float.max (-1.0) (Float.min 1.0 new_direction));
  
  (* Apply price change *)
  let new_price = Float.max (current_price *. 0.5) (current_price +. price_change) in
  update_price profile.symbol new_price;
  new_price

(* Generate realistic bid-ask spread *)
let generate_spread profile _current_price =
  let base_spread = profile.min_spread +. (Random.float (profile.max_spread -. profile.min_spread)) in
  let volatility_factor = 1.0 +. (Float.abs (get_price_direction profile.symbol) *. 2.0) in
  base_spread *. volatility_factor

(* Generate volume with clustering *)
let generate_volume profile =
  let base_volume = profile.avg_volume in
  let random_factor = 0.5 +. (Random.float profile.volume_variance) in
  let direction_factor = 1.0 +. (Float.abs (get_price_direction profile.symbol) *. 0.5) in
  base_volume *. random_factor *. direction_factor

(* Browser-compatible synchronous generation *)
let generate_market_tick profile =
  let current_time = Unix.time () in
  let last_price = generate_price_movement profile current_time in
  let spread = generate_spread profile last_price in
  let half_spread = spread /. 2.0 in
  let volume = generate_volume profile in
  
  create_market_data
    ~instrument_id:profile.symbol
    ~bid:(last_price -. half_spread)
    ~ask:(last_price +. half_spread)
    ~last_price
    ~volume
    ~timestamp:current_time

(* Generate multiple ticks at once *)
let generate_market_snapshot profiles =
  List.map profiles ~f:generate_market_tick

(* Initialize random seed for deterministic testing *)
let set_random_seed seed =
  Random.init seed

(* Conversion functions for server compatibility *)
let market_data_to_web (data : Ocamlot_market_data.Feed.market_data) : web_market_data =
  {
    instrument_id = data.instrument_id;
    bid = data.bid;
    ask = data.ask;
    last_price = data.last_price;
    volume = data.volume;
    timestamp = data.timestamp;
  }

let web_to_market_data (data : web_market_data) : Ocamlot_market_data.Feed.market_data =
  {
    instrument_id = data.instrument_id;
    bid = data.bid;
    ask = data.ask;
    last_price = data.last_price;
    volume = data.volume;
    timestamp = data.timestamp;
  }

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