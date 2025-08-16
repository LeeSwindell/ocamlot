open Base
open Lwt.Syntax
open Ocamlot_common

type market_data = {
  instrument_id: Types.instrument_id;
  bid: Types.decimal;
  ask: Types.decimal;
  last_price: Types.decimal;
  volume: Types.decimal;
  timestamp: Types.timestamp;
} [@@deriving show]

type feed_config = {
  instruments: Types.instrument_id list;
  update_interval_ms: int;
} [@@deriving show]

(* Enhanced synthetic data generation types *)
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

type synthetic_config = {
  profiles: instrument_profile list;
  random_seed: int option;
  market_hours: bool;       (* Simulate market hours effects *)
  correlation_matrix: (string * string * float) list; (* Inter-symbol correlations *)
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
  
  {
    instrument_id = profile.symbol;
    bid = last_price -. half_spread;
    ask = last_price +. half_spread;
    last_price;
    volume;
    timestamp = current_time;
  }

(* Generate multiple ticks at once *)
let generate_market_snapshot profiles =
  List.map profiles ~f:generate_market_tick

(* Initialize random seed for deterministic testing *)
let set_random_seed seed =
  Random.init seed

(* Original async feed interface (now using enhanced generation) *)
let create_feed_config ~instruments ~update_interval_ms =
  { instruments; update_interval_ms }

let start_feed ~config ~callback =
  (* Initialize with default profiles for requested instruments *)
  let profiles = List.filter_map config.instruments ~f:(fun symbol ->
    find_profile default_profiles symbol
  ) in
  initialize_market_state profiles;
  
  let rec loop () =
    let* () = Lwt_unix.sleep (Float.of_int config.update_interval_ms /. 1000.0) in
    List.iter profiles ~f:(fun profile ->
      let market_tick = generate_market_tick profile in
      callback market_tick
    );
    loop ()
  in
  loop ()