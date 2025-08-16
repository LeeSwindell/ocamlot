open Base
open Ocamlot_common.Types

(* Core market data types *)
type tick = {
  instrument_id: instrument_id;
  bid: decimal;
  ask: decimal;
  last_price: decimal;
  volume: decimal;
  timestamp: timestamp;
  sequence: int64;
} [@@deriving show, yojson]

(* Market regime types *)
type market_regime = 
  | Trending of { direction: [`Up | `Down]; strength: float }
  | Volatile of { intensity: float }
  | Quiet of { consolidation_level: float }
  | EventDriven of { impact: float; duration_ms: int }
  [@@deriving show]

type market_session = 
  | PreMarket 
  | RegularHours 
  | PostMarket 
  | Closed
  [@@deriving show]

(* Enhanced instrument profile *)
type instrument_profile = {
  symbol: string;
  base_price: decimal;
  
  (* Volatility parameters *)
  daily_volatility: float;      (* Annual volatility as decimal *)
  intraday_volatility: float;   (* Additional intraday component *)
  
  (* Spread parameters *)
  min_spread_bps: float;        (* Minimum spread in basis points *)
  max_spread_bps: float;        (* Maximum spread in basis points *)
  spread_volatility: float;     (* How much spread varies *)
  
  (* Volume parameters *)
  avg_volume_per_minute: decimal;
  volume_volatility: float;     (* Volume variance factor *)
  volume_clustering: float;     (* Tendency for volume to cluster *)
  
  (* Price dynamics *)
  momentum_factor: float;       (* Tendency to continue direction *)
  mean_reversion_factor: float; (* Tendency to return to base *)
  jump_frequency: float;        (* Probability of price jumps per hour *)
  jump_magnitude: float;        (* Average jump size as % of price *)
  
  (* Market microstructure *)
  tick_size: decimal;           (* Minimum price increment *)
  lot_size: decimal;            (* Standard trading unit *)
  
  (* Session behavior *)
  session_effects: (market_session * float) list; (* Volatility multipliers *)
} [@@deriving show]

(* Market state tracking *)
type instrument_state = {
  current_price: decimal;
  current_bid: decimal;
  current_ask: decimal;
  price_direction: float;       (* -1.0 to 1.0 momentum *)
  volatility_regime: float;     (* Current volatility multiplier *)
  volume_state: decimal;        (* Current volume rate *)
  last_jump_time: timestamp;
  sequence_number: int64;
} [@@deriving show]

type market_state = {
  instruments: (string, instrument_state) Map.Poly.t;
  current_regime: market_regime;
  current_session: market_session;
  regime_start_time: timestamp;
  random_state: Random.State.t;
  last_update: timestamp;
}

(* Configuration *)
type generator_config = {
  profiles: instrument_profile list;
  random_seed: int;
  tick_interval_ms: int;
  regime_change_probability: float; (* Probability per minute *)
  correlation_matrix: (string * string * float) list;
  enable_jumps: bool;
  enable_regime_changes: bool;
  market_calendar: (int * int * int) list; (* Trading days as (year, month, day) *)
} [@@deriving show]

(* Default instrument profiles *)
let default_profiles = [
  {
    symbol = "AAPL";
    base_price = 150.0;
    daily_volatility = 0.25;
    intraday_volatility = 0.15;
    min_spread_bps = 1.0;
    max_spread_bps = 5.0;
    spread_volatility = 0.3;
    avg_volume_per_minute = 1000.0;
    volume_volatility = 0.8;
    volume_clustering = 0.3;
    momentum_factor = 0.15;
    mean_reversion_factor = 0.05;
    jump_frequency = 2.0;
    jump_magnitude = 0.01;
    tick_size = 0.01;
    lot_size = 100.0;
    session_effects = [
      (PreMarket, 0.6); 
      (RegularHours, 1.0); 
      (PostMarket, 0.4); 
      (Closed, 0.1)
    ];
  };
  {
    symbol = "GOOGL";
    base_price = 2800.0;
    daily_volatility = 0.28;
    intraday_volatility = 0.18;
    min_spread_bps = 2.0;
    max_spread_bps = 10.0;
    spread_volatility = 0.4;
    avg_volume_per_minute = 500.0;
    volume_volatility = 0.9;
    volume_clustering = 0.4;
    momentum_factor = 0.20;
    mean_reversion_factor = 0.03;
    jump_frequency = 1.5;
    jump_magnitude = 0.015;
    tick_size = 0.01;
    lot_size = 100.0;
    session_effects = [
      (PreMarket, 0.5); 
      (RegularHours, 1.0); 
      (PostMarket, 0.3); 
      (Closed, 0.1)
    ];
  };
  {
    symbol = "TSLA";
    base_price = 800.0;
    daily_volatility = 0.45;
    intraday_volatility = 0.25;
    min_spread_bps = 2.0;
    max_spread_bps = 15.0;
    spread_volatility = 0.6;
    avg_volume_per_minute = 1500.0;
    volume_volatility = 1.2;
    volume_clustering = 0.5;
    momentum_factor = 0.30;
    mean_reversion_factor = 0.02;
    jump_frequency = 4.0;
    jump_magnitude = 0.025;
    tick_size = 0.01;
    lot_size = 100.0;
    session_effects = [
      (PreMarket, 0.8); 
      (RegularHours, 1.0); 
      (PostMarket, 0.6); 
      (Closed, 0.2)
    ];
  };
]

(* Market session detection *)
let get_market_session timestamp =
  let tm = Unix.gmtime timestamp in
  let hour = tm.tm_hour in
  let minute = tm.tm_min in
  let time_minutes = hour * 60 + minute in
  
  (* US Eastern Time market hours (in UTC) *)
  match time_minutes with
  | t when t >= 240 && t < 570 -> PreMarket      (* 4:00 - 9:30 AM ET *)
  | t when t >= 570 && t < 960 -> RegularHours   (* 9:30 AM - 4:00 PM ET *)
  | t when t >= 960 && t < 1200 -> PostMarket    (* 4:00 - 8:00 PM ET *)
  | _ -> Closed

(* Market regime transitions *)
let generate_new_regime current_regime random_state =
  let regime_weights = match current_regime with
    | Trending _ -> [(0.4, Trending { direction = `Up; strength = Random.State.float random_state 1.0 });
                     (0.4, Trending { direction = `Down; strength = Random.State.float random_state 1.0 });
                     (0.3, Volatile { intensity = 0.5 +. Random.State.float random_state 0.5 });
                     (0.2, Quiet { consolidation_level = Random.State.float random_state 1.0 })]
    | Volatile _ -> [(0.3, Trending { direction = `Up; strength = Random.State.float random_state 1.0 });
                     (0.3, Trending { direction = `Down; strength = Random.State.float random_state 1.0 });
                     (0.2, Volatile { intensity = 0.5 +. Random.State.float random_state 0.5 });
                     (0.4, Quiet { consolidation_level = Random.State.float random_state 1.0 })]
    | Quiet _ -> [(0.4, Trending { direction = `Up; strength = Random.State.float random_state 1.0 });
                  (0.4, Trending { direction = `Down; strength = Random.State.float random_state 1.0 });
                  (0.3, Volatile { intensity = 0.5 +. Random.State.float random_state 0.5 });
                  (0.1, Quiet { consolidation_level = Random.State.float random_state 1.0 })]
    | EventDriven _ -> [(0.2, Trending { direction = `Up; strength = Random.State.float random_state 1.0 });
                        (0.2, Trending { direction = `Down; strength = Random.State.float random_state 1.0 });
                        (0.4, Volatile { intensity = 0.7 +. Random.State.float random_state 0.3 });
                        (0.3, Quiet { consolidation_level = Random.State.float random_state 1.0 })]
  in
  
  let total_weight = List.fold regime_weights ~init:0.0 ~f:(fun acc (w, _) -> acc +. w) in
  let r = Random.State.float random_state total_weight in
  
  let rec select_regime cumulative = function
    | [] -> Quiet { consolidation_level = 0.5 }
    | (weight, regime) :: rest ->
        if Float.( <= ) r (cumulative +. weight) then regime
        else select_regime (cumulative +. weight) rest
  in
  select_regime 0.0 regime_weights

(* Price movement generation *)
let generate_price_movement profile state _current_time market_state =
  let dt = 1.0 /. (365.0 *. 24.0 *. 60.0 *. 60.0 *. 1000.0) in (* 1ms in years *)
  
  (* Session effect *)
  let session_multiplier = 
    List.Assoc.find profile.session_effects market_state.current_session ~equal:Poly.equal
    |> Option.value ~default:1.0
  in
  
  (* Regime effect *)
  let regime_volatility_multiplier = match market_state.current_regime with
    | Trending { strength; _ } -> 1.0 +. (strength *. 0.5)
    | Volatile { intensity } -> 1.0 +. intensity
    | Quiet { consolidation_level } -> 0.3 +. (consolidation_level *. 0.4)
    | EventDriven { impact; _ } -> 1.0 +. (Float.abs impact *. 2.0)
  in
  
  (* Directional bias from regime *)
  let regime_direction_bias = match market_state.current_regime with
    | Trending { direction = `Up; strength } -> strength *. 0.1
    | Trending { direction = `Down; strength } -> -.strength *. 0.1
    | EventDriven { impact; _ } -> impact *. 0.05
    | _ -> 0.0
  in
  
  (* Total volatility *)
  let effective_volatility = profile.daily_volatility *. 
                             regime_volatility_multiplier *. 
                             session_multiplier *. 
                             Float.sqrt dt in
  
  (* Random component *)
  let random_shock = Random.State.float_range market_state.random_state (-1.0) 1.0 in
  
  (* Momentum component *)
  let momentum_contribution = state.price_direction *. profile.momentum_factor in
  
  (* Mean reversion component *)
  let distance_from_base = (state.current_price -. profile.base_price) /. profile.base_price in
  let mean_reversion_contribution = -.distance_from_base *. profile.mean_reversion_factor in
  
  (* Jump detection *)
  let should_jump = 
    Float.( < ) (Random.State.float market_state.random_state 1.0) (profile.jump_frequency /. (60.0 *. 60.0 *. 1000.0))
  in
  
  let jump_component = 
    if should_jump then
      let jump_direction = if Random.State.bool market_state.random_state then 1.0 else -1.0 in
      jump_direction *. profile.jump_magnitude *. state.current_price
    else 0.0
  in
  
  (* Combine all factors *)
  let total_direction = random_shock +. 
                       momentum_contribution +. 
                       mean_reversion_contribution +. 
                       regime_direction_bias in
  
  let price_change = (total_direction *. effective_volatility *. state.current_price) +. jump_component in
  
  (* Update direction with decay *)
  let new_direction = total_direction *. 0.7 +. state.price_direction *. 0.3 in
  let clamped_direction = Float.max (-1.0) (Float.min 1.0 new_direction) in
  
  (* Apply price change with minimum price protection *)
  let new_price = Float.max (profile.base_price *. 0.1) (state.current_price +. price_change) in
  
  (* Round to tick size *)
  let rounded_price = Float.round_decimal new_price ~decimal_digits:2 in
  
  (rounded_price, clamped_direction)

(* Spread generation *)
let generate_spread profile state market_state =
  let base_spread_bps = profile.min_spread_bps +. 
                        (Random.State.float market_state.random_state 
                         (profile.max_spread_bps -. profile.min_spread_bps)) in
  
  (* Volatility increases spread *)
  let volatility_multiplier = 1.0 +. (Float.abs state.price_direction *. profile.spread_volatility) in
  
  (* Regime effect on spread *)
  let regime_multiplier = match market_state.current_regime with
    | Volatile { intensity } -> 1.0 +. intensity
    | EventDriven { impact; _ } -> 1.0 +. (Float.abs impact)
    | _ -> 1.0
  in
  
  let total_spread_bps = base_spread_bps *. volatility_multiplier *. regime_multiplier in
  let spread = state.current_price *. (total_spread_bps /. 10000.0) in
  
  (* Round to tick size *)
  Float.round_decimal spread ~decimal_digits:2

(* Volume generation *)
let generate_volume profile state market_state =
  let session_multiplier = 
    List.Assoc.find profile.session_effects market_state.current_session ~equal:Poly.equal
    |> Option.value ~default:1.0
  in
  
  let regime_multiplier = match market_state.current_regime with
    | Trending { strength; _ } -> 1.0 +. (strength *. 0.3)
    | Volatile { intensity } -> 1.0 +. (intensity *. 0.5)
    | EventDriven { impact; _ } -> 1.0 +. (Float.abs impact *. 2.0)
    | Quiet _ -> 0.5
  in
  
  let base_volume = profile.avg_volume_per_minute *. session_multiplier *. regime_multiplier in
  let random_factor = 0.3 +. (Random.State.float market_state.random_state profile.volume_volatility) in
  let direction_factor = 1.0 +. (Float.abs state.price_direction *. profile.volume_clustering) in
  
  let volume = base_volume *. random_factor *. direction_factor in
  Float.round_decimal volume ~decimal_digits:0

(* Initialize market state *)
let create_market_state config =
  let random_state = Random.State.make [| config.random_seed |] in
  let current_time = Unix.time () in
  
  let instruments = 
    List.fold config.profiles ~init:(Map.Poly.empty) ~f:(fun acc profile ->
      let initial_state = {
        current_price = profile.base_price;
        current_bid = profile.base_price -. (profile.base_price *. profile.min_spread_bps /. 20000.0);
        current_ask = profile.base_price +. (profile.base_price *. profile.min_spread_bps /. 20000.0);
        price_direction = 0.0;
        volatility_regime = 1.0;
        volume_state = profile.avg_volume_per_minute;
        last_jump_time = current_time;
        sequence_number = 0L;
      } in
      Map.Poly.set acc ~key:profile.symbol ~data:initial_state
    )
  in
  
  {
    instruments;
    current_regime = Quiet { consolidation_level = 0.5 };
    current_session = get_market_session current_time;
    regime_start_time = current_time;
    random_state;
    last_update = current_time;
  }

(* Generate single tick *)
let generate_tick profile market_state =
  let current_time = Unix.time () in
  
  match Map.Poly.find market_state.instruments profile.symbol with
  | None -> failwith ("Unknown instrument: " ^ profile.symbol)
  | Some state ->
    let (new_price, new_direction) = generate_price_movement profile state current_time market_state in
    let spread = generate_spread profile state market_state in
    let volume = generate_volume profile state market_state in
    
    let half_spread = spread /. 2.0 in
    let bid = new_price -. half_spread in
    let ask = new_price +. half_spread in
    
    let new_state = {
      state with
      current_price = new_price;
      current_bid = bid;
      current_ask = ask;
      price_direction = new_direction;
      sequence_number = Int64.succ state.sequence_number;
    } in
    
    let updated_market_state = {
      market_state with
      instruments = Map.Poly.set market_state.instruments ~key:profile.symbol ~data:new_state;
      current_session = get_market_session current_time;
      last_update = current_time;
    } in
    
    let tick = {
      instrument_id = profile.symbol;
      bid;
      ask;
      last_price = new_price;
      volume;
      timestamp = current_time;
      sequence = new_state.sequence_number;
    } in
    
    (tick, updated_market_state)

(* Generate batch of ticks *)
let generate_batch profiles market_state =
  List.fold_map profiles ~init:market_state ~f:(fun acc_state profile ->
    let (tick, new_state) = generate_tick profile acc_state in
    (new_state, tick)
  )

(* Default configuration *)
let default_config = {
  profiles = default_profiles;
  random_seed = 42;
  tick_interval_ms = 1;
  regime_change_probability = 0.1;  (* 10% chance per minute *)
  correlation_matrix = [];
  enable_jumps = true;
  enable_regime_changes = true;
  market_calendar = [];
}