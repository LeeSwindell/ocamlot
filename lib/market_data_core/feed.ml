open Base
open Ocamlot_core_types

(* Market Data Feed Types *)

type market_data = {
  instrument_id: string;
  bid: float;
  ask: float;
  last_price: float;
  volume: float;
  timestamp: Types.timestamp;
} [@@deriving show, yojson]

type tick_data = {
  instrument_id: string;
  price: float;
  quantity: float;
  side: [`Buy | `Sell];
  timestamp: Types.timestamp;
} [@@deriving show, yojson]

type depth_level = {
  price: float;
  quantity: float;
  order_count: int;
} [@@deriving show, yojson]

type market_depth = {
  instrument_id: string;
  bids: depth_level list;
  asks: depth_level list;
  timestamp: Types.timestamp;
} [@@deriving show, yojson]

(* Price Generation Models *)

type price_model = 
  | RandomWalk of { volatility: float; drift: float }
  | MeanReverting of { mean: float; reversion_speed: float; volatility: float }
  | JumpDiffusion of { volatility: float; jump_intensity: float; jump_size: float }
  [@@deriving show]

type instrument_profile = {
  symbol: string;
  initial_price: float;
  model: price_model;
  spread_bps: float;
  min_quantity: float;
  max_quantity: float;
  tick_size: float;
} [@@deriving show]

(* Global market state *)
let market_state : (string, float * float list) Hashtbl.t = Hashtbl.create (module String)
let random_state = Random.State.make [| 42 |]

(* Price Generation Functions *)

let generate_normal_random state =
  (* Box-Muller transform *)
  let u1 = Random.State.float state 1.0 in
  let u2 = Random.State.float state 1.0 in
  Float.sqrt (-2.0 *. Float.log u1) *. Float.cos (2.0 *. Float.pi *. u2)

let generate_next_price current_price model ~dt =
  match model with
  | RandomWalk { volatility; drift } ->
    let dw = generate_normal_random random_state *. Float.sqrt dt in
    current_price *. Float.exp ((drift -. 0.5 *. volatility *. volatility) *. dt +. volatility *. dw)
  
  | MeanReverting { mean; reversion_speed; volatility } ->
    let dw = generate_normal_random random_state *. Float.sqrt dt in
    current_price +. reversion_speed *. (mean -. current_price) *. dt +. volatility *. dw
  
  | JumpDiffusion { volatility; jump_intensity; jump_size } ->
    let dw = generate_normal_random random_state *. Float.sqrt dt in
    let diffusion = current_price *. volatility *. dw in
    let jump = if Float.(Random.State.float random_state 1.0 < jump_intensity *. dt) then
      current_price *. jump_size *. generate_normal_random random_state
    else 0.0 in
    Float.max 0.01 (current_price +. diffusion +. jump)

let round_to_tick_size price tick_size =
  Float.round (price /. tick_size) *. tick_size

(* Market Data Generation *)

let generate_bid_ask_spread ~mid_price ~spread_bps =
  let half_spread = mid_price *. spread_bps /. 20000.0 in
  let bid = mid_price -. half_spread in
  let ask = mid_price +. half_spread in
  (bid, ask)

let generate_volume profile =
  let base_volume = profile.min_quantity +. 
    Random.State.float random_state (profile.max_quantity -. profile.min_quantity) in
  Float.round base_volume

let generate_market_tick profile =
  let current_price, price_history = 
    Hashtbl.find market_state profile.symbol
    |> Option.value ~default:(profile.initial_price, [profile.initial_price])
  in
  
  let new_price = generate_next_price current_price profile.model ~dt:1.0 in
  let rounded_price = round_to_tick_size new_price profile.tick_size in
  let new_history = rounded_price :: (List.take price_history 100) in
  
  Hashtbl.set market_state ~key:profile.symbol ~data:(rounded_price, new_history);
  
  let bid, ask = generate_bid_ask_spread ~mid_price:rounded_price ~spread_bps:profile.spread_bps in
  let volume = generate_volume profile in
  
  {
    instrument_id = profile.symbol;
    bid = round_to_tick_size bid profile.tick_size;
    ask = round_to_tick_size ask profile.tick_size;
    last_price = rounded_price;
    volume;
    timestamp = Float.of_int (Int.of_float (Unix.gettimeofday ()));
  }

let generate_tick_data profile ~side =
  let current_price, _ = 
    Hashtbl.find market_state profile.symbol
    |> Option.value ~default:(profile.initial_price, [profile.initial_price])
  in
  
  let bid, ask = generate_bid_ask_spread ~mid_price:current_price ~spread_bps:profile.spread_bps in
  let price = match side with
    | `Buy -> ask
    | `Sell -> bid
  in
  let quantity = generate_volume profile in
  
  {
    instrument_id = profile.symbol;
    price = round_to_tick_size price profile.tick_size;
    quantity;
    side;
    timestamp = Float.of_int (Int.of_float (Unix.gettimeofday ()));
  }

let generate_market_depth profile ~levels =
  let current_price, _ = 
    Hashtbl.find market_state profile.symbol
    |> Option.value ~default:(profile.initial_price, [profile.initial_price])
  in
  
  let generate_depth_side ~is_bid ~base_price ~levels =
    List.range 1 (levels + 1)
    |> List.map ~f:(fun level ->
      let price_offset = Float.of_int level *. profile.tick_size in
      let price = if is_bid then base_price -. price_offset else base_price +. price_offset in
      let quantity = generate_volume profile *. (1.0 /. Float.of_int level) in
      let order_count = 1 + Random.State.int random_state (level * 3) in
      {
        price = round_to_tick_size price profile.tick_size;
        quantity;
        order_count;
      }
    )
  in
  
  let bid, ask = generate_bid_ask_spread ~mid_price:current_price ~spread_bps:profile.spread_bps in
  let bids = generate_depth_side ~is_bid:true ~base_price:bid ~levels in
  let asks = generate_depth_side ~is_bid:false ~base_price:ask ~levels in
  
  {
    instrument_id = profile.symbol;
    bids;
    asks;
    timestamp = Float.of_int (Int.of_float (Unix.gettimeofday ()));
  }

(* Default Instrument Profiles *)

let default_profiles = [
  {
    symbol = "AAPL";
    initial_price = 150.0;
    model = RandomWalk { volatility = 0.25; drift = 0.08 };
    spread_bps = 5.0;
    min_quantity = 100.0;
    max_quantity = 1000.0;
    tick_size = 0.01;
  };
  {
    symbol = "GOOGL";
    initial_price = 2800.0;
    model = MeanReverting { mean = 2800.0; reversion_speed = 0.1; volatility = 45.0 };
    spread_bps = 8.0;
    min_quantity = 10.0;
    max_quantity = 100.0;
    tick_size = 0.01;
  };
  {
    symbol = "MSFT";
    initial_price = 300.0;
    model = RandomWalk { volatility = 0.22; drift = 0.06 };
    spread_bps = 4.0;
    min_quantity = 100.0;
    max_quantity = 1000.0;
    tick_size = 0.01;
  };
  {
    symbol = "TSLA";
    initial_price = 800.0;
    model = JumpDiffusion { volatility = 0.45; jump_intensity = 0.02; jump_size = 0.05 };
    spread_bps = 15.0;
    min_quantity = 50.0;
    max_quantity = 500.0;
    tick_size = 0.01;
  };
  {
    symbol = "NVDA";
    initial_price = 400.0;
    model = RandomWalk { volatility = 0.35; drift = 0.12 };
    spread_bps = 12.0;
    min_quantity = 50.0;
    max_quantity = 500.0;
    tick_size = 0.01;
  };
]

(* Feed Management Functions *)

let set_random_seed seed =
  Random.State.int random_state seed |> ignore

let initialize_market_state profiles =
  List.iter profiles ~f:(fun profile ->
    Hashtbl.set market_state ~key:profile.symbol 
      ~data:(profile.initial_price, [profile.initial_price])
  )

let reset_market_state profiles =
  Hashtbl.clear market_state;
  initialize_market_state profiles

let get_current_price symbol =
  Hashtbl.find market_state symbol
  |> Option.map ~f:fst

let get_price_history symbol ~count =
  Hashtbl.find market_state symbol
  |> Option.map ~f:snd
  |> Option.map ~f:(fun list -> List.take list count)
  |> Option.value ~default:[]

let generate_market_snapshot profiles =
  List.map profiles ~f:generate_market_tick

let generate_tick_stream profiles ~count =
  List.range 0 count
  |> List.concat_map ~f:(fun _ ->
    List.map profiles ~f:(fun profile ->
      let side = if Random.State.bool random_state then `Buy else `Sell in
      generate_tick_data profile ~side
    )
  )

let generate_depth_snapshot profiles ~levels =
  List.map profiles ~f:(fun profile ->
    generate_market_depth profile ~levels
  )

(* Market Data Replay *)

type replay_event = 
  | MarketTick of market_data
  | TickTrade of tick_data  
  | DepthUpdate of market_depth
  [@@deriving show]

let create_replay_data profiles ~duration_seconds ~tick_frequency =
  let tick_interval = 1.0 /. tick_frequency in
  let total_ticks = Int.of_float (Float.of_int duration_seconds *. tick_frequency) in
  
  List.range 0 total_ticks
  |> List.concat_map ~f:(fun tick ->
    let timestamp = Float.of_int (Int.of_float (Unix.gettimeofday ())) +. (Float.of_int tick *. tick_interval) in
    profiles
    |> List.concat_map ~f:(fun profile ->
      let market_tick = generate_market_tick profile in
      let events = [MarketTick { market_tick with timestamp }] in
      
      (* Add occasional tick trades *)
      if Float.(Random.State.float random_state 1.0 < 0.3) then
        let side = if Random.State.bool random_state then `Buy else `Sell in
        let tick_trade = generate_tick_data profile ~side in
        TickTrade { tick_trade with timestamp } :: events
      else events
    )
  )

(* Feed Statistics *)

type feed_stats = {
  total_ticks: int;
  symbols: string list;
  price_ranges: (string * float * float) list;
  average_volume: float;
  uptime_seconds: float;
} [@@deriving show, yojson]

let calculate_feed_stats profiles ~runtime_seconds =
  let price_ranges = List.map profiles ~f:(fun profile ->
    let _, history = Hashtbl.find_exn market_state profile.symbol in
    let min_price = List.min_elt history ~compare:Float.compare |> Option.value ~default:0.0 in
    let max_price = List.max_elt history ~compare:Float.compare |> Option.value ~default:0.0 in
    (profile.symbol, min_price, max_price)
  ) in
  
  let total_volume = List.fold profiles ~init:0.0 ~f:(fun acc profile ->
    acc +. (profile.min_quantity +. profile.max_quantity) /. 2.0
  ) in
  let average_volume = total_volume /. Float.of_int (List.length profiles) in
  
  {
    total_ticks = Hashtbl.length market_state;
    symbols = List.map profiles ~f:(fun p -> p.symbol);
    price_ranges;
    average_volume;
    uptime_seconds = runtime_seconds;
  }