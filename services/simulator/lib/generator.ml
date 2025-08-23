open Base

type price_model = 
  | RandomWalk of { volatility: float; drift: float }
  | MeanReverting of { mean: float; reversion_speed: float; volatility: float }
  | Jump of { normal_vol: float; jump_prob: float; jump_size: float }
  [@@deriving show]

type instrument_config = {
  symbol: string;
  initial_price: float;
  model: price_model;
  tick_size: float;
  lot_size: float;
} [@@deriving show]

let generate_normal_random () =
  (* Box-Muller transform for normal distribution *)
  let u1 = Random.float 1.0 in
  let u2 = Random.float 1.0 in
  Float.sqrt (-2.0 *. Float.log u1) *. Float.cos (2.0 *. Float.pi *. u2)

let apply_tick_size price tick_size =
  Float.round (price /. tick_size) *. tick_size

let generate_next_price current_price model ~dt =
  match model with
  | RandomWalk { volatility; drift } ->
    let random_shock = generate_normal_random () *. volatility *. Float.sqrt dt in
    let drift_component = drift *. dt in
    current_price *. (1.0 +. drift_component +. random_shock)
  
  | MeanReverting { mean; reversion_speed; volatility } ->
    let mean_reversion = reversion_speed *. (mean -. current_price) *. dt in
    let random_shock = volatility *. Float.sqrt dt *. generate_normal_random () in
    current_price +. mean_reversion +. random_shock
  
  | Jump { normal_vol; jump_prob; jump_size } ->
    let normal_return = normal_vol *. Float.sqrt dt *. generate_normal_random () in
    let jump = if Float.(Random.float 1.0 < jump_prob) then
      jump_size *. (if Random.bool () then 1.0 else -1.0)
    else 0.0 in
    current_price *. (1.0 +. normal_return +. jump)

let generate_bid_ask ~mid_price ~spread_bps =
  let half_spread = mid_price *. spread_bps /. 20000.0 in
  let bid = mid_price -. half_spread in
  let ask = mid_price +. half_spread in
  (bid, ask)

let generate_quote instrument ~current_price ~timestamp =
  let spread_bps = 5.0 +. Random.float 10.0 in  (* 5-15 bps spread *)
  let (bid, ask) = generate_bid_ask ~mid_price:current_price ~spread_bps in
  let bid_size = Float.of_int (100 + Random.int 900) *. instrument.lot_size in
  let ask_size = Float.of_int (100 + Random.int 900) *. instrument.lot_size in
  
  `Assoc [
    ("instrument_id", `String instrument.symbol);
    ("bid", `Float (apply_tick_size bid instrument.tick_size));
    ("bid_size", `Float bid_size);
    ("ask", `Float (apply_tick_size ask instrument.tick_size));
    ("ask_size", `Float ask_size);
    ("mid", `Float current_price);
    ("timestamp", `Float timestamp);
  ]

let generate_trade instrument ~current_price ~timestamp =
  let trade_price = current_price *. (1.0 +. (Random.float 0.002 -. 0.001)) in
  let quantity = Float.of_int (100 + Random.int 400) *. instrument.lot_size in
  let side = if Random.bool () then "buy" else "sell" in
  
  `Assoc [
    ("instrument_id", `String instrument.symbol);
    ("price", `Float (apply_tick_size trade_price instrument.tick_size));
    ("quantity", `Float quantity);
    ("side", `String side);
    ("timestamp", `Float timestamp);
  ]