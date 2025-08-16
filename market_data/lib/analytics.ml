open Base
open Ocamlot_common.Types
open Generator
open Conflation

(* Technical indicator types *)
type moving_average_type = 
  | Simple
  | Exponential  
  | Weighted
  | Hull
  [@@deriving show]

type bollinger_bands = {
  upper: decimal;
  middle: decimal;  (* SMA *)
  lower: decimal;
  bandwidth: decimal;
  percent_b: decimal;
} [@@deriving show, yojson]

type rsi_data = {
  value: decimal;
  oversold: bool;      (* < 30 *)
  overbought: bool;    (* > 70 *)
  divergence: bool;    (* Price vs RSI divergence *)
} [@@deriving show, yojson]

type macd_data = {
  macd_line: decimal;
  signal_line: decimal;
  histogram: decimal;
  bullish_crossover: bool;
  bearish_crossover: bool;
} [@@deriving show, yojson]

type stochastic_data = {
  k_percent: decimal;
  d_percent: decimal;
  oversold: bool;
  overbought: bool;
} [@@deriving show, yojson]

type volume_profile = {
  price_levels: (decimal * decimal) list;  (* (price, volume) pairs *)
  poc: decimal;                            (* Point of Control - max volume price *)
  value_area_high: decimal;
  value_area_low: decimal;
  value_area_volume_percent: decimal;
} [@@deriving show, yojson]

(* Market microstructure metrics *)
type microstructure_metrics = {
  effective_spread: decimal;
  realized_spread: decimal;
  price_impact: decimal;
  order_flow_imbalance: decimal;
  trade_direction: [`Buy | `Sell | `Unknown];
  tick_rule: [`Up | `Down | `Zero_up | `Zero_down];
} [@@deriving show, yojson]

(* Statistical metrics *)
type statistical_metrics = {
  volatility_1m: decimal;
  volatility_5m: decimal; 
  volatility_1h: decimal;
  skewness: decimal;
  kurtosis: decimal;
  hurst_exponent: decimal;    (* Measure of long-term memory *)
  variance_ratio: decimal;    (* Random walk test *)
} [@@deriving show, yojson]

(* Aggregated analytics for a symbol *)
type symbol_analytics = {
  instrument_id: string;
  timestamp: timestamp;
  
  (* Price-based indicators *)
  sma_10: decimal option;
  sma_20: decimal option;  
  sma_50: decimal option;
  ema_10: decimal option;
  ema_20: decimal option;
  
  (* Volatility indicators *)
  bollinger_20_2: bollinger_bands option;
  atr_14: decimal option;       (* Average True Range *)
  
  (* Momentum indicators *)
  rsi_14: rsi_data option;
  macd_12_26_9: macd_data option;
  stochastic_14_3_3: stochastic_data option;
  
  (* Volume indicators *)
  volume_sma_20: decimal option;
  volume_ratio: decimal option; (* Current volume / average volume *)
  vwap_daily: decimal option;
  
  (* Microstructure *)
  microstructure: microstructure_metrics option;
  
  (* Statistical measures *)
  statistics: statistical_metrics option;
  
  (* Market regime indicators *)
  trend_strength: decimal option;  (* 0-1 scale *)
  market_phase: [`Accumulation | `Markup | `Distribution | `Markdown] option;
} [@@deriving show, yojson]

(* Circular buffer for efficient windowed calculations *)
module CircularBuffer = struct
  type 'a t = {
    data: 'a array;
    capacity: int;
    mutable size: int;
    mutable head: int;
  }
  
  let create capacity default_value =
    {
      data = Array.create ~len:capacity default_value;
      capacity;
      size = 0;
      head = 0;
    }
  
  let push buffer value =
    buffer.data.(buffer.head) <- value;
    buffer.head <- (buffer.head + 1) % buffer.capacity;
    if buffer.size < buffer.capacity then
      buffer.size <- buffer.size + 1
  
  let get buffer index =
    if index >= buffer.size then None
    else
      let actual_index = (buffer.head - 1 - index + buffer.capacity) % buffer.capacity in
      Some buffer.data.(actual_index)
  
  let to_list buffer =
    let result = ref [] in
    for i = 0 to buffer.size - 1 do
      match get buffer i with
      | Some value -> result := value :: !result
      | None -> ()
    done;
    !result
  
  let size buffer = buffer.size
  let is_full buffer = buffer.size = buffer.capacity
end

(* State for maintaining rolling calculations *)
type analytics_state = {
  instrument_id: string;
  
  (* Price history buffers *)
  prices: decimal CircularBuffer.t;
  volumes: decimal CircularBuffer.t;
  high_prices: decimal CircularBuffer.t;
  low_prices: decimal CircularBuffer.t;
  
  (* Intermediate calculations *)
  mutable ema_10: decimal option;
  mutable ema_20: decimal option;
  mutable rsi_gains: decimal CircularBuffer.t;
  mutable rsi_losses: decimal CircularBuffer.t;
  mutable macd_ema_12: decimal option;
  mutable macd_ema_26: decimal option;
  mutable macd_signal_ema: decimal option;
  
  (* Previous values for change detection *)
  mutable prev_price: decimal option;
  mutable prev_macd: decimal option;
  mutable prev_signal: decimal option;
  
  (* Statistics tracking *)
  mutable price_sum: decimal;
  mutable price_sum_squares: decimal;
  mutable returns: decimal CircularBuffer.t;
}

(* Create analytics state *)
let create_analytics_state instrument_id =
  {
    instrument_id;
    prices = CircularBuffer.create 200 0.0;
    volumes = CircularBuffer.create 200 0.0;
    high_prices = CircularBuffer.create 200 0.0;
    low_prices = CircularBuffer.create 200 0.0;
    ema_10 = None;
    ema_20 = None;
    rsi_gains = CircularBuffer.create 14 0.0;
    rsi_losses = CircularBuffer.create 14 0.0;
    macd_ema_12 = None;
    macd_ema_26 = None;
    macd_signal_ema = None;
    prev_price = None;
    prev_macd = None;
    prev_signal = None;
    price_sum = 0.0;
    price_sum_squares = 0.0;
    returns = CircularBuffer.create 100 0.0;
  }

(* Calculate Simple Moving Average *)
let calculate_sma buffer period =
  if CircularBuffer.size buffer < period then None
  else
    let sum = ref 0.0 in
    for i = 0 to period - 1 do
      match CircularBuffer.get buffer i with
      | Some value -> sum := !sum +. value
      | None -> ()
    done;
    Some (!sum /. Float.of_int period)

(* Calculate Exponential Moving Average *)
let calculate_ema state current_price period =
  let alpha = 2.0 /. (Float.of_int period +. 1.0) in
  match (period, state.ema_10, state.ema_20) with
  | (10, None, _) -> 
    state.ema_10 <- Some current_price;
    Some current_price
  | (20, _, None) ->
    state.ema_20 <- Some current_price;
    Some current_price
  | (10, Some prev_ema, _) ->
    let new_ema = alpha *. current_price +. (1.0 -. alpha) *. prev_ema in
    state.ema_10 <- Some new_ema;
    Some new_ema
  | (20, _, Some prev_ema) ->
    let new_ema = alpha *. current_price +. (1.0 -. alpha) *. prev_ema in
    state.ema_20 <- Some new_ema;
    Some new_ema
  | _ -> None

(* Calculate Bollinger Bands *)
let calculate_bollinger_bands prices period std_dev =
  match calculate_sma prices period with
  | None -> None
  | Some sma ->
    let sum_squares = ref 0.0 in
    for i = 0 to period - 1 do
      match CircularBuffer.get prices i with
      | Some price ->
        let diff = price -. sma in
        sum_squares := !sum_squares +. (diff *. diff)
      | None -> ()
    done;
    let variance = !sum_squares /. Float.of_int period in
    let std = Float.sqrt variance in
    let upper = sma +. (std_dev *. std) in
    let lower = sma -. (std_dev *. std) in
    let bandwidth = (upper -. lower) /. sma in
    let current_price = Option.value_exn (CircularBuffer.get prices 0) in
    let percent_b = (current_price -. lower) /. (upper -. lower) in
    Some {
      upper; middle = sma; lower; bandwidth; percent_b
    }

(* Calculate RSI *)
let calculate_rsi state current_price =
  match state.prev_price with
  | None -> 
    state.prev_price <- Some current_price;
    None
  | Some prev_price ->
    let change = current_price -. prev_price in
    let gain = if Float.( > ) change 0.0 then change else 0.0 in
    let loss = if Float.( < ) change 0.0 then Float.abs change else 0.0 in
    
    CircularBuffer.push state.rsi_gains gain;
    CircularBuffer.push state.rsi_losses loss;
    state.prev_price <- Some current_price;
    
    if CircularBuffer.size state.rsi_gains >= 14 then
      let avg_gain = Option.value_exn (calculate_sma state.rsi_gains 14) in
      let avg_loss = Option.value_exn (calculate_sma state.rsi_losses 14) in
      if Float.equal avg_loss 0.0 then Some { value = 100.0; oversold = false; overbought = true; divergence = false }
      else
        let rs = avg_gain /. avg_loss in
        let rsi = 100.0 -. (100.0 /. (1.0 +. rs)) in
        Some {
          value = rsi;
          oversold = Float.( < ) rsi 30.0;
          overbought = Float.( > ) rsi 70.0;
          divergence = false; (* TODO: implement divergence detection *)
        }
    else None

(* Calculate MACD *)
let calculate_macd state current_price =
  let ema_12 = calculate_ema state current_price 12 in
  let ema_26 = calculate_ema state current_price 26 in
  
  match (ema_12, ema_26) with
  | (Some e12, Some e26) ->
    let macd_line = e12 -. e26 in
    
    (* Calculate signal line (EMA of MACD) *)
    let signal_alpha = 2.0 /. (9.0 +. 1.0) in
    let signal_line = match state.macd_signal_ema with
      | None -> 
        state.macd_signal_ema <- Some macd_line;
        macd_line
      | Some prev_signal ->
        let new_signal = signal_alpha *. macd_line +. (1.0 -. signal_alpha) *. prev_signal in
        state.macd_signal_ema <- Some new_signal;
        new_signal
    in
    
    let histogram = macd_line -. signal_line in
    
    (* Detect crossovers *)
    let (bullish_crossover, bearish_crossover) = match (state.prev_macd, state.prev_signal) with
      | (Some prev_macd, Some prev_signal) ->
        let prev_histogram = prev_macd -. prev_signal in
        let bullish = Float.( <= ) prev_histogram 0.0 && Float.( > ) histogram 0.0 in
        let bearish = Float.( >= ) prev_histogram 0.0 && Float.( < ) histogram 0.0 in
        (bullish, bearish)
      | _ -> (false, false)
    in
    
    state.prev_macd <- Some macd_line;
    state.prev_signal <- Some signal_line;
    
    Some {
      macd_line;
      signal_line;
      histogram;
      bullish_crossover;
      bearish_crossover;
    }
  | _ -> None

(* Calculate Average True Range *)
let calculate_atr high_prices low_prices close_prices period =
  if CircularBuffer.size high_prices < period + 1 then None
  else
    let true_ranges = ref [] in
    for i = 0 to period - 1 do
      match (CircularBuffer.get high_prices i, 
             CircularBuffer.get low_prices i,
             CircularBuffer.get close_prices (i + 1)) with
      | (Some high, Some low, Some prev_close) ->
        let tr1 = high -. low in
        let tr2 = Float.abs (high -. prev_close) in
        let tr3 = Float.abs (low -. prev_close) in
        let true_range = Float.max tr1 (Float.max tr2 tr3) in
        true_ranges := true_range :: !true_ranges
      | _ -> ()
    done;
    
    let sum = List.fold !true_ranges ~init:0.0 ~f:(+.) in
    Some (sum /. Float.of_int period)

(* Calculate microstructure metrics *)
let calculate_microstructure tick prev_tick =
  match prev_tick with
  | None -> None
  | Some prev ->
    let spread = tick.ask -. tick.bid in
    let mid_price = (tick.ask +. tick.bid) /. 2.0 in
    let prev_mid = (prev.ask +. prev.bid) /. 2.0 in
    
    (* Tick rule for trade direction *)
    let tick_rule = 
      if Float.( > ) tick.last_price prev.last_price then `Up
      else if Float.( < ) tick.last_price prev.last_price then `Down  
      else if Float.( > ) prev.last_price prev_mid then `Zero_up
      else `Zero_down
    in
    
    let trade_direction = match tick_rule with
      | `Up | `Zero_up -> `Buy
      | `Down | `Zero_down -> `Sell
    in
    
    Some {
      effective_spread = spread /. mid_price;
      realized_spread = 0.0; (* Requires future price data *)
      price_impact = Float.abs (tick.last_price -. mid_price) /. mid_price;
      order_flow_imbalance = 0.0; (* Requires order book data *)
      trade_direction;
      tick_rule;
    }

(* Update analytics state with new bar *)
let update_analytics_state state (bar : ohlcv_bar) =
  CircularBuffer.push state.prices bar.close_price;
  CircularBuffer.push state.volumes bar.volume;
  CircularBuffer.push state.high_prices bar.high_price;
  CircularBuffer.push state.low_prices bar.low_price;
  
  (* Update running statistics *)
  state.price_sum <- state.price_sum +. bar.close_price;
  state.price_sum_squares <- state.price_sum_squares +. (bar.close_price *. bar.close_price);
  
  (* Calculate return *)
  (match CircularBuffer.get state.prices 1 with
   | Some prev_price when not (Float.equal prev_price 0.0) ->
     let return = (bar.close_price -. prev_price) /. prev_price in
     CircularBuffer.push state.returns return
   | _ -> ())

(* Calculate all analytics for a symbol *)
let calculate_symbol_analytics state bar _prev_tick =
  update_analytics_state state bar;
  
  let current_price = bar.close_price in
  
  {
    instrument_id = state.instrument_id;
    timestamp = bar.close_timestamp;
    
    sma_10 = calculate_sma state.prices 10;
    sma_20 = calculate_sma state.prices 20;
    sma_50 = calculate_sma state.prices 50;
    ema_10 = calculate_ema state current_price 10;
    ema_20 = calculate_ema state current_price 20;
    
    bollinger_20_2 = calculate_bollinger_bands state.prices 20 2.0;
    atr_14 = calculate_atr state.high_prices state.low_prices state.prices 14;
    
    rsi_14 = calculate_rsi state current_price;
    macd_12_26_9 = calculate_macd state current_price;
    stochastic_14_3_3 = None; (* TODO: implement stochastic *)
    
    volume_sma_20 = calculate_sma state.volumes 20;
    volume_ratio = (match calculate_sma state.volumes 20 with
      | Some avg_vol when not (Float.equal avg_vol 0.0) -> Some (bar.volume /. avg_vol)
      | _ -> None);
    vwap_daily = Some bar.vwap;
    
    microstructure = None; (* Would need tick data *)
    statistics = None; (* TODO: implement statistical measures *)
    
    trend_strength = None; (* TODO: implement trend strength *)
    market_phase = None; (* TODO: implement market phase detection *)
  }

(* Analytics configuration *)
type analytics_config = {
  instruments: string list;
  calculate_microstructure: bool;
  calculate_statistics: bool;
  lookback_periods: int list;
  update_frequency_ms: int;
} [@@deriving show]

(* Analytics engine state *)
type analytics_engine = {
  states: (string, analytics_state) Map.Poly.t;
  config: analytics_config;
}

(* Create analytics engine *)
let create_analytics_engine config =
  let states = 
    List.fold config.instruments ~init:Map.Poly.empty ~f:(fun acc instrument ->
      Map.Poly.set acc ~key:instrument ~data:(create_analytics_state instrument)
    )
  in
  { states; config }

(* Process bar through analytics *)
let process_bar (engine : analytics_engine) (ohlcv_bar : ohlcv_bar) =
  match Map.Poly.find engine.states ohlcv_bar.instrument_id with
  | None -> (engine, None)
  | Some state ->
    let analytics = calculate_symbol_analytics state ohlcv_bar None in
    (engine, Some analytics)

(* Process batch of bars *)
let process_bars engine bars =
  List.fold_map bars ~init:engine ~f:(fun acc_engine bar ->
    match process_bar acc_engine bar with
    | (new_engine, Some analytics) -> (new_engine, analytics)
    | (new_engine, None) -> (new_engine, {
        instrument_id = bar.instrument_id;
        timestamp = bar.close_timestamp;
        sma_10 = None; sma_20 = None; sma_50 = None;
        ema_10 = None; ema_20 = None;
        bollinger_20_2 = None; atr_14 = None;
        rsi_14 = None; macd_12_26_9 = None; stochastic_14_3_3 = None;
        volume_sma_20 = None; volume_ratio = None; vwap_daily = None;
        microstructure = None; statistics = None;
        trend_strength = None; market_phase = None;
      })
  )

(* Default configuration *)
let default_config = {
  instruments = ["AAPL"; "GOOGL"; "TSLA"];
  calculate_microstructure = true;
  calculate_statistics = true;
  lookback_periods = [10; 20; 50; 100; 200];
  update_frequency_ms = 1000;
}