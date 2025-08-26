open Base
open Ocamlot_core_types

(* Technical Analysis Types *)

type bollinger_bands = {
  upper: float;
  middle: float;
  lower: float;
  bandwidth: float;
  percent_b: float;
} [@@deriving show, yojson]

type rsi = {
  value: float;
  oversold: bool;
  overbought: bool;
  divergence: bool;
} [@@deriving show, yojson]

type macd = {
  macd_line: float;
  signal_line: float;
  histogram: float;
  bullish_crossover: bool;
  bearish_crossover: bool;
} [@@deriving show, yojson]

type stochastic = {
  k_percent: float;
  d_percent: float;
  oversold: bool;
  overbought: bool;
} [@@deriving show, yojson]

type volume_profile = {
  volume_weighted_price: float;
  volume_ratio: float;
  above_average: bool;
} [@@deriving show, yojson]

type microstructure = {
  bid_ask_spread: float;
  effective_spread: float;
  price_impact: float;
  market_depth: float;
} [@@deriving show, yojson]

type market_statistics = {
  volatility_1d: float;
  volatility_1w: float;
  volatility_1m: float;
  beta: float option;
  correlation_spy: float option;
  sharpe_ratio: float option;
} [@@deriving show, yojson]

type market_phase = [
  | `Markup
  | `Markdown  
  | `Sideways
  | `Breakout
  | `Reversal
] [@@deriving show, yojson]

type symbol_analytics = {
  instrument_id: string;
  timestamp: Types.timestamp;
  
  (* Moving Averages *)
  sma_10: float option;
  sma_20: float option;
  sma_50: float option;
  ema_10: float option;
  ema_20: float option;
  
  (* Technical Indicators *)
  bollinger_20_2: bollinger_bands option;
  atr_14: float option;
  rsi_14: rsi option;
  macd_12_26_9: macd option;
  stochastic_14_3_3: stochastic option;
  
  (* Volume Analysis *)
  volume_sma_20: float option;
  volume_ratio: volume_profile option;
  vwap_daily: float option;
  
  (* Market Microstructure *)
  microstructure: microstructure option;
  
  (* Statistical Analysis *)
  statistics: market_statistics option;
  
  (* Market Regime *)
  trend_strength: float option;
  market_phase: market_phase option;
} [@@deriving show, yojson]

(* Moving Average Calculations *)

let calculate_sma prices ~period =
  let len = List.length prices in
  if len < period then None
  else
    let recent = List.take prices period in
    let sum = List.fold recent ~init:0.0 ~f:(+.) in
    Some (sum /. Float.of_int period)

let calculate_ema prices ~period:_ ~alpha =
  match prices with
  | [] -> None
  | current :: rest ->
    let rec ema_calc prev_ema remaining =
      match remaining with
      | [] -> prev_ema
      | price :: tail ->
        let new_ema = alpha *. price +. (1.0 -. alpha) *. prev_ema in
        ema_calc new_ema tail
    in
    Some (ema_calc current rest)

(* RSI Calculation *)

let calculate_rsi prices ~period =
  if List.length prices < period + 1 then None
  else
    let price_changes = List.zip_exn (List.tl_exn prices) prices
      |> List.map ~f:(fun (curr, prev) -> curr -. prev) in
    
    let gains, losses = List.partition_tf price_changes ~f:(fun change -> Float.(change > 0.0)) in
    let avg_gain = List.fold gains ~init:0.0 ~f:(+.) /. Float.of_int (List.length gains) in
    let avg_loss = List.fold losses ~init:0.0 ~f:(fun acc loss -> acc +. Float.abs loss) 
      /. Float.of_int (List.length losses) in
    
    if Float.(avg_loss = 0.0) then Some { value = 100.0; oversold = false; overbought = true; divergence = false }
    else
      let rs = avg_gain /. avg_loss in
      let rsi_value = 100.0 -. (100.0 /. (1.0 +. rs)) in
      Some {
        value = rsi_value;
        oversold = Float.(rsi_value < 30.0);
        overbought = Float.(rsi_value > 70.0);
        divergence = false; (* Would need historical comparison *)
      }

(* Bollinger Bands Calculation *)

let calculate_bollinger_bands prices ~period ~std_dev_multiplier =
  match calculate_sma prices ~period with
  | None -> None
  | Some middle ->
    let recent = List.take prices period in
    let variance = List.fold recent ~init:0.0 ~f:(fun acc price ->
      acc +. ((price -. middle) *. (price -. middle))
    ) /. Float.of_int period in
    let std_dev = Float.sqrt variance in
    let upper = middle +. (std_dev_multiplier *. std_dev) in
    let lower = middle -. (std_dev_multiplier *. std_dev) in
    let bandwidth = (upper -. lower) /. middle *. 100.0 in
    let current_price = List.hd_exn prices in
    let percent_b = (current_price -. lower) /. (upper -. lower) in
    Some {
      upper; middle; lower; bandwidth; percent_b
    }

(* MACD Calculation *)

let calculate_macd prices ~fast_period ~slow_period ~signal_period =
  let alpha_fast = 2.0 /. (Float.of_int fast_period +. 1.0) in
  let alpha_slow = 2.0 /. (Float.of_int slow_period +. 1.0) in
  let alpha_signal = 2.0 /. (Float.of_int signal_period +. 1.0) in
  
  match calculate_ema prices ~period:fast_period ~alpha:alpha_fast,
        calculate_ema prices ~period:slow_period ~alpha:alpha_slow with
  | Some ema_fast, Some ema_slow ->
    let macd_line = ema_fast -. ema_slow in
    (* Simplified signal line calculation *)
    let signal_line = macd_line *. alpha_signal in
    let histogram = macd_line -. signal_line in
    Some {
      macd_line;
      signal_line;
      histogram;
      bullish_crossover = Float.(macd_line > signal_line && histogram > 0.0);
      bearish_crossover = Float.(macd_line < signal_line && histogram < 0.0);
    }
  | _ -> None

(* Volatility Calculation *)

let calculate_volatility prices ~period =
  if List.length prices < period then None
  else
    let recent = List.take prices period in
    let returns = List.zip_exn (List.tl_exn recent) recent
      |> List.map ~f:(fun (curr, prev) -> Float.log (curr /. prev)) in
    
    let mean_return = List.fold returns ~init:0.0 ~f:(+.) /. Float.of_int (List.length returns) in
    let variance = List.fold returns ~init:0.0 ~f:(fun acc ret ->
      acc +. ((ret -. mean_return) *. (ret -. mean_return))
    ) /. Float.of_int (List.length returns - 1) in
    
    Some (Float.sqrt variance *. Float.sqrt 252.0) (* Annualized volatility *)

(* Market Microstructure Analysis *)

let calculate_microstructure ~bid ~ask ~volume ~trades =
  let bid_ask_spread = ask -. bid in
  let mid_price = (bid +. ask) /. 2.0 in
  let effective_spread = bid_ask_spread /. mid_price *. 10000.0 in (* in bps *)
  let avg_trade_size = if trades > 0 then volume /. Float.of_int trades else 0.0 in
  let price_impact = effective_spread *. Float.log (1.0 +. avg_trade_size) in
  let market_depth = (bid +. ask) *. volume /. 2.0 in
  
  {
    bid_ask_spread;
    effective_spread;
    price_impact;
    market_depth;
  }

(* Trend Strength Calculation *)

let calculate_trend_strength prices ~period =
  if List.length prices < period then None
  else
    let recent = List.take prices period in
    let first = List.last_exn recent in
    let last = List.hd_exn recent in
    let total_change = Float.abs (last -. first) in
    let sum_absolute_changes = List.zip_exn (List.tl_exn recent) recent
      |> List.fold ~init:0.0 ~f:(fun acc (curr, prev) -> acc +. Float.abs (curr -. prev)) in
    
    if Float.(sum_absolute_changes = 0.0) then Some 0.0
    else Some (total_change /. sum_absolute_changes)

(* Market Phase Detection *)

let detect_market_phase prices ~short_period ~long_period =
  match calculate_sma prices ~period:short_period,
        calculate_sma prices ~period:long_period with
  | Some short_ma, Some long_ma ->
    let current_price = List.hd_exn prices in
    let price_vs_short = (current_price -. short_ma) /. short_ma in
    let price_vs_long = (current_price -. long_ma) /. long_ma in
    
    if Float.(price_vs_short > 0.02 && price_vs_long > 0.01) then Some `Markup
    else if Float.(price_vs_short < -0.02 && price_vs_long < -0.01) then Some `Markdown
    else if Float.(Float.abs price_vs_short < 0.005) then Some `Sideways
    else if Float.(Float.abs price_vs_short > 0.05) then Some `Breakout
    else Some `Reversal
  | _ -> None

(* Comprehensive Analytics Generation *)

let generate_analytics ~instrument_id ~prices ~volumes ~timestamp ?(bid=None) ?(ask=None) () =
  let current_price = List.hd_exn prices in
  
  {
    instrument_id;
    timestamp;
    
    (* Moving Averages *)
    sma_10 = calculate_sma prices ~period:10;
    sma_20 = calculate_sma prices ~period:20;
    sma_50 = calculate_sma prices ~period:50;
    ema_10 = calculate_ema prices ~period:10 ~alpha:0.1818;
    ema_20 = calculate_ema prices ~period:20 ~alpha:0.0952;
    
    (* Technical Indicators *)
    bollinger_20_2 = calculate_bollinger_bands prices ~period:20 ~std_dev_multiplier:2.0;
    atr_14 = None; (* Would need high/low data *)
    rsi_14 = calculate_rsi prices ~period:14;
    macd_12_26_9 = calculate_macd prices ~fast_period:12 ~slow_period:26 ~signal_period:9;
    stochastic_14_3_3 = None; (* Would need high/low data *)
    
    (* Volume Analysis *)
    volume_sma_20 = calculate_sma volumes ~period:20;
    volume_ratio = None;
    vwap_daily = Some current_price; (* Simplified *)
    
    (* Market Microstructure *)
    microstructure = (match bid, ask with
      | Some b, Some a -> Some (calculate_microstructure ~bid:b ~ask:a ~volume:(List.hd_exn volumes) ~trades:100)
      | _ -> None);
    
    (* Statistical Analysis *)
    statistics = (match calculate_volatility prices ~period:30 with
      | Some vol -> Some { 
          volatility_1d = vol /. Float.sqrt 252.0;
          volatility_1w = vol /. Float.sqrt 52.0;
          volatility_1m = vol /. Float.sqrt 12.0;
          beta = None;
          correlation_spy = None;
          sharpe_ratio = None;
        }
      | None -> None);
    
    (* Market Regime *)
    trend_strength = calculate_trend_strength prices ~period:20;
    market_phase = detect_market_phase prices ~short_period:10 ~long_period:30;
  }