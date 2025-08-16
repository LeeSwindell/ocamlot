open Base
open Ocamlot_common.Types
open Generator

(* Time interval types *)
type time_interval = 
  | Milliseconds of int
  | Seconds of int 
  | Minutes of int
  | Hours of int
  | Days of int
  [@@deriving show, yojson]

(* Conflation methods *)
type conflation_method =
  | TimeWeighted       (* Standard time-based conflation *)
  | VolumeWeighted     (* Conflate based on volume thresholds *)
  | TradeCount of int  (* Conflate after N trades *)
  | Adaptive of { 
      min_interval_ms: int; 
      max_interval_ms: int; 
      volatility_threshold: float 
    }
  [@@deriving show, yojson]

(* Conflated bar data *)
type ohlcv_bar = {
  instrument_id: string;
  interval: time_interval;
  open_price: decimal;
  high_price: decimal;
  low_price: decimal;
  close_price: decimal;
  volume: decimal;
  vwap: decimal;               (* Volume Weighted Average Price *)
  trade_count: int;
  open_timestamp: timestamp;
  close_timestamp: timestamp;
  sequence: int64;
} [@@deriving show, yojson]

(* Tick-level aggregation state *)
type tick_accumulator = {
  open_price: decimal;
  high_price: decimal;
  low_price: decimal;
  close_price: decimal;
  volume_sum: decimal;
  vwap_numerator: decimal;     (* Sum of (price * volume) *)
  trade_count: int;
  first_timestamp: timestamp;
  last_timestamp: timestamp;
  tick_count: int;
} [@@deriving show]

(* Conflation window state *)
type conflation_window = {
  instrument_id: string;
  interval: time_interval;
  conflation_method: conflation_method;
  accumulator: tick_accumulator option;
  window_start: timestamp;
  window_end: timestamp;
  sequence_counter: int64 ref;
}

(* Conflation configuration *)
type conflation_config = {
  intervals: time_interval list;
  conflation_method: conflation_method;
  instruments: string list;
  align_to_exchange: bool;      (* Align windows to exchange open times *)
  handle_gaps: bool;            (* Generate bars even during gaps *)
  max_gap_ms: int;             (* Maximum gap before starting new window *)
} [@@deriving show]

(* Conflation engine state *)
type conflation_engine = {
  windows: (string * time_interval, conflation_window) Map.Poly.t;
  config: conflation_config;
}

(* Time utilities *)
let interval_to_ms = function
  | Milliseconds ms -> ms
  | Seconds s -> s * 1000
  | Minutes m -> m * 60 * 1000  
  | Hours h -> h * 60 * 60 * 1000
  | Days d -> d * 24 * 60 * 60 * 1000

let interval_to_string = function
  | Milliseconds ms -> Printf.sprintf "%dms" ms
  | Seconds s -> Printf.sprintf "%ds" s
  | Minutes m -> Printf.sprintf "%dm" m
  | Hours h -> Printf.sprintf "%dh" h
  | Days d -> Printf.sprintf "%dd" d

(* Calculate window boundaries *)
let get_window_boundaries interval timestamp align_to_exchange =
  let interval_ms = interval_to_ms interval in
  let interval_ms_float = Float.of_int interval_ms in
  
  if align_to_exchange then
    (* Align to market open times (9:30 AM ET = 13:30 UTC) *)
    let market_open_offset_ms = 13 * 60 * 60 * 1000 + 30 * 60 * 1000 in
    let timestamp_ms = Int.of_float (timestamp *. 1000.0) in
    let adjusted_timestamp = timestamp_ms - market_open_offset_ms in
    let window_number = adjusted_timestamp / interval_ms in
    let window_start_adjusted = window_number * interval_ms in
    let window_start = Float.of_int (window_start_adjusted + market_open_offset_ms) /. 1000.0 in
    let window_end = window_start +. (interval_ms_float /. 1000.0) in
    (window_start, window_end)
  else
    (* Simple truncation alignment *)
    let timestamp_ms = timestamp *. 1000.0 in
    let window_start_ms = Float.round_down (timestamp_ms /. interval_ms_float) *. interval_ms_float in
    let window_start = window_start_ms /. 1000.0 in
    let window_end = window_start +. (interval_ms_float /. 1000.0) in
    (window_start, window_end)

(* Create empty accumulator *)
let create_accumulator first_tick =
  {
    open_price = first_tick.last_price;
    high_price = first_tick.last_price;
    low_price = first_tick.last_price;
    close_price = first_tick.last_price;
    volume_sum = first_tick.volume;
    vwap_numerator = first_tick.last_price *. first_tick.volume;
    trade_count = 1;
    first_timestamp = first_tick.timestamp;
    last_timestamp = first_tick.timestamp;
    tick_count = 1;
  }

(* Update accumulator with new tick *)
let update_accumulator acc tick =
  {
    open_price = acc.open_price;  (* Keep original open *)
    high_price = Float.max acc.high_price tick.last_price;
    low_price = Float.min acc.low_price tick.last_price;
    close_price = tick.last_price;  (* Update to latest *)
    volume_sum = acc.volume_sum +. tick.volume;
    vwap_numerator = acc.vwap_numerator +. (tick.last_price *. tick.volume);
    trade_count = acc.trade_count + 1;
    first_timestamp = acc.first_timestamp;  (* Keep original *)
    last_timestamp = tick.timestamp;  (* Update to latest *)
    tick_count = acc.tick_count + 1;
  }

(* Convert accumulator to OHLCV bar *)
let accumulator_to_bar instrument_id interval acc sequence window_start window_end =
  let vwap = if Float.equal acc.volume_sum 0.0 then acc.close_price 
             else acc.vwap_numerator /. acc.volume_sum in
  {
    instrument_id;
    interval;
    open_price = acc.open_price;
    high_price = acc.high_price;
    low_price = acc.low_price;
    close_price = acc.close_price;
    volume = acc.volume_sum;
    vwap;
    trade_count = acc.trade_count;
    open_timestamp = window_start;
    close_timestamp = window_end;
    sequence;
  }

(* Check if tick should trigger conflation *)
let should_conflate (window : conflation_window) (tick : tick) =
  match window.conflation_method with
  | TimeWeighted -> 
    Float.( >= ) tick.timestamp window.window_end
  | VolumeWeighted ->
    (match window.accumulator with
     | None -> false
     | Some acc -> Float.( >= ) acc.volume_sum 10000.0) (* TODO: make configurable *)
  | TradeCount n ->
    (match window.accumulator with
     | None -> false  
     | Some acc -> acc.trade_count >= n)
  | Adaptive { min_interval_ms; max_interval_ms; volatility_threshold } ->
    (match window.accumulator with
     | None -> false
     | Some acc -> 
       let elapsed_ms = Int.of_float ((tick.timestamp -. window.window_start) *. 1000.0) in
       let price_change = Float.abs ((acc.close_price -. acc.open_price) /. acc.open_price) in
       elapsed_ms >= min_interval_ms && 
       (elapsed_ms >= max_interval_ms || Float.( >= ) price_change volatility_threshold))

(* Create conflation window *)
let create_window instrument_id interval conflation_method config timestamp =
  let (window_start, window_end) = get_window_boundaries interval timestamp config.align_to_exchange in
  {
    instrument_id;
    interval;
    conflation_method;
    accumulator = None;
    window_start;
    window_end;
    sequence_counter = ref 0L;
  }

(* Initialize conflation engine *)
let create_engine config =
  let windows = 
    List.fold config.instruments ~init:Map.Poly.empty ~f:(fun acc_instr instrument ->
      List.fold config.intervals ~init:acc_instr ~f:(fun acc_interval interval ->
        let key = (instrument, interval) in
        let window = create_window instrument interval config.conflation_method config (Unix.time ()) in
        Map.Poly.set acc_interval ~key ~data:window
      )
    )
  in
  { windows; config }

(* Process single tick through conflation *)
let process_tick (engine : conflation_engine) (input_tick : tick) =
  let completed_bars = ref [] in
  
  let updated_windows = 
    Map.Poly.fold engine.windows ~init:Map.Poly.empty ~f:(fun ~key:(instrument, interval) ~data:window acc ->
      let map_key = (instrument, interval) in
      if String.equal instrument input_tick.instrument_id then
        let should_complete = should_conflate window input_tick in
        
        (* Complete current window if needed *)
        (if should_complete then
          match window.accumulator with
          | Some acc_data ->
            let sequence = !(window.sequence_counter) in
            window.sequence_counter := Int64.succ !(window.sequence_counter);
            let bar = accumulator_to_bar 
              instrument interval acc_data sequence window.window_start window.window_end in
            completed_bars := bar :: !completed_bars
          | None -> ());
        
        (* Create new window if completing or if no accumulator *)
        let new_window = 
          if should_complete || Option.is_none window.accumulator then
            let (new_start, new_end) = get_window_boundaries interval input_tick.timestamp engine.config.align_to_exchange in
            {
              window with
              accumulator = Some (create_accumulator input_tick);
              window_start = new_start;
              window_end = new_end;
            }
          else
            (* Update existing accumulator *)
            match window.accumulator with
            | None -> 
              { window with accumulator = Some (create_accumulator input_tick) }
            | Some acc_data ->
              { window with accumulator = Some (update_accumulator acc_data input_tick) }
        in
        
        Map.Poly.set acc ~key:map_key ~data:new_window
      else
        Map.Poly.set acc ~key:map_key ~data:window
    )
  in
  
  let updated_engine = { engine with windows = updated_windows } in
  (updated_engine, List.rev !completed_bars)

(* Process batch of ticks *)
let process_batch engine ticks =
  List.fold ticks ~init:(engine, []) ~f:(fun (acc_engine, acc_bars) tick ->
    let (new_engine, new_bars) = process_tick acc_engine tick in
    (new_engine, acc_bars @ new_bars)
  )

(* Force completion of all current windows *)
let force_complete_all engine =
  let completed_bars = ref [] in
  let current_time = Unix.time () in
  
  let updated_windows = 
    Map.Poly.map engine.windows ~f:(fun window ->
      match window.accumulator with
      | Some acc ->
        let sequence = !(window.sequence_counter) in
        window.sequence_counter := Int64.succ !(window.sequence_counter);
        let bar = accumulator_to_bar 
          window.instrument_id window.interval acc sequence window.window_start current_time in
        completed_bars := bar :: !completed_bars;
        { window with accumulator = None }
      | None -> window
    )
  in
  
  let updated_engine = { engine with windows = updated_windows } in
  (updated_engine, List.rev !completed_bars)

(* Get current window status *)
let get_window_status engine instrument_id interval =
  let key = (instrument_id, interval) in
  Map.Poly.find engine.windows key
  |> Option.map ~f:(fun window ->
    let progress = match window.accumulator with
      | None -> 0.0
      | Some acc ->
        let elapsed = acc.last_timestamp -. window.window_start in
        let total = window.window_end -. window.window_start in
        elapsed /. total
    in
    (window, progress)
  )

(* Default conflation configuration *)
let default_config = {
  intervals = [
    Milliseconds 100;
    Seconds 1;
    Minutes 1;
    Minutes 5;
    Minutes 15;
    Hours 1;
  ];
  conflation_method = TimeWeighted;
  instruments = ["AAPL"; "GOOGL"; "TSLA"];
  align_to_exchange = true;
  handle_gaps = true;
  max_gap_ms = 5000;
}