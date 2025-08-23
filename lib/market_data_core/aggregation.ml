open Base
open Ocamlot_core_types

type ohlcv = {
  open_price: float;
  high: float;
  low: float;
  close: float;
  volume: float;
  timestamp: Types.timestamp;
  period_seconds: int;
} [@@deriving show]

type tick = {
  price: float;
  quantity: float;
  timestamp: Types.timestamp;
  side: Types.side;
} [@@deriving show]

let create_empty_bar ~timestamp ~period_seconds =
  {
    open_price = 0.0;
    high = Float.neg_infinity;
    low = Float.infinity;
    close = 0.0;
    volume = 0.0;
    timestamp;
    period_seconds;
  }

let update_bar bar tick =
  let open_price = if Float.(bar.open_price = 0.0) then tick.price else bar.open_price in
  {
    open_price;
    high = Float.max bar.high tick.price;
    low = Float.min bar.low tick.price;
    close = tick.price;
    volume = bar.volume +. tick.quantity;
    timestamp = bar.timestamp;
    period_seconds = bar.period_seconds;
  }

let aggregate_ticks ticks ~period_seconds =
  match ticks with
  | [] -> None
  | first :: _ ->
    let start_time = Float.round_down first.timestamp in
    let bar = create_empty_bar ~timestamp:start_time ~period_seconds in
    Some (List.fold ticks ~init:bar ~f:update_bar)

let calculate_vwap ticks =
  let total_value, total_volume = 
    List.fold ticks ~init:(0.0, 0.0) ~f:(fun (value, volume) tick ->
      (value +. (tick.price *. tick.quantity), volume +. tick.quantity)
    )
  in
  if Float.(total_volume > 0.0) then
    total_value /. total_volume
  else
    0.0

let calculate_twap prices ~start_time ~end_time =
  let filtered = List.filter prices ~f:(fun (_, timestamp) ->
    Float.(timestamp >= start_time && timestamp <= end_time)
  ) in
  match filtered with
  | [] -> 0.0
  | prices -> 
    let sum = List.fold prices ~init:0.0 ~f:(fun acc (price, _) -> acc +. price) in
    sum /. Float.of_int (List.length prices)

let calculate_spread ~bid ~ask =
  ask -. bid

let calculate_mid_price ~bid ~ask =
  (bid +. ask) /. 2.0

let calculate_weighted_mid ~bid ~bid_size ~ask ~ask_size =
  let total_size = bid_size +. ask_size in
  if Float.(total_size > 0.0) then
    ((bid *. ask_size) +. (ask *. bid_size)) /. total_size
  else
    calculate_mid_price ~bid ~ask