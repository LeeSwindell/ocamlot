open Base
open Ocamlot_core_types
open Ocamlot_core_domain

type risk_limits = {
  max_order_size: Types.decimal;
  max_daily_volume: Types.decimal;
  max_position_size: Types.decimal;
  max_notional_exposure: Types.decimal;
  concentration_limit: float;  (* Percentage of portfolio *)
} [@@deriving show]

type risk_result = 
  | Passed
  | Failed of string
  | Warning of string
  [@@deriving show]

type position = {
  instrument_id: string;
  quantity: float;
  avg_price: float;
  market_value: float;
  unrealized_pnl: float;
}

type portfolio_state = {
  positions: (string, position) Hashtbl.t;
  cash_balance: float;
  daily_volume: float;
  total_exposure: float;
}

let default_limits = {
  max_order_size = 10000.0;
  max_daily_volume = 100000.0;
  max_position_size = 50000.0;
  max_notional_exposure = 500000.0;
  concentration_limit = 0.2;
}

let check_order_size limits order =
  if Float.(order.Order.quantity > limits.max_order_size) then
    Failed (Printf.sprintf "Order size %.2f exceeds limit %.2f" 
            order.quantity limits.max_order_size)
  else
    Passed

let check_daily_volume limits current_volume order =
  let new_volume = current_volume +. order.Order.quantity in
  if Float.(new_volume > limits.max_daily_volume) then
    Failed (Printf.sprintf "Daily volume would be %.2f, exceeds limit %.2f"
            new_volume limits.max_daily_volume)
  else
    Passed

let calculate_position_after_order portfolio order =
  match Hashtbl.find portfolio.positions order.Order.instrument_id with
  | Some pos ->
    let new_qty = match order.side with
      | Types.Buy -> pos.quantity +. order.quantity
      | Types.Sell -> pos.quantity -. order.quantity
    in
    Float.abs new_qty
  | None -> order.quantity

let check_position_size limits portfolio order =
  let new_position = calculate_position_after_order portfolio order in
  if Float.(new_position > limits.max_position_size) then
    Failed (Printf.sprintf "Position size would be %.2f, exceeds limit %.2f"
            new_position limits.max_position_size)
  else
    Passed

let calculate_order_notional order ~market_price =
  match order.Order.order_type with
  | Types.Market -> order.quantity *. market_price
  | Types.Limit price -> order.quantity *. price
  | Types.Stop price -> order.quantity *. price
  | Types.StopLimit (_, limit) -> order.quantity *. limit

let check_notional_exposure limits portfolio order ~market_price =
  let order_notional = calculate_order_notional order ~market_price in
  let new_exposure = portfolio.total_exposure +. order_notional in
  if Float.(new_exposure > limits.max_notional_exposure) then
    Failed (Printf.sprintf "Total exposure would be %.2f, exceeds limit %.2f"
            new_exposure limits.max_notional_exposure)
  else
    Passed

let check_concentration limits portfolio order ~market_price =
  let order_notional = calculate_order_notional order ~market_price in
  let total_portfolio_value = portfolio.cash_balance +. portfolio.total_exposure in
  let concentration = order_notional /. total_portfolio_value in
  if Float.(concentration > limits.concentration_limit) then
    Warning (Printf.sprintf "Order represents %.1f%% of portfolio, exceeds concentration limit %.1f%%"
            (concentration *. 100.0) (limits.concentration_limit *. 100.0))
  else
    Passed

let combine_results results =
  List.fold results ~init:Passed ~f:(fun acc result ->
    match acc, result with
    | Failed _, _ -> acc
    | _, Failed msg -> Failed msg
    | Warning _, Passed -> acc
    | Passed, Warning msg -> Warning msg
    | _ -> acc
  )

let run_pre_trade_checks ~limits ~portfolio ~order ~market_price =
  let checks = [
    check_order_size limits order;
    check_daily_volume limits portfolio.daily_volume order;
    check_position_size limits portfolio order;
    check_notional_exposure limits portfolio order ~market_price;
    check_concentration limits portfolio order ~market_price;
  ] in
  combine_results checks