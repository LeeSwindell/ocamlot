open Base
open Lwt.Syntax
open Ocamlot_common
open Ocamlot_core

type risk_limits = {
  max_order_size: Types.decimal;
  max_daily_volume: Types.decimal;
  max_position_size: Types.decimal;
} [@@deriving show]

type risk_result = 
  | Passed
  | Failed of string
  [@@deriving show]

let default_limits = {
  max_order_size = 10000.0;
  max_daily_volume = 100000.0;
  max_position_size = 50000.0;
}

let check_order_size limits order =
  if Float.(order.Order.quantity > limits.max_order_size) then
    Failed (Printf.sprintf "Order size %.2f exceeds limit %.2f" 
            order.quantity limits.max_order_size)
  else
    Passed

let check_pre_trade_risk ~limits ~order =
  let* () = Lwt.return () in
  match check_order_size limits order with
  | Failed reason -> Lwt.return (Failed reason)
  | Passed -> Lwt.return Passed

let check_intraday_risk ~limits:_ ~order:_ =
  let* () = Lwt.return () in
  Lwt.return Passed