open Base
open Ocamlot_core_types
open Ocamlot_core_domain

type order_event = 
  | OrderSubmitted of Order.order
  | OrderFilled of { order_id: Types.order_id; fill_qty: Types.decimal; fill_price: Types.decimal; timestamp: Types.timestamp }
  | OrderCancelled of { order_id: Types.order_id; timestamp: Types.timestamp }
  | OrderRejected of { order_id: Types.order_id; reason: string; timestamp: Types.timestamp }
  [@@deriving show]

type market_event = 
  | PriceUpdate of { instrument_id: Types.instrument_id; bid: Types.decimal; ask: Types.decimal; timestamp: Types.timestamp }
  | TradeExecuted of { instrument_id: Types.instrument_id; price: Types.decimal; quantity: Types.decimal; timestamp: Types.timestamp }
  [@@deriving show]

type system_event = 
  | RiskCheck of { order_id: Types.order_id; passed: bool; reason: string option; timestamp: Types.timestamp }
  | SystemAlert of { level: string; message: string; timestamp: Types.timestamp }
  [@@deriving show]