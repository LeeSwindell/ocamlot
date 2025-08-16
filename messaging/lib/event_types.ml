open Base
open Ocamlot_common

(* Core event envelope *)
type 'a event = {
  id: string;
  subject: string;
  payload: 'a;
  timestamp: float;
  correlation_id: string option;
  causation_id: string option;
  version: int;
} [@@deriving show]

(* Event metadata *)
type event_metadata = {
  event_id: string;
  event_type: string;
  timestamp: float;
  correlation_id: string option;
  causation_id: string option;
  version: int;
} [@@deriving show]

(* Order lifecycle events *)
type order_event_payload = 
  | OrderSubmitted of {
      order_id: Types.order_id;
      client_id: Types.client_id;
      instrument_id: Types.instrument_id;
      side: Types.side;
      order_type: Types.order_type;
      quantity: Types.decimal;
      submitted_at: Types.timestamp;
    }
  | OrderValidated of {
      order_id: Types.order_id;
      validated_at: Types.timestamp;
    }
  | OrderRejected of {
      order_id: Types.order_id;
      reason: string;
      rejected_at: Types.timestamp;
    }
  | OrderFilled of {
      order_id: Types.order_id;
      fill_id: string;
      fill_qty: Types.decimal;
      fill_price: Types.decimal;
      filled_at: Types.timestamp;
    }
  | OrderCancelled of {
      order_id: Types.order_id;
      cancelled_at: Types.timestamp;
    }
  [@@deriving show]

(* Market data events *)
type market_event_payload =
  | PriceUpdate of {
      instrument_id: Types.instrument_id;
      bid: Types.decimal;
      ask: Types.decimal;
      timestamp: Types.timestamp;
    }
  | TradeExecuted of {
      instrument_id: Types.instrument_id;
      trade_id: string;
      price: Types.decimal;
      quantity: Types.decimal;
      timestamp: Types.timestamp;
    }
  | MarketStatusChange of {
      instrument_id: Types.instrument_id;
      status: string; (* "open", "closed", "halted", etc. *)
      timestamp: Types.timestamp;
    }
  [@@deriving show]

(* Risk events *)
type risk_event_payload =
  | RiskCheckPassed of {
      order_id: Types.order_id;
      check_type: string;
      timestamp: Types.timestamp;
    }
  | RiskCheckFailed of {
      order_id: Types.order_id;
      check_type: string;
      reason: string;
      timestamp: Types.timestamp;
    }
  | RiskLimitExceeded of {
      client_id: Types.client_id;
      limit_type: string;
      current_value: Types.decimal;
      limit_value: Types.decimal;
      timestamp: Types.timestamp;
    }
  [@@deriving show]

(* System events *)
type system_event_payload =
  | SystemStarted of {
      component: string;
      version: string;
      timestamp: Types.timestamp;
    }
  | SystemStopped of {
      component: string;
      timestamp: Types.timestamp;
    }
  | SystemAlert of {
      level: string; (* "info", "warning", "error", "critical" *)
      component: string;
      message: string;
      timestamp: Types.timestamp;
    }
  | HealthCheck of {
      component: string;
      status: string; (* "healthy", "degraded", "unhealthy" *)
      timestamp: Types.timestamp;
    }
  [@@deriving show]

(* Union type for all events *)
type event_payload = 
  | OrderEvent of order_event_payload
  | MarketEvent of market_event_payload
  | RiskEvent of risk_event_payload
  | SystemEvent of system_event_payload
  [@@deriving show]

(* Event construction helpers *)
let create_event ~subject ~payload ?correlation_id ?causation_id () =
  let id = "evt_" ^ (Int.to_string (Random.int 1000000)) ^ "_" ^ (Float.to_string (Unix.time ())) in
  {
    id;
    subject;
    payload;
    timestamp = Unix.time ();
    correlation_id;
    causation_id;
    version = 1;
  }

let get_event_type = function
  | OrderEvent (OrderSubmitted _) -> "order.submitted"
  | OrderEvent (OrderValidated _) -> "order.validated"
  | OrderEvent (OrderRejected _) -> "order.rejected"
  | OrderEvent (OrderFilled _) -> "order.filled"
  | OrderEvent (OrderCancelled _) -> "order.cancelled"
  | MarketEvent (PriceUpdate _) -> "market.price_update"
  | MarketEvent (TradeExecuted _) -> "market.trade_executed"
  | MarketEvent (MarketStatusChange _) -> "market.status_change"
  | RiskEvent (RiskCheckPassed _) -> "risk.check_passed"
  | RiskEvent (RiskCheckFailed _) -> "risk.check_failed"
  | RiskEvent (RiskLimitExceeded _) -> "risk.limit_exceeded"
  | SystemEvent (SystemStarted _) -> "system.started"
  | SystemEvent (SystemStopped _) -> "system.stopped"
  | SystemEvent (SystemAlert _) -> "system.alert"
  | SystemEvent (HealthCheck _) -> "system.health_check"

let get_subject_for_event = function
  | OrderEvent (OrderSubmitted { order_id; _ }) -> "orders.submitted." ^ order_id
  | OrderEvent (OrderValidated { order_id; _ }) -> "orders.validated." ^ order_id
  | OrderEvent (OrderRejected { order_id; _ }) -> "orders.rejected." ^ order_id
  | OrderEvent (OrderFilled { order_id; _ }) -> "orders.filled." ^ order_id
  | OrderEvent (OrderCancelled { order_id; _ }) -> "orders.cancelled." ^ order_id
  | MarketEvent (PriceUpdate { instrument_id; _ }) -> "market.prices." ^ instrument_id
  | MarketEvent (TradeExecuted { instrument_id; _ }) -> "market.trades." ^ instrument_id
  | MarketEvent (MarketStatusChange { instrument_id; _ }) -> "market.status." ^ instrument_id
  | RiskEvent (RiskCheckPassed { order_id; _ }) -> "risk.checks.passed." ^ order_id
  | RiskEvent (RiskCheckFailed { order_id; _ }) -> "risk.checks.failed." ^ order_id
  | RiskEvent (RiskLimitExceeded { client_id; _ }) -> "risk.limits." ^ client_id
  | SystemEvent (SystemStarted { component; _ }) -> "system.lifecycle." ^ component
  | SystemEvent (SystemStopped { component; _ }) -> "system.lifecycle." ^ component
  | SystemEvent (SystemAlert { component; level; _ }) -> "system.alerts." ^ level ^ "." ^ component
  | SystemEvent (HealthCheck { component; _ }) -> "system.health." ^ component