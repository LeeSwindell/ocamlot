open Base
open Ocamlot_core_types

type order_status = 
  | New
  | PartiallyFilled of { filled_qty: Types.decimal; avg_price: Types.decimal }
  | Filled of { filled_qty: Types.decimal; avg_price: Types.decimal }
  | Cancelled
  | Rejected of string
  [@@deriving show]

type order = {
  id: Types.order_id;
  client_id: Types.client_id;
  instrument_id: Types.instrument_id;
  side: Types.side;
  order_type: Types.order_type;
  quantity: Types.decimal;
  status: order_status;
  created_at: Types.timestamp;
  updated_at: Types.timestamp;
} [@@deriving show]

let create_order ~id ~client_id ~instrument_id ~side ~order_type ~quantity ~timestamp =
  {
    id;
    client_id;
    instrument_id;
    side;
    order_type;
    quantity;
    status = New;
    created_at = timestamp;
    updated_at = timestamp;
  }