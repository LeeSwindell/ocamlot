open Base
open Ocamlot_common
open Types

type order_status = 
  | New
  | PartiallyFilled of { filled_qty: decimal; avg_price: decimal }
  | Filled of { filled_qty: decimal; avg_price: decimal }
  | Cancelled
  | Rejected of string
  [@@deriving show]

type order = {
  id: order_id;
  client_id: client_id;
  instrument_id: instrument_id;
  side: side;
  order_type: order_type;
  quantity: decimal;
  status: order_status;
  created_at: timestamp;
  updated_at: timestamp;
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