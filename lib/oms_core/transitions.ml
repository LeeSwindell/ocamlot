open Base
open Ocamlot_core_types
open Ocamlot_core_domain
open Ocamlot_core_events

type transition_error = [
  | `InvalidTransition of string
  | `InsufficientQuantity of float * float
] [@@deriving show]

type side_effect = 
  | PublishEvent of Events.order_event
  | UpdatePosition of { instrument_id: string; quantity: float; side: Types.side }
  | UpdateBalance of float
  [@@deriving show]

let transition_to_filled order ~fill_qty ~fill_price ~timestamp =
  match order.Order.status with
  | New | PartiallyFilled _ ->
    let total_filled = match order.status with
      | PartiallyFilled { filled_qty; _ } -> filled_qty +. fill_qty
      | _ -> fill_qty
    in
    if Float.(total_filled >= order.quantity) then
      let updated_order = { order with 
        status = Filled { filled_qty = order.quantity; avg_price = fill_price };
        updated_at = timestamp 
      } in
      let effects = [
        PublishEvent (OrderFilled { 
          order_id = order.id; 
          fill_qty; 
          fill_price; 
          timestamp 
        });
        UpdatePosition { 
          instrument_id = order.instrument_id;
          quantity = fill_qty;
          side = order.side
        };
      ] in
      Ok (updated_order, effects)
    else
      let updated_order = { order with 
        status = PartiallyFilled { filled_qty = total_filled; avg_price = fill_price };
        updated_at = timestamp 
      } in
      let effects = [
        PublishEvent (OrderFilled { 
          order_id = order.id; 
          fill_qty; 
          fill_price; 
          timestamp 
        });
        UpdatePosition { 
          instrument_id = order.instrument_id;
          quantity = fill_qty;
          side = order.side
        };
      ] in
      Ok (updated_order, effects)
  | Cancelled | Rejected _ | Filled _ ->
    Error (`InvalidTransition "Cannot fill a cancelled, rejected, or already filled order")

let transition_to_cancelled order ~timestamp =
  match order.Order.status with
  | New | PartiallyFilled _ ->
    let updated_order = { order with 
      status = Cancelled;
      updated_at = timestamp 
    } in
    let effects = [
      PublishEvent (OrderCancelled { order_id = order.id; timestamp })
    ] in
    Ok (updated_order, effects)
  | Cancelled -> Ok (order, [])  (* Idempotent *)
  | Rejected _ | Filled _ ->
    Error (`InvalidTransition "Cannot cancel a rejected or filled order")

let transition_to_rejected order ~reason ~timestamp =
  match order.Order.status with
  | New ->
    let updated_order = { order with 
      status = Rejected reason;
      updated_at = timestamp 
    } in
    let effects = [
      PublishEvent (OrderRejected { order_id = order.id; reason; timestamp })
    ] in
    Ok (updated_order, effects)
  | _ ->
    Error (`InvalidTransition "Can only reject new orders")

let apply_amendment order ~new_quantity ~new_price ~timestamp =
  match order.Order.status with
  | New | PartiallyFilled { filled_qty; _ } ->
    let min_qty = match order.status with
      | PartiallyFilled { filled_qty; _ } -> filled_qty
      | _ -> 0.0
    in
    if Float.(new_quantity < min_qty) then
      Error (`InsufficientQuantity (new_quantity, min_qty))
    else
      let new_order_type = match order.order_type, new_price with
        | Types.Limit _, Some price -> Types.Limit price
        | Types.Stop _, Some price -> Types.Stop price
        | order_type, None -> order_type
        | _ -> order.order_type
      in
      let updated_order = { order with
        quantity = new_quantity;
        order_type = new_order_type;
        updated_at = timestamp
      } in
      Ok (updated_order, [])
  | _ ->
    Error (`InvalidTransition "Cannot amend cancelled, rejected, or filled orders")