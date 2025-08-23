open Base
open Ocamlot_core_types
open Ocamlot_core_domain

type validation_error = [
  | `MaxQuantityExceeded of float * float
  | `InvalidSymbol of string
  | `InsufficientBuyingPower of float * float
  | `InvalidOrderType
  | `InvalidPrice of float
] [@@deriving show]

type validation_rules = {
  max_quantity: float;
  valid_symbols: string list;
  buying_power: float;
  allow_market_orders: bool;
  min_price: float;
  max_price: float;
}

let validate_quantity ~max_quantity order =
  if Float.(order.Order.quantity <= max_quantity) then 
    Ok order
  else 
    Error (`MaxQuantityExceeded (order.quantity, max_quantity))

let validate_symbol ~valid_symbols order =
  if List.mem valid_symbols order.Order.instrument_id ~equal:String.equal then 
    Ok order
  else 
    Error (`InvalidSymbol order.instrument_id)

let validate_order_type ~allow_market_orders order =
  match order.Order.order_type with
  | Types.Market when not allow_market_orders -> Error `InvalidOrderType
  | _ -> Ok order

let validate_price_bounds ~min_price ~max_price order =
  match order.Order.order_type with
  | Types.Limit price | Types.Stop price ->
    if Float.(price >= min_price && price <= max_price) then Ok order
    else Error (`InvalidPrice price)
  | Types.StopLimit (stop, limit) ->
    if Float.(stop >= min_price && stop <= max_price && 
             limit >= min_price && limit <= max_price) then Ok order
    else Error (`InvalidPrice (Float.max stop limit))
  | Types.Market -> Ok order

let calculate_order_value order =
  match order.Order.order_type with
  | Types.Limit price -> order.quantity *. price
  | Types.Market -> order.quantity *. 999999.0  (* Worst case assumption *)
  | Types.Stop price -> order.quantity *. price
  | Types.StopLimit (_, limit) -> order.quantity *. limit

let validate_buying_power ~buying_power order =
  match order.Order.side with
  | Types.Buy ->
    let required = calculate_order_value order in
    if Float.(required <= buying_power) then Ok order
    else Error (`InsufficientBuyingPower (required, buying_power))
  | Types.Sell -> Ok order  (* Assume short selling allowed *)

let validation_pipeline rules order =
  let open Result in
  order
  |> validate_quantity ~max_quantity:rules.max_quantity
  >>= validate_symbol ~valid_symbols:rules.valid_symbols
  >>= validate_order_type ~allow_market_orders:rules.allow_market_orders
  >>= validate_price_bounds ~min_price:rules.min_price ~max_price:rules.max_price
  >>= validate_buying_power ~buying_power:rules.buying_power