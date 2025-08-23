open Base
open Lwt.Syntax
open Ocamlot_core_types
open Ocamlot_core_domain
open Ocamlot_oms_core

type order_book = (string, Order.order) Hashtbl.t

type oms_state = {
  order_book: order_book;
  rules_cache: (string, Validation.validation_rules) Hashtbl.t;
  mutable daily_volume: float;
  mutable last_update: Types.timestamp;
}

let create () = {
  order_book = Hashtbl.create (module String);
  rules_cache = Hashtbl.create (module String);
  daily_volume = 0.0;
  last_update = Unix.time ();
}

let get_order state order_id =
  Hashtbl.find state.order_book order_id

let store_order state order =
  Hashtbl.set state.order_book ~key:order.Order.id ~data:order;
  state.daily_volume <- state.daily_volume +. order.quantity;
  state.last_update <- Unix.time ();
  Lwt.return_unit

let update_order state order =
  Hashtbl.set state.order_book ~key:order.Order.id ~data:order;
  state.last_update <- Unix.time ();
  Lwt.return_unit

let remove_order state order_id =
  Hashtbl.remove state.order_book order_id

let get_rules_for_account state account_id =
  match Hashtbl.find state.rules_cache account_id with
  | Some rules -> Lwt.return rules
  | None ->
    (* In production, fetch from database or config service *)
    let default_rules = Validation.{
      max_quantity = 10000.0;
      valid_symbols = ["AAPL"; "GOOGL"; "MSFT"; "AMZN"; "TSLA"];
      buying_power = 100000.0;
      allow_market_orders = true;
      min_price = 0.01;
      max_price = 10000.0;
    } in
    Hashtbl.set state.rules_cache ~key:account_id ~data:default_rules;
    Lwt.return default_rules

let cache_rules state account_id rules =
  Hashtbl.set state.rules_cache ~key:account_id ~data:rules

let get_active_orders state =
  Hashtbl.to_alist state.order_book
  |> List.filter_map ~f:(fun (_, order) ->
    match order.Order.status with
    | New | PartiallyFilled _ -> Some order
    | _ -> None
  )

let get_orders_by_instrument state instrument_id =
  Hashtbl.to_alist state.order_book
  |> List.filter_map ~f:(fun (_, order) ->
    if String.equal order.Order.instrument_id instrument_id then
      Some order
    else None
  )

let reset_daily_volume state =
  state.daily_volume <- 0.0;
  state.last_update <- Unix.time ()