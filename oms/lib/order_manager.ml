open Base
open Lwt.Syntax
open Ocamlot_common
open Ocamlot_core

module OrderBook = struct
  type t = (Types.order_id, Order.order) Hashtbl.t

  let create () = Hashtbl.create (module String)

  let add_order book order =
    Hashtbl.set book ~key:order.Order.id ~data:order

  let get_order book order_id =
    Hashtbl.find book order_id

  let update_order book order =
    Hashtbl.set book ~key:order.Order.id ~data:order

  let remove_order book order_id =
    Hashtbl.remove book order_id
end

type t = {
  order_book: OrderBook.t;
  event_handlers: (Events.order_event -> unit Lwt.t) list;
}

let create () = {
  order_book = OrderBook.create ();
  event_handlers = [];
}

let add_event_handler oms handler =
  { oms with event_handlers = handler :: oms.event_handlers }

let emit_event oms event =
  Lwt_list.iter_p (fun handler -> handler event) oms.event_handlers

let submit_order oms order =
  let* () = emit_event oms (Events.OrderSubmitted order) in
  OrderBook.add_order oms.order_book order;
  Lwt.return (Ok order.id)

let cancel_order oms order_id =
  match OrderBook.get_order oms.order_book order_id with
  | Some order ->
    let cancelled_order = { order with status = Order.Cancelled } in
    OrderBook.update_order oms.order_book cancelled_order;
    let* () = emit_event oms (Events.OrderCancelled { order_id; timestamp = Unix.time () }) in
    Lwt.return (Ok ())
  | None ->
    Lwt.return (Error (Error.Invalid_order "Order not found"))