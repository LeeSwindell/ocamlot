open Base
open Lwt.Syntax
open Ocamlot_core_types
open Ocamlot_core_domain
open Ocamlot_core_events
open Ocamlot_oms_core
open Ocamlot_infrastructure_nats

type new_order_request = {
  order: Order.order;
  account_id: string;
} [@@deriving yojson]

type order_response = {
  order_id: string;
  status: [`Accepted | `Rejected];
  reason: string option;
} [@@deriving yojson]

let handle_new_order state nats_client request =
  let* rules = State.get_rules_for_account state request.account_id in
  match Validation.validation_pipeline rules request.order with
  | Ok validated_order ->
    let* () = State.store_order state validated_order in
    let event = Events.OrderSubmitted validated_order in
    let* () = 
      (* Publish to NATS *)
      let event_json = Yojson.Safe.to_string (`Assoc [
        ("type", `String "order_submitted");
        ("order_id", `String validated_order.id);
        ("instrument", `String validated_order.instrument_id);
        ("quantity", `Float validated_order.quantity);
        ("timestamp", `Float (Unix.time ()))
      ]) in
      Nats_client.publish nats_client 
        ~subject:"orders.accepted"
        ~payload:event_json
    in
    (* Send to risk check *)
    let* () = 
      let risk_request = Yojson.Safe.to_string (`Assoc [
        ("order_id", `String validated_order.id);
        ("account_id", `String request.account_id);
        ("instrument", `String validated_order.instrument_id);
        ("quantity", `Float validated_order.quantity);
        ("side", `String (match validated_order.side with Buy -> "buy" | Sell -> "sell"));
      ]) in
      Nats_client.publish nats_client
        ~subject:"risk.check_request"
        ~payload:risk_request
    in
    Lwt.return (Ok { order_id = validated_order.id; status = `Accepted; reason = None })
  | Error err ->
    let reason = match err with
      | `MaxQuantityExceeded (qty, max) -> 
        Printf.sprintf "Quantity %.2f exceeds max %.2f" qty max
      | `InvalidSymbol sym -> 
        Printf.sprintf "Invalid symbol: %s" sym
      | `InsufficientBuyingPower (req, avail) ->
        Printf.sprintf "Insufficient buying power: need %.2f, have %.2f" req avail
      | `InvalidOrderType -> "Invalid order type"
      | `InvalidPrice p -> Printf.sprintf "Invalid price: %.2f" p
    in
    let* () = 
      let event_json = Yojson.Safe.to_string (`Assoc [
        ("type", `String "order_rejected");
        ("order_id", `String request.order.id);
        ("reason", `String reason);
        ("timestamp", `Float (Unix.time ()))
      ]) in
      Nats_client.publish nats_client
        ~subject:"orders.rejected"
        ~payload:event_json
    in
    Lwt.return (Ok { order_id = request.order.id; status = `Rejected; reason = Some reason })

let handle_fill_notification state nats_client notification =
  let order_id = notification##order_id in
  let fill_qty = notification##fill_qty in
  let fill_price = notification##fill_price in
  
  match State.get_order state order_id with
  | Some order ->
    (match Transitions.transition_to_filled order ~fill_qty ~fill_price ~timestamp:(Unix.time ()) with
    | Ok (updated_order, side_effects) ->
      let* () = State.update_order state updated_order in
      let* () = 
        Lwt_list.iter_p (function
          | Transitions.PublishEvent event ->
            let event_json = match event with
              | OrderFilled fill ->
                Yojson.Safe.to_string (`Assoc [
                  ("type", `String "order_filled");
                  ("order_id", `String fill.order_id);
                  ("fill_qty", `Float fill.fill_qty);
                  ("fill_price", `Float fill.fill_price);
                  ("timestamp", `Float fill.timestamp);
                ])
              | _ -> "{}"
            in
            Nats_client.publish nats_client ~subject:"orders.filled" ~payload:event_json
          | Transitions.UpdatePosition pos ->
            let position_json = Yojson.Safe.to_string (`Assoc [
              ("instrument_id", `String pos.instrument_id);
              ("quantity", `Float pos.quantity);
              ("side", `String (match pos.side with Buy -> "buy" | Sell -> "sell"));
            ]) in
            Nats_client.publish nats_client ~subject:"positions.update" ~payload:position_json
          | Transitions.UpdateBalance amount ->
            Lwt.return_unit
        ) side_effects
      in
      Lwt.return (Ok ())
    | Error err ->
      Lwt.return (Error (Transitions.show_transition_error err)))
  | None ->
    Lwt.return (Error "Order not found")

let handle_cancel_request state nats_client request =
  let order_id = request##order_id in
  match State.get_order state order_id with
  | Some order ->
    (match Transitions.transition_to_cancelled order ~timestamp:(Unix.time ()) with
    | Ok (updated_order, side_effects) ->
      let* () = State.update_order state updated_order in
      let* () =
        Lwt_list.iter_p (function
          | Transitions.PublishEvent (OrderCancelled cancel) ->
            let event_json = Yojson.Safe.to_string (`Assoc [
              ("type", `String "order_cancelled");
              ("order_id", `String cancel.order_id);
              ("timestamp", `Float cancel.timestamp);
            ]) in
            Nats_client.publish nats_client ~subject:"orders.cancelled" ~payload:event_json
          | _ -> Lwt.return_unit
        ) side_effects
      in
      Lwt.return (Ok ())
    | Error err ->
      Lwt.return (Error (Transitions.show_transition_error err)))
  | None ->
    Lwt.return (Error "Order not found")