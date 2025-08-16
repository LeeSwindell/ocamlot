open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_common
open Ocamlot_messaging

let test_event_creation _switch () =
  let payload = Event_types.OrderEvent (Event_types.OrderSubmitted {
    order_id = "order-123";
    client_id = "client-1";
    instrument_id = "AAPL";
    side = Types.Buy;
    order_type = Types.Market;
    quantity = 100.0;
    submitted_at = Unix.time ();
  }) in
  let event = Event_types.create_event ~subject:"orders.submitted.order-123" ~payload () in
  Alcotest.(check string) "event has id" "order-123" (
    match event.payload with
    | OrderEvent (OrderSubmitted { order_id; _ }) -> order_id
    | _ -> "wrong-id"
  );
  Lwt.return ()

let test_event_serialization _switch () =
  let payload = Event_types.OrderEvent (Event_types.OrderSubmitted {
    order_id = "order-456";
    client_id = "client-2";
    instrument_id = "GOOGL";
    side = Types.Sell;
    order_type = Types.Limit 2800.0;
    quantity = 50.0;
    submitted_at = Unix.time ();
  }) in
  let event = Event_types.create_event ~subject:"orders.submitted.order-456" ~payload () in
  let serialized = Event_serialization.serialize event in
  let deserialized : Event_types.event_payload Event_types.event = Event_serialization.deserialize serialized in
  Alcotest.(check bool) "serialization roundtrip" true (
    String.equal event.id deserialized.id
  );
  Lwt.return ()

let test_subject_generation _switch () =
  let payload = Event_types.MarketEvent (Event_types.PriceUpdate {
    instrument_id = "AAPL";
    bid = 149.50;
    ask = 149.55;
    timestamp = Unix.time ();
  }) in
  let subject = Event_types.get_subject_for_event payload in
  Alcotest.(check string) "market subject" "market.prices.AAPL" subject;
  Lwt.return ()

let test_nats_connection _switch () =
  let* result = Event_stream.connect () in
  match result with
  | Ok () ->
    Alcotest.(check bool) "connection established" true (Event_stream.is_connected ());
    let* () = Event_stream.disconnect () in
    Lwt.return ()
  | Error msg ->
    Alcotest.fail ("Connection failed: " ^ msg)

let test_event_publishing _switch () =
  let* connection_result = Event_stream.connect () in
  match connection_result with
  | Error msg -> Alcotest.fail ("Connection failed: " ^ msg)
  | Ok () ->
    let payload = Event_types.SystemEvent (Event_types.SystemStarted {
      component = "test-component";
      version = "1.0.0";
      timestamp = Unix.time ();
    }) in
    let* publish_result = Event_stream.publish_payload payload in
    let* () = Event_stream.disconnect () in
    match publish_result with
    | Ok () -> 
      Alcotest.(check bool) "event published" true true;
      Lwt.return ()
    | Error msg ->
      Alcotest.fail ("Publish failed: " ^ msg)

let tests = [
  "Events", [
    test_case "event creation" `Quick test_event_creation;
    test_case "event serialization" `Quick test_event_serialization;
    test_case "subject generation" `Quick test_subject_generation;
  ];
  "NATS", [
    test_case "connection" `Quick test_nats_connection;
    test_case "publishing" `Quick test_event_publishing;
  ];
]

let () =
  Lwt_main.run (run "Messaging Tests" tests)