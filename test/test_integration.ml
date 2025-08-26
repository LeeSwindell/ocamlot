open Alcotest_lwt
open Ocamlot_core_types
open Ocamlot_core_domain
open Ocamlot_oms_core
open Ocamlot_risk_core

let test_order_creation _switch () =
  let order = Order.create_order
    ~id:"order-1"
    ~client_id:"client-1"
    ~instrument_id:"AAPL"
    ~side:Types.Buy
    ~order_type:(Types.Limit 150.0)
    ~quantity:100.0
    ~timestamp:(Unix.time ())
  in
  Alcotest.(check string) "order id" "order-1" order.id;
  Lwt.return ()

let test_order_validation _switch () =
  let rules = Validation.{
    max_quantity = 1000.0;
    valid_symbols = ["AAPL"; "GOOGL"];
    buying_power = 50000.0;
    allow_market_orders = true;
    min_price = 0.01;
    max_price = 10000.0;
  } in
  let order = Order.create_order
    ~id:"order-1"
    ~client_id:"client-1"
    ~instrument_id:"AAPL"
    ~side:Types.Buy
    ~order_type:(Types.Limit 150.0)
    ~quantity:100.0
    ~timestamp:(Unix.time ())
  in
  let result = Validation.validation_pipeline rules order in
  Alcotest.(check bool) "validation passes" true (Result.is_ok result);
  Lwt.return ()

let test_risk_limits _switch () =
  let limits = Checks.default_limits in
  Alcotest.(check bool) "max order size positive" true (limits.max_order_size > 0.0);
  Lwt.return ()

let test_order_transitions _switch () =
  let order = Order.create_order
    ~id:"order-1"
    ~client_id:"client-1"
    ~instrument_id:"AAPL"
    ~side:Types.Buy
    ~order_type:(Types.Limit 150.0)
    ~quantity:100.0
    ~timestamp:(Unix.time ())
  in
  let result = Transitions.transition_to_filled order 
    ~fill_qty:50.0 
    ~fill_price:149.5 
    ~timestamp:(Unix.time ()) 
  in
  Alcotest.(check bool) "transition succeeds" true (Result.is_ok result);
  Lwt.return ()

let tests = [
  "Order", [
    test_case "creation" `Quick test_order_creation;
  ];
  "Validation", [
    test_case "order validation" `Quick test_order_validation;
  ];
  "Risk", [
    test_case "risk limits" `Quick test_risk_limits;
  ];
  "Transitions", [
    test_case "order transitions" `Quick test_order_transitions;
  ];
]

let () =
  Lwt_main.run (run "OCamlot Integration Tests" tests)