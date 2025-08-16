open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_common
open Ocamlot_core

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

let test_risk_check _switch () =
  let limits = Ocamlot_risk.Checks.default_limits in
  let order = Order.create_order
    ~id:"order-1"
    ~client_id:"client-1"
    ~instrument_id:"AAPL"
    ~side:Types.Buy
    ~order_type:(Types.Limit 150.0)
    ~quantity:5000.0
    ~timestamp:(Unix.time ())
  in
  let* result = Ocamlot_risk.Checks.check_pre_trade_risk ~limits ~order in
  Alcotest.(check bool) "risk check passes" true 
    (match result with Ocamlot_risk.Checks.Passed -> true | _ -> false);
  Lwt.return ()

let tests = [
  "Order", [
    test_case "creation" `Quick test_order_creation;
  ];
  "Risk", [
    test_case "pre_trade_check" `Quick test_risk_check;
  ];
]

let () =
  Lwt_main.run (run "OCamlot Tests" tests)