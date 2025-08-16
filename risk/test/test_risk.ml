open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_common
open Ocamlot_core
open Ocamlot_risk

let test_risk_limits _switch () =
  let limits = Checks.default_limits in
  Alcotest.(check bool) "max order size" true Float.(limits.max_order_size > 0.0);
  Alcotest.(check bool) "max daily volume" true Float.(limits.max_daily_volume > 0.0);
  Lwt.return ()

let test_order_size_check _switch () =
  let limits = Checks.default_limits in
  let small_order = Order.create_order
    ~id:"order-1"
    ~client_id:"client-1"
    ~instrument_id:"AAPL"
    ~side:Types.Buy
    ~order_type:(Types.Limit 150.0)
    ~quantity:5000.0
    ~timestamp:(Unix.time ())
  in
  let* result = Checks.check_pre_trade_risk ~limits ~order:small_order in
  match result with
  | Checks.Passed -> Lwt.return ()
  | Checks.Failed reason -> Alcotest.fail ("Risk check failed: " ^ reason)

let test_large_order_rejection _switch () =
  let limits = Checks.default_limits in
  let large_order = Order.create_order
    ~id:"order-2"
    ~client_id:"client-1"
    ~instrument_id:"AAPL"
    ~side:Types.Buy
    ~order_type:(Types.Limit 150.0)
    ~quantity:50000.0
    ~timestamp:(Unix.time ())
  in
  let* result = Checks.check_pre_trade_risk ~limits ~order:large_order in
  match result with
  | Checks.Failed _ -> Lwt.return ()
  | Checks.Passed -> Alcotest.fail "Expected large order to be rejected"

let tests = [
  "Risk", [
    test_case "default limits" `Quick test_risk_limits;
    test_case "order size check" `Quick test_order_size_check;
    test_case "large order rejection" `Quick test_large_order_rejection;
  ];
]

let () =
  Lwt_main.run (run "Risk Tests" tests)