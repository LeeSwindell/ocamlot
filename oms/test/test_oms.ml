open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_common
open Ocamlot_core
open Ocamlot_oms

let test_order_manager_creation _switch () =
  let oms = Order_manager.create () in
  Alcotest.(check bool) "oms created" true (phys_equal oms oms);
  Lwt.return ()

let test_order_submission _switch () =
  let oms = Order_manager.create () in
  let order = Order.create_order
    ~id:"order-1"
    ~client_id:"client-1"
    ~instrument_id:"AAPL"
    ~side:Types.Buy
    ~order_type:(Types.Limit 150.0)
    ~quantity:100.0
    ~timestamp:(Unix.time ())
  in
  let* result = Order_manager.submit_order oms order in
  match result with
  | Ok order_id -> 
    Alcotest.(check string) "order submitted" "order-1" order_id;
    Lwt.return ()
  | Error _ -> Alcotest.fail "Order submission failed"

let tests = [
  "OrderManager", [
    test_case "creation" `Quick test_order_manager_creation;
    test_case "order submission" `Quick test_order_submission;
  ];
]

let () =
  Lwt_main.run (run "OMS Tests" tests)