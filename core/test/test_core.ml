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
  Alcotest.(check string) "client id" "client-1" order.client_id;
  Lwt.return ()

let test_order_status _switch () =
  let order = Order.create_order
    ~id:"order-2"
    ~client_id:"client-1"
    ~instrument_id:"AAPL"
    ~side:Types.Sell
    ~order_type:Types.Market
    ~quantity:50.0
    ~timestamp:(Unix.time ())
  in
  match order.status with
  | Order.New -> Lwt.return ()
  | _ -> Alcotest.fail "Expected order status to be New"

let tests = [
  "Order", [
    test_case "creation" `Quick test_order_creation;
    test_case "status" `Quick test_order_status;
  ];
]

let () =
  Lwt_main.run (run "Core Tests" tests)