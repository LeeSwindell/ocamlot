open Base
open Alcotest_lwt
open Ocamlot_sim

let test_simulator_creation _switch () =
  let config = Simulator.{
    instruments = ["AAPL"; "GOOGL"];
    initial_prices = [("AAPL", 150.0); ("GOOGL", 2800.0)];
    volatility = 0.1;
    tick_interval_ms = 1000;
  } in
  let sim = Simulator.create_simulator config in
  Alcotest.(check bool) "simulator created" true (not !(sim.is_running));
  Lwt.return ()

let test_price_movement _switch () =
  let current_price = 100.0 in
  let volatility = 0.1 in
  let new_price = Simulator.generate_price_movement ~current_price ~volatility in
  Alcotest.(check bool) "price is positive" true Float.(new_price > 0.0);
  Lwt.return ()

let tests = [
  "Simulator", [
    test_case "creation" `Quick test_simulator_creation;
    test_case "price movement" `Quick test_price_movement;
  ];
]

let () =
  Lwt_main.run (run "Simulator Tests" tests)