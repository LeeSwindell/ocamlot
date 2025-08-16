open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_common
open Ocamlot_market_data

let test_feed_config _switch () =
  let config = Feed.create_feed_config
    ~instruments:["AAPL"; "GOOGL"]
    ~update_interval_ms:1000
  in
  Alcotest.(check int) "update interval" 1000 config.update_interval_ms;
  Alcotest.(check int) "instrument count" 2 (List.length config.instruments);
  Lwt.return ()

let test_market_data_structure _switch () =
  let market_data = Feed.{
    instrument_id = "AAPL";
    bid = 149.50;
    ask = 149.55;
    last_price = 149.52;
    volume = 1000.0;
    timestamp = Unix.time ();
  } in
  Alcotest.(check string) "instrument" "AAPL" market_data.instrument_id;
  Alcotest.(check bool) "bid < ask" true Float.(market_data.bid < market_data.ask);
  Lwt.return ()

let tests = [
  "Feed", [
    test_case "config creation" `Quick test_feed_config;
    test_case "market data structure" `Quick test_market_data_structure;
  ];
]

let () =
  Lwt_main.run (run "Market Data Tests" tests)