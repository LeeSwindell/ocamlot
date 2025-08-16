open Base
open Alcotest_lwt
open Ocamlot_market_data

(* Test that market data system properly publishes 1-second conflated bars to NATS *)

let test_conflated_bars_nats_subject _switch () =
  (* Test the subject generation for 1-second bars *)
  let bar : Conflation.ohlcv_bar = {
    instrument_id = "AAPL";
    interval = Conflation.Seconds 1;
    open_price = 150.0;
    high_price = 151.0;
    low_price = 149.5;
    close_price = 150.5;
    volume = 1000.0;
    vwap = 150.3;
    trade_count = 10;
    open_timestamp = Unix.time ();
    close_timestamp = Unix.time ();
    sequence = 1L;
  } in
  
  let data_product = Publisher.ConflatedBars [bar] in
  let nats_channel = Publisher.NATS { 
    subject = "market.bars.{symbol}.{interval}"; 
    stream = Some "MARKET_BARS"; 
    max_age_ms = Some (24 * 60 * 60 * 1000) 
  } in
  
  let generated_subject = Publisher.generate_subject_key data_product nats_channel in
  
  (* Should generate: market.bars.AAPL.1s *)
  Alcotest.(check string) "NATS subject for 1s bars" "market.bars.AAPL.1s" generated_subject;
  Lwt.return_unit

let test_analytics_nats_subject _switch () =
  (* Test the subject generation for analytics *)
  let analytics : Analytics.symbol_analytics = {
    instrument_id = "AAPL";
    timestamp = Unix.time ();
    sma_10 = Some 150.0;
    sma_20 = Some 149.5;
    sma_50 = None;
    ema_10 = Some 150.2;
    ema_20 = Some 149.8;
    bollinger_20_2 = None;
    atr_14 = None;
    rsi_14 = None;
    macd_12_26_9 = None;
    stochastic_14_3_3 = None;
    volume_sma_20 = None;
    volume_ratio = None;
    vwap_daily = Some 150.1;
    microstructure = None;
    statistics = None;
    trend_strength = None;
    market_phase = None;
  } in
  
  let data_product = Publisher.Analytics [analytics] in
  let nats_channel = Publisher.NATS { 
    subject = "market.analytics.{symbol}"; 
    stream = Some "MARKET_ANALYTICS"; 
    max_age_ms = Some (60 * 60 * 1000) 
  } in
  
  let generated_subject = Publisher.generate_subject_key data_product nats_channel in
  
  (* Should generate: market.analytics.AAPL *)
  Alcotest.(check string) "NATS subject for analytics" "market.analytics.AAPL" generated_subject;
  Lwt.return_unit

let test_publisher_config_structure _switch () =
  (* Test that the default config has proper NATS channels configured *)
  let config = Publisher.default_config in
  
  (* Check that ConflatedBars has NATS channel configured *)
  let conflated_bars_channels = List.Assoc.find config.channels (Publisher.ConflatedBars []) ~equal:Poly.equal in
  
  Alcotest.(check bool) "ConflatedBars has channels configured" true 
    (Option.is_some conflated_bars_channels);
  
  (match conflated_bars_channels with
  | Some channels ->
    let has_nats_channel = List.exists channels ~f:(function
      | Publisher.NATS { subject; _ } -> String.is_substring subject ~substring:"market.bars"
      | _ -> false
    ) in
    Alcotest.(check bool) "ConflatedBars has NATS channel" true has_nats_channel
  | None -> Alcotest.fail "ConflatedBars channels not found");
  
  Lwt.return_unit

let test_market_data_system_config _switch () =
  (* Test that the market data system has 1-second conflation configured *)
  let conflation_config = Conflation.default_config in
  
  (* Check that 1-second interval is in the default intervals *)
  let has_1s_interval = List.exists conflation_config.intervals ~f:(function
    | Conflation.Seconds 1 -> true
    | _ -> false
  ) in
  
  Alcotest.(check bool) "1-second interval configured" true has_1s_interval;
  Lwt.return_unit

(* Integration test using actual components (without real NATS connection) *)
let test_conflation_to_publisher_flow _switch () =
  (* Create a conflation engine *)
  let conflation_config = Conflation.{
    intervals = [Seconds 1];
    conflation_method = TimeWeighted;
    instruments = ["AAPL"];
    align_to_exchange = false;
    handle_gaps = true;
    max_gap_ms = 5000;
  } in
  let engine = Conflation.create_engine conflation_config in
  
  (* Create a synthetic tick *)
  let tick : Generator.tick = {
    instrument_id = "AAPL";
    bid = 149.5;
    ask = 150.5;
    last_price = 150.0;
    volume = 100.0;
    timestamp = Unix.time ();
    sequence = 1L;
  } in
  
  (* Process tick through conflation *)
  let (updated_engine, bars) = Conflation.process_tick engine tick in
  
  (* For this test, we expect no completed bars yet (need more ticks or time) *)
  Alcotest.(check int) "No completed bars on first tick" 0 (List.length bars);
  
  (* Verify the engine is updated *)
  Alcotest.(check bool) "Engine updated" true (not (phys_equal engine updated_engine));
  
  Lwt.return_unit

let tests = [
  "Subject Generation", [
    test_case "1-second conflated bars NATS subject" `Quick test_conflated_bars_nats_subject;
    test_case "analytics NATS subject" `Quick test_analytics_nats_subject;
  ];
  "Configuration", [
    test_case "publisher config structure" `Quick test_publisher_config_structure;
    test_case "market data system 1s config" `Quick test_market_data_system_config;
  ];
  "Flow", [
    test_case "conflation to publisher flow" `Quick test_conflation_to_publisher_flow;
  ];
]

let () =
  Lwt_main.run (run "Market Data NATS Integration Tests" tests)