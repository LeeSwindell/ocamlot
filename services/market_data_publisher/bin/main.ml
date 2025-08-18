open Base
open Lwt.Syntax
open Stdio

module Conflation = Ocamlot_market_data.Conflation
module Analytics = Ocamlot_market_data.Analytics
module Feed = Ocamlot_market_data.Feed
module Nats = Ocamlot_nats.Nats

(* Market Data Publisher Service *)
(* Generates synthetic market data and publishes to NATS *)

let nats_config = 
  let host = Option.value (Sys.getenv "NATS_HOST") ~default:"127.0.0.1" in
  let port = Option.value_map (Sys.getenv "NATS_PORT") ~default:4222 ~f:Int.of_string in
  {
    Nats.Connection.host;
    port;
    connect_timeout = 10.0;
    reconnect_attempts = 3;
    reconnect_delay = 2.0;
  }

let symbols = ["AAPL"; "GOOGL"; "MSFT"; "TSLA"; "NVDA"]

let create_market_data_system () =
  printf "[PUBLISHER] Initializing market data system...\n%!";
  
  (* Initialize market data generation *)
  let profiles = Feed.default_profiles in
  Feed.set_random_seed 42;
  Feed.initialize_market_state profiles;
  
  printf "[PUBLISHER] Market data system initialized with %d symbols\n%!" 
    (List.length symbols);
  
  profiles

let connect_to_nats () =
  let* result = 
    try%lwt
      printf "[PUBLISHER] Connecting to NATS at %s:%d...\n%!" 
        nats_config.host nats_config.port;
      
      let client = Nats.create ~config:nats_config () in
      let* () = Nats.connect client in
      
      printf "[PUBLISHER] Successfully connected to NATS server\n%!";
      Lwt.return (Ok client)
    with
    | exn ->
      printf "[PUBLISHER] Failed to connect to NATS: %s\n%!" (Exn.to_string exn);
      Lwt.return (Error (Exn.to_string exn))
  in
  
  match result with
  | Ok client -> Lwt.return client
  | Error err -> 
    printf "[PUBLISHER] Retrying NATS connection in 5 seconds...\n%!";
    let* () = Lwt_unix.sleep 5.0 in
    Lwt.fail (Failure ("NATS connection failed: " ^ err))

let generate_and_publish_data client profiles =
  printf "[PUBLISHER] Starting market data generation and publishing...\n%!";
  
  let rec publish_loop iteration =
    try%lwt
      (* Generate fresh market data *)
      let market_snapshot = Feed.generate_market_snapshot profiles in
      
      (* Create conflated bars from the raw market data *)
      let bars = List.mapi market_snapshot ~f:(fun i tick ->
        let bar : Conflation.ohlcv_bar = {
          instrument_id = tick.instrument_id;
          interval = Conflation.Seconds 1;
          open_price = tick.bid;
          high_price = tick.ask;
          low_price = tick.bid -. 0.01;
          close_price = tick.last_price;
          volume = tick.volume;
          vwap = tick.last_price;
          trade_count = Int.of_float (tick.volume /. 100.0);
          open_timestamp = tick.timestamp;
          close_timestamp = tick.timestamp +. 1.0;
          sequence = Int64.of_int (iteration * 1000 + i);
        } in
        bar
      ) in
      
      (* Publish bars to NATS *)
      let* () = Lwt_list.iter_s (fun (bar : Conflation.ohlcv_bar) ->
        try%lwt
          let subject = Printf.sprintf "market.bars.%s.1s" bar.instrument_id in
          let json = Conflation.ohlcv_bar_to_yojson bar in
          let payload = Yojson.Safe.to_string json in
          
          let* () = Nats.publish_string client ~subject payload in
          
          printf "[PUBLISHER] Published bar for %s: OHLCV=%.2f/%.2f/%.2f/%.2f/%.0f\n%!" 
            bar.instrument_id bar.open_price bar.high_price bar.low_price bar.close_price bar.volume;
          
          Lwt.return_unit
        with
        | exn ->
          printf "[PUBLISHER] Failed to publish bar for %s: %s\n%!" 
            bar.instrument_id (Exn.to_string exn);
          Lwt.return_unit
      ) bars in
      
      (* Generate and publish analytics *)
      let* () = Lwt_list.iter_s (fun (bar : Conflation.ohlcv_bar) ->
        try%lwt
          let analytics : Analytics.symbol_analytics = {
            instrument_id = bar.instrument_id;
            timestamp = bar.close_timestamp;
            sma_10 = None; sma_20 = None; sma_50 = None;
            ema_10 = None; ema_20 = None;
            bollinger_20_2 = Some { 
              upper = bar.close_price +. 2.0; 
              middle = bar.close_price; 
              lower = bar.close_price -. 2.0;
              bandwidth = 4.0;
              percent_b = 0.5;
            };
            atr_14 = None;
            rsi_14 = Some { 
              value = 50.0 +. (Random.float 50.0 -. 25.0);
              oversold = false;
              overbought = false;
              divergence = false;
            };
            macd_12_26_9 = Some { 
              macd_line = 0.15; 
              signal_line = 0.1; 
              histogram = 0.05; 
              bullish_crossover = false;
              bearish_crossover = false;
            };
            stochastic_14_3_3 = None;
            volume_sma_20 = None;
            volume_ratio = None;
            vwap_daily = Some bar.vwap;
            microstructure = None;
            statistics = None;
            trend_strength = Some 0.5;
            market_phase = Some `Markup;
          } in
          
          let subject = Printf.sprintf "market.analytics.%s" bar.instrument_id in
          let json = Analytics.symbol_analytics_to_yojson analytics in
          let payload = Yojson.Safe.to_string json in
          
          let* () = Nats.publish_string client ~subject payload in
          
          printf "[PUBLISHER] Published analytics for %s: RSI=%.1f, VWAP=%.2f\n%!" 
            analytics.instrument_id 
            (Option.value_map analytics.rsi_14 ~default:0.0 ~f:(fun r -> r.value))
            (Option.value analytics.vwap_daily ~default:0.0);
          
          Lwt.return_unit
        with
        | exn ->
          printf "[PUBLISHER] Failed to publish analytics for %s: %s\n%!" 
            bar.instrument_id (Exn.to_string exn);
          Lwt.return_unit
      ) bars in
      
      printf "[PUBLISHER] Completed iteration %d, sleeping for 1 second...\n%!" iteration;
      let* () = Lwt_unix.sleep 1.0 in
      publish_loop (iteration + 1)
      
    with
    | exn ->
      printf "[PUBLISHER] Error in publish loop: %s\n%!" (Exn.to_string exn);
      let* () = Lwt_unix.sleep 5.0 in
      publish_loop iteration
  in
  
  publish_loop 1

let signal_handler _signum =
  printf "\n[PUBLISHER] Received shutdown signal, cleaning up...\n%!";
  Stdlib.exit 0

let () =
  (* Setup signal handlers *)
  Stdlib.Sys.set_signal Stdlib.Sys.sigint (Stdlib.Sys.Signal_handle signal_handler);
  Stdlib.Sys.set_signal Stdlib.Sys.sigterm (Stdlib.Sys.Signal_handle signal_handler);
  
  printf "[PUBLISHER] OCamlot Market Data Publisher starting...\n%!";
  
  Lwt_main.run (
    let profiles = create_market_data_system () in
    let* client = connect_to_nats () in
    
    printf "[PUBLISHER] Starting continuous market data publishing (Ctrl+C to stop)...\n%!";
    generate_and_publish_data client profiles
  )