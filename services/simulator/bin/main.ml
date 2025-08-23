open Base
open Lwt.Syntax
open Simulator_service

let create_default_instruments () = 
  let open Generator in
  [
    { symbol = "AAPL"; initial_price = 150.0; 
      model = RandomWalk { volatility = 0.02; drift = 0.0001 };
      tick_size = 0.01; lot_size = 1.0 };
    { symbol = "GOOGL"; initial_price = 2800.0;
      model = MeanReverting { mean = 2800.0; reversion_speed = 0.1; volatility = 30.0 };
      tick_size = 0.01; lot_size = 1.0 };
    { symbol = "MSFT"; initial_price = 300.0;
      model = RandomWalk { volatility = 0.015; drift = 0.0002 };
      tick_size = 0.01; lot_size = 1.0 };
    { symbol = "TSLA"; initial_price = 800.0;
      model = Jump { normal_vol = 0.03; jump_prob = 0.01; jump_size = 0.05 };
      tick_size = 0.01; lot_size = 1.0 };
    { symbol = "AMZN"; initial_price = 3200.0;
      model = RandomWalk { volatility = 0.018; drift = 0.00015 };
      tick_size = 0.01; lot_size = 1.0 };
  ]

type simulator_state = {
  instruments: Generator.instrument_config list;
  current_prices: (string, float) Hashtbl.t;
  mutable is_running: bool;
  mutable ticks_generated: int;
}

let create_state instruments =
  let current_prices = Hashtbl.create (module String) in
  List.iter instruments ~f:(fun inst ->
    Hashtbl.set current_prices ~key:inst.Generator.symbol ~data:inst.initial_price
  );
  { instruments; current_prices; is_running = true; ticks_generated = 0 }

let publish_market_data nats_client state ~dt =
  let timestamp = Unix.time () in
  
  let* () = Lwt_list.iter_p (fun instrument ->
    let current_price = Hashtbl.find_exn state.current_prices instrument.Generator.symbol in
    let new_price = Generator.generate_next_price current_price instrument.model ~dt in
    Hashtbl.set state.current_prices ~key:instrument.symbol ~data:new_price;
    
    (* Publish quote *)
    let quote = Generator.generate_quote instrument ~current_price:new_price ~timestamp in
    let* () = Ocamlot_infrastructure_nats.Nats_client.publish nats_client
      ~subject:(Printf.sprintf "market.data.quote.%s" instrument.symbol)
      ~payload:(Yojson.Safe.to_string quote)
    in
    
    (* Occasionally publish trades *)
    if Random.float 1.0 < 0.3 then
      let trade = Generator.generate_trade instrument ~current_price:new_price ~timestamp in
      Ocamlot_infrastructure_nats.Nats_client.publish nats_client
        ~subject:(Printf.sprintf "market.data.trade.%s" instrument.symbol)
        ~payload:(Yojson.Safe.to_string trade)
    else
      Lwt.return_unit
  ) state.instruments in
  
  state.ticks_generated <- state.ticks_generated + 1;
  Lwt.return_unit

let run_simulation nats_client state ~tick_interval_ms =
  let dt = Float.of_int tick_interval_ms /. 1000.0 in
  let rec loop () =
    if state.is_running then
      let* () = publish_market_data nats_client state ~dt in
      let* () = Lwt_unix.sleep dt in
      loop ()
    else
      Lwt.return_unit
  in
  loop ()

let handle_control_message state msg =
  match msg##command with
  | "stop" -> 
    state.is_running <- false;
    Logs.info (fun m -> m "Simulator stopped");
    Lwt.return_unit
  | "start" ->
    state.is_running <- true;
    Logs.info (fun m -> m "Simulator started");
    Lwt.return_unit
  | "reset" ->
    List.iter state.instruments ~f:(fun inst ->
      Hashtbl.set state.current_prices ~key:inst.Generator.symbol ~data:inst.initial_price
    );
    state.ticks_generated <- 0;
    Logs.info (fun m -> m "Simulator reset");
    Lwt.return_unit
  | _ ->
    Lwt.return_unit

let start_server () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  
  let instruments = create_default_instruments () in
  let state = create_state instruments in
  
  let* nats_client = Ocamlot_infrastructure_nats.Nats_client.connect 
    ~url:"nats://localhost:4222" () in
  
  Logs.info (fun m -> m "Market Simulator started with %d instruments" (List.length instruments));
  
  (* Subscribe to control messages *)
  let* _ = Ocamlot_infrastructure_nats.Nats_client.subscribe nats_client
    ~subject:"simulator.control"
    ~callback:(fun msg ->
      try
        let control = Js_of_ocaml.Js.JSON.parse msg##payload in
        handle_control_message state control
      with e ->
        Logs.err (fun m -> m "Error handling control: %s" (Printexc.to_string e));
        Lwt.return_unit
    )
  in
  
  (* Start simulation and heartbeat *)
  let* () = Lwt.join [
    run_simulation nats_client state ~tick_interval_ms:1000;
    (* Heartbeat *)
    let rec heartbeat () =
      let* () = Lwt_unix.sleep 30.0 in
      Logs.info (fun m -> m "Simulator heartbeat - ticks: %d, running: %b" 
        state.ticks_generated state.is_running);
      heartbeat ()
    in
    heartbeat ()
  ] in
  
  Lwt.return_unit

let () =
  Lwt_main.run (start_server ())