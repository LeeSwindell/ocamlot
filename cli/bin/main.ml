open Lwt.Syntax
open Cmdliner

let setup_logging () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info)

let run_simulator () =
  setup_logging ();
  let config = Ocamlot_sim.Simulator.{
    instruments = ["AAPL"; "GOOGL"; "MSFT"];
    initial_prices = [("AAPL", 150.0); ("GOOGL", 2800.0); ("MSFT", 300.0)];
    volatility = 0.1;
    tick_interval_ms = 1000;
  } in
  let sim = Ocamlot_sim.Simulator.create_simulator config in
  let on_price_update instrument_id price =
    Logs.info (fun m -> m "Price update: %s = %.2f" instrument_id price)
  in
  let* () = Ocamlot_sim.Simulator.run_simulation sim ~on_price_update in
  Lwt.return ()

let simulator_cmd =
  let doc = "Run the market data simulator" in
  let info = Cmd.info "simulator" ~doc in
  Cmd.v info Term.(const (fun () -> Lwt_main.run (run_simulator ())) $ const ())

let default_cmd =
  let doc = "OCaml Trading System" in
  let info = Cmd.info "ocamlot" ~doc in
  Cmd.group info [simulator_cmd]

let () = Stdlib.exit (Cmd.eval default_cmd)