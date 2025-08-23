open Lwt.Syntax
open Cmdliner

let setup_logging () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info)

let run_services () =
  setup_logging ();
  Logs.info (fun m -> m "Use 'overmind start' to run all services, or individual service executables:");
  Logs.info (fun m -> m "  - ocamlot-oms: Order Management Service");
  Logs.info (fun m -> m "  - ocamlot-risk: Risk Check Service");  
  Logs.info (fun m -> m "  - ocamlot-simulator: Market Data Simulator");
  Lwt.return ()

let services_cmd =
  let doc = "Show available services" in
  let info = Cmd.info "services" ~doc in
  Cmd.v info Term.(const (fun () -> Lwt_main.run (run_services ())) $ const ())

let default_cmd =
  let doc = "OCaml Trading System" in
  let info = Cmd.info "ocamlot" ~doc in
  Cmd.group info [services_cmd]

let () = Stdlib.exit (Cmd.eval default_cmd)