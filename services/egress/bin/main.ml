open Lwt.Syntax

let start_market_adapter () =
  let* () = Lwt.return () in
  Logs.info (fun m -> m "Starting market adapter");
  Lwt.return ()

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_main.run (start_market_adapter ())