open Lwt.Syntax

let start_monitoring () =
  let* () = Lwt.return () in
  Logs.info (fun m -> m "Starting monitoring service");
  Lwt.return ()

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_main.run (start_monitoring ())