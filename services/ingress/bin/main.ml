open Lwt.Syntax

let start_server port =
  let* () = Lwt.return () in
  Logs.info (fun m -> m "Starting ingress service on port %d" port);
  Lwt.return ()

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_main.run (start_server 8080)