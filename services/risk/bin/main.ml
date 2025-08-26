open Lwt.Syntax
open Risk_service

let setup_subscriptions state nats_client =
  let* _ = Ocamlot_infrastructure_nats.Nats_client.subscribe nats_client
    ~subject:"risk.check_request"
    ~callback:(fun msg ->
      try
        let payload_string = Bytes.to_string msg.payload in
        let* _ = Handlers.handle_risk_check_request state nats_client payload_string in
        Lwt.return_unit
      with e ->
        Logs.err (fun m -> m "Error handling risk check: %s" (Printexc.to_string e));
        Lwt.return_unit
    )
  in
  
  let* _ = Ocamlot_infrastructure_nats.Nats_client.subscribe nats_client
    ~subject:"positions.update"
    ~callback:(fun msg ->
      try
        let payload_string = Bytes.to_string msg.payload in
        let* () = Handlers.handle_position_update state payload_string in
        Lwt.return_unit
      with e ->
        Logs.err (fun m -> m "Error handling position update: %s" (Printexc.to_string e));
        Lwt.return_unit
    )
  in
  
  let* _ = Ocamlot_infrastructure_nats.Nats_client.subscribe nats_client
    ~subject:"market.data.>"
    ~callback:(fun msg ->
      try
        let payload_string = Bytes.to_string msg.payload in
        let* () = Handlers.handle_market_data_update state payload_string in
        Lwt.return_unit
      with e ->
        Logs.err (fun m -> m "Error handling market data: %s" (Printexc.to_string e));
        Lwt.return_unit
    )
  in
  
  let* _ = Ocamlot_infrastructure_nats.Nats_client.subscribe nats_client
    ~subject:"risk.limits.update"
    ~callback:(fun msg ->
      try
        let payload_string = Bytes.to_string msg.payload in
        let* () = Handlers.handle_limits_update state nats_client payload_string in
        Lwt.return_unit
      with e ->
        Logs.err (fun m -> m "Error handling limits update: %s" (Printexc.to_string e));
        Lwt.return_unit
    )
  in
  
  Lwt.return_unit

let start_server () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  
  let state = State.create () in
  
  let nats_client = Ocamlot_infrastructure_nats.Nats_client.create () in
  let* () = Ocamlot_infrastructure_nats.Nats_client.connect nats_client in
  
  Logs.info (fun m -> m "Risk Service started, connected to NATS");
  
  let* () = setup_subscriptions state nats_client in
  
  Logs.info (fun m -> m "Risk Service subscriptions active");
  
  (* Keep service running with periodic stats *)
  let rec keep_alive () =
    let* () = Lwt_unix.sleep 60.0 in
    let stats = State.get_statistics state in
    Logs.info (fun m -> m "Risk Service stats: %s" (Yojson.Safe.to_string stats));
    keep_alive ()
  in
  keep_alive ()

let () =
  Lwt_main.run (start_server ())