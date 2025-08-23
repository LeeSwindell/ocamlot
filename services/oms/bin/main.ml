open Lwt.Syntax
open Oms_service

let setup_subscriptions state nats_client =
  let* _ = Ocamlot_infrastructure_nats.Nats_client.subscribe nats_client
    ~subject:"orders.new"
    ~callback:(fun msg ->
      try
        let json = Yojson.Safe.from_string msg##payload in
        let request = match Handlers.new_order_request_of_yojson json with
          | Ok req -> req
          | Error err -> failwith err
        in
        let* _ = Handlers.handle_new_order state nats_client request in
        Lwt.return_unit
      with e ->
        Logs.err (fun m -> m "Error handling new order: %s" (Printexc.to_string e));
        Lwt.return_unit
    )
  in
  
  let* _ = Ocamlot_infrastructure_nats.Nats_client.subscribe nats_client
    ~subject:"orders.fill"
    ~callback:(fun msg ->
      try
        let notification = Js_of_ocaml.Js.JSON.parse msg##payload in
        let* _ = Handlers.handle_fill_notification state nats_client notification in
        Lwt.return_unit
      with e ->
        Logs.err (fun m -> m "Error handling fill: %s" (Printexc.to_string e));
        Lwt.return_unit
    )
  in
  
  let* _ = Ocamlot_infrastructure_nats.Nats_client.subscribe nats_client
    ~subject:"orders.cancel"
    ~callback:(fun msg ->
      try
        let request = Js_of_ocaml.Js.JSON.parse msg##payload in
        let* _ = Handlers.handle_cancel_request state nats_client request in
        Lwt.return_unit
      with e ->
        Logs.err (fun m -> m "Error handling cancel: %s" (Printexc.to_string e));
        Lwt.return_unit
    )
  in
  
  Lwt.return_unit

let start_server () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  
  let state = State.create () in
  
  let* nats_client = Ocamlot_infrastructure_nats.Nats_client.connect 
    ~url:"nats://localhost:4222" () in
  
  Logs.info (fun m -> m "OMS Service started, connected to NATS");
  
  let* () = setup_subscriptions state nats_client in
  
  Logs.info (fun m -> m "OMS Service subscriptions active");
  
  (* Keep service running *)
  let rec keep_alive () =
    let* () = Lwt_unix.sleep 60.0 in
    Logs.info (fun m -> m "OMS Service heartbeat - daily volume: %.2f" state.daily_volume);
    keep_alive ()
  in
  keep_alive ()

let () =
  Lwt_main.run (start_server ())