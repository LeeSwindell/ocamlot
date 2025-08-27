open Lwt.Syntax
module Connection = Ocamlot_infrastructure_redis.Connection
module Resp3 = Ocamlot_infrastructure_redis.Resp3

(* Mock Redis Server Helper *)
let create_mock_redis_server port =
  let server_running = ref true in
  
  let handle_client client_socket =
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
    
    let reader () = 
      let* chunk = Lwt_io.read ~count:4096 ic in
      Printf.printf "Server received: %S\n" chunk;
      Lwt.return chunk
    in
    
    let rec handle_commands () =
      if not !server_running then Lwt.return_unit
      else
        let* result = Resp3.parse_resp_buffered reader in
        Printf.printf "Server parsed: %s\n" 
        (match result with 
         | Ok v -> Resp3.show_resp_value v 
         | Error e -> Printf.sprintf "Error: %s" e);
        match result with
        | Ok (Array (Some [BulkString (Some "PING")])) ->
            Printf.printf "Server sending PONG\n";
            let* () = Lwt_io.write oc (Resp3.serialize_resp3 (SimpleString "PONG")) in
            let* () = Lwt_io.flush oc in
            Printf.printf "PONG sent\n";
            handle_commands ()
        | Ok (Array (Some [BulkString (Some "UNKNOWN")])) ->
            let* () = Lwt_io.write oc (Resp3.serialize_resp3 (SimpleError "ERR unknown command")) in
            let* () = Lwt_io.flush oc in
            handle_commands ()
        | Ok _ ->
            let* () = Lwt_io.write oc (Resp3.serialize_resp3 (SimpleError "ERR invalid command")) in
            let* () = Lwt_io.flush oc in
            handle_commands ()
        | Error _ -> 
            (* Connection probably closed *)
            Lwt.return_unit
    in

    Lwt.finalize 
      handle_commands 
      (fun () -> 
        let* () = Lwt_io.close ic in
        Lwt_io.close oc)
      
  in

  let server_socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.setsockopt server_socket SO_REUSEADDR true;
  let* () = Lwt_unix.bind server_socket (ADDR_INET (Unix.inet_addr_loopback, port)) in
  Lwt_unix.listen server_socket 5;
  
  (* Accept connections in background *)
  let rec accept_loop () =
    if not !server_running then
      Lwt.return_unit
    else
      Lwt.catch
        (fun () ->
          let* (client, _) = Lwt_unix.accept server_socket in
          (* Handle client in background *)
          Lwt.async (fun () -> 
            Lwt.catch
              (fun () -> handle_client client)
              (fun _ -> 
                Lwt.catch
                  (fun () -> Lwt_unix.close client)
                  (fun _ -> Lwt.return_unit)));
          accept_loop ())
        (fun _ -> Lwt.return_unit)
  in
  
  Lwt.async accept_loop;
  
  Lwt.return (server_socket, server_running)

(* Testables for custom types *)
let connection_error_testable = 
  Alcotest.testable 
    (fun ppf e -> Fmt.pf ppf "%s" (Connection.show_connection_error e))
    (fun e1 e2 -> Connection.show_connection_error e1 = Connection.show_connection_error e2)

let resp_value_testable = 
  Alcotest.testable 
    (fun ppf v -> Fmt.pf ppf "%s" (Resp3.show_resp_value v))
    (fun v1 v2 -> Resp3.equal_resp_value v1 v2)

(* Helper to run Lwt tests in Alcotest *)
let lwt_test name f =
  Alcotest.test_case name `Quick (fun () ->
    Lwt_main.run (f ())
  )

(* Test helper that manages server lifecycle *)
let with_mock_server port f =
  let* (server, server_running) = create_mock_redis_server port in
  (* Give server time to start *)
  let* () = Lwt_unix.sleep 0.05 in
  
  Lwt.finalize
    (fun () -> f ())
    (fun () -> 
      server_running := false;
      Lwt.catch
        (fun () -> Lwt_unix.close server)
        (fun _ -> Lwt.return_unit))

(* Individual test functions *)
let test_connection_establishes () =
  with_mock_server 7777 (fun () ->
    let* result = Connection.connect "127.0.0.1" 7777 in
    match result with
    | Ok handle ->
        let* () = Connection.close handle in
        Lwt.return_unit
    | Error e ->
        Alcotest.fail (Printf.sprintf "Connection failed: %s" 
          (Connection.show_connection_error e))
  )

let test_ping_command () =
  with_mock_server 7778 (fun () ->
    let* result = Connection.connect "127.0.0.1" 7778 in
    match result with
    | Ok handle ->
        (* Send PING - the Connection module should handle RESP formatting *)

        let* send_result = Connection.send_command handle 
          (Resp3.serialize_resp3 (Array (Some [BulkString (Some "PING")]))) in
        let* () = match send_result with
          | Error e -> 
              let* () = Connection.close handle in
              Alcotest.fail (Printf.sprintf "Send failed: %s" 
                (Connection.show_connection_error e))
          | Ok () -> 
              Lwt.return_unit 
        in
        
        (* Receive PONG *)
        let* resp = Connection.receive_resp handle in
        let* () = match resp with
          | Error e ->
              let* () = Connection.close handle in
              Alcotest.fail (Printf.sprintf "Receive failed: %s" 
                (Connection.show_connection_error e))
          | Ok (Resp3.SimpleString "PONG") -> 
              Lwt.return_unit
          | Ok other ->
              let* () = Connection.close handle in
              Alcotest.fail (Printf.sprintf "Expected PONG, got: %s" 
                (Resp3.show_resp_value other))
        in
        
        let* () = Connection.close handle in
        Lwt.return_unit
        
    | Error e ->
        Alcotest.fail (Printf.sprintf "Connection failed: %s" 
          (Connection.show_connection_error e))
  )

let test_response_latency () =
  with_mock_server 7779 (fun () ->
    let* result = Connection.connect "127.0.0.1" 7779 in
    match result with
    | Ok handle ->
        (* Warm up connection *)
        let* _ = Connection.send_command handle 
          (Resp3.serialize_resp3 (Array (Some [BulkString (Some "PING")]))) in
        let* _ = Connection.receive_resp handle in
        
        (* Measure latency *)
        let start_time = Unix.gettimeofday () in
        let* send_result = Connection.send_command handle 
          (Resp3.serialize_resp3 (Array (Some [BulkString (Some "PING")]))) in
        let* () = match send_result with
          | Error e -> 
              let* () = Connection.close handle in
              Alcotest.fail (Printf.sprintf "Send failed: %s" 
                (Connection.show_connection_error e))
          | Ok () -> 
              Lwt.return_unit 
        in
        
        let* resp = Connection.receive_resp handle in
        let elapsed = Unix.gettimeofday () -. start_time in
        
        let* () = match resp with
          | Error e ->
              let* () = Connection.close handle in
              Alcotest.fail (Printf.sprintf "Receive failed: %s" 
                (Connection.show_connection_error e))
          | Ok _ ->
              if elapsed > 0.1 then begin
                let* () = Connection.close handle in
                Alcotest.fail (Printf.sprintf "Response too slow: %f seconds" elapsed)
              end else
                Lwt.return_unit
        in
        
        let* () = Connection.close handle in
        Lwt.return_unit
        
    | Error e ->
        Alcotest.fail (Printf.sprintf "Connection failed: %s" 
          (Connection.show_connection_error e))
  )

let test_unknown_command () =
  with_mock_server 7780 (fun () ->
    let* result = Connection.connect "127.0.0.1" 7780 in
    match result with
    | Ok handle ->
        (* Send unknown command *)
        let* send_result = Connection.send_command handle 
          (Resp3.serialize_resp3 (Array (Some [BulkString (Some "UNKNOWN")]))) in
        let* () = match send_result with
          | Error e -> 
              let* () = Connection.close handle in
              Alcotest.fail (Printf.sprintf "Send failed: %s" 
                (Connection.show_connection_error e))
          | Ok () -> 
              Lwt.return_unit 
        in
        
        (* Should receive error *)
        let* resp = Connection.receive_resp handle in
        let* () = match resp with
          | Ok (Resp3.SimpleError msg) when String.length msg >= 3 && String.sub msg 0 3 = "ERR" -> 
              Lwt.return_unit
          | Ok other ->
              let* () = Connection.close handle in
              Alcotest.fail (Printf.sprintf "Expected error, got: %s" 
                (Resp3.show_resp_value other))
          | Error e ->
              let* () = Connection.close handle in
              Alcotest.fail (Printf.sprintf "Receive failed: %s" 
                (Connection.show_connection_error e))
        in
        
        let* () = Connection.close handle in
        Lwt.return_unit
        
    | Error e ->
        Alcotest.fail (Printf.sprintf "Connection failed: %s" 
          (Connection.show_connection_error e))
  )

(* Test suite *)
let connection_tests = [
  lwt_test "connection establishes successfully" test_connection_establishes;
  lwt_test "ping command returns pong" test_ping_command;
  lwt_test "response latency under 100ms" test_response_latency;
  lwt_test "unknown command returns error" test_unknown_command;
]

(* If you want to add more test suites in the same style as your parser tests *)
let make_connection_test name port test_fn =
  lwt_test name (fun () -> with_mock_server port test_fn)

let advanced_connection_tests = [
  make_connection_test "multiple pings" 7781 (fun () ->
    let* result = Connection.connect "127.0.0.1" 7781 in
    match result with
    | Ok handle ->
        (* Send multiple pings *)
        let rec send_pings n =
          if n = 0 then Lwt.return_unit
          else
            let* _ = Connection.send_command handle 
              (Resp3.serialize_resp3 (Array (Some [BulkString (Some "PING")]))) in
            let* resp = Connection.receive_resp handle in
            match resp with
            | Ok (Resp3.SimpleString "PONG") -> send_pings (n - 1)
            | Ok other -> 
                Alcotest.fail (Printf.sprintf "Expected PONG, got: %s" 
                  (Resp3.show_resp_value other))
            | Error e ->
                Alcotest.fail (Printf.sprintf "Receive failed: %s" 
                  (Connection.show_connection_error e))
        in
        let* () = send_pings 5 in
        let* () = Connection.close handle in
        Lwt.return_unit
    | Error e ->
        Alcotest.fail (Printf.sprintf "Connection failed: %s" 
          (Connection.show_connection_error e))
  );
]

(* Main test runner *)
(* let () =
  Alcotest.run "Redis Connection Tests" [
    ("connection", connection_tests);
    ("advanced", advanced_connection_tests);
  ] *)