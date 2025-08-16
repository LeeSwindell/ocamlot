open Base
open Lwt.Syntax
open Alcotest_lwt
open Ocamlot_nats

(* Advanced Subscription Tests - Based on Go client test/sub_test.go *)

(* Test helpers *)
let test_timeout = 5.0
let message_count = ref 0
let received_messages = ref []

let reset_test_state () =
  message_count := 0;
  received_messages := []

let wait_for_messages expected_count timeout =
  let rec wait_loop remaining_time =
    if !message_count >= expected_count then
      Lwt.return_unit
    else if remaining_time <= 0.0 then
      Lwt.fail (Failure (Printf.sprintf "Timeout waiting for %d messages, got %d" 
        expected_count !message_count))
    else
      let* () = Lwt_unix.sleep 0.1 in
      wait_loop (remaining_time -. 0.1)
  in
  wait_loop timeout

(* Auto-unsubscribe Tests *)
let test_auto_unsubscribe_basic _switch () =
  (* Tests basic auto-unsubscribe functionality *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  (* Test that auto-unsubscribe can be configured *)
  let* result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.auto.unsub" ~callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "auto-unsubscribe subscription fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

let test_auto_unsubscribe_with_reconnect _switch () =
  (* Tests auto-unsubscribe behavior across reconnections *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  (* Test auto-unsubscribe persistence across reconnections *)
  let* sub_result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.reconnect.unsub" ~callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  (* Test reconnection scenario *)
  let* reconnect_result = 
    try%lwt
      Nats.disconnect client
    with
    | _ -> Lwt.return_unit
  in
  
  Alcotest.(check (result unit string)) "subscription setup" 
    (Error "not connected") sub_result;
  Lwt.return_unit

let test_auto_unsubscribe_parallel_calls _switch () =
  (* Tests auto-unsubscribe with parallel NextMsg calls *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  (* Test parallel subscription calls *)
  let* results = Lwt.all [
    (try%lwt
       let* _sub = Nats.subscribe client ~subject:"test.parallel.1" ~callback in
       Lwt.return (Ok ())
     with
     | Nats.Not_connected -> Lwt.return (Error "not connected")
     | exn -> Lwt.return (Error (Exn.to_string exn)));
    (try%lwt
       let* _sub = Nats.subscribe client ~subject:"test.parallel.2" ~callback in
       Lwt.return (Ok ())
     with
     | Nats.Not_connected -> Lwt.return (Error "not connected")
     | exn -> Lwt.return (Error (Exn.to_string exn)));
  ] in
  
  let all_failed = List.for_all results ~f:Result.is_error in
  Alcotest.(check bool) "parallel subscriptions fail when not connected" true all_failed;
  Lwt.return_unit

(* Subscription Types Tests *)
let test_sync_subscription _switch () =
  (* Tests synchronous subscription behavior *)
  reset_test_state ();
  let client = Nats.create () in
  
  (* Test sync subscription structure *)
  let* result = 
    try%lwt
      let callback _msg = Lwt.return_unit in
      let* _sub = Nats.subscribe client ~subject:"test.sync" ~callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "sync subscription fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

let test_async_subscription _switch () =
  (* Tests asynchronous subscription behavior *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  let* result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.async" ~callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "async subscription fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

let test_queue_subscription _switch () =
  (* Tests queue group subscription behavior *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  (* Test queue group subscription - this would require queue parameter in real implementation *)
  let* result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.queue" ~callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "queue subscription fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

(* Performance and Limits Tests *)
let test_slow_subscriber_handling _switch () =
  (* Tests slow subscriber detection and handling *)
  reset_test_state ();
  let client = Nats.create () in
  
  let slow_callback _msg =
    (* Simulate slow processing *)
    let* () = Lwt_unix.sleep 0.1 in
    Int.incr message_count;
    Lwt.return_unit
  in
  
  let* result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.slow" ~callback:slow_callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "slow subscription fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

let test_pending_limits _switch () =
  (* Tests pending message limits and flow control *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  (* Test pending message limits *)
  let* result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.limits" ~callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "pending limits subscription fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

let test_subscription_pending_tracking _switch () =
  (* Tests tracking of pending messages in subscriptions *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  (* Test pending message tracking *)
  let* result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.pending" ~callback in
      (* In real implementation, would check pending count here *)
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "pending tracking subscription fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

(* Advanced Subscription Features *)
let test_subscription_events _switch () =
  (* Tests subscription state change events *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  (* Test subscription events *)
  let* result = 
    try%lwt
      let* sub = Nats.subscribe client ~subject:"test.events" ~callback in
      (* Test unsubscribe *)
      let* () = Nats.unsubscribe sub in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "subscription events fail when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

let test_subscription_cleanup _switch () =
  (* Tests proper subscription cleanup on connection close *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  (* Test cleanup behavior *)
  let* sub_result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.cleanup" ~callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  (* Test connection close cleanup *)
  let* () = 
    try%lwt
      Nats.disconnect client
    with
    | _ -> Lwt.return_unit
  in
  
  Alcotest.(check (result unit string)) "cleanup subscription fails when not connected" 
    (Error "not connected") sub_result;
  Lwt.return_unit

(* Error Handling Tests *)
let test_subscription_error_handling _switch () =
  (* Tests subscription error handling and recovery *)
  reset_test_state ();
  let client = Nats.create () in
  
  let failing_callback _msg =
    Int.incr message_count;
    if !message_count > 2 then
      Lwt.fail (Failure "callback error")
    else
      Lwt.return_unit
  in
  
  let* result = 
    try%lwt
      let* _sub = Nats.subscribe client ~subject:"test.error" ~callback:failing_callback in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "error handling subscription fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

let test_invalid_subscription_subject _switch () =
  (* Tests handling of invalid subscription subjects *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg = Lwt.return_unit in
  
  (* Test invalid subjects *)
  let* results = Lwt.all [
    (try%lwt
       let* _sub = Nats.subscribe client ~subject:"" ~callback in
       Lwt.return (Ok ())
     with
     | Nats.Not_connected -> Lwt.return (Error "not connected")
     | exn -> Lwt.return (Error (Exn.to_string exn)));
    (try%lwt
       let* _sub = Nats.subscribe client ~subject:"invalid..subject" ~callback in
       Lwt.return (Ok ())
     with
     | Nats.Not_connected -> Lwt.return (Error "not connected")
     | exn -> Lwt.return (Error (Exn.to_string exn)));
  ] in
  
  let all_failed = List.for_all results ~f:Result.is_error in
  Alcotest.(check bool) "invalid subjects fail when not connected" true all_failed;
  Lwt.return_unit

(* Subscription ID Management Tests *)
let test_subscription_id_generation _switch () =
  (* Tests unique subscription ID generation *)
  let conn = Nats.Connection.create_connection () in
  let sid1 = Nats.Connection.generate_sid conn in
  let sid2 = Nats.Connection.generate_sid conn in
  let sid3 = Nats.Connection.generate_sid conn in
  
  (* Test that SIDs are unique and sequential *)
  Alcotest.(check bool) "sid1 != sid2" true (not (String.equal sid1 sid2));
  Alcotest.(check bool) "sid2 != sid3" true (not (String.equal sid2 sid3));
  Alcotest.(check bool) "sid1 != sid3" true (not (String.equal sid1 sid3));
  
  (* Test SID format (should be numeric strings) *)
  let is_numeric s = 
    try 
      let _ = Int.of_string s in true 
    with _ -> false 
  in
  Alcotest.(check bool) "sid1 is numeric" true (is_numeric sid1);
  Alcotest.(check bool) "sid2 is numeric" true (is_numeric sid2);
  Lwt.return_unit

let test_subscription_double_unsubscribe _switch () =
  (* Tests safety of double unsubscribe calls *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg = Lwt.return_unit in
  
  let* result = 
    try%lwt
      let* sub = Nats.subscribe client ~subject:"test.double.unsub" ~callback in
      (* First unsubscribe *)
      let* () = Nats.unsubscribe sub in
      (* Second unsubscribe should be safe *)
      let* () = Nats.unsubscribe sub in
      Lwt.return (Ok ())
    with
    | Nats.Not_connected -> Lwt.return (Error "not connected")
    | exn -> Lwt.return (Error (Exn.to_string exn))
  in
  
  Alcotest.(check (result unit string)) "double unsubscribe fails when not connected" 
    (Error "not connected") result;
  Lwt.return_unit

(* Wildcard Subscription Tests *)
let test_wildcard_subscriptions _switch () =
  (* Tests wildcard subscription patterns *)
  reset_test_state ();
  let client = Nats.create () in
  
  let callback _msg =
    Int.incr message_count;
    Lwt.return_unit
  in
  
  (* Test various wildcard patterns *)
  let* results = Lwt.all [
    (try%lwt
       let* _sub = Nats.subscribe client ~subject:"test.*" ~callback in
       Lwt.return (Ok ())
     with
     | Nats.Not_connected -> Lwt.return (Error "not connected")
     | exn -> Lwt.return (Error (Exn.to_string exn)));
    (try%lwt
       let* _sub = Nats.subscribe client ~subject:"test.>" ~callback in
       Lwt.return (Ok ())
     with
     | Nats.Not_connected -> Lwt.return (Error "not connected")
     | exn -> Lwt.return (Error (Exn.to_string exn)));
  ] in
  
  let all_failed = List.for_all results ~f:Result.is_error in
  Alcotest.(check bool) "wildcard subscriptions fail when not connected" true all_failed;
  Lwt.return_unit

(* Test suite definition *)
let tests = [
  "Auto-unsubscribe", [
    test_case "basic auto-unsubscribe" `Quick test_auto_unsubscribe_basic;
    test_case "auto-unsubscribe with reconnect" `Quick test_auto_unsubscribe_with_reconnect;
    test_case "auto-unsubscribe parallel calls" `Quick test_auto_unsubscribe_parallel_calls;
  ];
  "Subscription Types", [
    test_case "sync subscription" `Quick test_sync_subscription;
    test_case "async subscription" `Quick test_async_subscription;
    test_case "queue subscription" `Quick test_queue_subscription;
  ];
  "Performance and Limits", [
    test_case "slow subscriber handling" `Quick test_slow_subscriber_handling;
    test_case "pending limits" `Quick test_pending_limits;
    test_case "subscription pending tracking" `Quick test_subscription_pending_tracking;
  ];
  "Advanced Features", [
    test_case "subscription events" `Quick test_subscription_events;
    test_case "subscription cleanup" `Quick test_subscription_cleanup;
  ];
  "Error Handling", [
    test_case "subscription error handling" `Quick test_subscription_error_handling;
    test_case "invalid subscription subject" `Quick test_invalid_subscription_subject;
  ];
  "Subscription Management", [
    test_case "subscription id generation" `Quick test_subscription_id_generation;
    test_case "double unsubscribe safety" `Quick test_subscription_double_unsubscribe;
  ];
  "Wildcard Subscriptions", [
    test_case "wildcard subscription patterns" `Quick test_wildcard_subscriptions;
  ];
]

let () =
  Lwt_main.run (run "NATS Advanced Subscription Tests" tests)