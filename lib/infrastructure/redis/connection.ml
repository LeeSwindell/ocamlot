open Lwt.Syntax
(* module Buffered = Angstrom.Buffered *)
module Resp3 = Resp3

(* Connection handle only needs the channels for Redis operations *)
type connection_handle = {
  input: Lwt_io.input_channel;
  output: Lwt_io.output_channel;
  socket: Lwt_unix.file_descr;
}

let pp_exn fmt exn = 
  Format.fprintf fmt "%s" (Printexc.to_string exn)

type connection_error =
  | No_addresses of string
  | Connection_failed of exn
  | IO_error of exn
[@@deriving show]

(* let show_connection_error = function
  | No_addresses host -> Printf.sprintf "No_addresses(%s)" host
  | Connection_failed exn -> Printf.sprintf "Connection_failed(%s)" (Printexc.to_string exn)
  | IO_error exn -> Printf.sprintf "IO_error(%s)" (Printexc.to_string exn) *)

let connect host port =
  Lwt.catch
    (fun () ->
      (* Step 1: Create the socket using Lwt_unix *)
      let socket = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
      
      (* Step 2: Configure socket options BEFORE connecting *)
      Lwt_unix.setsockopt socket TCP_NODELAY true;
      Lwt_unix.setsockopt socket SO_KEEPALIVE true;
      
      (* Step 3: Connect the socket *)
      let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
      let* () = Lwt_unix.connect socket addr in
      
      (* Step 4: Create Lwt_io channels from the connected socket *)
      (* Important: Only the output channel owns the socket for closing *)
      let input = Lwt_io.of_fd ~mode:Lwt_io.input ~close:(fun () -> Lwt.return_unit) socket in
      let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in
      
      Lwt.return_ok { input; output; socket })
    (fun exn -> 
      Lwt.return_error (Connection_failed exn))

(* All I/O operations use Lwt_io exclusively *)
let send_command handle data =
  Lwt.catch
    (fun () ->
      let* () = Lwt_io.write handle.output data in
      let* () = Lwt_io.flush handle.output in
      Lwt.return_ok ())
    (fun exn -> Lwt.return_error (IO_error exn))

(* Read operations using Lwt_io's buffered reading *)
let read_line handle =
  Lwt.catch
    (fun () ->
      let* line = Lwt_io.read_line handle.input in
      Lwt.return_ok line)
    (fun exn -> Lwt.return_error (IO_error exn))

let read_exact handle count =
  Lwt.catch
    (fun () ->
      let* data = Lwt_io.read ~count handle.input in
      Lwt.return_ok data)
    (fun exn -> Lwt.return_error (IO_error exn))

(* RESP parsing using Lwt_io *)
let receive_resp handle = 
  let* result = Resp3.parse_resp_from_channel handle.input in
  match result with
  | Ok value -> Lwt.return_ok value
  | Error msg -> Lwt.return_error (IO_error (Failure msg))

(* Clean shutdown using Lwt_io *)
let close handle =
  (* Close input first (doesn't close socket due to ~close parameter) *)
  let* () = Lwt_io.close handle.input in
  (* Close output, which will close the underlying socket *)
  Lwt_io.close handle.output


(* Higher-level helper with automatic resource cleanup *)
let with_connection host port f =
  let* conn_result = connect host port in
  match conn_result with
  | Error e -> Lwt.return_error e
  | Ok handle ->
      Lwt.finalize
        (fun () -> f handle)
        (fun () -> close handle)

(* Pipeline support using Lwt_io's buffering *)
(* let pipeline handle commands =
  let open Lwt_result.Syntax in
  (* Send all commands *)
  let* () = 
    Lwt_list.iter_s (fun cmd ->
      send_command handle cmd |> Lwt_result.map ignore
    ) commands |> Lwt_result.ok
  in
  (* Read all responses *)
  Lwt_list.map_s (fun _ -> receive_resp handle) commands *)