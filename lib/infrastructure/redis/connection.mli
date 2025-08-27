(* redis_connection.mli *)

(** Redis connection management with automatic resource cleanup and error handling *)

(** {1 Types} *)

(** Handle to an active Redis connection *)
type connection_handle

(** Possible connection errors *)
type connection_error =
  | No_addresses of string     (** Hostname resolution failed *)
  | Connection_failed of exn    (** TCP connection failed *)
  | IO_error of exn            (** I/O operation failed *)

val show_connection_error: connection_error -> string

(** {1 Connection Management} *)

(** [connect host port] establishes a TCP connection to Redis server.
    @param host IP address as string (e.g., "127.0.0.1")
    @param port Port number
    @return Ok connection_handle or Error with reason *)
val connect : string -> int -> (connection_handle, connection_error) result Lwt.t

(** [close handle] cleanly shuts down the connection.
    Flushes output buffer and closes both channels. *)
val close : connection_handle -> unit Lwt.t

(** [with_connection host port f] opens a connection, runs [f] with it,
    and guarantees cleanup even if [f] fails.
    
    Example:
    {[
      with_connection "127.0.0.1" 6379 (fun conn ->
        send_command conn "PING\r\n"
      )
    ]}
*)
val with_connection : 
  string -> 
  int -> 
  (connection_handle -> ('a, connection_error) result Lwt.t) -> 
  ('a, connection_error) result Lwt.t

(** {1 I/O Operations} *)

(** [send_command handle data] sends raw data to Redis.
    Automatically flushes the output buffer. *)
val send_command : 
  connection_handle -> 
  string -> 
  (unit, connection_error) result Lwt.t

(** [read_line handle] reads until '\n' or '\r\n'.
    Used for reading RESP simple strings, errors, and integers. *)
val read_line : 
  connection_handle -> 
  (string, connection_error) result Lwt.t

(** [read_exact handle n] reads exactly [n] bytes.
    Used for reading RESP bulk string payloads. *)
val read_exact : 
  connection_handle -> 
  int -> 
  (string, connection_error) result Lwt.t

(** [receive_resp handle] reads and parses a complete RESP3 value.
    Handles all RESP3 types including nested arrays. *)
val receive_resp : 
  connection_handle -> 
  (Resp3.resp_value, connection_error) result Lwt.t

(** {1 Pipeline Support} *)

(* Uncomment when implementing pipelining:
(** [pipeline handle commands] sends multiple commands and collects all responses.
    More efficient than sequential send/receive for batch operations. *)
val pipeline : 
  connection_handle -> 
  string list -> 
  (Resp3.resp_value list, connection_error) result Lwt.t
*)