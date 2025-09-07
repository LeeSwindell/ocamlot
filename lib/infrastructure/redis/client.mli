(** Redis client with connection pooling and high-level commands *)

(** {1 Types} *)

(** Opaque Redis client handle with connection pool *)
type t

type client_config =
  {host: string; port: int; pool_size: int; connection_timeout: float}

type client_error =
  | Connection_error of Connection.connection_error
  | Pool_exhausted
  | Redis_error of string  (** Redis returned an error response *)
  | Parse_error of string

(** {1 Client Management} *)

val create : client_config -> (t, client_error) result Lwt.t
(** Create a new Redis client with connection pool *)

val close : t -> unit Lwt.t
(** Close all connections in the pool *)

val with_client :
     client_config
  -> (t -> ('a, client_error) result Lwt.t)
  -> ('a, client_error) result Lwt.t
(** Resource-safe client usage *)

(** {1 Redis Commands} *)

val get : t -> string -> (string option, client_error) result Lwt.t
(** Basic key-value operations *)

val set : t -> string -> string -> (unit, client_error) result Lwt.t

val del : t -> string list -> (int, client_error) result Lwt.t

val hget :
  t -> string -> string -> (string option, client_error) result Lwt.t
(** Hash operations *)

val hset :
  t -> string -> string -> string -> (bool, client_error) result Lwt.t

val lpush : t -> string -> string list -> (int, client_error) result Lwt.t
(** List operations *)

val rpop : t -> string -> (string option, client_error) result Lwt.t

val ping : t -> (string, client_error) result Lwt.t
(** Utility commands *)

val info : t -> (string, client_error) result Lwt.t

(** {1 Stream Operations} *)

(** Stream entry: (entry_id, [(field, value); ...]) *)
type stream_entry = string * (string * string) list

val xlen : t -> string -> (int, client_error) result Lwt.t
(** Get the number of entries in a stream *)

val xrange : t -> string -> string -> string -> ?count:int -> unit -> (stream_entry list, client_error) result Lwt.t
(** Get stream entries in a range. Use "-" and "+" for min/max IDs. 
    Returns list of (entry_id, field_value_pairs) *)

val xrevrange : t -> string -> string -> string -> ?count:int -> unit -> (stream_entry list, client_error) result Lwt.t
(** Get stream entries in reverse order. Arguments: key end_id start_id.
    Use "+" and "-" for max/min IDs. Returns list of (entry_id, field_value_pairs) in reverse order *)

(** {1 Low-level Operations} *)

val execute :
  t -> Resp3.resp_value -> (Resp3.resp_value, client_error) result Lwt.t
(** Execute arbitrary RESP3 command *)

val pipeline :
     t
  -> Resp3.resp_value list
  -> (Resp3.resp_value list, client_error) result Lwt.t
(** Execute multiple commands in a pipeline *)
