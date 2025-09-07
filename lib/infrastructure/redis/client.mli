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

(** XREAD result: [(stream_name, [entries]); ...] *)
type xread_result = (string * stream_entry list) list

(** XREAD stream specification *)
type xread_stream = {
  key: string;
  id: string;
}

val xlen : t -> string -> (int, client_error) result Lwt.t
(** Get the number of entries in a stream *)

val xrange : t -> string -> string -> string -> ?count:int -> unit -> (stream_entry list, client_error) result Lwt.t
(** Get stream entries in a range. Use "-" and "+" for min/max IDs. 
    Returns list of (entry_id, field_value_pairs) *)

val xrevrange : t -> string -> string -> string -> ?count:int -> unit -> (stream_entry list, client_error) result Lwt.t
(** Get stream entries in reverse order. Arguments: key end_id start_id.
    Use "+" and "-" for max/min IDs. Returns list of (entry_id, field_value_pairs) in reverse order *)

val xread : t -> ?count:int -> ?block:int -> xread_stream list -> (xread_result, client_error) result Lwt.t
(** Read from multiple streams. 
    - count: max entries per stream
    - block: timeout in milliseconds (0 = infinite)
    - streams: list of {key; id} where id is last seen ID ("$" for new only, "0-0" for all)
    Returns [(stream_name, entries)] for streams with new data *)

val xreadgroup : t -> string -> string -> ?count:int -> ?block:int -> ?noack:bool -> xread_stream list -> (xread_result, client_error) result Lwt.t
(** Read from multiple streams using consumer groups.
    - group_name: consumer group name
    - consumer: consumer name within group
    - count: max entries per stream
    - block: timeout in milliseconds (0 = infinite)
    - noack: if true, don't add messages to PEL (no acknowledgment needed)
    - streams: list of {key; id} where id is ">" for new messages or specific ID for pending
    Returns [(stream_name, entries)] for streams with new data delivered to this consumer *)

(** {1 Consumer Group Operations} *)

val xgroup_create : t -> string -> string -> string -> ?mkstream:bool -> ?entriesread:int -> unit -> (unit, client_error) result Lwt.t
(** Create a consumer group for a stream.
    - key: stream name
    - groupname: consumer group name  
    - id: starting message ID ("0-0" for all messages, "$" for new only)
    - mkstream: if true, create the stream if it doesn't exist
    - entriesread: approximate number of entries read by group (for lag tracking)
    Returns unit on success *)

val xgroup_destroy : t -> string -> string -> (bool, client_error) result Lwt.t
(** Destroy a consumer group for a stream.
    - key: stream name
    - groupname: consumer group name
    Returns true if group was destroyed, false if group didn't exist *)

val xack : t -> string -> string -> string list -> (int, client_error) result Lwt.t
(** Acknowledge processed messages in a consumer group, removing them from the Pending Entries List (PEL).
    - key: stream name
    - group_name: consumer group name
    - ids: list of entry IDs to acknowledge (e.g., ["1526985054069-0"; "1526985055000-1"])
    Returns the number of messages that were successfully acknowledged *)

val xackdel : t -> string -> string -> string list -> ?ref_handling:Commands.xtrim_ref_handling -> unit -> (int list, client_error) result Lwt.t
(** Acknowledge and conditionally delete entries from a stream in a single atomic operation.
    
    Usage: xackdel client "stream" "group" ["id1"; "id2"] ~ref_handling:Delref ()
    
    Parameters:
    - key: stream name
    - group_name: consumer group name
    - ids: list of entry IDs to acknowledge and delete (e.g., ["1526985054069-0"; "1526985055000-1"])
    - ref_handling: how to handle consumer group references (default: Keepref)
      * Keepref (default): Acknowledge entries and delete from stream but preserve PEL references
      * Delref: Acknowledge entries, delete from stream, and remove all PEL references across groups
      * Acked: Acknowledge entries and only delete entries acknowledged by all consumer groups
    
    Returns a list of integers, one for each requested ID:
    - 1: Entry was successfully acknowledged and deleted from the stream
    - -1: ID does not exist in the stream (or stream doesn't exist)
    - 2: Entry was acknowledged but not deleted due to dangling references (ACKED option only)
    
    This command combines XACK and XDEL functionality, providing atomic acknowledge-and-delete
    operations with fine-grained control over consumer group reference handling.
    
    Examples:
    - Basic ack+delete: xackdel client "stream" "group" ["id1"; "id2"] ()
    - Clean ack+delete: xackdel client "stream" "group" ["id1"] ~ref_handling:Delref ()
    - Multi-group safe: xackdel client "stream" "group" ["id1"] ~ref_handling:Acked () *)

(** {1 XPENDING Operations} *)

(** XPENDING summary information *)
type xpending_summary = {
  count: int;                    (** Total number of pending messages *)
  min_id: string option;         (** Smallest pending message ID (None if no pending messages) *)
  max_id: string option;         (** Greatest pending message ID (None if no pending messages) *)
  consumers: (string * int) list; (** List of (consumer_name, pending_count) pairs *)
}

(** XPENDING extended entry information *)
type xpending_entry = {
  id: string;            (** Message ID *)
  consumer: string;      (** Consumer that owns this pending message *)
  idle_time: int;        (** Milliseconds since last delivery *)
  delivery_count: int;   (** Number of times this message has been delivered *)
}

(** XPENDING result type *)
type xpending_result =
  | Summary of xpending_summary        (** Summary form: overall PEL statistics *)
  | Extended of xpending_entry list    (** Extended form: detailed pending message info *)

val xpending : t -> string -> string -> ?range:Commands.xpending_range -> unit -> (xpending_result, client_error) result Lwt.t
(** View pending messages in a consumer group (Pending Entries List - PEL).
    
    Usage forms:
    1. Summary form: xpending client "stream" "group" () 
       Returns overall statistics: total count, min/max IDs, and consumer list
       
    2. Extended form: xpending client "stream" "group" ~range:(Extended {start="-"; end_="+"; count=10; consumer=None; idle=None}) ()
       Returns detailed information for specific pending messages
       
    Extended form parameters:
    - start: starting message ID (use "-" for oldest, specific ID, or "(exclusive_id")
    - end_: ending message ID (use "+" for newest or specific ID) 
    - count: maximum number of entries to return
    - consumer: optional consumer name to filter results
    - idle: optional minimum idle time in milliseconds to filter stale messages
    
    Examples:
    - Basic summary: xpending client "mystream" "mygroup" ()
    - All pending: xpending client "stream" "group" ~range:(Extended {start="-"; end_="+"; count=100; consumer=None; idle=None}) ()
    - Stale messages: xpending client "stream" "group" ~range:(Extended {start="-"; end_="+"; count=10; consumer=None; idle=Some 30000}) ()
    - Consumer-specific: xpending client "stream" "group" ~range:(Extended {start="-"; end_="+"; count=50; consumer=Some "consumer1"; idle=None}) ()
    
    Returns Summary or Extended result based on the range parameter *)

(** {1 XCLAIM Operations} *)

(** XCLAIM result type - depends on JUSTID option *)
type xclaim_result =
  | ClaimEntries of stream_entry list  (** Normal mode: returns full stream entries with data *)
  | ClaimIds of string list            (** JUSTID mode: returns only message IDs *)

val xclaim : t -> string -> string -> string -> int -> string list -> ?options:Commands.xclaim_option list -> unit -> (xclaim_result, client_error) result Lwt.t
(** Claim ownership of pending messages from another consumer.
    
    Basic usage: xclaim client "stream" "group" "new_consumer" 3600000 ["msg-id-1"; "msg-id-2"] ()
    
    Parameters:
    - key: stream name
    - group_name: consumer group name  
    - consumer: new consumer to transfer ownership to
    - min_idle_time: minimum idle time in milliseconds (only claim messages idle longer than this)
    - ids: list of message IDs to attempt to claim
    - options: optional list of XCLAIM options
    
    Options (Commands.xclaim_option):
    - Idle ms: set idle time to specific milliseconds
    - Time unix_ms: set idle time to specific Unix timestamp (milliseconds)
    - RetryCount count: set retry counter to specific value
    - Force: create PEL entry even if message not currently in PEL
    - JustId: return only message IDs, don't increment retry counter (changes return type)
    - LastId id: internal use for AOF/replication
    
    Returns:
    - ClaimEntries: full stream entries (id, field-value pairs) when JUSTID not used
    - ClaimIds: just message IDs when JUSTID option used
    
    Examples:
    - Basic claim: xclaim client "stream" "group" "consumer2" 60000 ["1526985054069-0"] ()
    - With options: xclaim client "stream" "group" "consumer2" 60000 ids ~options:[Idle 0; RetryCount 1] ()
    - Just IDs: xclaim client "stream" "group" "consumer2" 60000 ids ~options:[JustId] () *)

(** {1 XAUTOCLAIM Operations} *)

(** XAUTOCLAIM result for normal mode *)
type xautoclaim_result = {
  next_cursor: string;                    (** Cursor for next XAUTOCLAIM call *)
  claimed_entries: stream_entry list;     (** Successfully claimed messages *)
  deleted_ids: string list;               (** Message IDs that were deleted from PEL *)
}

(** XAUTOCLAIM result for JUSTID mode *)
type xautoclaim_justid_result = {
  next_cursor: string;                    (** Cursor for next XAUTOCLAIM call *)
  claimed_ids: string list;               (** Successfully claimed message IDs *)
  deleted_ids: string list;               (** Message IDs that were deleted from PEL *)
}

(** XAUTOCLAIM response type - depends on justid parameter *)
type xautoclaim_response =
  | AutoClaimEntries of xautoclaim_result      (** Normal mode: returns full entries *)
  | AutoClaimIds of xautoclaim_justid_result   (** JUSTID mode: returns only IDs *)

val xautoclaim : t -> string -> string -> string -> int -> string -> ?count:int option -> ?justid:bool -> unit -> (xautoclaim_response, client_error) result Lwt.t
(** Automatically claim stale pending messages using cursor-based scanning.
    
    This is equivalent to calling XPENDING + XCLAIM but more efficient for bulk operations.
    
    Basic usage: xautoclaim client "stream" "group" "consumer" 3600000 "0-0" ()
    
    Parameters:
    - key: stream name
    - group_name: consumer group name
    - consumer: consumer to transfer ownership to  
    - min_idle_time: minimum idle time in milliseconds
    - start: starting cursor ("0-0" to start from beginning, or cursor from previous call)
    - count: optional maximum number of messages to claim (default: Redis default ~100)
    - justid: if true, return only IDs without message data (default: false)
    
    Returns a 3-element structure:
    - next_cursor: use this as 'start' for subsequent calls ("0-0" means scan complete)
    - claimed_entries/claimed_ids: successfully claimed messages
    - deleted_ids: message IDs that were found in PEL but deleted from stream
    
    Scanning pattern:
    ```
    let rec claim_all cursor =
      let* result = xautoclaim client "stream" "group" "consumer" 60000 cursor ~count:(Some 50) () in
      match result with
      | Ok (AutoClaimEntries {next_cursor; claimed_entries; _}) ->
          (* Process claimed_entries *)
          if next_cursor = "0-0" then 
            (* Scan complete *) ()
          else 
            claim_all next_cursor
      | Error e -> (* handle error *)
    ```
    
    Examples:
    - Basic: xautoclaim client "stream" "group" "consumer" 60000 "0-0" ()
    - Limited: xautoclaim client "stream" "group" "consumer" 60000 cursor ~count:(Some 25) ()
    - IDs only: xautoclaim client "stream" "group" "consumer" 60000 "0-0" ~justid:true () *)

val xdel : t -> string -> string list -> (int, client_error) result Lwt.t
(** Delete one or more entries from a stream.
    - key: stream name
    - ids: list of entry IDs to delete (e.g., ["1526985054069-0"; "1526985055000-1"])
    Returns the number of entries that were actually deleted (may be less than requested if some IDs don't exist) *)

val xdelex : t -> string -> string list -> ?ref_handling:Commands.xtrim_ref_handling -> unit -> (int list, client_error) result Lwt.t
(** Extended delete with fine-grained control over consumer group references.
    
    Usage: xdelex client "stream" ["id1"; "id2"] ~ref_handling:Delref ()
    
    Parameters:
    - key: stream name
    - ids: list of entry IDs to delete (e.g., ["1526985054069-0"; "1526985055000-1"])
    - ref_handling: how to handle consumer group references (default: Keepref)
      * Keepref (default): Delete entries but preserve PEL references (like XDEL)
      * Delref: Delete entries and remove all PEL references across consumer groups
      * Acked: Only delete entries that were acknowledged by all consumer groups
    
    Returns a list of integers, one for each requested ID:
    - -1: ID does not exist in the stream (or stream doesn't exist)
    - 1: Entry was successfully deleted from the stream  
    - 2: Entry not deleted but dangling references exist (ACKED option only)
    
    Examples:
    - Basic deletion: xdelex client "stream" ["id1"; "id2"] ()
    - Clean deletion: xdelex client "stream" ["id1"] ~ref_handling:Delref ()
    - Acked-only: xdelex client "stream" ["id1"] ~ref_handling:Acked () *)

val xtrim : t -> string -> Commands.xtrim_strategy -> ?operator:Commands.xtrim_operator -> ?limit:int -> ?ref_handling:Commands.xtrim_ref_handling -> unit -> (int, client_error) result Lwt.t
(** Trim a stream by removing older entries.
    - key: stream name
    - strategy: trimming strategy (Maxlen count or Minid id)
    - operator: Exact (=) or Approximate (~) trimming (default: Exact)
    - limit: maximum number of entries to examine during trimming
    - ref_handling: how to handle consumer group references (Keepref, Delref, Acked)
    Returns the number of entries that were removed from the stream *)

(** {1 Low-level Operations} *)

val execute :
  t -> Resp3.resp_value -> (Resp3.resp_value, client_error) result Lwt.t
(** Execute arbitrary RESP3 command *)

val pipeline :
     t
  -> Resp3.resp_value list
  -> (Resp3.resp_value list, client_error) result Lwt.t
(** Execute multiple commands in a pipeline *)
