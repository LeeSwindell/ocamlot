open Lwt.Syntax

type client_config =
  {host: string; port: int; pool_size: int; connection_timeout: float}

type client_error =
  | Connection_error of Connection.connection_error
  | Pool_exhausted
  | Redis_error of string
  | Parse_error of string

(* Connection pool implementation *)
module ConnectionPool = struct
  type t =
    { available: Connection.connection_handle Queue.t
    ; mutable in_use: Connection.connection_handle ref list
    ; config: client_config
    ; mutex: Lwt_mutex.t }

  let create config =
    let pool =
      { available= Queue.create ()
      ; in_use= []
      ; config
      ; mutex= Lwt_mutex.create () }
    in
    (* Pre-populate pool *)
    let rec create_connections n acc =
      if n <= 0 then Lwt.return (Ok acc)
      else
        let* conn_result = Connection.connect config.host config.port in
        match conn_result with
        | Error e -> Lwt.return (Error (Connection_error e))
        | Ok conn ->
            Queue.push conn pool.available ;
            create_connections (n - 1) (conn :: acc)
    in
    let* result = create_connections config.pool_size [] in
    match result with
    | Error e -> Lwt.return (Error e)
    | Ok _ -> Lwt.return (Ok pool)

  let acquire pool =
    Lwt_mutex.with_lock pool.mutex (fun () ->
        if Queue.is_empty pool.available then
          Lwt.return (Error Pool_exhausted)
        else
          let conn = Queue.pop pool.available in
          let conn_ref = ref conn in
          pool.in_use <- conn_ref :: pool.in_use ;
          Lwt.return (Ok conn) )

  let release pool conn =
    Lwt_mutex.with_lock pool.mutex (fun () ->
        Queue.push conn pool.available ;
        pool.in_use <- List.filter (fun r -> !r != conn) pool.in_use ;
        Lwt.return () )

  let close_all pool =
    let* connections =
      Lwt_mutex.with_lock pool.mutex (fun () ->
          let all_connections = Queue.to_seq pool.available |> List.of_seq in
          Queue.clear pool.available ;
          Lwt.return all_connections )
    in
    Lwt_list.iter_p Connection.close connections
end

(* Client implementation *)
type t = {pool: ConnectionPool.t; config: client_config}

let create config =
  let* pool_result = ConnectionPool.create config in
  match pool_result with
  | Error e -> Lwt.return (Error e)
  | Ok pool -> Lwt.return (Ok {pool; config})

let close client = ConnectionPool.close_all client.pool

let with_client config f =
  let* client_result = create config in
  match client_result with
  | Error e -> Lwt.return (Error e)
  | Ok client -> Lwt.finalize (fun () -> f client) (fun () -> close client)

(* Core operation: borrow connection, execute, return *)
let with_connection client f =
  let* conn_result = ConnectionPool.acquire client.pool in
  match conn_result with
  | Error e -> Lwt.return (Error e)
  | Ok conn ->
      Lwt.finalize
        (fun () -> f conn)
        (fun () -> ConnectionPool.release client.pool conn)

(* Execute RESP3 command *)
let execute client command =
  with_connection client (fun conn ->
      let serialized = Resp3.serialize_resp3 command in
      let* send_result = Connection.send_command conn serialized in
      match send_result with
      | Error e -> Lwt.return (Error (Connection_error e))
      | Ok () -> (
          let* resp_result = Connection.receive_resp conn in
          match resp_result with
          | Error e -> Lwt.return (Error (Connection_error e))
          | Ok response -> (
            (* Check for Redis errors *)
            match response with
            | Resp3.SimpleError msg -> Lwt.return (Error (Redis_error msg))
            | _ -> Lwt.return (Ok response) ) ) )

let pipeline client commands =
  with_connection client (fun conn ->
      (* Send all commands *)
      let* send_results =
        Lwt_list.map_s
          (fun cmd ->
            let serialized = Resp3.serialize_resp3 cmd in
            Connection.send_command conn serialized )
          commands
      in
      (* Check all sends succeeded *)
      let rec check_sends = function
        | [] -> Ok ()
        | Error e :: _ -> Error (Connection_error e)
        | Ok () :: rest -> check_sends rest
      in
      match check_sends send_results with
      | Error e -> Lwt.return (Error e)
      | Ok () -> (
          (* Read all responses *)
          let* responses =
            Lwt_list.map_s (fun _ -> Connection.receive_resp conn) commands
          in
          (* Convert to result list *)
          let rec collect_responses acc = function
            | [] -> Ok (List.rev acc)
            | Error e :: _ -> Error (Connection_error e)
            | Ok resp :: rest -> collect_responses (resp :: acc) rest
          in
          match collect_responses [] responses with
          | Error e -> Lwt.return (Error e)
          | Ok responses -> Lwt.return (Ok responses) ) )

(* High-level Redis commands *)

let ping client =
  let command = Commands.ping () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.SimpleString s) -> Lwt.return (Ok s)
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected SimpleString, got " ^ Resp3.show_resp_value resp) )
        )

let get client key =
  let command = Commands.get key in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString None) -> Lwt.return (Ok None)
  | Ok (Resp3.BulkString (Some value)) -> Lwt.return (Ok (Some value))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected BulkString, got " ^ Resp3.show_resp_value resp) ) )

let set client key value =
  let command = Commands.set key value () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.SimpleString "OK") -> Lwt.return (Ok ())
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error ("Expected OK, got " ^ Resp3.show_resp_value resp))
        )

let del client keys =
  let command = Commands.del keys in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer count) -> Lwt.return (Ok (Int64.to_int count))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer, got " ^ Resp3.show_resp_value resp) ) )

let hget client hash field =
  let command = Commands.hget hash field in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString None) -> Lwt.return (Ok None)
  | Ok (Resp3.BulkString (Some value)) -> Lwt.return (Ok (Some value))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected BulkString, got " ^ Resp3.show_resp_value resp) ) )

let hset client hash field value =
  let command = Commands.hset hash [(field, value)] in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer 0L) -> Lwt.return (Ok false) (* field existed *)
  | Ok (Resp3.Integer 1L) -> Lwt.return (Ok true) (* new field *)
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer 0 or 1, got " ^ Resp3.show_resp_value resp)
           ) )

let lpush client list values =
  let command = Commands.lpush list values in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer count) -> Lwt.return (Ok (Int64.to_int count))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer, got " ^ Resp3.show_resp_value resp) ) )

let rpop client list =
  let command = Commands.rpop list in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString None) -> Lwt.return (Ok None)
  | Ok (Resp3.BulkString (Some value)) -> Lwt.return (Ok (Some value))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected BulkString, got " ^ Resp3.show_resp_value resp) ) )

let info client =
  let command = Commands.info () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString (Some info_text)) -> Lwt.return (Ok info_text)
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected BulkString, got " ^ Resp3.show_resp_value resp) ) )

(* Stream operations *)
let xlen client key =
  let command = Commands.xlen key in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer count) -> Lwt.return (Ok (Int64.to_int count))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer, got " ^ Resp3.show_resp_value resp) ) )

(* XADD operations *)
let xadd client key id field_values ?nomkstream ?ref_handling ?trim_strategy ?limit () =
  let command = Commands.xadd key id field_values ?nomkstream ?ref_handling ?trim_strategy ?limit () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString (Some entry_id)) -> Lwt.return (Ok (Some entry_id))
  | Ok (Resp3.BulkString None) -> Lwt.return (Ok None) (* NOMKSTREAM case *)
  | Ok Resp3.Null -> Lwt.return (Ok None) (* NOMKSTREAM case *)
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected BulkString or Null, got " ^ Resp3.show_resp_value resp) ) )

(* Type for stream entries: (entry_id * (field * value) list) *)
type stream_entry = string * (string * string) list

(* Type for XREAD results: (stream_name * stream_entries) list *)
type xread_result = (string * stream_entry list) list

(* Type for XREAD stream specification *)
type xread_stream = {
  key: string;
  id: string;
}

(* Helper function to parse stream entries from RESP3 array *)
let rec parse_stream_entries = function
  | [] -> Ok []
  | (Resp3.Array (Some [Resp3.BulkString (Some entry_id); Resp3.Array (Some field_values)])) :: rest ->
      (match parse_field_values field_values [] with
       | Ok fields ->
           (match parse_stream_entries rest with
            | Ok entries -> Ok ((entry_id, fields) :: entries)
            | Error e -> Error e)
       | Error e -> Error e)
  | entry :: _ ->
      Error ("Invalid stream entry format: " ^ Resp3.show_resp_value entry)

and parse_field_values field_values acc =
  match field_values with
  | [] -> Ok (List.rev acc)
  | (Resp3.BulkString (Some field)) :: (Resp3.BulkString (Some value)) :: rest ->
      parse_field_values rest ((field, value) :: acc)
  | _ -> Error "Invalid field-value pair format"

(* Helper function to parse XREAD response: [["stream1", [entries]], ["stream2", [entries]]] *)
let rec parse_xread_response = function
  | [] -> Ok []
  | (Resp3.Array (Some [Resp3.BulkString (Some stream_name); Resp3.Array (Some entries)])) :: rest ->
      (match parse_stream_entries entries with
       | Ok parsed_entries ->
           (match parse_xread_response rest with
            | Ok remaining -> Ok ((stream_name, parsed_entries) :: remaining)
            | Error e -> Error e)
       | Error e -> Error e)
  | stream :: _ ->
      Error ("Invalid XREAD stream format: " ^ Resp3.show_resp_value stream)

let xrange client key start_id end_id ?count () =
  let command = Commands.xrange key start_id end_id ?count () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some entries)) ->
      (match parse_stream_entries entries with
       | Ok parsed_entries -> Lwt.return (Ok parsed_entries)
       | Error msg -> Lwt.return (Error (Parse_error msg)))
  | Ok (Resp3.Array None) -> Lwt.return (Ok [])
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

let xrevrange client key end_id start_id ?count () =
  let command = Commands.xrevrange key end_id start_id ?count () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some entries)) ->
      (match parse_stream_entries entries with
       | Ok parsed_entries -> Lwt.return (Ok parsed_entries)
       | Error msg -> Lwt.return (Error (Parse_error msg)))
  | Ok (Resp3.Array None) -> Lwt.return (Ok [])
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

let xread client ?count ?block streams =
  let commands_streams = List.map (fun {key; id} -> Commands.{key; id}) streams in
  let command = Commands.xread ?count ?block commands_streams in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some stream_responses)) ->
      (match parse_xread_response stream_responses with
       | Ok parsed_streams -> Lwt.return (Ok parsed_streams)
       | Error msg -> Lwt.return (Error (Parse_error msg)))
  | Ok (Resp3.Array None) -> Lwt.return (Ok []) (* Timeout or no data *)
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

let xreadgroup client group_name consumer ?count ?block ?noack streams =
  let commands_streams = List.map (fun {key; id} -> Commands.{key; id}) streams in
  let command = Commands.xreadgroup group_name consumer ?count ?block ?noack commands_streams in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some stream_responses)) ->
      (match parse_xread_response stream_responses with
       | Ok parsed_streams -> Lwt.return (Ok parsed_streams)
       | Error msg -> Lwt.return (Error (Parse_error msg)))
  | Ok (Resp3.Array None) -> Lwt.return (Ok []) (* Timeout or no data *)
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

(* XGROUP operations *)
let xgroup_create client key groupname id ?mkstream ?entriesread () =
  let command = Commands.xgroup_create key groupname id ?mkstream ?entriesread () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.SimpleString "OK") -> Lwt.return (Ok ())
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error ("Expected OK, got " ^ Resp3.show_resp_value resp))
        )

let xgroup_destroy client key groupname =
  let command = Commands.xgroup_destroy key groupname in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer 1L) -> Lwt.return (Ok true) (* group destroyed *)
  | Ok (Resp3.Integer 0L) -> Lwt.return (Ok false) (* group didn't exist *)
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer, got " ^ Resp3.show_resp_value resp) ) )

let xgroup_createconsumer client key groupname consumer =
  let command = Commands.xgroup_createconsumer key groupname consumer in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer 1L) -> Lwt.return (Ok true) (* consumer created *)
  | Ok (Resp3.Integer 0L) -> Lwt.return (Ok false) (* consumer already existed *)
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer, got " ^ Resp3.show_resp_value resp) ) )

let xgroup_delconsumer client key groupname consumer =
  let command = Commands.xgroup_delconsumer key groupname consumer in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer count) -> Lwt.return (Ok (Int64.to_int count))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer, got " ^ Resp3.show_resp_value resp) ) )

let xgroup_setid client key groupname id ?entriesread () =
  let command = Commands.xgroup_setid key groupname id ?entriesread () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.SimpleString "OK") -> Lwt.return (Ok ())
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected SimpleString OK, got " ^ Resp3.show_resp_value resp) ) )

(* XINFO CONSUMERS data types and operations *)
type consumer_info = {
  name: string;
  pending: int;
  idle: int;
  inactive: int option; (* Available since Redis 7.2.0 *)
}

let parse_consumer_info_array arr =
  let rec parse_pairs acc = function
    | (Resp3.BulkString (Some "name")) :: (Resp3.BulkString (Some name)) :: rest ->
        parse_pairs (("name", name) :: acc) rest
    | (Resp3.BulkString (Some "pending")) :: (Resp3.Integer pending) :: rest ->
        parse_pairs (("pending", Int64.to_string pending) :: acc) rest  
    | (Resp3.BulkString (Some "idle")) :: (Resp3.Integer idle) :: rest ->
        parse_pairs (("idle", Int64.to_string idle) :: acc) rest
    | (Resp3.BulkString (Some "inactive")) :: (Resp3.Integer inactive) :: rest ->
        parse_pairs (("inactive", Int64.to_string inactive) :: acc) rest
    | [] -> Ok (List.rev acc)
    | _ -> Error "Invalid consumer info format"
  in
  match parse_pairs [] arr with
  | Ok pairs ->
      let name = List.assoc_opt "name" pairs |> Option.value ~default:"" in
      let pending = List.assoc_opt "pending" pairs |> Option.value ~default:"0" |> int_of_string in
      let idle = List.assoc_opt "idle" pairs |> Option.value ~default:"0" |> int_of_string in
      let inactive = List.assoc_opt "inactive" pairs |> Option.map int_of_string in
      Ok { name; pending; idle; inactive }
  | Error msg -> Error msg

let xinfo_consumers client key groupname =
  let command = Commands.xinfo_consumers key groupname in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some consumers)) ->
      let parse_consumer = function
        | Resp3.Array (Some consumer_data) ->
            parse_consumer_info_array consumer_data
        | resp ->
            Error ("Expected Array for consumer, got " ^ Resp3.show_resp_value resp)
      in
      (match List.fold_right (fun consumer acc ->
         match acc with
         | Error e -> Error e
         | Ok consumers_acc ->
             match parse_consumer consumer with
             | Ok consumer_info -> Ok (consumer_info :: consumers_acc)
             | Error e -> Error e
       ) consumers (Ok []) with
       | Ok consumer_list -> Lwt.return (Ok consumer_list)
       | Error e -> Lwt.return (Error (Parse_error e)))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

(* XINFO GROUPS data types and operations *)
type group_info = {
  name: string;
  consumers: int;
  pending: int;
  last_delivered_id: string;
  entries_read: int;
  lag: int option; (* NULL when lag can't be determined *)
}

let parse_group_info_array arr =
  let rec parse_pairs acc = function
    | (Resp3.BulkString (Some "name")) :: (Resp3.BulkString (Some name)) :: rest ->
        parse_pairs (("name", name) :: acc) rest
    | (Resp3.BulkString (Some "consumers")) :: (Resp3.Integer consumers) :: rest ->
        parse_pairs (("consumers", Int64.to_string consumers) :: acc) rest  
    | (Resp3.BulkString (Some "pending")) :: (Resp3.Integer pending) :: rest ->
        parse_pairs (("pending", Int64.to_string pending) :: acc) rest
    | (Resp3.BulkString (Some "last-delivered-id")) :: (Resp3.BulkString (Some id)) :: rest ->
        parse_pairs (("last-delivered-id", id) :: acc) rest
    | (Resp3.BulkString (Some "entries-read")) :: (Resp3.Integer entries_read) :: rest ->
        parse_pairs (("entries-read", Int64.to_string entries_read) :: acc) rest
    | (Resp3.BulkString (Some "lag")) :: (Resp3.Integer lag) :: rest ->
        parse_pairs (("lag", Int64.to_string lag) :: acc) rest
    | (Resp3.BulkString (Some "lag")) :: Resp3.Null :: rest ->
        parse_pairs (("lag", "null") :: acc) rest
    | [] -> Ok (List.rev acc)
    | (Resp3.BulkString (Some _)) :: _ :: rest ->
        (* Skip unknown field-value pairs *)
        parse_pairs acc rest
    | _ -> Error "Invalid group info format"
  in
  match parse_pairs [] arr with
  | Ok pairs ->
      let name = List.assoc_opt "name" pairs |> Option.value ~default:"" in
      let consumers = List.assoc_opt "consumers" pairs |> Option.value ~default:"0" |> int_of_string in
      let pending = List.assoc_opt "pending" pairs |> Option.value ~default:"0" |> int_of_string in
      let last_delivered_id = List.assoc_opt "last-delivered-id" pairs |> Option.value ~default:"0-0" in
      let entries_read = List.assoc_opt "entries-read" pairs |> Option.value ~default:"0" |> int_of_string in
      let lag = 
        match List.assoc_opt "lag" pairs with
        | Some "null" | None -> None
        | Some lag_str -> Some (int_of_string lag_str)
      in
      Ok { name; consumers; pending; last_delivered_id; entries_read; lag }
  | Error msg -> Error msg

let xinfo_groups client key =
  let command = Commands.xinfo_groups key in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some groups)) ->
      let parse_group = function
        | Resp3.Array (Some group_data) ->
            parse_group_info_array group_data
        | resp ->
            Error ("Expected Array for group, got " ^ Resp3.show_resp_value resp)
      in
      (match List.fold_right (fun group acc ->
         match acc with
         | Error e -> Error e
         | Ok groups_acc ->
             match parse_group group with
             | Ok group_info -> Ok (group_info :: groups_acc)
             | Error e -> Error e
       ) groups (Ok []) with
       | Ok group_list -> Lwt.return (Ok group_list)
       | Error e -> Lwt.return (Error (Parse_error e)))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

(* XINFO STREAM data types and operations *)
type stream_entry_info = string * (string * string) list

type stream_info_basic = {
  length: int;
  radix_tree_keys: int;
  radix_tree_nodes: int;
  last_generated_id: string;
  max_deleted_entry_id: string option;
  entries_added: int option;
  recorded_first_entry_id: string option;
  groups: int;
  first_entry: stream_entry_info option;
  last_entry: stream_entry_info option;
}

type pending_entry_full = {
  entry_id: string;
  consumer: string option; (* None in consumer-specific context *)
  timestamp: int;
  delivery_count: int;
}

type consumer_full = {
  name: string;
  seen_time: int;
  active_time: int option;
  pel_count: int;
  pending: pending_entry_full list;
}

type group_full = {
  name: string;
  last_delivered_id: string;
  entries_read: int;
  lag: int option;
  pel_count: int;
  pending: pending_entry_full list;
  consumers: consumer_full list;
}

type stream_info_full = {
  length: int;
  radix_tree_keys: int;
  radix_tree_nodes: int;
  last_generated_id: string;
  max_deleted_entry_id: string option;
  entries_added: int option;
  recorded_first_entry_id: string option;
  entries: stream_entry_info list;
  groups: group_full list;
}

type stream_info = 
  | Basic of stream_info_basic
  | Full of stream_info_full

let parse_stream_entry_from_array = function
  | [Resp3.BulkString (Some id); Resp3.Array (Some fields)] ->
      let rec parse_fields acc = function
        | [] -> Ok (List.rev acc)
        | (Resp3.BulkString (Some k)) :: (Resp3.BulkString (Some v)) :: rest ->
            parse_fields ((k, v) :: acc) rest
        | _ -> Error "Invalid field format in stream entry"
      in
      (match parse_fields [] fields with
       | Ok field_list -> Ok (id, field_list)
       | Error e -> Error e)
  | _ -> Error "Invalid stream entry format"

let parse_stream_info_array arr =
  let rec parse_pairs first_entry_acc last_entry_acc acc = function
    | (Resp3.BulkString (Some "length")) :: (Resp3.Integer len) :: rest ->
        parse_pairs first_entry_acc last_entry_acc (("length", Int64.to_string len) :: acc) rest
    | (Resp3.BulkString (Some "radix-tree-keys")) :: (Resp3.Integer keys) :: rest ->
        parse_pairs first_entry_acc last_entry_acc (("radix-tree-keys", Int64.to_string keys) :: acc) rest
    | (Resp3.BulkString (Some "radix-tree-nodes")) :: (Resp3.Integer nodes) :: rest ->
        parse_pairs first_entry_acc last_entry_acc (("radix-tree-nodes", Int64.to_string nodes) :: acc) rest
    | (Resp3.BulkString (Some "last-generated-id")) :: (Resp3.BulkString (Some id)) :: rest ->
        parse_pairs first_entry_acc last_entry_acc (("last-generated-id", id) :: acc) rest
    | (Resp3.BulkString (Some "max-deleted-entry-id")) :: (Resp3.BulkString (Some id)) :: rest ->
        parse_pairs first_entry_acc last_entry_acc (("max-deleted-entry-id", id) :: acc) rest
    | (Resp3.BulkString (Some "entries-added")) :: (Resp3.Integer count) :: rest ->
        parse_pairs first_entry_acc last_entry_acc (("entries-added", Int64.to_string count) :: acc) rest
    | (Resp3.BulkString (Some "recorded-first-entry-id")) :: (Resp3.BulkString (Some id)) :: rest ->
        parse_pairs first_entry_acc last_entry_acc (("recorded-first-entry-id", id) :: acc) rest
    | (Resp3.BulkString (Some "groups")) :: (Resp3.Integer count) :: rest ->
        parse_pairs first_entry_acc last_entry_acc (("groups", Int64.to_string count) :: acc) rest
    | (Resp3.BulkString (Some "first-entry")) :: (Resp3.Array (Some entry_data)) :: rest ->
        (* Parse the first entry *)
        (match parse_stream_entry_from_array entry_data with
         | Ok entry -> parse_pairs (Some entry) last_entry_acc (("first-entry", "parsed") :: acc) rest
         | Error e -> Error e)
    | (Resp3.BulkString (Some "first-entry")) :: Resp3.Null :: rest ->
        (* No first entry *)
        parse_pairs None last_entry_acc (("first-entry", "null") :: acc) rest
    | (Resp3.BulkString (Some "last-entry")) :: (Resp3.Array (Some entry_data)) :: rest ->
        (* Parse the last entry *)
        (match parse_stream_entry_from_array entry_data with
         | Ok entry -> parse_pairs first_entry_acc (Some entry) (("last-entry", "parsed") :: acc) rest
         | Error e -> Error e)
    | (Resp3.BulkString (Some "last-entry")) :: Resp3.Null :: rest ->
        (* No last entry *)
        parse_pairs first_entry_acc None (("last-entry", "null") :: acc) rest
    | (Resp3.BulkString (Some "entries")) :: (Resp3.Array (Some _)) :: rest ->
        (* This means it's a FULL response - switch to full parsing *)
        parse_pairs first_entry_acc last_entry_acc (("entries", "present") :: acc) rest
    | [] -> Ok (List.rev acc, first_entry_acc, last_entry_acc)
    | (Resp3.BulkString (Some _)) :: _ :: rest ->
        (* Skip unknown field-value pairs *)
        parse_pairs first_entry_acc last_entry_acc acc rest
    | _ -> Error "Invalid stream info format"
  in
  match parse_pairs None None [] arr with
  | Ok (pairs, first_entry, last_entry) -> Ok (pairs, first_entry, last_entry)
  | Error e -> Error e

let parse_group_full_from_array = function
  | group_data ->
      let rec parse_group_fields acc = function
        | (Resp3.BulkString (Some "name")) :: (Resp3.BulkString (Some name)) :: rest ->
            parse_group_fields (("name", name) :: acc) rest
        | (Resp3.BulkString (Some "last-delivered-id")) :: (Resp3.BulkString (Some id)) :: rest ->
            parse_group_fields (("last-delivered-id", id) :: acc) rest
        | (Resp3.BulkString (Some "entries-read")) :: (Resp3.Integer count) :: rest ->
            parse_group_fields (("entries-read", Int64.to_string count) :: acc) rest
        | (Resp3.BulkString (Some "lag")) :: (Resp3.Integer lag) :: rest ->
            parse_group_fields (("lag", Int64.to_string lag) :: acc) rest
        | (Resp3.BulkString (Some "lag")) :: Resp3.Null :: rest ->
            parse_group_fields (("lag", "null") :: acc) rest
        | (Resp3.BulkString (Some "pel-count")) :: (Resp3.Integer count) :: rest ->
            parse_group_fields (("pel-count", Int64.to_string count) :: acc) rest
        | (Resp3.BulkString (Some "pending")) :: (Resp3.Array (Some _)) :: rest ->
            parse_group_fields (("pending", "array") :: acc) rest
        | (Resp3.BulkString (Some "consumers")) :: (Resp3.Array (Some _)) :: rest ->
            parse_group_fields (("consumers", "array") :: acc) rest
        | [] -> Ok (List.rev acc)
        | _ :: _ :: rest ->
            (* Skip unknown field-value pairs *)
            parse_group_fields acc rest
        | _ -> Error "Invalid group info format"
      in
      match parse_group_fields [] group_data with
      | Ok pairs ->
          let name = List.assoc_opt "name" pairs |> Option.value ~default:"" in
          let last_delivered_id = List.assoc_opt "last-delivered-id" pairs |> Option.value ~default:"0-0" in
          let entries_read = List.assoc_opt "entries-read" pairs |> Option.value ~default:"0" |> int_of_string in
          let lag = match List.assoc_opt "lag" pairs with
            | Some "null" -> None
            | Some lag_str -> Some (int_of_string lag_str)
            | None -> None
          in
          let pel_count = List.assoc_opt "pel-count" pairs |> Option.value ~default:"0" |> int_of_string in
          (* For now, return empty lists for pending and consumers - full parsing would be very complex *)
          Ok {
            name;
            last_delivered_id;
            entries_read;
            lag;
            pel_count;
            pending = [];
            consumers = [];
          }
      | Error e -> Error e

let parse_full_stream_info arr =
  let rec extract_data entries_acc groups_acc acc = function
    | (Resp3.BulkString (Some "length")) :: (Resp3.Integer len) :: rest ->
        extract_data entries_acc groups_acc (("length", Int64.to_string len) :: acc) rest
    | (Resp3.BulkString (Some "radix-tree-keys")) :: (Resp3.Integer keys) :: rest ->
        extract_data entries_acc groups_acc (("radix-tree-keys", Int64.to_string keys) :: acc) rest
    | (Resp3.BulkString (Some "radix-tree-nodes")) :: (Resp3.Integer nodes) :: rest ->
        extract_data entries_acc groups_acc (("radix-tree-nodes", Int64.to_string nodes) :: acc) rest
    | (Resp3.BulkString (Some "last-generated-id")) :: (Resp3.BulkString (Some id)) :: rest ->
        extract_data entries_acc groups_acc (("last-generated-id", id) :: acc) rest
    | (Resp3.BulkString (Some "max-deleted-entry-id")) :: (Resp3.BulkString (Some id)) :: rest ->
        extract_data entries_acc groups_acc (("max-deleted-entry-id", id) :: acc) rest
    | (Resp3.BulkString (Some "entries-added")) :: (Resp3.Integer count) :: rest ->
        extract_data entries_acc groups_acc (("entries-added", Int64.to_string count) :: acc) rest
    | (Resp3.BulkString (Some "recorded-first-entry-id")) :: (Resp3.BulkString (Some id)) :: rest ->
        extract_data entries_acc groups_acc (("recorded-first-entry-id", id) :: acc) rest
    | (Resp3.BulkString (Some "entries")) :: (Resp3.Array (Some entries_arr)) :: rest ->
        (* Parse entries array *)
        let parse_entries entries_list =
          List.fold_right (fun entry_elem acc ->
            match acc with
            | Error e -> Error e
            | Ok entries_acc ->
                match entry_elem with
                | Resp3.Array (Some entry_data) ->
                    (match parse_stream_entry_from_array entry_data with
                     | Ok entry -> Ok (entry :: entries_acc)
                     | Error e -> Error e)
                | _ -> Error "Invalid entry in entries array"
          ) entries_list (Ok [])
        in
        (match parse_entries entries_arr with
         | Ok entries_list -> extract_data entries_list groups_acc acc rest
         | Error e -> Error e)
    | (Resp3.BulkString (Some "groups")) :: (Resp3.Array (Some groups_arr)) :: rest ->
        (* Parse groups array *)
        let parse_groups groups_list =
          List.fold_right (fun group_elem acc ->
            match acc with
            | Error e -> Error e
            | Ok groups_acc ->
                match group_elem with
                | Resp3.Array (Some group_data) ->
                    (match parse_group_full_from_array group_data with
                     | Ok group -> Ok (group :: groups_acc)
                     | Error e -> Error e)
                | _ -> Error "Invalid group in groups array"
          ) groups_list (Ok [])
        in
        (match parse_groups groups_arr with
         | Ok groups_list -> extract_data entries_acc groups_list acc rest
         | Error e -> Error e)
    | [] -> Ok (List.rev acc, entries_acc, groups_acc)
    | _ :: _ :: rest ->
        (* Skip unknown field-value pairs *)
        extract_data entries_acc groups_acc acc rest
    | _ -> Error "Invalid full stream info format"
  in
  match extract_data [] [] [] arr with
  | Ok (pairs, entries_list, groups_list) ->
      let length = List.assoc_opt "length" pairs |> Option.value ~default:"0" |> int_of_string in
      let radix_tree_keys = List.assoc_opt "radix-tree-keys" pairs |> Option.value ~default:"0" |> int_of_string in
      let radix_tree_nodes = List.assoc_opt "radix-tree-nodes" pairs |> Option.value ~default:"0" |> int_of_string in
      let last_generated_id = List.assoc_opt "last-generated-id" pairs |> Option.value ~default:"0-0" in
      let max_deleted_entry_id = List.assoc_opt "max-deleted-entry-id" pairs in
      let entries_added = List.assoc_opt "entries-added" pairs |> Option.map int_of_string in
      let recorded_first_entry_id = List.assoc_opt "recorded-first-entry-id" pairs in
      Ok {
        length;
        radix_tree_keys;
        radix_tree_nodes;
        last_generated_id;
        max_deleted_entry_id;
        entries_added;
        recorded_first_entry_id;
        entries = entries_list;
        groups = groups_list;
      }
  | Error e -> Error e

let xinfo_stream client key ?full ?count () =
  let command = Commands.xinfo_stream key ?full ?count () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some arr)) ->
      (* For now, let's implement basic parsing only *)
      (match parse_stream_info_array arr with
       | Ok (pairs, first_entry, last_entry) ->
           let length = List.assoc_opt "length" pairs |> Option.value ~default:"0" |> int_of_string in
           let radix_tree_keys = List.assoc_opt "radix-tree-keys" pairs |> Option.value ~default:"0" |> int_of_string in
           let radix_tree_nodes = List.assoc_opt "radix-tree-nodes" pairs |> Option.value ~default:"0" |> int_of_string in
           let last_generated_id = List.assoc_opt "last-generated-id" pairs |> Option.value ~default:"0-0" in
           let groups = List.assoc_opt "groups" pairs |> Option.value ~default:"0" |> int_of_string in
           let max_deleted_entry_id = List.assoc_opt "max-deleted-entry-id" pairs in
           let entries_added = List.assoc_opt "entries-added" pairs |> Option.map int_of_string in
           let recorded_first_entry_id = List.assoc_opt "recorded-first-entry-id" pairs in
           
           (* Check if this is a FULL response *)
           if List.mem_assoc "entries" pairs then
             (* FULL mode parsing *)
             (match parse_full_stream_info arr with
              | Ok full_info -> Lwt.return (Ok (Full full_info))
              | Error e -> Lwt.return (Error (Parse_error e)))
           else
             (* Basic response *)
             let basic_info = {
               length;
               radix_tree_keys;
               radix_tree_nodes;
               last_generated_id;
               max_deleted_entry_id;
               entries_added;
               recorded_first_entry_id;
               groups;
               first_entry;
               last_entry;
             } in
             Lwt.return (Ok (Basic basic_info))
       | Error e -> Lwt.return (Error (Parse_error e)))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

(* XACK operations *)
let xack client key group_name ids =
  let command = Commands.xack key group_name ids in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer count) -> Lwt.return (Ok (Int64.to_int count))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer, got " ^ Resp3.show_resp_value resp) ) )

(* XACKDEL operations *)
let xackdel client key group_name ids ?ref_handling () =
  let command = Commands.xackdel key group_name ids ?ref_handling () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some elements)) ->
      let parse_element = function
        | Resp3.Integer i -> Ok (Int64.to_int i)
        | resp -> Error (Parse_error ("Expected Integer in array, got " ^ Resp3.show_resp_value resp))
      in
      (match List.fold_right (fun elem acc ->
         match acc with
         | Error e -> Error e
         | Ok acc_list ->
           match parse_element elem with
           | Ok value -> Ok (value :: acc_list)  
           | Error e -> Error e
       ) elements (Ok []) with
       | Ok result_list -> Lwt.return (Ok result_list)
       | Error e -> Lwt.return (Error e))
  | Ok (Resp3.Array None) ->
      Lwt.return (Ok [])
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

(* XPENDING operations *)

(* Type for XPENDING summary: (count * min_id * max_id * consumers) *)
type xpending_summary = {
  count: int;
  min_id: string option;
  max_id: string option;
  consumers: (string * int) list;
}

(* Type for XPENDING extended entry: (id * consumer * idle_time * delivery_count) *)
type xpending_entry = {
  id: string;
  consumer: string;
  idle_time: int;
  delivery_count: int;
}

type xpending_result =
  | Summary of xpending_summary
  | Extended of xpending_entry list

(* Helper function to parse XPENDING summary response *)
let parse_xpending_summary = function
  | [Resp3.Integer count; min_id_resp; max_id_resp; consumers_resp] ->
      let min_id = match min_id_resp with
        | Resp3.BulkString (Some id) -> Some id
        | Resp3.BulkString None | Resp3.Null -> None
        | _ -> None
      in
      let max_id = match max_id_resp with
        | Resp3.BulkString (Some id) -> Some id
        | Resp3.BulkString None | Resp3.Null -> None
        | _ -> None
      in
      let rec parse_consumers acc = function
        | [] -> Ok (List.rev acc)
        | (Resp3.Array (Some [Resp3.BulkString (Some consumer_name); Resp3.BulkString (Some count_str)])) :: rest ->
            (match int_of_string_opt count_str with
             | Some count -> parse_consumers ((consumer_name, count) :: acc) rest
             | None -> Error "Invalid consumer count")
        | (Resp3.Array (Some [Resp3.BulkString (Some consumer_name); Resp3.Integer count_int])) :: rest ->
            (* Handle case where count is returned as Integer instead of BulkString *)
            parse_consumers ((consumer_name, Int64.to_int count_int) :: acc) rest
        | _ -> Error "Invalid consumer format"
      in
      (match consumers_resp with
       | Resp3.Array (Some consumer_pairs) ->
           (match parse_consumers [] consumer_pairs with
            | Ok consumers -> Ok {count = Int64.to_int count; min_id; max_id; consumers}
            | Error msg -> Error msg)
       | Resp3.Array None ->
           (* Empty consumer list *)
           Ok {count = Int64.to_int count; min_id; max_id; consumers = []}
       | _ -> Error "Invalid consumer list format")
  | response -> Error ("Invalid XPENDING summary format, got: " ^ 
                       (String.concat "; " (List.map Resp3.show_resp_value response)))

(* Helper function to parse XPENDING extended response *)
let parse_xpending_extended entries =
  let rec parse_entries acc = function
    | [] -> Ok (List.rev acc)
    | (Resp3.Array (Some [Resp3.BulkString (Some id); Resp3.BulkString (Some consumer); 
                          Resp3.Integer idle_time; Resp3.Integer delivery_count])) :: rest ->
        let entry = {
          id;
          consumer;
          idle_time = Int64.to_int idle_time;
          delivery_count = Int64.to_int delivery_count;
        } in
        parse_entries (entry :: acc) rest
    | _ -> Error "Invalid XPENDING extended entry format"
  in
  parse_entries [] entries

let xpending client key group_name ?(range=Commands.Summary) () =
  let command = Commands.xpending key group_name ~range () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some response)) ->
      (match range with
       | Commands.Summary ->
           (match parse_xpending_summary response with
            | Ok summary -> Lwt.return (Ok (Summary summary))
            | Error msg -> Lwt.return (Error (Parse_error msg)))
       | Commands.Extended _ ->
           (match parse_xpending_extended response with
            | Ok entries -> Lwt.return (Ok (Extended entries))
            | Error msg -> Lwt.return (Error (Parse_error msg))))
  | Ok (Resp3.Array None) -> 
      (* Empty result - return appropriate empty response based on range type *)
      (match range with
       | Commands.Summary -> Lwt.return (Ok (Summary {count = 0; min_id = None; max_id = None; consumers = []}))
       | Commands.Extended _ -> Lwt.return (Ok (Extended [])))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

(* XCLAIM operations *)
type xclaim_result =
  | ClaimEntries of stream_entry list  (* Normal mode: returns stream entries *)
  | ClaimIds of string list            (* JUSTID mode: returns only message IDs *)

let xclaim client key group_name consumer min_idle_time ids ?(options=[]) () =
  let command = Commands.xclaim key group_name consumer min_idle_time ids ~options () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some response)) ->
      (* Check if JUSTID option was used *)
      let has_justid = List.exists (function Commands.JustId -> true | _ -> false) options in
      if has_justid then
        (* JUSTID mode: response is array of strings (message IDs) *)
        let rec parse_ids acc = function
          | [] -> Ok (List.rev acc)
          | (Resp3.BulkString (Some id)) :: rest -> parse_ids (id :: acc) rest
          | _ -> Error "Invalid JUSTID response format"
        in
        (match parse_ids [] response with
         | Ok ids -> Lwt.return (Ok (ClaimIds ids))
         | Error msg -> Lwt.return (Error (Parse_error msg)))
      else
        (* Normal mode: response is array of stream entries *)
        (match parse_stream_entries response with
         | Ok entries -> Lwt.return (Ok (ClaimEntries entries))
         | Error msg -> Lwt.return (Error (Parse_error msg)))
  | Ok (Resp3.Array None) -> 
      (* Empty result *)
      let has_justid = List.exists (function Commands.JustId -> true | _ -> false) options in
      if has_justid then Lwt.return (Ok (ClaimIds []))
      else Lwt.return (Ok (ClaimEntries []))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

(* XAUTOCLAIM operations *)
type xautoclaim_result = {
  next_cursor: string;                    (* Next cursor for subsequent calls *)
  claimed_entries: stream_entry list;     (* Successfully claimed messages *)
  deleted_ids: string list;               (* IDs that were deleted from PEL *)
}

type xautoclaim_justid_result = {
  next_cursor: string;                    (* Next cursor for subsequent calls *)
  claimed_ids: string list;               (* Successfully claimed message IDs *)
  deleted_ids: string list;               (* IDs that were deleted from PEL *)
}

type xautoclaim_response =
  | AutoClaimEntries of xautoclaim_result      (* Normal mode *)
  | AutoClaimIds of xautoclaim_justid_result   (* JUSTID mode *)

let xautoclaim client key group_name consumer min_idle_time start ?(count=None) ?(justid=false) () =
  let command = Commands.xautoclaim key group_name consumer min_idle_time start ~count ~justid () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some [cursor_resp; entries_resp; deleted_resp])) ->
      (* Parse cursor *)
      (match cursor_resp with
       | Resp3.BulkString (Some cursor) ->
           (* Parse deleted IDs *)
           let deleted_ids = match deleted_resp with
             | Resp3.Array (Some del_array) ->
                 let rec parse_deleted acc = function
                   | [] -> List.rev acc
                   | (Resp3.BulkString (Some id)) :: rest -> parse_deleted (id :: acc) rest
                   | _ :: rest -> parse_deleted acc rest (* Skip invalid entries *)
                 in parse_deleted [] del_array
             | Resp3.Array None -> []
             | _ -> []
           in
           if justid then
             (* JUSTID mode: entries_resp is array of message IDs *)
             (match entries_resp with
              | Resp3.Array (Some id_array) ->
                  let rec parse_ids acc = function
                    | [] -> Ok (List.rev acc)
                    | (Resp3.BulkString (Some id)) :: rest -> parse_ids (id :: acc) rest
                    | _ -> Error "Invalid JUSTID entries format"
                  in
                  (match parse_ids [] id_array with
                   | Ok claimed_ids -> 
                       Lwt.return (Ok (AutoClaimIds {next_cursor = cursor; claimed_ids; deleted_ids}))
                   | Error msg -> Lwt.return (Error (Parse_error msg)))
              | Resp3.Array None ->
                  Lwt.return (Ok (AutoClaimIds {next_cursor = cursor; claimed_ids = []; deleted_ids}))
              | _ -> Lwt.return (Error (Parse_error "Invalid JUSTID entries response")))
           else
             (* Normal mode: entries_resp is array of stream entries *)
             (match entries_resp with
              | Resp3.Array (Some entry_array) ->
                  (match parse_stream_entries entry_array with
                   | Ok claimed_entries ->
                       Lwt.return (Ok (AutoClaimEntries {next_cursor = cursor; claimed_entries; deleted_ids}))
                   | Error msg -> Lwt.return (Error (Parse_error msg)))
              | Resp3.Array None ->
                  Lwt.return (Ok (AutoClaimEntries {next_cursor = cursor; claimed_entries = []; deleted_ids}))
              | _ -> Lwt.return (Error (Parse_error "Invalid entries response")))
       | _ -> Lwt.return (Error (Parse_error "Invalid cursor response")))
  | Ok (Resp3.Array (Some _)) ->
      Lwt.return (Error (Parse_error "XAUTOCLAIM response must have exactly 3 elements"))
  | Ok (Resp3.Array None) ->
      (* Empty response - shouldn't happen with XAUTOCLAIM but handle gracefully *)
      if justid then
        Lwt.return (Ok (AutoClaimIds {next_cursor = "0-0"; claimed_ids = []; deleted_ids = []}))
      else
        Lwt.return (Ok (AutoClaimEntries {next_cursor = "0-0"; claimed_entries = []; deleted_ids = []}))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

(* XDEL operations *)
let xdel client key ids =
  let command = Commands.xdel key ids in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer count) -> Lwt.return (Ok (Int64.to_int count))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer, got " ^ Resp3.show_resp_value resp) ) )

(* XDELEX operations *)  
let xdelex client key ids ?ref_handling () =
  let command = Commands.xdelex key ids ?ref_handling () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Array (Some elements)) ->
      let parse_element = function
        | Resp3.Integer i -> Ok (Int64.to_int i)
        | resp -> Error (Parse_error ("Expected Integer in array, got " ^ Resp3.show_resp_value resp))
      in
      (match List.fold_right (fun elem acc ->
         match acc with
         | Error e -> Error e
         | Ok acc_list ->
           match parse_element elem with
           | Ok value -> Ok (value :: acc_list)  
           | Error e -> Error e
       ) elements (Ok []) with
       | Ok result_list -> Lwt.return (Ok result_list)
       | Error e -> Lwt.return (Error e))
  | Ok (Resp3.Array None) ->
      Lwt.return (Ok [])
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Array, got " ^ Resp3.show_resp_value resp) ) )

(* XTRIM operations *)
let xtrim client key strategy ?operator ?limit ?ref_handling () =
  let command = Commands.xtrim key strategy ?operator ?limit ?ref_handling () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer count) -> Lwt.return (Ok (Int64.to_int count))
  | Ok resp ->
      Lwt.return
        (Error
           (Parse_error
              ("Expected Integer, got " ^ Resp3.show_resp_value resp) ) )
