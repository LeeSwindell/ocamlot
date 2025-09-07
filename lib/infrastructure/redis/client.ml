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
