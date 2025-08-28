open Lwt.Syntax

type client_config = {
  host: string;
  port: int;
  pool_size: int;
  connection_timeout: float;
}

type client_error = 
  | Connection_error of Connection.connection_error
  | Pool_exhausted
  | Redis_error of string
  | Parse_error of string

(* Connection pool implementation *)
module ConnectionPool = struct
  type t = {
    available: Connection.connection_handle Queue.t;
    mutable in_use: Connection.connection_handle ref list;
    config: client_config;
    mutex: Lwt_mutex.t;
  }
  
  let create config =
    let pool = {
      available = Queue.create ();
      in_use = [];
      config;
      mutex = Lwt_mutex.create ();
    } in
    (* Pre-populate pool *)
    let rec create_connections n acc =
      if n <= 0 then Lwt.return (Ok acc)
      else
        let* conn_result = Connection.connect config.host config.port in
        match conn_result with
        | Error e -> Lwt.return (Error (Connection_error e))
        | Ok conn ->
            Queue.push conn pool.available;
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
        pool.in_use <- conn_ref :: pool.in_use;
        Lwt.return (Ok conn))

  let release pool conn =
    Lwt_mutex.with_lock pool.mutex (fun () ->
      Queue.push conn pool.available;
      pool.in_use <- List.filter (fun r -> !r != conn) pool.in_use;
      Lwt.return ())

  let close_all pool =
  let* connections = 
    Lwt_mutex.with_lock pool.mutex (fun () ->
      let all_connections = Queue.to_seq pool.available |> List.of_seq in
      Queue.clear pool.available;
      Lwt.return all_connections) in
  Lwt_list.iter_p Connection.close connections
end

(* Client implementation *)
type t = {
  pool: ConnectionPool.t;
  config: client_config;
}

let create config =
  let* pool_result = ConnectionPool.create config in
  match pool_result with
  | Error e -> Lwt.return (Error e)
  | Ok pool -> Lwt.return (Ok { pool; config })

let close client =
  ConnectionPool.close_all client.pool

let with_client config f =
  let* client_result = create config in
  match client_result with
  | Error e -> Lwt.return (Error e)
  | Ok client ->
      Lwt.finalize
        (fun () -> f client)
        (fun () -> close client)

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
    | Ok () ->
        let* resp_result = Connection.receive_resp conn in
        match resp_result with
        | Error e -> Lwt.return (Error (Connection_error e))
        | Ok response -> 
            (* Check for Redis errors *)
            match response with
            | Resp3.SimpleError msg -> Lwt.return (Error (Redis_error msg))
            | _ -> Lwt.return (Ok response)
  )

let pipeline client commands =
  with_connection client (fun conn ->
    (* Send all commands *)
    let* send_results = 
      Lwt_list.map_s (fun cmd ->
        let serialized = Resp3.serialize_resp3 cmd in
        Connection.send_command conn serialized
      ) commands
    in
    
    (* Check all sends succeeded *)
    let rec check_sends = function
      | [] -> Ok ()
      | (Error e) :: _ -> Error (Connection_error e)
      | (Ok ()) :: rest -> check_sends rest
    in
    
    match check_sends send_results with
    | Error e -> Lwt.return (Error e)
    | Ok () ->
        (* Read all responses *)
        let* responses = 
          Lwt_list.map_s (fun _ -> Connection.receive_resp conn) commands
        in
        
        (* Convert to result list *)
        let rec collect_responses acc = function
          | [] -> Ok (List.rev acc)
          | (Error e) :: _ -> Error (Connection_error e)
          | (Ok resp) :: rest -> collect_responses (resp :: acc) rest
        in
        
        match collect_responses [] responses with
        | Error e -> Lwt.return (Error e)
        | Ok responses -> Lwt.return (Ok responses)
  )

(* High-level Redis commands *)

let ping client =
  let command = Commands.ping () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.SimpleString s) -> Lwt.return (Ok s)
  | Ok resp -> Lwt.return (Error (Parse_error ("Expected SimpleString, got " ^ (Resp3.show_resp_value resp))))

let get client key =
  let command = Commands.get key in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString None) -> Lwt.return (Ok None)
  | Ok (Resp3.BulkString (Some value)) -> Lwt.return (Ok (Some value))
  | Ok resp -> Lwt.return (Error (Parse_error ("Expected BulkString, got " ^ (Resp3.show_resp_value resp))))

let set client key value =
  let command = Commands.set key value () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.SimpleString "OK") -> Lwt.return (Ok ())
  | Ok resp -> Lwt.return (Error (Parse_error ("Expected OK, got " ^ (Resp3.show_resp_value resp))))

let del client keys =
  let command = Commands.del keys in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer count) -> Lwt.return (Ok (Int64.to_int count))
  | Ok resp -> Lwt.return (Error (Parse_error ("Expected Integer, got " ^ (Resp3.show_resp_value resp))))

let hget client hash field =
  let command = Commands.hget hash field in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString None) -> Lwt.return (Ok None)
  | Ok (Resp3.BulkString (Some value)) -> Lwt.return (Ok (Some value))
  | Ok resp -> Lwt.return (Error (Parse_error ("Expected BulkString, got " ^ (Resp3.show_resp_value resp))))

let hset client hash field value =
  let command = Commands.hset hash [(field, value)] in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer 0L) -> Lwt.return (Ok false)  (* field existed *)
  | Ok (Resp3.Integer 1L) -> Lwt.return (Ok true)   (* new field *)
  | Ok resp -> Lwt.return (Error (Parse_error ("Expected Integer 0 or 1, got " ^ (Resp3.show_resp_value resp))))

let lpush client list values =
  let command = Commands.lpush list values in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.Integer count) -> Lwt.return (Ok (Int64.to_int count))
  | Ok resp -> Lwt.return (Error (Parse_error ("Expected Integer, got " ^ (Resp3.show_resp_value resp))))

let rpop client list =
  let command = Commands.rpop list in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString None) -> Lwt.return (Ok None)
  | Ok (Resp3.BulkString (Some value)) -> Lwt.return (Ok (Some value))
  | Ok resp -> Lwt.return (Error (Parse_error ("Expected BulkString, got " ^ (Resp3.show_resp_value resp))))

let info client =
  let command = Commands.info () in
  let* result = execute client command in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok (Resp3.BulkString (Some info_text)) -> Lwt.return (Ok info_text)
  | Ok resp -> Lwt.return (Error (Parse_error ("Expected BulkString, got " ^ (Resp3.show_resp_value resp))))