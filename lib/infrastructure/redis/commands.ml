open Resp3

(** Redis Commands Module
    Provides type-safe, ergonomic Redis command builders and response parsers
    organized by Redis data types and functionality *)

(* =============================================================================
   GENERAL TYPES AND UTILITIES
   ============================================================================= *)

type redis_result = (Resp3.resp_value, string) result Lwt.t

(* Command execution type - will be provided by client *)
type 'a command_executor = Resp3.resp_value -> 'a

(** Helper functions for Redis command argument formatting *)

(** Format timeout values for Redis commands - produces clean integers when possible *)
let format_timeout timeout =
  if Float.is_integer timeout then
    string_of_int (int_of_float timeout)
  else
    string_of_float timeout

(** Format numeric arguments that should be clean integers when possible *)
let format_numeric_arg f =
  if Float.is_integer f then
    string_of_int (int_of_float f)
  else
    string_of_float f

(* =============================================================================
   CONNECTION COMMANDS
   ============================================================================= *)

(** PING [message] - Test connection *)
let ping ?message () =
  match message with
  | None -> Array (Some [BulkString (Some "PING")])
  | Some msg -> Array (Some [BulkString (Some "PING"); BulkString (Some msg)])

(** ECHO message - Echo the given string *)
let echo message = 
  Array (Some [BulkString (Some "ECHO"); BulkString (Some message)])

(** QUIT - Close the connection *)
let quit () =
  Array (Some [BulkString (Some "QUIT")])

(** SELECT index - Change the selected database *)
let select db_index =
  Array (Some [BulkString (Some "SELECT"); BulkString (Some (string_of_int db_index))])

(* =============================================================================
   KEY MANAGEMENT COMMANDS
   ============================================================================= *)

(** EXISTS key [key ...] - Check if keys exist *)
let exists keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "EXISTS") :: key_args))

(** TYPE key - Get the type of a key *)
let type_of_key key =
  Array (Some [BulkString (Some "TYPE"); BulkString (Some key)])

(** EXPIRE key seconds - Set key expiration *)
let expire key seconds =
  Array (Some [BulkString (Some "EXPIRE"); BulkString (Some key); BulkString (Some (string_of_int seconds))])

(** EXPIREAT key timestamp - Set key expiration at timestamp *)
let expireat key timestamp =
  Array (Some [BulkString (Some "EXPIREAT"); BulkString (Some key); BulkString (Some (Int64.to_string timestamp))])

(** TTL key - Get time to live *)
let ttl key =
  Array (Some [BulkString (Some "TTL"); BulkString (Some key)])

(** PERSIST key - Remove expiration *)
let persist key =
  Array (Some [BulkString (Some "PERSIST"); BulkString (Some key)])

(** RENAME key newkey - Rename a key *)
let rename key newkey =
  Array (Some [BulkString (Some "RENAME"); BulkString (Some key); BulkString (Some newkey)])

(** RENAMENX key newkey - Rename key if new key doesn't exist *)
let renamenx key newkey =
  Array (Some [BulkString (Some "RENAMENX"); BulkString (Some key); BulkString (Some newkey)])

(** DUMP key - Serialize key value *)
let dump key =
  Array (Some [BulkString (Some "DUMP"); BulkString (Some key)])

(** RESTORE key ttl serialized-value - Restore serialized key *)
let restore key ttl serialized_value ?replace () =
  let base_cmd = [BulkString (Some "RESTORE"); BulkString (Some key); 
                  BulkString (Some (string_of_int ttl)); BulkString (Some serialized_value)] in
  let cmd = match replace with
    | Some true -> base_cmd @ [BulkString (Some "REPLACE")]
    | _ -> base_cmd in
  Array (Some cmd)

(** KEYS pattern - Find keys matching pattern *)
let keys pattern =
  Array (Some [BulkString (Some "KEYS"); BulkString (Some pattern)])

(** SCAN cursor [MATCH pattern] [COUNT count] - Incrementally scan keys *)
let scan cursor ?pattern ?count () =
  let base_cmd = [BulkString (Some "SCAN"); BulkString (Some (string_of_int cursor))] in
  let cmd_with_match = match pattern with
    | None -> base_cmd
    | Some p -> base_cmd @ [BulkString (Some "MATCH"); BulkString (Some p)] in
  let final_cmd = match count with
    | None -> cmd_with_match
    | Some c -> cmd_with_match @ [BulkString (Some "COUNT"); BulkString (Some (string_of_int c))] in
  Array (Some final_cmd)

(** RANDOMKEY - Return a random key *)
let randomkey () =
  Array (Some [BulkString (Some "RANDOMKEY")])

(** MOVE key db - Move key to another database *)
let move key db =
  Array (Some [BulkString (Some "MOVE"); BulkString (Some key); BulkString (Some (string_of_int db))])

(** DEL key [key ...] - Delete keys *)
let del keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "DEL") :: key_args))

(* =============================================================================
   STRING COMMANDS  
   ============================================================================= *)

(** GET key - Get string value *)
let get key =
  Array (Some [BulkString (Some "GET"); BulkString (Some key)])

(** SET key value [EX seconds] [PX milliseconds] [NX|XX] - Set string value *)
let set key value ?ex ?px ?nx ?xx () =
  let base_cmd = [BulkString (Some "SET"); BulkString (Some key); BulkString (Some value)] in
  let cmd_with_ex = match ex with
    | None -> base_cmd
    | Some seconds -> base_cmd @ [BulkString (Some "EX"); BulkString (Some (string_of_int seconds))] in
  let cmd_with_px = match px with
    | None -> cmd_with_ex
    | Some milliseconds -> cmd_with_ex @ [BulkString (Some "PX"); BulkString (Some (string_of_int milliseconds))] in
  let cmd_with_nx = match nx with
    | Some true -> cmd_with_px @ [BulkString (Some "NX")]
    | _ -> cmd_with_px in
  let final_cmd = match xx with
    | Some true -> cmd_with_nx @ [BulkString (Some "XX")]
    | _ -> cmd_with_nx in
  Array (Some final_cmd)

(** MGET key [key ...] - Get multiple values *)
let mget keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "MGET") :: key_args))

(** MSET key value [key value ...] - Set multiple key-value pairs *)
let mset key_values =
  let args = List.fold_right (fun (key, value) acc ->
    BulkString (Some key) :: BulkString (Some value) :: acc) key_values [] in
  Array (Some (BulkString (Some "MSET") :: args))

(** MSETNX key value [key value ...] - Set multiple if none exist *)
let msetnx key_values =
  let args = List.fold_right (fun (key, value) acc ->
    BulkString (Some key) :: BulkString (Some value) :: acc) key_values [] in
  Array (Some (BulkString (Some "MSETNX") :: args))

(** GETSET key value - Set and return old value *)
let getset key value =
  Array (Some [BulkString (Some "GETSET"); BulkString (Some key); BulkString (Some value)])

(** SETNX key value - Set if not exists *)
let setnx key value =
  Array (Some [BulkString (Some "SETNX"); BulkString (Some key); BulkString (Some value)])

(** SETEX key seconds value - Set with expiration *)
let setex key seconds value =
  Array (Some [BulkString (Some "SETEX"); BulkString (Some key); 
               BulkString (Some (string_of_int seconds)); BulkString (Some value)])

(** PSETEX key milliseconds value - Set with expiration in milliseconds *)
let psetex key milliseconds value =
  Array (Some [BulkString (Some "PSETEX"); BulkString (Some key); 
               BulkString (Some (string_of_int milliseconds)); BulkString (Some value)])

(** INCR key - Increment integer value *)
let incr key =
  Array (Some [BulkString (Some "INCR"); BulkString (Some key)])

(** INCRBY key increment - Increment by amount *)
let incrby key increment =
  Array (Some [BulkString (Some "INCRBY"); BulkString (Some key); BulkString (Some (string_of_int increment))])

(** INCRBYFLOAT key increment - Increment by float *)
let incrbyfloat key increment =
  Array (Some [BulkString (Some "INCRBYFLOAT"); BulkString (Some key); 
               BulkString (Some (format_numeric_arg increment))])

(** DECR key - Decrement integer value *)
let decr key =
  Array (Some [BulkString (Some "DECR"); BulkString (Some key)])

(** DECRBY key decrement - Decrement by amount *)
let decrby key decrement =
  Array (Some [BulkString (Some "DECRBY"); BulkString (Some key); 
               BulkString (Some (string_of_int decrement))])

(** APPEND key value - Append to string *)
let append key value =
  Array (Some [BulkString (Some "APPEND"); BulkString (Some key); BulkString (Some value)])

(** STRLEN key - Get string length *)
let strlen key =
  Array (Some [BulkString (Some "STRLEN"); BulkString (Some key)])

(** GETRANGE key start end - Get substring *)
let getrange key start_pos end_pos =
  Array (Some [BulkString (Some "GETRANGE"); BulkString (Some key); 
               BulkString (Some (string_of_int start_pos)); BulkString (Some (string_of_int end_pos))])

(** SETRANGE key offset value - Overwrite string at offset *)
let setrange key offset value =
  Array (Some [BulkString (Some "SETRANGE"); BulkString (Some key); 
               BulkString (Some (string_of_int offset)); BulkString (Some value)])

(* =============================================================================
   HASH COMMANDS
   ============================================================================= *)

(** HGET key field - Get hash field value *)
let hget key field =
  Array (Some [BulkString (Some "HGET"); BulkString (Some key); BulkString (Some field)])

(** HSET key field value [field value ...] - Set hash fields *)
let hset key field_values =
  let field_value_args = List.fold_right (fun (field, value) acc ->
      BulkString (Some field) :: BulkString (Some value) :: acc
    ) field_values [] in
  let cmd = BulkString (Some "HSET") :: BulkString (Some key) :: field_value_args in
  Array (Some cmd)

(** HMGET key field [field ...] - Get multiple hash fields *)
let hmget key fields =
  let field_args = List.map (fun f -> BulkString (Some f)) fields in
  Array (Some (BulkString (Some "HMGET") :: BulkString (Some key) :: field_args))

(** HMSET key field value [field value ...] - Set multiple hash fields *)
let hmset key field_values =
  let field_value_args = List.fold_right (fun (field, value) acc ->
      BulkString (Some field) :: BulkString (Some value) :: acc
    ) field_values [] in
  let cmd = BulkString (Some "HMSET") :: BulkString (Some key) :: field_value_args in
  Array (Some cmd)

(** HGETALL key - Get all hash fields and values *)
let hgetall key =
  Array (Some [BulkString (Some "HGETALL"); BulkString (Some key)])

(** HKEYS key - Get all hash field names *)
let hkeys key =
  Array (Some [BulkString (Some "HKEYS"); BulkString (Some key)])

(** HVALS key - Get all hash values *)
let hvals key =
  Array (Some [BulkString (Some "HVALS"); BulkString (Some key)])

(** HLEN key - Get hash field count *)
let hlen key =
  Array (Some [BulkString (Some "HLEN"); BulkString (Some key)])

(** HDEL key field [field ...] - Delete hash fields *)
let hdel key fields =
  let field_args = List.map (fun f -> BulkString (Some f)) fields in
  Array (Some (BulkString (Some "HDEL") :: BulkString (Some key) :: field_args))

(** HEXISTS key field - Check if hash field exists *)
let hexists key field =
  Array (Some [BulkString (Some "HEXISTS"); BulkString (Some key); BulkString (Some field)])

(** HINCRBY key field increment - Increment hash field by integer *)
let hincrby key field increment =
  Array (Some [BulkString (Some "HINCRBY"); BulkString (Some key); 
               BulkString (Some field); BulkString (Some (string_of_int increment))])

(** HINCRBYFLOAT key field increment - Increment hash field by float *)
let hincrbyfloat key field increment =
  Array (Some [BulkString (Some "HINCRBYFLOAT"); BulkString (Some key); 
               BulkString (Some field); BulkString (Some (format_numeric_arg increment))])

(** HSCAN key cursor [MATCH pattern] [COUNT count] - Scan hash fields *)
let hscan key cursor ?pattern ?count () =
  let base_cmd = [BulkString (Some "HSCAN"); BulkString (Some key); BulkString (Some (string_of_int cursor))] in
  let cmd_with_match = match pattern with
    | None -> base_cmd
    | Some p -> base_cmd @ [BulkString (Some "MATCH"); BulkString (Some p)] in
  let final_cmd = match count with
    | None -> cmd_with_match
    | Some c -> cmd_with_match @ [BulkString (Some "COUNT"); BulkString (Some (string_of_int c))] in
  Array (Some final_cmd)

(** HSTRLEN key field - Get hash field value length *)
let hstrlen key field =
  Array (Some [BulkString (Some "HSTRLEN"); BulkString (Some key); BulkString (Some field)])

(** HSETNX key field value - Set hash field if not exists *)
let hsetnx key field value =
  Array (Some [BulkString (Some "HSETNX"); BulkString (Some key); 
               BulkString (Some field); BulkString (Some value)])

(* =============================================================================
   LIST COMMANDS
   ============================================================================= *)

(** LPUSH key value [value ...] - Push to list head *)
let lpush key values =
  let value_args = List.map (fun v -> BulkString (Some v)) values in
  Array (Some (BulkString (Some "LPUSH") :: BulkString (Some key) :: value_args))

(** RPUSH key value [value ...] - Push to list tail *)
let rpush key values =
  let value_args = List.map (fun v -> BulkString (Some v)) values in
  Array (Some (BulkString (Some "RPUSH") :: BulkString (Some key) :: value_args))

(** LPOP key - Pop from list head *)
let lpop key =
  Array (Some [BulkString (Some "LPOP"); BulkString (Some key)])

(** RPOP key - Pop from list tail *)
let rpop key =
  Array (Some [BulkString (Some "RPOP"); BulkString (Some key)])

(** LLEN key - Get list length *)
let llen key =
  Array (Some [BulkString (Some "LLEN"); BulkString (Some key)])

(** LRANGE key start stop - Get list range *)
let lrange key start_idx stop_idx =
  Array (Some [BulkString (Some "LRANGE"); BulkString (Some key); 
               BulkString (Some (string_of_int start_idx)); BulkString (Some (string_of_int stop_idx))])

(** LTRIM key start stop - Trim list to range *)
let ltrim key start_idx stop_idx =
  Array (Some [BulkString (Some "LTRIM"); BulkString (Some key); 
               BulkString (Some (string_of_int start_idx)); BulkString (Some (string_of_int stop_idx))])

(** LINDEX key index - Get list element by index *)
let lindex key index =
  Array (Some [BulkString (Some "LINDEX"); BulkString (Some key); BulkString (Some (string_of_int index))])

(** LSET key index value - Set list element by index *)
let lset key index value =
  Array (Some [BulkString (Some "LSET"); BulkString (Some key); 
               BulkString (Some (string_of_int index)); BulkString (Some value)])

(** LREM key count value - Remove list elements *)
let lrem key count value =
  Array (Some [BulkString (Some "LREM"); BulkString (Some key); 
               BulkString (Some (string_of_int count)); BulkString (Some value)])

(** LINSERT key BEFORE|AFTER pivot value - Insert into list *)
let linsert key ~before pivot value =
  let direction = if before then "BEFORE" else "AFTER" in
  Array (Some [BulkString (Some "LINSERT"); BulkString (Some key); 
               BulkString (Some direction); BulkString (Some pivot); BulkString (Some value)])

(** BLPOP key [key ...] timeout - Blocking pop from list head *)
let blpop keys timeout =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  let timeout_str = format_timeout timeout in
  Array (Some (BulkString (Some "BLPOP") :: key_args @ [BulkString (Some timeout_str)]))

(** BRPOP key [key ...] timeout - Blocking pop from list tail *)
let brpop keys timeout =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  let timeout_str = format_timeout timeout in
  Array (Some (BulkString (Some "BRPOP") :: key_args @ [BulkString (Some timeout_str)]))

(** BRPOPLPUSH source destination timeout - Blocking pop and push *)
let brpoplpush source destination timeout =
  Array (Some [BulkString (Some "BRPOPLPUSH"); BulkString (Some source); 
               BulkString (Some destination); BulkString (Some (format_timeout timeout))])

(** RPOPLPUSH source destination - Pop and push *)
let rpoplpush source destination =
  Array (Some [BulkString (Some "RPOPLPUSH"); BulkString (Some source); BulkString (Some destination)])

(* =============================================================================
   SET COMMANDS
   ============================================================================= *)

(** SADD key member [member ...] - Add to set *)
let sadd key members =
  let member_args = List.map (fun m -> BulkString (Some m)) members in
  Array (Some (BulkString (Some "SADD") :: BulkString (Some key) :: member_args))

(** SREM key member [member ...] - Remove from set *)
let srem key members =
  let member_args = List.map (fun m -> BulkString (Some m)) members in
  Array (Some (BulkString (Some "SREM") :: BulkString (Some key) :: member_args))

(** SCARD key - Get set cardinality *)
let scard key =
  Array (Some [BulkString (Some "SCARD"); BulkString (Some key)])

(** SISMEMBER key member - Check set membership *)
let sismember key member =
  Array (Some [BulkString (Some "SISMEMBER"); BulkString (Some key); BulkString (Some member)])

(** SMEMBERS key - Get all set members *)
let smembers key =
  Array (Some [BulkString (Some "SMEMBERS"); BulkString (Some key)])

(** SRANDMEMBER key [count] - Get random set member(s) *)
let srandmember key ?count () =
  let base_cmd = [BulkString (Some "SRANDMEMBER"); BulkString (Some key)] in
  let final_cmd = match count with
    | None -> base_cmd
    | Some c -> base_cmd @ [BulkString (Some (string_of_int c))] in
  Array (Some final_cmd)

(** SPOP key [count] - Pop random set member(s) *)
let spop key ?count () =
  let base_cmd = [BulkString (Some "SPOP"); BulkString (Some key)] in
  let final_cmd = match count with
    | None -> base_cmd
    | Some c -> base_cmd @ [BulkString (Some (string_of_int c))] in
  Array (Some final_cmd)

(** SMOVE source destination member - Move set member *)
let smove source destination member =
  Array (Some [BulkString (Some "SMOVE"); BulkString (Some source); 
               BulkString (Some destination); BulkString (Some member)])

(** SINTER key [key ...] - Set intersection *)
let sinter keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "SINTER") :: key_args))

(** SINTERSTORE destination key [key ...] - Store set intersection *)
let sinterstore destination keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "SINTERSTORE") :: BulkString (Some destination) :: key_args))

(** SUNION key [key ...] - Set union *)
let sunion keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "SUNION") :: key_args))

(** SUNIONSTORE destination key [key ...] - Store set union *)
let sunionstore destination keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "SUNIONSTORE") :: BulkString (Some destination) :: key_args))

(** SDIFF key [key ...] - Set difference *)
let sdiff keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "SDIFF") :: key_args))

(** SDIFFSTORE destination key [key ...] - Store set difference *)
let sdiffstore destination keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "SDIFFSTORE") :: BulkString (Some destination) :: key_args))

(** SSCAN key cursor [MATCH pattern] [COUNT count] - Scan set members *)
let sscan key cursor ?pattern ?count () =
  let base_cmd = [BulkString (Some "SSCAN"); BulkString (Some key); BulkString (Some (string_of_int cursor))] in
  let cmd_with_match = match pattern with
    | None -> base_cmd
    | Some p -> base_cmd @ [BulkString (Some "MATCH"); BulkString (Some p)] in
  let final_cmd = match count with
    | None -> cmd_with_match
    | Some c -> cmd_with_match @ [BulkString (Some "COUNT"); BulkString (Some (string_of_int c))] in
  Array (Some final_cmd)

(* =============================================================================
   SORTED SET COMMANDS
   ============================================================================= *)

(** ZADD key score member [score member ...] - Add to sorted set *)
let zadd key ?nx:_nx ?xx:_xx ?ch:_ch ?incr:_incr score_members =
  let score_member_args = List.fold_right (fun (score, member) acc ->
    BulkString (Some (format_numeric_arg score)) :: BulkString (Some member) :: acc) score_members [] in
  Array (Some (BulkString (Some "ZADD") :: BulkString (Some key) :: score_member_args))

(** ZREM key member [member ...] - Remove from sorted set *)
let zrem key members =
  let member_args = List.map (fun m -> BulkString (Some m)) members in
  Array (Some (BulkString (Some "ZREM") :: BulkString (Some key) :: member_args))

(** ZCARD key - Get sorted set cardinality *)
let zcard key =
  Array (Some [BulkString (Some "ZCARD"); BulkString (Some key)])

(** ZCOUNT key min max - Count members in score range *)
let zcount key min_score max_score =
  Array (Some [BulkString (Some "ZCOUNT"); BulkString (Some key); 
               BulkString (Some (format_numeric_arg min_score)); BulkString (Some (format_numeric_arg max_score))])

(** ZSCORE key member - Get member score *)
let zscore key member =
  Array (Some [BulkString (Some "ZSCORE"); BulkString (Some key); BulkString (Some member)])

(** ZRANK key member - Get member rank (ascending) *)
let zrank key member =
  Array (Some [BulkString (Some "ZRANK"); BulkString (Some key); BulkString (Some member)])

(** ZREVRANK key member - Get member rank (descending) *)
let zrevrank key member =
  Array (Some [BulkString (Some "ZREVRANK"); BulkString (Some key); BulkString (Some member)])

(** ZRANGE key start stop [WITHSCORES] - Get range by rank *)
let zrange key start_idx stop_idx ?withscores () =
  let base_cmd = [BulkString (Some "ZRANGE"); BulkString (Some key); 
                  BulkString (Some (string_of_int start_idx)); BulkString (Some (string_of_int stop_idx))] in
  let cmd = match withscores with
    | Some true -> base_cmd @ [BulkString (Some "WITHSCORES")]
    | _ -> base_cmd in
  Array (Some cmd)

(** ZREVRANGE key start stop [WITHSCORES] - Get range by rank (descending) *)
let zrevrange key start_idx stop_idx ?withscores () =
  let base_cmd = [BulkString (Some "ZREVRANGE"); BulkString (Some key); 
                  BulkString (Some (string_of_int start_idx)); BulkString (Some (string_of_int stop_idx))] in
  let cmd = match withscores with
    | Some true -> base_cmd @ [BulkString (Some "WITHSCORES")]
    | _ -> base_cmd in
  Array (Some cmd)

(** ZRANGEBYSCORE key min max [WITHSCORES] [LIMIT offset count] - Get range by score *)
let zrangebyscore key min_score max_score ?withscores ?limit () =
  let base_cmd = [BulkString (Some "ZRANGEBYSCORE"); BulkString (Some key); 
                  BulkString (Some (format_numeric_arg min_score)); BulkString (Some (format_numeric_arg max_score))] in
  let cmd_with_scores = match withscores with
    | Some true -> base_cmd @ [BulkString (Some "WITHSCORES")]
    | _ -> base_cmd in
  let final_cmd = match limit with
    | None -> cmd_with_scores
    | Some (offset, count) -> cmd_with_scores @ [BulkString (Some "LIMIT"); 
                                                BulkString (Some (string_of_int offset)); BulkString (Some (string_of_int count))] in
  Array (Some final_cmd)

(** ZREVRANGEBYSCORE key max min [WITHSCORES] [LIMIT offset count] - Get range by score (descending) *)
let zrevrangebyscore key max_score min_score ?withscores ?limit () =
  let base_cmd = [BulkString (Some "ZREVRANGEBYSCORE"); BulkString (Some key); 
                  BulkString (Some (format_numeric_arg max_score)); BulkString (Some (format_numeric_arg min_score))] in
  let cmd_with_scores = match withscores with
    | Some true -> base_cmd @ [BulkString (Some "WITHSCORES")]
    | _ -> base_cmd in
  let final_cmd = match limit with
    | None -> cmd_with_scores
    | Some (offset, count) -> cmd_with_scores @ [BulkString (Some "LIMIT"); 
                                                BulkString (Some (string_of_int offset)); BulkString (Some (string_of_int count))] in
  Array (Some final_cmd)

(** ZREMRANGEBYRANK key start stop - Remove by rank range *)
let zremrangebyrank key start_idx stop_idx =
  Array (Some [BulkString (Some "ZREMRANGEBYRANK"); BulkString (Some key); 
               BulkString (Some (string_of_int start_idx)); BulkString (Some (string_of_int stop_idx))])

(** ZREMRANGEBYSCORE key min max - Remove by score range *)
let zremrangebyscore key min_score max_score =
  Array (Some [BulkString (Some "ZREMRANGEBYSCORE"); BulkString (Some key); 
               BulkString (Some (format_numeric_arg min_score)); BulkString (Some (format_numeric_arg max_score))])

(** ZINCRBY key increment member - Increment member score *)
let zincrby key increment member =
  Array (Some [BulkString (Some "ZINCRBY"); BulkString (Some key); 
               BulkString (Some (format_numeric_arg increment)); BulkString (Some member)])

(** ZINTERSTORE destination numkeys key [key ...] - Store sorted set intersection *)
let zinterstore destination keys ?weights ?aggregate () =
  let numkeys = List.length keys in
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  let base_cmd = BulkString (Some "ZINTERSTORE") :: BulkString (Some destination) :: 
                 BulkString (Some (string_of_int numkeys)) :: key_args in
  let cmd_with_weights = match weights with
    | None -> base_cmd
    | Some w -> base_cmd @ [BulkString (Some "WEIGHTS")] @ List.map (fun weight -> BulkString (Some (format_numeric_arg weight))) w in
  let final_cmd = match aggregate with
    | None -> cmd_with_weights
    | Some agg -> 
        let agg_str = match agg with
          | `SUM -> "SUM" | `MIN -> "MIN" | `MAX -> "MAX" in
        cmd_with_weights @ [BulkString (Some "AGGREGATE"); BulkString (Some agg_str)] in
  Array (Some final_cmd)

(** ZUNIONSTORE destination numkeys key [key ...] - Store sorted set union *)
let zunionstore destination keys ?weights ?aggregate () =
  let numkeys = List.length keys in
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  let base_cmd = BulkString (Some "ZUNIONSTORE") :: BulkString (Some destination) :: 
                 BulkString (Some (string_of_int numkeys)) :: key_args in
  let cmd_with_weights = match weights with
    | None -> base_cmd
    | Some w -> base_cmd @ [BulkString (Some "WEIGHTS")] @ List.map (fun weight -> BulkString (Some (format_numeric_arg weight))) w in
  let final_cmd = match aggregate with
    | None -> cmd_with_weights
    | Some agg -> 
        let agg_str = match agg with
          | `SUM -> "SUM" | `MIN -> "MIN" | `MAX -> "MAX" in
        cmd_with_weights @ [BulkString (Some "AGGREGATE"); BulkString (Some agg_str)] in
  Array (Some final_cmd)

(** ZSCAN key cursor [MATCH pattern] [COUNT count] - Scan sorted set members *)
let zscan key cursor ?pattern ?count () =
  let base_cmd = [BulkString (Some "ZSCAN"); BulkString (Some key); BulkString (Some (string_of_int cursor))] in
  let cmd_with_match = match pattern with
    | None -> base_cmd
    | Some p -> base_cmd @ [BulkString (Some "MATCH"); BulkString (Some p)] in
  let final_cmd = match count with
    | None -> cmd_with_match
    | Some c -> cmd_with_match @ [BulkString (Some "COUNT"); BulkString (Some (string_of_int c))] in
  Array (Some final_cmd)

(* =============================================================================
   BITMAP COMMANDS
   ============================================================================= *)

(** SETBIT key offset value - Set bit *)
let setbit key offset value =
  let value_str = if value then "1" else "0" in
  Array (Some [
    BulkString (Some "SETBIT");
    BulkString (Some key);
    BulkString (Some (string_of_int offset));
    BulkString (Some value_str);
  ])

(** GETBIT key offset - Get bit *)
let getbit key offset =
  Array (Some [
    BulkString (Some "GETBIT");
    BulkString (Some key);
    BulkString (Some (string_of_int offset));
  ])

(** BITCOUNT key [start end] - Count set bits *)
let bitcount key ?start ?end_pos () =
  let base_cmd = [
    BulkString (Some "BITCOUNT");
    BulkString (Some key);
  ] in
  let cmd_with_range = match (start, end_pos) with
    | (Some s, Some e) -> base_cmd @ [
        BulkString (Some (string_of_int s));
        BulkString (Some (string_of_int e));
      ]
    | (Some s, None) -> base_cmd @ [BulkString (Some (string_of_int s))]
    | _ -> base_cmd
  in
  Array (Some cmd_with_range)

(** BITOP operation destkey key [key ...] - Bitwise operations *)
let bitop operation destkey keys =
  let operation_str = match operation with
    | `AND -> "AND"
    | `OR -> "OR"
    | `XOR -> "XOR"
    | `NOT -> "NOT"
  in
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  let base_cmd = [
    BulkString (Some "BITOP");
    BulkString (Some operation_str);
    BulkString (Some destkey);
  ] in
  Array (Some (base_cmd @ key_args))

(** BITPOS key bit [start] [end] - Find first bit *)
let bitpos key bit ?start ?end_pos () =
  let bit_str = if bit then "1" else "0" in
  let base_cmd = [
    BulkString (Some "BITPOS");
    BulkString (Some key);
    BulkString (Some bit_str);
  ] in
  let cmd_with_range = match (start, end_pos) with
    | (Some s, Some e) -> base_cmd @ [
        BulkString (Some (string_of_int s));
        BulkString (Some (string_of_int e));
      ]
    | (Some s, None) -> base_cmd @ [BulkString (Some (string_of_int s))]
    | _ -> base_cmd
  in
  Array (Some cmd_with_range)

(** BITFIELD key [GET type offset] [SET type offset value] [INCRBY type offset increment] - Bit field operations *)
type bitfield_operation = 
  | Get of string * int  (* type, offset *)
  | Set of string * int * int  (* type, offset, value *)
  | Incrby of string * int * int  (* type, offset, increment *)

let bitfield key operations =
  let operation_to_args = function
    | Get (typ, offset) -> [
        BulkString (Some "GET");
        BulkString (Some typ);
        BulkString (Some (string_of_int offset));
      ]
    | Set (typ, offset, value) -> [
        BulkString (Some "SET");
        BulkString (Some typ);
        BulkString (Some (string_of_int offset));
        BulkString (Some (string_of_int value));
      ]
    | Incrby (typ, offset, increment) -> [
        BulkString (Some "INCRBY");
        BulkString (Some typ);
        BulkString (Some (string_of_int offset));
        BulkString (Some (string_of_int increment));
      ]
  in
  let operation_args = List.concat_map operation_to_args operations in
  let base_cmd = [
    BulkString (Some "BITFIELD");
    BulkString (Some key);
  ] in
  Array (Some (base_cmd @ operation_args))

(* =============================================================================
   HYPERLOGLOG COMMANDS
   ============================================================================= *)

(** PFADD key element [element ...] - Add to HyperLogLog *)
let pfadd _key _elements =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** PFCOUNT key [key ...] - Count HyperLogLog cardinality *)
let pfcount _keys =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** PFMERGE destkey sourcekey [sourcekey ...] - Merge HyperLogLogs *)
let pfmerge _destkey _sourcekeys =
  (* TODO: Implementation *)
  failwith "Not implemented"

(* =============================================================================
   STREAM COMMANDS
   ============================================================================= *)

(** XADD key ID field value [field value ...] - Add to stream *)
let xadd _key _id _field_values =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** XLEN key - Get stream length *)
let xlen _key =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** XRANGE key start end [COUNT count] - Get stream range *)
let xrange _key _start_id _end_id ?count:_count () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** XREVRANGE key end start [COUNT count] - Get stream range (reverse) *)
let xrevrange _key _end_id _start_id ?count:_count () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** XREAD [COUNT count] [BLOCK milliseconds] STREAMS key [key ...] id [id ...] - Read from streams *)
let xread ?count:_count ?block:_block _streams =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** XREADGROUP GROUP group consumer [COUNT count] [BLOCK milliseconds] STREAMS key [key ...] ID [ID ...] - Read from stream group *)
let xreadgroup _group_name _consumer ?count:_count ?block:_block _streams =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** XGROUP CREATE key groupname id [MKSTREAM] - Create consumer group *)
let xgroup_create _key _groupname _id ?mkstream:_mkstream () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** XGROUP DESTROY key groupname - Destroy consumer group *)
let xgroup_destroy _key _groupname =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** XDEL key ID [ID ...] - Delete stream entries *)
let xdel _key _ids =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** XTRIM key MAXLEN [~] count - Trim stream *)
let xtrim _key ~maxlen:_maxlen ?approximate:_approximate _count =
  (* TODO: Implementation *)
  failwith "Not implemented"

(* =============================================================================
   PUBSUB COMMANDS
   ============================================================================= *)

(** PUBLISH channel message - Publish message *)
let publish channel message =
  Array (Some [BulkString (Some "PUBLISH"); BulkString (Some channel); BulkString (Some message)])

(** SUBSCRIBE channel [channel ...] - Subscribe to channels *)
let subscribe channels =
  let channel_args = List.map (fun c -> BulkString (Some c)) channels in
  Array (Some (BulkString (Some "SUBSCRIBE") :: channel_args))

(** UNSUBSCRIBE [channel [channel ...]] - Unsubscribe from channels *)
let unsubscribe ?channels () =
  match channels with
  | None -> Array (Some [BulkString (Some "UNSUBSCRIBE")])
  | Some chans ->
      let channel_args = List.map (fun c -> BulkString (Some c)) chans in
      Array (Some (BulkString (Some "UNSUBSCRIBE") :: channel_args))

(** PSUBSCRIBE pattern [pattern ...] - Subscribe to patterns *)
let psubscribe patterns =
  let pattern_args = List.map (fun p -> BulkString (Some p)) patterns in
  Array (Some (BulkString (Some "PSUBSCRIBE") :: pattern_args))

(** PUNSUBSCRIBE [pattern [pattern ...]] - Unsubscribe from patterns *)
let punsubscribe ?patterns:_patterns () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** PUBSUB CHANNELS [pattern] - List active channels *)
let pubsub_channels ?pattern:_pattern () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** PUBSUB NUMSUB [channel [channel ...]] - Get channel subscriber counts *)
let pubsub_numsub ?channels:_channels () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** PUBSUB NUMPAT - Get pattern subscription count *)
let pubsub_numpat () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(* =============================================================================
   TRANSACTION COMMANDS
   ============================================================================= *)

(** MULTI - Start transaction *)
let multi () =
  Array (Some [BulkString (Some "MULTI")])

(** EXEC - Execute transaction *)
let exec () =
  Array (Some [BulkString (Some "EXEC")])

(** DISCARD - Discard transaction *)
let discard () =
  Array (Some [BulkString (Some "DISCARD")])

(** WATCH key [key ...] - Watch keys for changes *)
let watch keys =
  let key_args = List.map (fun k -> BulkString (Some k)) keys in
  Array (Some (BulkString (Some "WATCH") :: key_args))

(** UNWATCH - Stop watching all keys *)
let unwatch () =
  Array (Some [BulkString (Some "UNWATCH")])

(* =============================================================================
   SERVER COMMANDS
   ============================================================================= *)

(** INFO [section] - Get server information *)
let info ?section () =
  match section with
  | None -> Array (Some [BulkString (Some "INFO")])
  | Some s -> Array (Some [BulkString (Some "INFO"); BulkString (Some s)])

(** FLUSHDB [ASYNC] - Clear current database *)
let flushdb ?async:_async () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** FLUSHALL [ASYNC] - Clear all databases *)
let flushall ?async:_async () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** DBSIZE - Get database key count *)
let dbsize () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** LASTSAVE - Get last save timestamp *)
let lastsave () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** SAVE - Synchronous save *)
let save () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** BGSAVE - Background save *)
let bgsave () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** BGREWRITEAOF - Background rewrite AOF *)
let bgrewriteaof () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** CONFIG GET parameter - Get configuration *)
let config_get _parameter =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** CONFIG SET parameter value - Set configuration *)
let config_set _parameter _value =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** CONFIG REWRITE - Rewrite configuration file *)
let config_rewrite () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** CONFIG RESETSTAT - Reset statistics *)
let config_resetstat () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** CLIENT LIST - List client connections *)
let client_list () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** CLIENT GETNAME - Get client name *)
let client_getname () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** CLIENT SETNAME connection-name - Set client name *)
let client_setname _name =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** CLIENT KILL [filters] - Kill client connection *)
let client_kill ?addr:_addr ?client_id:_client_id ?client_type:_client_type () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** TIME - Get server time *)
let time () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** SLOWLOG GET [count] - Get slow log entries *)
let slowlog_get ?count:_count () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** SLOWLOG LEN - Get slow log length *)
let slowlog_len () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** SLOWLOG RESET - Clear slow log *)
let slowlog_reset () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** MONITOR - Monitor all commands *)
let monitor () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** DEBUG OBJECT key - Get object debug info *)
let debug_object _key =
  (* TODO: Implementation *)
  failwith "Not implemented"

(* =============================================================================
   LUA SCRIPTING COMMANDS
   ============================================================================= *)

(** EVAL script numkeys key [key ...] arg [arg ...] - Execute Lua script *)
let eval _script _keys _args =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** EVALSHA sha1 numkeys key [key ...] arg [arg ...] - Execute cached Lua script *)
let evalsha _sha1 _keys _args =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** SCRIPT LOAD script - Cache Lua script *)
let script_load _script =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** SCRIPT EXISTS script [script ...] - Check script existence *)
let script_exists _scripts =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** SCRIPT FLUSH - Clear script cache *)
let script_flush () =
  (* TODO: Implementation *)
  failwith "Not implemented"

(** SCRIPT KILL - Kill running script *)
let script_kill () =
  (* TODO: Implementation *)
  failwith "Not implemented"