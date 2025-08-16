open Base
open Lwt.Syntax
open Ocamlot_common.Types
open Generator
open Conflation
open Analytics

(* Publishing channels *)
type publish_channel = 
  | NATS of { 
      subject: string; 
      stream: string option;
      max_age_ms: int option;
    }
  | Redis of { 
      key_pattern: string;  (* e.g., "market:ticks:{symbol}" *)
      ttl_seconds: int option;
      data_type: [`Hash | `String | `Stream];
    }
  | File of { 
      path_pattern: string; (* e.g., "/data/{date}/{symbol}.csv" *)
      format: [`CSV | `JSON | `Parquet];
      rotation: [`Daily | `Hourly | `Size_mb of int];
    }
  | Webhook of {
      url: string;
      headers: (string * string) list;
      batch_size: int;
      timeout_ms: int;
    }
  [@@deriving show]

(* Data products to publish *)
type data_product = 
  | RawTicks of tick list
  | ConflatedBars of ohlcv_bar list  
  | Analytics of symbol_analytics list
  | MarketSummary of market_summary
  | HealthCheck of health_status
  [@@deriving show]

and market_summary = {
  timestamp: timestamp;
  total_instruments: int;
  total_ticks: int64;
  total_volume: decimal;
  market_state: [`Open | `Closed | `PreMarket | `PostMarket];
  top_movers: (string * decimal) list; (* symbol, % change *)
  volume_leaders: (string * decimal) list; (* symbol, volume *)
} [@@deriving show, yojson]

and health_status = {
  health_timestamp: timestamp;
  service_status: [`Healthy | `Degraded | `Critical];
  components: (string * [`Up | `Down | `Unknown]) list;
  metrics: (string * float) list;
  alerts: string list;
} [@@deriving show, yojson]

(* Publishing configuration *)
type publishing_config = {
  channels: (data_product * publish_channel list) list;
  rate_limits: (publish_channel * int) list; (* messages per second *)
  retry_policy: retry_config;
  monitoring: monitoring_config;
  circuit_breaker: circuit_breaker_config;
}

and retry_config = {
  max_retries: int;
  initial_delay_ms: int;
  backoff_multiplier: float;
  max_delay_ms: int;
}

and monitoring_config = {
  track_latency: bool;
  track_throughput: bool;
  track_errors: bool;
  alert_on_failure_rate: float; (* 0.0 - 1.0 *)
  metrics_retention_hours: int;
}

and circuit_breaker_config = {
  failure_threshold: int;
  recovery_time_ms: int;
  half_open_max_calls: int;
} [@@deriving show]

(* Publisher state *)
type publisher_state = {
  config: publishing_config;
  connections: (publish_channel, connection_state) Map.Poly.t;
  metrics: publishing_metrics;
  circuit_breakers: (publish_channel, circuit_breaker_state) Map.Poly.t;
}

and connection_state = {
  status: [`Connected | `Disconnected | `Connecting | `Error];
  last_successful_publish: timestamp option;
  consecutive_failures: int;
  total_messages_sent: int64;
  last_error: string option;
}

and circuit_breaker_state = {
  state: [`Closed | `Open | `HalfOpen];
  failure_count: int;
  last_failure_time: timestamp option;
  next_attempt_time: timestamp option;
}

and publishing_metrics = {
  messages_published: (publish_channel, int64) Map.Poly.t;
  publish_latencies: (publish_channel, float list) Map.Poly.t;
  error_counts: (publish_channel, int64) Map.Poly.t;
  throughput: (publish_channel, float) Map.Poly.t; (* messages per second *)
  last_updated: timestamp;
}

(* NATS integration *)
module NatsPublisher = struct
  type nats_connection = {
    client: Ocamlot_nats.Nats.client;
    is_connected: bool ref;
  }
  
  let connect ?(timeout_ms=5000) ~host ~port () =
    try%lwt
      let config : Ocamlot_nats.Nats.connection_config = {
        host;
        port;
        connect_timeout = Float.of_int timeout_ms /. 1000.0;
        reconnect_attempts = 3;
        reconnect_delay = 2.0;
      } in
      let client = Ocamlot_nats.Nats.create ~config () in
      let* () = Ocamlot_nats.Nats.connect client in
      Lwt.return (Ok { 
        client; 
        is_connected = ref true;
      })
    with
    | exn -> Lwt.return (Error (Exn.to_string exn))
  
  let publish_message nats_conn ~subject ~data =
    try%lwt
      if !(nats_conn.is_connected) then (
        let* () = Ocamlot_nats.Nats.publish_string nats_conn.client ~subject data in
        Lwt.return (Ok ())
      ) else
        Lwt.return (Error "NATS connection not active")
    with
    | exn -> 
      nats_conn.is_connected := false;
      Lwt.return (Error (Exn.to_string exn))
  
  let publish_to_stream nats_conn ~stream ~subject ~data =
    (* For now, use regular publish until JetStream is implemented *)
    let stream_subject = Printf.sprintf "%s.%s" stream subject in
    publish_message nats_conn ~subject:stream_subject ~data
  
  let create_stream _nats_conn ~name:_ ~subjects:_ ~max_age_ms:_ =
    (* JetStream stream creation - placeholder for future implementation *)
    Lwt.return (Ok ())
  
  let disconnect nats_conn =
    try%lwt
      if !(nats_conn.is_connected) then (
        nats_conn.is_connected := false;
        Ocamlot_nats.Nats.disconnect nats_conn.client
      ) else
        Lwt.return_unit
    with
    | _ -> Lwt.return_unit
end

(* Redis integration *)
module RedisPublisher = struct
  type redis_connection = {
    connection: unit; (* TODO: Replace with actual Redis connection type *)
  }
  
  let connect ?(timeout_ms=5000) ~host:_ ~port:_ () =
    (* TODO: Implement actual Redis connection *)
    ignore timeout_ms;
    Lwt.return (Ok { connection = () })
  
  let set_string _redis_conn ~key:_ ~value:_ ~ttl_seconds:_ =
    (* TODO: Implement Redis SET *)
    let* () = Lwt_unix.sleep 0.0005 in
    Lwt.return (Ok ())
  
  let hset _redis_conn ~key:_ ~field:_ ~value:_ =
    (* TODO: Implement Redis HSET *)
    let* () = Lwt_unix.sleep 0.0005 in
    Lwt.return (Ok ())
  
  let xadd _redis_conn ~stream:_ ~id:_ ~fields:_ =
    (* TODO: Implement Redis XADD for streams *)
    let* () = Lwt_unix.sleep 0.0005 in
    Lwt.return (Ok ())
end

(* Format data for publishing *)
let format_data_for_channel data_product channel =
  match (data_product, channel) with
  | (RawTicks ticks, _) ->
    ticks |> List.map ~f:tick_to_yojson |> fun jsons -> 
    `List jsons |> Yojson.Safe.to_string
  
  | (ConflatedBars bars, _) ->
    bars |> List.map ~f:ohlcv_bar_to_yojson |> fun jsons ->
    `List jsons |> Yojson.Safe.to_string
  
  | (Analytics analytics_list, _) ->
    analytics_list |> List.map ~f:symbol_analytics_to_yojson |> fun jsons ->
    `List jsons |> Yojson.Safe.to_string
  
  | (MarketSummary summary, _) ->
    market_summary_to_yojson summary |> Yojson.Safe.to_string
  
  | (HealthCheck health, _) ->
    health_status_to_yojson health |> Yojson.Safe.to_string

(* Generate subject/key for channel *)
let generate_subject_key data_product channel =
  match (data_product, channel) with
  | (RawTicks ticks, NATS { subject; _ }) ->
    (match ticks with
     | tick :: _ -> String.substr_replace_all subject ~pattern:"{symbol}" ~with_:tick.instrument_id
     | [] -> subject)
  
  | (ConflatedBars bars, NATS { subject; _ }) ->
    (match bars with
     | bar :: _ -> 
       let with_symbol = String.substr_replace_all subject ~pattern:"{symbol}" ~with_:bar.instrument_id in
       let interval_str = Conflation.interval_to_string bar.interval in
       String.substr_replace_all with_symbol ~pattern:"{interval}" ~with_:interval_str
     | [] -> subject)
  
  | (Analytics analytics_list, NATS { subject; _ }) ->
    (match analytics_list with
     | analytics :: _ -> String.substr_replace_all subject ~pattern:"{symbol}" ~with_:analytics.instrument_id
     | [] -> subject)
  
  | (MarketSummary _, NATS { subject; _ }) -> subject
  | (HealthCheck _, NATS { subject; _ }) -> subject
  
  | (RawTicks ticks, Redis { key_pattern; _ }) ->
    (match ticks with
     | tick :: _ -> String.substr_replace_all key_pattern ~pattern:"{symbol}" ~with_:tick.instrument_id
     | [] -> key_pattern)
  
  | (ConflatedBars bars, Redis { key_pattern; _ }) ->
    (match bars with
     | bar :: _ -> 
       let with_symbol = String.substr_replace_all key_pattern ~pattern:"{symbol}" ~with_:bar.instrument_id in
       let interval_str = Conflation.interval_to_string bar.interval in
       String.substr_replace_all with_symbol ~pattern:"{interval}" ~with_:interval_str
     | [] -> key_pattern)
  
  | (Analytics analytics_list, Redis { key_pattern; _ }) ->
    (match analytics_list with
     | analytics :: _ -> String.substr_replace_all key_pattern ~pattern:"{symbol}" ~with_:analytics.instrument_id
     | [] -> key_pattern)
  
  | (MarketSummary _, Redis { key_pattern; _ }) -> key_pattern
  | (HealthCheck _, Redis { key_pattern; _ }) -> key_pattern
  | _ -> "default"

(* Rate limiter *)
module RateLimiter = struct
  type t = {
    max_rate: int;
    mutable tokens: int;
    mutable last_refill: timestamp;
    interval: float;
  }
  
  let create max_rate =
    {
      max_rate;
      tokens = max_rate;
      last_refill = Unix.time ();
      interval = 1.0; (* 1 second *)
    }
  
  let try_acquire limiter : bool =
    let now = Unix.time () in
    let elapsed = now -. limiter.last_refill in
    
    if Float.( >= ) elapsed limiter.interval then (
      let tokens_to_add = Int.of_float (elapsed /. limiter.interval) * limiter.max_rate in
      limiter.tokens <- Int.min limiter.max_rate (limiter.tokens + tokens_to_add);
      limiter.last_refill <- now
    );
    
    if limiter.tokens > 0 then (
      limiter.tokens <- limiter.tokens - 1;
      true
    ) else 
      false
end

(* Circuit breaker implementation *)
module CircuitBreaker = struct
  let should_allow_request breaker_state _config =
    let now = Unix.time () in
    match breaker_state.state with
    | `Closed -> true
    | `Open ->
      (match breaker_state.next_attempt_time with
       | Some next_time when Float.( >= ) now next_time -> true
       | _ -> false)
    | `HalfOpen -> true
  
  let record_success breaker_state =
    { breaker_state with 
      state = `Closed; 
      failure_count = 0;
      next_attempt_time = None }
  
  let record_failure breaker_state config =
    let now = Unix.time () in
    let new_failure_count = breaker_state.failure_count + 1 in
    let new_state = 
      if new_failure_count >= config.failure_threshold then `Open
      else breaker_state.state
    in
    let next_attempt_time = 
      if Poly.(new_state = `Open) then 
        Some (now +. (Float.of_int config.recovery_time_ms /. 1000.0))
      else breaker_state.next_attempt_time
    in
    {
      state = new_state;
      failure_count = new_failure_count;
      last_failure_time = Some now;
      next_attempt_time;
    }
end

(* Publish to single channel *)
let publish_to_channel publisher_state data_product channel =
  let start_time = Unix.time () in
  
  (* Check circuit breaker *)
  let breaker_state = Map.Poly.find publisher_state.circuit_breakers channel
    |> Option.value ~default:{ state = `Closed; failure_count = 0; last_failure_time = None; next_attempt_time = None } in
  
  if not (CircuitBreaker.should_allow_request breaker_state publisher_state.config.circuit_breaker) then
    Lwt.return (Error "Circuit breaker open")
  else
    let formatted_data = format_data_for_channel data_product channel in
    let subject_key = generate_subject_key data_product channel in
    
    let* result = match channel with
      | NATS { stream = None; _ } ->
        (* Simple NATS publish *)
        let* nats_result = NatsPublisher.connect ~host:"localhost" ~port:4222 () in
        (match nats_result with
         | Ok nats_conn -> NatsPublisher.publish_message nats_conn ~subject:subject_key ~data:formatted_data
         | Error err -> Lwt.return (Error err))
      
      | NATS { stream = Some stream_name; _ } ->
        (* JetStream publish *)
        let* nats_result = NatsPublisher.connect ~host:"localhost" ~port:4222 () in
        (match nats_result with
         | Ok nats_conn -> NatsPublisher.publish_to_stream nats_conn ~stream:stream_name ~subject:subject_key ~data:formatted_data
         | Error err -> Lwt.return (Error err))
      
      | Redis { data_type = `String; ttl_seconds; _ } ->
        let* redis_result = RedisPublisher.connect ~host:"localhost" ~port:6379 () in
        (match redis_result with
         | Ok redis_conn -> RedisPublisher.set_string redis_conn ~key:subject_key ~value:formatted_data ~ttl_seconds
         | Error err -> Lwt.return (Error err))
      
      | Redis { data_type = `Hash; _ } ->
        let* redis_result = RedisPublisher.connect ~host:"localhost" ~port:6379 () in
        (match redis_result with
         | Ok redis_conn -> RedisPublisher.hset redis_conn ~key:subject_key ~field:"data" ~value:formatted_data
         | Error err -> Lwt.return (Error err))
      
      | Redis { data_type = `Stream; _ } ->
        let* redis_result = RedisPublisher.connect ~host:"localhost" ~port:6379 () in
        (match redis_result with
         | Ok redis_conn -> RedisPublisher.xadd redis_conn ~stream:subject_key ~id:"*" ~fields:[("data", formatted_data)]
         | Error err -> Lwt.return (Error err))
      
      | File { path_pattern = _; format = _; _ } ->
        (* TODO: Implement file publishing *)
        Lwt.return (Ok ())
      
      | Webhook { url = _; headers = _; timeout_ms = _; _ } ->
        (* TODO: Implement webhook publishing *)
        Lwt.return (Ok ())
    in
    
    let end_time = Unix.time () in
    let _latency = end_time -. start_time in
    
    (* Update metrics and circuit breaker *)
    let _updated_breaker_state = match result with
      | Ok () -> CircuitBreaker.record_success breaker_state
      | Error _ -> CircuitBreaker.record_failure breaker_state publisher_state.config.circuit_breaker
    in
    
    (* TODO: Update publisher metrics *)
    
    Lwt.return result

(* Publish data product to all configured channels *)
let publish_data_product publisher_state data_product =
  let channels = List.Assoc.find publisher_state.config.channels data_product ~equal:Poly.equal
    |> Option.value ~default:[] in
  
  let* results = Lwt_list.map_p (publish_to_channel publisher_state data_product) channels in
  
  let successful_publishes = List.count results ~f:(function Ok () -> true | Error _ -> false) in
  let total_publishes = List.length results in
  
  if successful_publishes > 0 then
    Lwt.return (Ok (successful_publishes, total_publishes))
  else
    Lwt.return (Error "All channel publishes failed")

(* Batch publisher for high-throughput scenarios *)
let batch_publish publisher_state data_products max_batch_size =
  let rec publish_batches acc = function
    | [] -> Lwt.return (List.rev acc)
    | products ->
      let batch, remaining = List.split_n products max_batch_size in
      let* batch_results = Lwt_list.map_p (publish_data_product publisher_state) batch in
      let* () = Lwt_unix.sleep 0.001 in (* Small delay between batches *)
      publish_batches (batch_results @ acc) remaining
  in
  publish_batches [] data_products

(* Create publisher *)
let create_publisher config =
  let connections = Map.Poly.empty in
  let circuit_breakers = Map.Poly.empty in
  let metrics = {
    messages_published = Map.Poly.empty;
    publish_latencies = Map.Poly.empty;
    error_counts = Map.Poly.empty;
    throughput = Map.Poly.empty;
    last_updated = Unix.time ();
  } in
  {
    config;
    connections;
    metrics;
    circuit_breakers;
  }

(* Health check *)
let get_health_status publisher_state =
  let now = Unix.time () in
  let total_errors = Map.Poly.fold publisher_state.metrics.error_counts ~init:0L ~f:(fun ~key:_ ~data acc -> Int64.(+) acc data) in
  let total_messages = Map.Poly.fold publisher_state.metrics.messages_published ~init:0L ~f:(fun ~key:_ ~data acc -> Int64.(+) acc data) in
  
  let error_rate = if Int64.(>) total_messages 0L then 
    Int64.to_float total_errors /. Int64.to_float total_messages
  else 0.0 in
  
  let service_status = 
    if Float.( < ) error_rate 0.01 then `Healthy
    else if Float.( < ) error_rate 0.05 then `Degraded  
    else `Critical
  in
  
  let component_statuses = Map.Poly.to_alist publisher_state.connections
    |> List.map ~f:(fun (channel, conn) ->
        let channel_name = match channel with
          | NATS { subject; _ } -> "nats:" ^ subject
          | Redis { key_pattern; _ } -> "redis:" ^ key_pattern
          | File { path_pattern; _ } -> "file:" ^ path_pattern
          | Webhook { url; _ } -> "webhook:" ^ url
        in
        let status = match conn.status with
          | `Connected -> `Up
          | `Disconnected | `Error -> `Down
          | `Connecting -> `Unknown
        in
        (channel_name, status)
      ) in
  
  {
    health_timestamp = now;
    service_status;
    components = component_statuses;
    metrics = [
      ("error_rate", error_rate);
      ("total_messages", Int64.to_float total_messages);
      ("total_errors", Int64.to_float total_errors);
    ];
    alerts = if Float.( >= ) error_rate 0.05 then ["High error rate detected"] else [];
  }

(* Default publishing configuration *)
let default_config = {
  channels = [
    (ConflatedBars [], [
      NATS { subject = "market.bars.{symbol}.{interval}"; stream = Some "MARKET_BARS"; max_age_ms = Some (24 * 60 * 60 * 1000) };
      Redis { key_pattern = "market:bars:{symbol}:{interval}"; ttl_seconds = Some 3600; data_type = `Hash };
    ]);
    (Analytics [], [
      NATS { subject = "market.analytics.{symbol}"; stream = Some "MARKET_ANALYTICS"; max_age_ms = Some (60 * 60 * 1000) };
      Redis { key_pattern = "market:analytics:{symbol}"; ttl_seconds = Some 300; data_type = `Hash };
    ]);
    (MarketSummary { 
      timestamp = 0.0; total_instruments = 0; total_ticks = 0L; total_volume = 0.0;
      market_state = `Closed; top_movers = []; volume_leaders = []
    }, [
      NATS { subject = "market.summary"; stream = None; max_age_ms = None };
      Redis { key_pattern = "market:summary"; ttl_seconds = Some 60; data_type = `String };
    ]);
  ];
  rate_limits = [];
  retry_policy = {
    max_retries = 3;
    initial_delay_ms = 100;
    backoff_multiplier = 2.0;
    max_delay_ms = 5000;
  };
  monitoring = {
    track_latency = true;
    track_throughput = true;
    track_errors = true;
    alert_on_failure_rate = 0.05;
    metrics_retention_hours = 24;
  };
  circuit_breaker = {
    failure_threshold = 5;
    recovery_time_ms = 30000;
    half_open_max_calls = 3;
  };
}