open Base
open Lwt.Syntax
open Ocamlot_common.Types
open Generator
open Conflation
open Analytics
open Publisher

(* Main system configuration *)
type system_config = {
  generator_config: generator_config;
  conflation_config: conflation_config;
  analytics_config: analytics_config;
  publishing_config: publishing_config;
  monitoring_config: system_monitoring_config;
  performance_config: performance_config;
}

and system_monitoring_config = {
  enable_metrics: bool;
  metrics_interval_ms: int;
  enable_profiling: bool;
  log_level: [`Debug | `Info | `Warn | `Error];
  alert_thresholds: alert_thresholds;
}

and alert_thresholds = {
  max_tick_latency_ms: float;
  min_throughput_tps: float;
  max_memory_mb: float;
  max_error_rate: float;
}

and performance_config = {
  tick_buffer_size: int;
  conflation_batch_size: int;
  analytics_batch_size: int;
  publishing_batch_size: int;
  worker_threads: int;
  gc_tuning: gc_config;
}

and gc_config = {
  minor_heap_size: int;
  major_heap_increment: int;
  space_overhead: int;
  max_overhead: int;
} [@@deriving show]

(* System state *)
type system_state = {
  generator_state: market_state;
  conflation_engine: conflation_engine;
  analytics_engine: analytics_engine;
  publisher_state: publisher_state;
  monitoring: monitoring_state;
  performance_metrics: performance_metrics;
  system_status: [`Starting | `Running | `Stopping | `Stopped | `Error of string];
  start_time: timestamp;
}

and monitoring_state = {
  tick_count: int64;
  bar_count: int64;
  analytics_count: int64;
  publish_count: int64;
  error_count: int64;
  last_tick_time: timestamp option;
  throughput_tps: float;
  latency_stats: latency_statistics;
  memory_usage: memory_statistics;
}

and latency_statistics = {
  tick_generation_ms: float;
  conflation_ms: float;
  analytics_ms: float;
  publishing_ms: float;
  end_to_end_ms: float;
}

and memory_statistics = {
  heap_size_mb: float;
  live_words: int;
  free_words: int;
  major_collections: int;
  minor_collections: int;
}

and performance_metrics = {
  cpu_usage_percent: float;
  memory_usage_mb: float;
  gc_pressure: float;
  cache_hit_rate: float;
  queue_depths: (string * int) list;
}

(* Metrics collection *)
module Metrics = struct
  type metric_type = 
    | Counter of string * int64
    | Gauge of string * float
    | Histogram of string * float list
    | Timer of string * float
    [@@deriving show]
  
  type metrics_collector = {
    mutable metrics: metric_type list;
    mutable last_collection: timestamp;
    collection_interval: float;
  }
  
  let create_collector ?(interval=1.0) () = {
    metrics = [];
    last_collection = Unix.time ();
    collection_interval = interval;
  }
  
  let record_counter collector name value =
    collector.metrics <- Counter (name, value) :: collector.metrics
  
  let record_gauge collector name value =
    collector.metrics <- Gauge (name, value) :: collector.metrics
  
  let record_timer collector name duration_ms =
    collector.metrics <- Timer (name, duration_ms) :: collector.metrics
  
  let record_histogram collector name values =
    collector.metrics <- Histogram (name, values) :: collector.metrics
  
  let collect_system_metrics collector =
    let gc_stats = Stdlib.Gc.stat () in
    let now = Unix.time () in
    
    record_gauge collector "memory.heap_size_mb" 
      (Float.of_int gc_stats.heap_words *. 8.0 /. 1024.0 /. 1024.0);
    record_gauge collector "memory.live_words" (Float.of_int gc_stats.live_words);
    record_counter collector "gc.major_collections" (Int64.of_int gc_stats.major_collections);
    record_counter collector "gc.minor_collections" (Int64.of_int gc_stats.minor_collections);
    record_gauge collector "system.uptime_seconds" (now -. collector.last_collection)
  
  let get_metrics collector =
    let metrics = List.rev collector.metrics in
    collector.metrics <- [];
    collector.last_collection <- Unix.time ();
    metrics
end

(* Performance monitoring *)
module Performance = struct
  let measure_time f =
    let start_time = Unix.gettimeofday () in
    let result = f () in
    let end_time = Unix.gettimeofday () in
    let duration_ms = (end_time -. start_time) *. 1000.0 in
    (result, duration_ms)
  
  let measure_time_lwt f =
    let start_time = Unix.gettimeofday () in
    let* result = f () in
    let end_time = Unix.gettimeofday () in
    let duration_ms = (end_time -. start_time) *. 1000.0 in
    Lwt.return (result, duration_ms)
  
  let calculate_throughput count duration_seconds =
    if Float.( > ) duration_seconds 0.0 then 
      Int64.to_float count /. duration_seconds
    else 0.0
  
  let update_moving_average current_avg new_value alpha =
    alpha *. new_value +. (1.0 -. alpha) *. current_avg
end

(* System orchestrator *)
module SystemOrchestrator = struct
  let create_system config =
    let generator_state = create_market_state config.generator_config in
    let conflation_engine = create_engine config.conflation_config in
    let analytics_engine = create_analytics_engine config.analytics_config in
    let publisher_state = create_publisher config.publishing_config in
    
    let monitoring = {
      tick_count = 0L;
      bar_count = 0L;
      analytics_count = 0L;
      publish_count = 0L;
      error_count = 0L;
      last_tick_time = None;
      throughput_tps = 0.0;
      latency_stats = { tick_generation_ms = 0.0; conflation_ms = 0.0; analytics_ms = 0.0; publishing_ms = 0.0; end_to_end_ms = 0.0 };
      memory_usage = { heap_size_mb = 0.0; live_words = 0; free_words = 0; major_collections = 0; minor_collections = 0 };
    } in
    
    let performance_metrics = {
      cpu_usage_percent = 0.0;
      memory_usage_mb = 0.0;
      gc_pressure = 0.0;
      cache_hit_rate = 0.0;
      queue_depths = [];
    } in
    
    {
      generator_state;
      conflation_engine;
      analytics_engine;
      publisher_state;
      monitoring;
      performance_metrics;
      system_status = `Starting;
      start_time = Unix.time ();
    }
  
  (* Main processing pipeline *)
  let process_tick_pipeline system_state _config tick =
    let start_time = Unix.gettimeofday () in
    
    (* Step 1: Process through conflation *)
    let (conflation_result, conflation_time) = Performance.measure_time (fun () ->
      process_tick system_state.conflation_engine tick
    ) in
    let (updated_conflation_engine, new_bars) = conflation_result in
    
    (* Step 2: Process bars through analytics *)
    let (analytics_result, analytics_time) = Performance.measure_time (fun () ->
      process_bars system_state.analytics_engine new_bars
    ) in
    let (updated_analytics_engine, analytics_results) = analytics_result in
    
    (* Step 3: Publish data *)
    let* (_publish_results, publishing_time) = Performance.measure_time_lwt (fun () ->
      let* bar_publish_results = if not (List.is_empty new_bars) then
        publish_data_product system_state.publisher_state (ConflatedBars new_bars)
      else Lwt.return (Ok (0, 0)) in
      
      let* analytics_publish_results = if not (List.is_empty analytics_results) then
        publish_data_product system_state.publisher_state (Analytics analytics_results)
      else Lwt.return (Ok (0, 0)) in
      
      Lwt.return (bar_publish_results, analytics_publish_results)
    ) in
    
    let end_time = Unix.gettimeofday () in
    let total_time_ms = (end_time -. start_time) *. 1000.0 in
    
    (* Update monitoring *)
    let updated_monitoring = {
      system_state.monitoring with
      tick_count = Int64.succ system_state.monitoring.tick_count;
      bar_count = Int64.(+) system_state.monitoring.bar_count (Int64.of_int (List.length new_bars));
      analytics_count = Int64.(+) system_state.monitoring.analytics_count (Int64.of_int (List.length analytics_results));
      last_tick_time = Some tick.timestamp;
      latency_stats = {
        tick_generation_ms = Performance.update_moving_average system_state.monitoring.latency_stats.tick_generation_ms 0.0 0.1;
        conflation_ms = Performance.update_moving_average system_state.monitoring.latency_stats.conflation_ms conflation_time 0.1;
        analytics_ms = Performance.update_moving_average system_state.monitoring.latency_stats.analytics_ms analytics_time 0.1;
        publishing_ms = Performance.update_moving_average system_state.monitoring.latency_stats.publishing_ms publishing_time 0.1;
        end_to_end_ms = Performance.update_moving_average system_state.monitoring.latency_stats.end_to_end_ms total_time_ms 0.1;
      };
    } in
    
    let updated_system_state = {
      system_state with
      conflation_engine = updated_conflation_engine;
      analytics_engine = updated_analytics_engine;
      monitoring = updated_monitoring;
    } in
    
    Lwt.return updated_system_state
  
  (* Generate and process single tick *)
  let generate_and_process_tick system_state config =
    let (updated_generator_state, tick_batch) = generate_batch config.generator_config.profiles system_state.generator_state in
    
    let updated_system_state = { system_state with generator_state = updated_generator_state } in
    
    (* Process each tick through pipeline *)
    Lwt_list.fold_left_s (fun acc_state tick ->
      process_tick_pipeline acc_state config tick
    ) updated_system_state tick_batch
  
  (* Main system loop *)
  let run_system system_state config =
    let metrics_collector = Metrics.create_collector ~interval:1.0 () in
    
    let rec main_loop current_state =
      if Poly.(current_state.system_status = `Running) then
        let* updated_state = generate_and_process_tick current_state config in
        
        (* Collect metrics periodically *)
        let now = Unix.time () in
        let time_since_last_metrics = now -. metrics_collector.last_collection in
        if Float.( >= ) time_since_last_metrics metrics_collector.collection_interval then
          Metrics.collect_system_metrics metrics_collector
        ;
        
        (* Control tick rate *)
        let* () = Lwt_unix.sleep (Float.of_int config.generator_config.tick_interval_ms /. 1000.0) in
        
        main_loop updated_state
      else
        Lwt.return current_state
    in
    
    let updated_state = { system_state with system_status = `Running } in
    main_loop updated_state
  
  (* Graceful shutdown *)
  let shutdown_system system_state =
    let final_conflation_bars = force_complete_all system_state.conflation_engine in
    let (_, final_bars) = final_conflation_bars in
    
    (* Publish any remaining data *)
    let* _final_publish_result = if not (List.is_empty final_bars) then
      publish_data_product system_state.publisher_state (ConflatedBars final_bars)
    else Lwt.return (Ok (0, 0)) in
    
    let shutdown_state = { system_state with system_status = `Stopped } in
    Lwt.return shutdown_state
end

(* Health checks *)
module HealthCheck = struct
  let check_system_health system_state config =
    let now = Unix.time () in
    let uptime = now -. system_state.start_time in
    
    (* Check tick generation health *)
    let tick_health = match system_state.monitoring.last_tick_time with
      | Some last_tick when Float.( < ) (now -. last_tick) 5.0 -> `Healthy
      | Some _ -> `Degraded
      | None -> `Critical
    in
    
    (* Check latency health *)
    let latency_health = 
      if Float.( < ) system_state.monitoring.latency_stats.end_to_end_ms config.monitoring_config.alert_thresholds.max_tick_latency_ms then `Healthy
      else `Degraded
    in
    
    (* Check throughput health *)
    let throughput_health =
      if Float.( > ) system_state.monitoring.throughput_tps config.monitoring_config.alert_thresholds.min_throughput_tps then `Healthy
      else `Degraded
    in
    
    (* Check memory health *)
    let memory_health =
      if Float.( < ) system_state.monitoring.memory_usage.heap_size_mb config.monitoring_config.alert_thresholds.max_memory_mb then `Healthy
      else `Critical
    in
    
    let overall_health = match [tick_health; latency_health; throughput_health; memory_health] with
      | health_list when List.for_all health_list ~f:(Poly.equal `Healthy) -> `Healthy
      | health_list when List.exists health_list ~f:(Poly.equal `Critical) -> `Critical
      | _ -> `Degraded
    in
    
    Publisher.{
      health_timestamp = now;
      service_status = overall_health;
      components = [
        ("tick_generation", match tick_health with `Healthy -> `Up | _ -> `Down);
        ("conflation", `Up);
        ("analytics", `Up);
        ("publishing", `Up);
      ];
      metrics = [
        ("uptime_seconds", uptime);
        ("tick_count", Int64.to_float system_state.monitoring.tick_count);
        ("throughput_tps", system_state.monitoring.throughput_tps);
        ("latency_ms", system_state.monitoring.latency_stats.end_to_end_ms);
        ("memory_mb", system_state.monitoring.memory_usage.heap_size_mb);
      ];
      alerts = [];
    }
end

(* System API *)
let create_and_start_system config =
  let system_state = SystemOrchestrator.create_system config in
  SystemOrchestrator.run_system system_state config

let get_system_status system_state config =
  HealthCheck.check_system_health system_state config

let stop_system system_state =
  SystemOrchestrator.shutdown_system system_state

(* Default system configuration *)
let default_system_config = {
  generator_config = Generator.default_config;
  conflation_config = Conflation.default_config;
  analytics_config = Analytics.default_config;
  publishing_config = Publisher.default_config;
  monitoring_config = {
    enable_metrics = true;
    metrics_interval_ms = 1000;
    enable_profiling = false;
    log_level = `Info;
    alert_thresholds = {
      max_tick_latency_ms = 10.0;
      min_throughput_tps = 100.0;
      max_memory_mb = 512.0;
      max_error_rate = 0.05;
    };
  };
  performance_config = {
    tick_buffer_size = 10000;
    conflation_batch_size = 100;
    analytics_batch_size = 50;
    publishing_batch_size = 20;
    worker_threads = 4;
    gc_tuning = {
      minor_heap_size = 2097152;
      major_heap_increment = 15;
      space_overhead = 80;
      max_overhead = 500;
    };
  };
}