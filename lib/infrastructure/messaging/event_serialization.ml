open Base
open Event_types

(* Serialization errors *)
exception Serialization_error of string
exception Deserialization_error of string

(* Binary serialization using Marshal for performance *)
module Binary = struct
  let serialize event =
    try
      Stdlib.Marshal.to_string event []
    with
    | exn -> 
      raise (Serialization_error ("Failed to serialize event: " ^ (Exn.to_string exn)))

  let deserialize data =
    try
      Stdlib.Marshal.from_string data 0
    with
    | exn ->
      raise (Deserialization_error ("Failed to deserialize event: " ^ (Exn.to_string exn)))

  let serialize_payload payload =
    try
      Stdlib.Marshal.to_string payload []
    with
    | exn ->
      raise (Serialization_error ("Failed to serialize payload: " ^ (Exn.to_string exn)))

  let deserialize_payload data =
    try
      Stdlib.Marshal.from_string data 0
    with
    | exn ->
      raise (Deserialization_error ("Failed to deserialize payload: " ^ (Exn.to_string exn)))
end

(* JSON serialization for debugging and cross-service compatibility *)
module Json = struct
  open Yojson.Safe

  let event_to_json event =
    let payload_json = match event.payload with
      | OrderEvent payload -> 
        `Assoc [("type", `String "order"); ("data", `String (show_order_event_payload payload))]
      | MarketEvent payload ->
        `Assoc [("type", `String "market"); ("data", `String (show_market_event_payload payload))]
      | RiskEvent payload ->
        `Assoc [("type", `String "risk"); ("data", `String (show_risk_event_payload payload))]
      | SystemEvent payload ->
        `Assoc [("type", `String "system"); ("data", `String (show_system_event_payload payload))]
    in
    `Assoc [
      ("id", `String event.id);
      ("subject", `String event.subject);
      ("payload", payload_json);
      ("timestamp", `Float event.timestamp);
      ("correlation_id", match event.correlation_id with 
        | Some id -> `String id 
        | None -> `Null);
      ("causation_id", match event.causation_id with 
        | Some id -> `String id 
        | None -> `Null);
      ("version", `Int event.version);
    ]

  let serialize event =
    try
      to_string (event_to_json event)
    with
    | exn ->
      raise (Serialization_error ("Failed to serialize event to JSON: " ^ (Exn.to_string exn)))

  (* Note: JSON deserialization is more complex and would require 
     proper parsing of the payload data. For now, we'll focus on 
     binary serialization for performance. *)
end

(* Default serialization format *)
let serialize = Binary.serialize
let deserialize = Binary.deserialize
let serialize_payload = Binary.serialize_payload
let deserialize_payload = Binary.deserialize_payload

(* Utility functions *)
let event_size event =
  String.length (serialize event)

let payload_size payload =
  String.length (serialize_payload payload)

(* Compression helpers for large events *)
let compress_if_large ?(threshold=1024) data =
  if String.length data > threshold then
    (* In a real implementation, you'd use a compression library like zlib *)
    (* For now, we'll just return the original data *)
    data
  else
    data

let decompress data =
  (* Placeholder for decompression logic *)
  data