(* Main NATS client library interface *)

(* Re-export types and modules *)
module Protocol = Nats_protocol
module Connection = Nats_connection  
module Client = Nats_client

(* Re-export main types for convenience *)
type client = Nats_client.client
type subscription = Nats_client.subscription
type message = Nats_protocol.message
type connection_config = Nats_connection.connection_config

(* Re-export main functions *)
let create = Nats_client.create
let connect = Nats_client.connect
let disconnect = Nats_client.disconnect
let is_connected = Nats_client.is_connected

let publish = Nats_client.publish
let publish_string = Nats_client.publish_string

let subscribe = Nats_client.subscribe
let subscribe_string = Nats_client.subscribe_string
let unsubscribe = Nats_client.unsubscribe

let request = Nats_client.request

(* Utility functions *)
let get_server_info = Nats_client.get_server_info
let get_connection_config = Nats_client.get_connection_config

(* Exceptions *)
exception Client_error = Nats_client.Client_error
exception Not_connected = Nats_client.Not_connected
exception Protocol_error = Nats_protocol.Protocol_error
exception Connection_closed = Nats_protocol.Connection_closed
exception Invalid_message = Nats_protocol.Invalid_message