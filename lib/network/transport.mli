(** Transport Layer Module
    
    Network transport abstraction for the protocol.
*)

open Ambience_core.Types

(** {1 Types} *)

type transport_type = 
  | TCP
  | UDP
  | WebSocket
  | HTTP
  | QUIC
  | Memory

type connection_state =
  | Disconnected
  | Connecting
  | Connected
  | Disconnecting
  | Failed of string

type endpoint = {
  transport: transport_type;
  host: string;
  port: int;
  path: string option;
}

type connection

type connection_stats = {
  bytes_sent: int;
  bytes_received: int;
  messages_sent: int;
  messages_received: int;
  connected_since: timestamp option;
  latency_ms: float option;
  packet_loss: float;
}

(** {1 Endpoint Management} *)

val endpoint_from_uri : string -> endpoint
(** Parse endpoint from URI string *)

val endpoint_to_string : endpoint -> string
(** Convert endpoint to string representation *)

(** {1 Connection Management} *)

val create : endpoint -> connection
(** Create a new connection *)

val connect : connection -> (unit, string) result
(** Connect to the endpoint *)

val send : connection -> bytes -> (unit, string) result
(** Send data over connection *)

val receive : connection -> bytes option
(** Receive data (for polling transports) *)

val close : connection -> unit
(** Close the connection *)

val get_stats : connection -> connection_stats
(** Get connection statistics *)

(** {1 Callbacks} *)

val on_message : connection -> (bytes -> unit) -> unit
(** Set message received callback *)

val on_error : connection -> (string -> unit) -> unit
(** Set error callback *)

val on_close : connection -> (unit -> unit) -> unit
(** Set connection closed callback *)

(** {1 Connection Pool} *)

module Pool : sig
  type t
  type pool_stats = {
    active_connections: int;
    total_connections: int;
    bytes_sent: int;
    bytes_received: int;
  }
  
  val create : ?max_connections:int -> unit -> t
  val get_connection : t -> endpoint -> (connection, string) result
  val add_connection : t -> connection -> (unit, string) result
  val remove_connection : t -> endpoint -> unit
  val get_stats : t -> pool_stats
end