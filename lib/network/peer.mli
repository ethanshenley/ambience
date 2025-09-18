(** Peer Management Module
    
    Manages peer connections and reputation in the P2P network.
*)

open Ambience_core.Types
open Transport
open Protocol

(** Import capability type from trust module *)
type capability = Ambience_trust.Capability.capability

(** {1 Types} *)

type peer_info = {
  peer_id: public_key;
  endpoints: endpoint list;
  first_seen: timestamp;
  last_seen: timestamp;
  reputation_score: float;
  capabilities: capability list;
  metadata: (string * string) list;
  connection_state: peer_connection_state;
  statistics: peer_stats;
}

and peer_connection_state =
  | NotConnected
  | Connecting
  | Connected of connection
  | Disconnecting
  | Banned of string

and peer_stats = {
  messages_sent: int;
  messages_received: int;
  bytes_sent: int;
  bytes_received: int;
  intents_shared: int;
  matches_found: int;
  failed_connections: int;
  average_latency: float;
}

type peer_manager

(** {1 Manager Creation} *)

val create_manager : 
  public_key -> ?max_peers:int -> ?min_peers:int -> unit -> peer_manager

(** {1 Peer Discovery} *)

module Discovery : sig
  type method_ =
    | Bootstrap of endpoint list
    | MDNS
    | DHT
    | Exchange
  
  val bootstrap_peers : peer_manager -> endpoint list -> unit
  val mdns_discovery : peer_manager -> unit
  val peer_exchange : peer_manager -> public_key -> unit
  val handle_peer_list : peer_manager -> (public_key * endpoint list) list -> unit
end

(** {1 Connection Management} *)

val connect_to_peer : peer_manager -> public_key -> (unit, string) result
val disconnect_from_peer : peer_manager -> public_key -> unit
val maintain_connections : peer_manager -> unit

(** {1 Peer Selection} *)

type selection_strategy =
  | Random
  | HighestReputation
  | LowestLatency
  | MostActive
  | Balanced

val select_peers : peer_manager -> selection_strategy -> int -> public_key list

(** {1 Reputation} *)

module Reputation : sig
  type event =
    | SuccessfulConnection
    | FailedConnection
    | ValidMessageReceived
    | InvalidMessageReceived
    | IntentShared
    | MatchFound
    | SettlementCompleted
    | MaliciousBehavior of string
  
  val update_reputation : peer_manager -> public_key -> event -> unit
  val get_reputation : peer_manager -> public_key -> float
end

(** {1 Statistics} *)

type manager_stats = {
  total_peers: int;
  connected_peers: int;
  banned_peers: int;
  average_reputation: float;
  total_messages: int;
  total_bytes: int;
}

val get_manager_stats : peer_manager -> manager_stats