(** Gossip Protocol Module
    
    Epidemic-style message propagation for the P2P network.
*)

open Ambience_core.Types
open Protocol
open Peer

(** {1 Configuration} *)

type config = {
  fanout: int;
  max_hops: int;
  cache_size: int;
  cache_ttl: float;
  forward_probability: float;
  pull_interval: float;
}

val default_config : config

(** {1 Gossip Engine} *)

type gossip_engine
type gossip_message

val create_engine : peer_manager -> ?config:config -> unit -> gossip_engine

(** {1 Message Operations} *)

val broadcast : gossip_engine -> protocol_message -> unit
(** Broadcast a new message to the network *)

val handle_received : gossip_engine -> gossip_message -> unit
(** Handle a received gossip message *)

(** {1 Gossip Strategies} *)

type strategy =
  | Flood
  | Random
  | Proximity
  | Gradient

val apply_strategy : gossip_engine -> strategy -> protocol_message -> unit

(** {1 Engine Control} *)

val start : gossip_engine -> unit
val stop : gossip_engine -> unit

(** {1 Statistics} *)

type stats = {
  messages_sent: int;
  messages_received: int;
  messages_dropped: int;
  cache_size: int;
  seen_messages: int;
  cache_hit_rate: float;
}

val get_stats : gossip_engine -> stats

(** {1 Advanced Algorithms} *)

module Advanced : sig
  type plumtree
  val create_plumtree : unit -> plumtree
  
  type hyparview = {
    active_view: public_key list;
    passive_view: public_key list;
  }
  
  type tman = {
    ranking_function: peer_info -> peer_info -> int;
    view_size: int;
  }
end