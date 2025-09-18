(** State management module
    
    Manages the global protocol state including agents, intents, matches,
    and settlements.
*)

open Types

(** {1 State Type} *)

type t
(** The global protocol state *)

type agent_info = {
  public_key: public_key;
  joined_at: timestamp;
  last_seen: timestamp;
  capabilities: string list;
  metadata: (string * string) list;
}
(** Information about a registered agent *)

type negotiation = {
  match_id: uuid;
  participants: public_key list;
  state: negotiation_state;
  proposals: (public_key * settlement_point * timestamp) list;
  started_at: timestamp;
  last_activity: timestamp;
}
(** State of an ongoing negotiation *)

type stats = {
  total_intents_posted: int;
  total_matches_found: int;
  total_settlements_completed: int;
  total_volume_transacted: float;
  active_intents: int;
  active_agents: int;
  state_version: int;
}
(** Protocol statistics *)

(** {1 State Creation} *)

val create : unit -> t
(** Create a new empty state *)

(** {1 State Transitions} *)

module Transitions : sig
  val register_agent : t -> public_key -> string list -> (unit, string) result
  (** Register a new agent with capabilities *)
  
  val post_intent : t -> intent -> (unit, string) result
  (** Post a new intent to the state *)
  
  val record_match : t -> match_t -> (unit, string) result
  (** Record a discovered match *)
  
  val start_negotiation : t -> uuid -> (unit, string) result
  (** Start negotiation for a match *)
  
  val execute_settlement : t -> settlement -> (unit, string) result
  (** Execute a settlement *)
  
  val cancel_intent : t -> uuid -> public_key -> (unit, string) result
  (** Cancel an intent (only by owner) *)
end

(** {1 Query Operations} *)

module Queries : sig
  val get_active_intents : t -> intent list
  (** Get all currently active intents *)
  
  val get_agent_intents : t -> public_key -> intent list
  (** Get all intents posted by an agent *)
  
  val get_resource_intents : t -> resource_uri -> intent list
  (** Get all intents for a specific resource type *)
  
  val get_intent_matches : t -> uuid -> match_t list
  (** Get all matches involving an intent *)
  
  val get_reputation : t -> public_key -> reputation option
  (** Get reputation score for an agent *)
  
  val get_stats : t -> stats
  (** Get protocol statistics *)
end

(** {1 Persistence} *)

module Persistence : sig
  val to_json : t -> Yojson.Safe.t
  (** Serialize state to JSON *)
  
  val save_checkpoint : t -> string -> unit
  (** Save state checkpoint to file *)
  
  val load_checkpoint : string -> (t, string) result
  (** Load state from checkpoint file *)
end