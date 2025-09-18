(** Reputation Module
    
    Manages reputation scores and trust relationships in the protocol.
*)

open Ambience_core.Types

(** {1 Types} *)

type dimension =
  | Reliability
  | Timeliness
  | VolumeHandling
  | DisputeResolution
  | PriceDiscovery
  | Liquidity

type score = {
  value: float;
  confidence: float;
  samples: int;
  last_updated: timestamp;
}

type reputation

type interaction_type =
  | Settlement
  | Match
  | Negotiation
  | Arbitration
  | Endorsement

type outcome =
  | Success
  | Failure of string
  | Partial of float
  | Disputed

type verification_level =
  | Unverified
  | BasicVerified
  | IdentityVerified
  | InstitutionBacked

type reputation_manager

(** {1 Manager} *)

val create_manager :
  ?decay_rate:float ->
  ?min_confidence:float ->
  ?max_history:int ->
  ?propagation_depth:int ->
  unit -> reputation_manager

(** {1 Reputation Management} *)

val initialize_reputation :
  reputation_manager -> public_key -> 
  ?verification:verification_level -> unit -> reputation

val record_interaction :
  reputation_manager -> public_key -> public_key -> 
  interaction_type -> outcome -> float -> unit

(** {1 Trust Network} *)

val add_trust :
  reputation_manager -> public_key -> public_key -> float -> 
  (unit, string) result

val get_trust_score :
  reputation_manager -> public_key -> public_key -> float

(** {1 Queries} *)

val get_reputation : reputation_manager -> public_key -> float

val get_dimension_score :
  reputation_manager -> public_key -> dimension -> score option

val meets_threshold :
  reputation_manager -> public_key -> float -> bool

(** {1 Sybil Resistance} *)

val detect_sybil_pattern : reputation_manager -> public_key -> bool

(** {1 Statistics} *)

type reputation_stats = {
  total_agents: int;
  verified_agents: int;
  average_reputation: float;
  high_reputation_agents: int;
  low_reputation_agents: int;
  total_trust_relationships: int;
}

val get_stats : reputation_manager -> reputation_stats