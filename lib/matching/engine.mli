(** Matching Engine
    
    The core engine that discovers matches between intents and computes
    settlement spaces.
*)

open Ambience_core.Types

(** {1 Configuration} *)

type config = {
  matching_interval: float;
  max_intents_per_round: int;
  pareto_samples: int;
  min_match_quality: float;
  enable_multilateral: bool;
  max_settlement_dimensions: int;
}
(** Configuration for the matching engine *)

val default_config : config
(** Default configuration values *)

(** {1 Engine Type} *)

type t
(** The matching engine *)

val create :
  ?config:config ->
  ?reputation_mgr:Ambience_trust.Reputation.reputation_manager ->
  ?capability_mgr:Ambience_trust.Capability.capability_manager ->
  ?collateral_mgr:Ambience_trust.Collateral.collateral_manager ->
  ?escrow_mgr:Ambience_settlement.Escrow.escrow_manager ->
  Ambience_core.State.t -> t
(** Create a new matching engine with optional configuration and trust managers *)

(** {1 Settlement Space Computation} *)

val compute_price_range : intent -> intent -> (float * float) option
(** Compute valid price range for a match *)

val compute_quantity_range : intent -> intent -> (float * float) option
(** Compute valid quantity range for a match *)

val compute_time_window : intent -> intent -> (timestamp * timestamp) option
(** Compute valid time window for execution *)

val compute_settlement_manifold :
  config -> intent -> intent -> settlement_manifold option
(** Compute the settlement space for two intents.
    Returns [None] if no feasible settlement exists. *)

val calculate_match_quality : 
  intent -> intent -> settlement_manifold -> float
(** Calculate quality score for a match (0.0 to 1.0) *)

val find_pareto_frontier : 
  intent -> intent -> settlement_point list -> settlement_point list
(** Find Pareto-optimal points in settlement space *)

(** {1 Match Discovery} *)

val run_matching_round : t -> match_t list
(** Run one round of matching, returning discovered matches *)

val discover_bilateral_matches : config -> intent list -> match_t list
(** Discover two-party matches *)

val discover_multilateral_matches : config -> intent list -> match_t list
(** Discover multi-party matches (3+ participants) *)

(** {1 Engine Control} *)

val start : t -> unit
(** Start the continuous matching engine *)

val stop : t -> unit
(** Stop the matching engine *)

(** {1 Statistics} *)

type stats = {
  rounds_completed: int;
  total_matches_found: int;
  total_time_spent: float;
  average_time_per_round: float;
  matches_per_second: float;
  is_running: bool;
}

val get_stats : t -> stats
(** Get engine statistics *)

(** {1 Settlement Solutions} *)

val find_nash_solution : settlement_manifold -> intent -> intent -> settlement_point option
(** Find Nash bargaining solution that maximizes the product of surplus utilities *)

val find_kalai_smorodinsky_solution : settlement_manifold -> intent -> intent -> settlement_point option
(** Find Kalai-Smorodinsky solution that maintains proportional utilities *)

(** {1 Match Ranking and Filtering} *)

val rank_matches : match_t list -> match_t list
(** Rank matches by quality and urgency *)

val filter_non_conflicting : match_t list -> match_t list
(** Filter to remove conflicting matches *)

(** {1 Advanced Algorithms} *)

module Advanced : sig
  val stable_matching : intent list -> intent list -> (intent * intent) list
  (** Find stable matching using Gale-Shapley algorithm *)
  
  val optimal_matching : 
    intent list -> intent list -> float array array -> (int * int) list
  (** Find optimal matching using Hungarian algorithm *)
  
  val find_clearing_prices : intent list -> (resource_uri * float) list
  (** Find market-clearing prices *)
end