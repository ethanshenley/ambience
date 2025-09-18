(** Intent management module
    
    Handles creation, validation, and manipulation of intents - the core
    primitive of ambient commerce.
*)

open Types

(** {1 Creation and Validation} *)

val create : 
  agent_id:public_key ->
  offers:resource_field ->
  wants:resource_field ->
  ?constraints:constraint_t list ->
  ?lifecycle:lifecycle ->
  ?priority:float ->
  unit ->
  (intent, string) result
(** Create a new intent with validation.
    Returns [Error] if any field is invalid. *)

val is_valid : intent -> timestamp -> bool
(** Check if an intent is still valid at a given time *)

val constraints_satisfied : intent -> timestamp -> bool
(** Check if an intent's constraints are satisfied at a given time *)

(** {1 Compatibility and Matching} *)

val are_compatible : intent -> intent -> bool
(** Quick check if two intents could potentially match *)

val priority_score : intent -> float
(** Calculate priority score for matching order *)

val meets_counterparty_requirements : 
  intent -> public_key -> float -> bool
(** Check if an agent meets the counterparty requirements of an intent *)

(** {1 Intent Manipulation} *)

val consume_match : intent -> (intent, string) result
(** Consume one match from a consumable intent *)

val cancel : intent -> intent
(** Cancel an intent by marking it as expired *)

val extend_expiry : intent -> float -> (intent, string) result
(** Extend the expiry time of an intent *)

(** {1 Information Extraction} *)

val get_time_windows : intent -> (timestamp * timestamp) list
(** Extract time window constraints from an intent *)

val get_price_ranges : intent -> (float * float) list
(** Extract price range constraints from an intent *)

val get_counterparty_requirements : intent -> counterparty_req list
(** Extract counterparty requirements from an intent *)

(** Intent statistics *)
type intent_stats = {
  age: float;
  time_remaining: float option;
  matches_remaining: int option;
  is_active: bool;
}

val get_stats : intent -> intent_stats
(** Get statistics about an intent *)

(** {1 Utility Functions} *)

val generate_uuid : unit -> uuid
(** Generate a new UUID *)

val now : unit -> timestamp
(** Get current timestamp *)

val calculate_commitment : intent -> string
(** Calculate cryptographic commitment for an intent *)

val to_string : intent -> string
(** Convert intent to string representation for debugging *)

(** {1 Batch Operations} *)

module Batch : sig
  val filter_valid : intent list -> timestamp -> intent list
  (** Filter to only valid intents *)
  
  val sort_by_priority : intent list -> intent list
  (** Sort intents by priority (highest first) *)
  
  val find_compatible_pairs : intent list -> (intent * intent) list
  (** Find all potentially compatible intent pairs *)
  
  val group_by_resource : intent list -> (string, intent list) Hashtbl.t
  (** Group intents by resource type for efficient matching *)
end