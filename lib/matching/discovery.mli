(** Match Discovery Module
    
    Efficient discovery of compatible intent combinations.
*)

open Ambience_core.Types

(** {1 Discovery Strategy} *)

type strategy = 
  | Exhaustive              
  | Indexed                  
  | Probabilistic of float   
  | Hybrid

(** {1 Discovery Context} *)

type context

val create_context : 
  ?strategy:strategy -> 
  ?max_path_length:int -> 
  ?cache_enabled:bool -> 
  unit -> context
(** Create a discovery context with configuration *)

val clear_cache : context -> unit
(** Clear the compatibility cache *)

(** {1 Intent Indexing} *)

module Index : sig
  type t
  
  val create : unit -> t
  val add : t -> intent -> unit
  val find_candidates : t -> intent -> intent list
  val build : intent list -> t
end

(** {1 Discovery Functions} *)

val discover_pairs : context -> intent list -> (intent * intent) list
(** Discover compatible intent pairs using configured strategy *)

val check_compatibility : context -> intent -> intent -> bool
(** Check if two intents are compatible (with caching) *)

(** {1 Multi-hop Discovery} *)

type trade_path = {
  intents: intent list;
  resources: resource_uri list;
  cycle: bool;
}

val find_circular_paths : context -> intent list -> int -> trade_path list
(** Find circular trade paths up to given length *)

(** {1 Optimization} *)

val prefilter_intents : intent list -> intent list
(** Pre-filter intents before discovery *)

val partition_by_resource : intent list -> intent list list
(** Partition intents by resource for parallel processing *)

(** {1 Statistics} *)

type stats = {
  total_intents: int;
  pairs_checked: int;
  matches_found: int;
  cache_hit_rate: float;
  discovery_time: float;
}

val discover_with_stats : context -> intent list -> (intent * intent) list * stats
(** Run discovery and return statistics *)