(** Dynamic Resource Registry

    A distributed, community-driven registry for resource types.
    Resources are discovered automatically when agents use them,
    and gain credibility through endorsements and successful trades.
*)

open Types

(** Resource metadata *)
type resource_metadata = {
  uri: string;
  discovered_at: timestamp;
  first_seen_from: public_key;
  description: string option;
  properties: (string * string) list;  (* Key-value metadata *)
  parent: string option;               (* Hierarchical organization *)
  mutable total_usage: int;            (* Times seen in intents *)
  mutable successful_trades: int;      (* Successful settlements *)
  mutable failed_trades: int;          (* Failed settlements *)
}

(** Endorsement of a resource type *)
type endorsement = {
  agent_id: public_key;
  reputation_stake: float;  (* 0.1 to 100.0 reputation points *)
  timestamp: timestamp;
  notes: string option;
  mutable outcome: endorsement_outcome option;
}

and endorsement_outcome =
  | Validated     (* Resource proven legitimate through trades *)
  | Invalidated   (* Resource proven fake/undeliverable *)
  | Pending       (* Not yet determined *)

(** Usage statistics *)
type usage_stats = {
  first_seen: timestamp;
  last_seen: timestamp;
  total_intents: int;
  total_matches: int;
  total_settlements: int;
  success_rate: float;
  avg_reputation_users: float;
}

(** Registry type *)
type t

(** Registry events for monitoring *)
type registry_event =
  | ResourceDiscovered of string * public_key * timestamp
  | ResourceEndorsed of string * endorsement
  | ResourceUsed of string * string * bool  (* uri, match_id, success *)
  | ResourceMapped of string * string       (* uri, canonical_uri *)

(** {1 Creation and Management} *)

val create : unit -> t
(** Create a new registry *)

val clear : t -> unit
(** Clear all registry entries *)

(** {1 Resource Discovery} *)

val discover : t -> uri:string -> agent_id:public_key -> ?description:string ->
              ?properties:(string * string) list -> ?parent:string -> unit -> unit
(** Discover/register a new resource type *)

val exists : t -> string -> bool
(** Check if a resource URI exists in the registry *)

val lookup : t -> string -> resource_metadata option
(** Look up metadata for a resource *)

val search : t -> pattern:string -> resource_metadata list
(** Search for resources matching a pattern *)

val list_all : t -> resource_metadata list
(** List all known resources *)

(** {1 Endorsement System} *)

val endorse : t -> uri:string -> agent_id:public_key ->
             stake:float -> ?notes:string -> unit -> (unit, string) result
(** Endorse a resource type by staking reputation *)

val get_endorsements : t -> string -> endorsement list
(** Get all endorsements for a resource *)

val get_endorsement_score : t -> string -> float
(** Calculate weighted endorsement score (0.0 to 1.0) *)

val update_endorsement_outcome : t -> uri:string -> agent_id:public_key ->
                                 outcome:endorsement_outcome -> unit
(** Update endorsement outcome after trades *)

(** {1 Usage Tracking} *)

val record_usage : t -> uri:string -> usage_type:[`Intent|`Match|`Settlement] ->
                  success:bool -> unit
(** Record resource usage *)

val get_usage_stats : t -> string -> usage_stats option
(** Get usage statistics for a resource *)

val get_credibility_score : t -> string -> float
(** Calculate overall credibility (endorsements + usage) *)

(** {1 Resource Relationships} *)

val suggest_parent : t -> uri:string -> parent:string -> agent_id:public_key -> unit
(** Suggest a parent category for a resource *)

val get_children : t -> string -> string list
(** Get child resources of a category *)

val get_hierarchy : t -> string -> (string * int) list
(** Get full hierarchy path (uri, depth) pairs *)

val find_similar : t -> string -> limit:int -> (string * float) list
(** Find similar resources by properties and usage patterns *)

(** {1 Analytics} *)

val trending : t -> ?window:float -> limit:int -> (string * float) list
(** Get trending resources (uri, score) *)

val get_innovation_rate : t -> float
(** Rate of new resource discovery per day *)

val get_endorsement_accuracy : t -> public_key -> float
(** Get agent's endorsement accuracy rate *)

(** {1 Synchronization} *)

type registry_update = {
  update_type: [`Discovery | `Endorsement | `Usage];
  resource_uri: string;
  data: string;  (* JSON-encoded update data *)
  timestamp: timestamp;
  source: public_key;
}

val get_updates_since : t -> timestamp -> registry_update list
(** Get all updates since a timestamp for gossip *)

val apply_update : t -> registry_update -> unit
(** Apply an update from a remote registry *)

val merge : t -> t -> unit
(** Merge another registry into this one *)

(** {1 Persistence} *)

val to_json : t -> Yojson.Safe.t
(** Serialize registry to JSON *)

val from_json : Yojson.Safe.t -> t
(** Deserialize registry from JSON *)

(** {1 Seeding and Defaults} *)

val seed_common_resources : t -> unit
(** Seed with commonly used resources (USD, BTC, GPU-hours, etc.) *)

val add_mapping : t -> local:string -> canonical:string -> unit
(** Add mapping between local and canonical resource names *)

val resolve : t -> string -> string
(** Resolve a resource URI to its canonical form *)