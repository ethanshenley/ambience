(** Core type definitions for the Ambient Commerce Protocol
    
    These types form the foundation of the entire protocol. They are designed
    to be:
    - Transport agnostic (can work over any network)
    - Settlement agnostic (can settle in any currency/resource)
    - Serialization friendly (can be converted to/from JSON)
    
    Design principle: Keep types simple and composable. Complexity should
    emerge from composition, not from complicated base types.
*)

(** Unique identifier for any entity in the system.
    Using strings for simplicity, but in production this should be
    a proper UUID type for guarantees of uniqueness. *)
type uuid = string [@@deriving yojson]

(** Public key identifying an agent.
    In production, this would be an actual cryptographic public key
    (e.g., Ed25519), but we use string for initial simplicity. *)
type public_key = string [@@deriving yojson]

(** Unix timestamp in seconds.
    We use float to allow sub-second precision when needed. *)
type timestamp = float [@@deriving yojson]

(** A rational number for exact representation of quantities.
    This prevents floating point errors in financial calculations. *)
type rational = {
  numerator: int;
  denominator: int;
} [@@deriving yojson]

(** URI identifying a resource type in the ontology.
    Examples:
    - "compute:gpu:nvidia:4090"
    - "currency:fiat:USD"
    - "storage:ssd:gb"
    
    The hierarchical structure allows for inheritance and substitution. *)
type resource_uri = string [@@deriving yojson]

(** Quality can be fungible (all units identical) or graded (quality score).
    This distinction is crucial for matching logic. *)
type quality = 
  | Fungible                    (* All units are identical *)
  | Graded of float              (* Quality score from 0.0 to 1.0 *)
  | Unique of string             (* Non-fungible, unique identifier *)
[@@deriving yojson]

(** A resource field describes what an agent offers or wants.
    It's not a specific amount, but a range of acceptable values.
    This allows for flexible matching and negotiation. *)
type resource_field = {
  resource_type: resource_uri;       (* What type of resource *)
  quantity_range: float * float;     (* Min and max acceptable quantity *)
  quality: quality;                  (* Quality requirements *)
  metadata: (string * string) list;  (* Additional properties *)
} [@@deriving yojson]

(** Constraints limit when and how a match can occur.
    They can be hard (must be satisfied) or soft (preferences). *)
type constraint_t = 
  | TimeWindow of timestamp * timestamp  (* Must occur within this window *)
  | PriceRange of float * float         (* Acceptable price range *)
  | Counterparty of counterparty_req    (* Requirements for other party *)
  | Custom of string * string            (* Extension point for new constraints *)
[@@deriving yojson]

and counterparty_req = {
  min_reputation: float option;         (* Minimum reputation score *)
  required_capabilities: string list;   (* Must have these capabilities *)
  excluded_agents: public_key list;     (* Blacklist *)
  preferred_agents: public_key list;    (* Whitelist/preferences *)
} [@@deriving yojson]

(** Lifecycle determines how long an intent remains valid.
    This is crucial for the "ambient" nature - intents can persist
    and evolve rather than being one-shot orders. *)
type lifecycle = 
  | Eternal                           (* Always valid until explicitly cancelled *)
  | Expiring of timestamp             (* Valid until specific time *)
  | Consumable of int                 (* Valid for N matches *)
  | Conditional of string             (* Valid while condition holds - evaluated externally *)
[@@deriving yojson]

(** An intent is the core primitive of ambient commerce.
    It's NOT an order - it's a declaration of economic desire that
    exists in space until it finds a match. *)
type intent = {
  intent_id: uuid;                    (* Unique identifier *)
  agent_id: public_key;               (* Who posted this intent *)
  
  (* What the agent is offering and wanting *)
  offer_field: resource_field;
  want_field: resource_field;
  
  (* Constraints that must be satisfied for a match *)
  constraints: constraint_t list;
  
  (* How long this intent lives *)
  lifecycle: lifecycle;
  
  (* Metadata *)
  created_at: timestamp;
  updated_at: timestamp;
  
  (* Cryptographic commitment - hash of all above fields *)
  commitment: string;
  
  (* Priority - higher priority intents match first (paid feature) *)
  priority: float;
} [@@deriving yojson]

(** A point in settlement space.
    When two intents match, there's usually not one way to settle,
    but a space of possible settlements. This represents one point. *)
type settlement_point = {
  price: float;                       (* Exchange rate *)
  quantity: float;                    (* Amount to exchange *)
  execution_time: timestamp;         (* When to execute *)
  quality_level: float option;        (* If resource is graded *)
  additional_terms: (string * string) list;  (* Extension point *)
} [@@deriving yojson]

(** The settlement space (manifold) of all possible settlements.
    This is one of the key innovations - matches aren't binary,
    they exist in a continuous space of possibilities. *)
type settlement_manifold = {
  dimensions: string list;            (* e.g., ["price", "time", "quantity"] *)
  
  (* Function that determines if a point is valid.
     In practice, this is represented as constraints that can be evaluated. *)
  valid_region_constraints: constraint_t list;
  
  (* The Pareto-optimal points - settlements where you can't improve
     one dimension without making another worse. *)
  pareto_frontier: settlement_point list;
  
  (* How "good" each point is - used for negotiation *)
  optimality_scores: (settlement_point * float) list option;
} [@@deriving yojson]

(** A match represents the discovery that two or more intents can satisfy
    each other. It doesn't mean a trade has happened, just that one could. *)
type match_t = {
  match_id: uuid;
  intent_ids: uuid list;              (* Can be multi-party *)
  settlement_space: settlement_manifold;
  discovered_at: timestamp;
  discovered_by: public_key;          (* Who found this match - may get rewards *)
  expires_at: timestamp;              (* Matches don't last forever *)
} [@@deriving yojson]

(** Negotiation state for a particular match.
    Agents explore the settlement space to find mutually agreeable point. *)
type negotiation_state = 
  | Proposing                         (* Initial proposals being made *)
  | Negotiating of settlement_point list  (* Offers and counteroffers *)
  | Agreed of settlement_point        (* Agreement reached *)
  | Failed of string                  (* Negotiation failed *)
[@@deriving yojson]

(** A settlement is an executed match - value has actually been exchanged. *)
type settlement = {
  settlement_id: uuid;
  match_id: uuid;
  
  (* The actual point in settlement space that was executed *)
  executed_point: settlement_point;
  
  (* Proof of execution - in practice would be merkle proof or similar *)
  execution_proof: string;
  
  (* State transition *)
  pre_state_hash: string;            (* Hash of state before settlement *)
  post_state_hash: string;           (* Hash of state after *)
  
  (* Reversal window - settlements can be undone within this period *)
  reversible_until: timestamp option;
  
  (* Status *)
  status: settlement_status;
  
  executed_at: timestamp;
} [@@deriving yojson]

and settlement_status = 
  | Pending                           (* Settlement initiated but not complete *)
  | Completed                         (* Successfully executed *)
  | Reversed of string                (* Was reversed - with reason *)
  | Failed of string                  (* Failed to execute *)
[@@deriving yojson]

(** Reputation score for an agent.
    This builds over time through successful settlements. *)
type reputation = {
  agent_id: public_key;
  score: float;                       (* 0.0 to 1.0 *)
  total_settlements: int;
  successful_settlements: int;
  failed_settlements: int;
  total_volume: float;                (* Total value transacted *)
  last_updated: timestamp;
  
  (* Reputation can be domain-specific *)
  domain_scores: (resource_uri * float) list option;
} [@@deriving yojson]

(** Network message types for protocol communication *)
type message = 
  | IntentMessage of intent
  | MatchDiscovery of match_t  
  | NegotiationMessage of uuid * negotiation_state
  | SettlementMessage of settlement
  | HeartbeatMessage of public_key * timestamp
  | ReputationUpdate of reputation
[@@deriving yojson]

(** Common error types in the protocol *)
type protocol_error =
  | InvalidIntent of string
  | MatchingFailed of string
  | SettlementFailed of string
  | NetworkError of string
  | UnauthorizedError of string
[@@deriving yojson]

(** Helper functions *)
let create_uuid () =
  (* Simple UUID v4 implementation *)
  let random_hex n =
    let chars = "0123456789abcdef" in
    String.init n (fun _ -> chars.[Random.int 16])
  in
  Printf.sprintf "%s-%s-%s-%s-%s"
    (random_hex 8) (random_hex 4) (random_hex 4)
    (random_hex 4) (random_hex 12)

let current_timestamp () = Time_provider.now ()