(** Capability Module
    
    Manages agent capabilities and permissions in the protocol.
*)

open Ambience_core.Types

(** {1 Types} *)

type capability = 
  | CanTrade of resource_uri
  | CanMatch
  | CanSettle
  | CanArbitrate
  | CanProvideEscrow
  | CanIssueCredentials
  | MaxVolume of float
  | MaxFrequency of int * duration
  | ResourceLimit of resource_uri * float
  | Custom of string * string

and duration = Hour | Day | Week | Month

type certificate = {
  cert_id: uuid;
  subject: public_key;
  issuer: public_key;
  capabilities: capability list;
  issued_at: timestamp;
  expires_at: timestamp option;
  revoked: bool;
  delegatable: bool;
  delegation_depth: int;
  parent_cert: uuid option;
  signature: bytes;
  proof_of_authority: bytes option;
  metadata: (string * string) list;
}

type capability_manager

(** {1 Manager} *)

val create_manager :
  ?trust_anchors:public_key list ->
  ?max_delegation_depth:int ->
  ?require_proof:bool ->
  unit -> capability_manager

(** {1 Certificate Operations} *)

val issue_certificate :
  capability_manager ->
  issuer:public_key ->
  subject:public_key ->
  capabilities:capability list ->
  ?expires_in:float ->
  ?delegatable:bool ->
  ?parent_cert:uuid option ->
  unit -> (certificate, string) result

val revoke_certificate :
  capability_manager -> uuid -> public_key -> string -> (unit, string) result

(** {1 Verification} *)

val has_capability : capability_manager -> public_key -> capability -> bool
val get_capabilities : capability_manager -> public_key -> capability list
val verify_chain : capability_manager -> certificate -> bool

(** {1 Delegation} *)

val delegate_capability :
  capability_manager -> uuid -> public_key -> public_key ->
  ?capabilities:capability list option ->
  ?expires_in:float ->
  unit -> (certificate, string) result

(** {1 Capability Checking} *)

val can_trade : capability_manager -> public_key -> resource_uri -> bool
val can_settle : capability_manager -> public_key -> bool
val can_arbitrate : capability_manager -> public_key -> bool
val check_volume_limit : capability_manager -> public_key -> float -> bool

(** {1 Trust Anchors} *)

val add_trust_anchor : capability_manager -> public_key -> capability_manager
val remove_trust_anchor : capability_manager -> public_key -> capability_manager

(** {1 Statistics} *)

type capability_stats = {
  total_certificates: int;
  active_certificates: int;
  revoked_certificates: int;
  verifications_performed: int;
  average_delegation_depth: float;
}

val get_stats : capability_manager -> capability_stats