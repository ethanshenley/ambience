(** Settlement Proof Module
    
    Generates and verifies cryptographic proofs of settlement.
*)

open Ambience_core.Types

(** {1 Types} *)

type proof_type =
  | StateTransition
  | MerkleInclusion
  | Signature
  | Witness
  | Aggregate

type settlement_proof = {
  proof_id: uuid;
  settlement_id: uuid;
  proof_type: proof_type;
  pre_state_root: bytes;
  post_state_root: bytes;
  merkle_proof: merkle_proof option;
  signatures: signature list;
  witnesses: witness list;
  generated_at: timestamp;
  generator: public_key;
  verified: bool;
}

and merkle_proof = {
  root: bytes;
  leaf: bytes;
  path: bytes list;
  index: int;
}

and signature = {
  signer: public_key;
  signature_data: bytes;
  signed_at: timestamp;  (* Renamed to avoid conflict with witness.witnessed_at *)
}

and witness = {
  witness_id: public_key;
  attestation: string;
  data: bytes option;
  witnessed_at: timestamp;  (* Renamed to avoid conflict with signature.signed_at *)
}

type proof_generator

(** {1 Generator} *)

val create_generator : unit -> proof_generator

(** {1 Proof Generation} *)

val generate_state_proof : 
  proof_generator -> settlement -> settlement -> settlement_proof

val generate_signature_proof :
  proof_generator -> settlement -> public_key list -> settlement_proof

val generate_witness_proof :
  proof_generator -> settlement -> (public_key * string) list -> settlement_proof

val aggregate_proofs :
  proof_generator -> settlement_proof list -> settlement_proof

(** {1 Proof Verification} *)

val verify_proof : proof_generator -> settlement_proof -> bool
val verify_state_transition : settlement_proof -> settlement -> settlement -> bool
val verify_signatures : settlement_proof -> bytes -> bool

(** {1 Storage} *)

val store_proof : proof_generator -> settlement_proof -> unit
val get_proof : proof_generator -> uuid -> settlement_proof option
val get_settlement_proofs : proof_generator -> uuid -> settlement_proof list

(** {1 Merkle Tree} *)

module MerkleTree : sig
  type node
  val build_tree : bytes list -> node
  val generate_proof : node -> int -> merkle_proof
  val verify_proof : merkle_proof -> bool
end

(** {1 Statistics} *)

type proof_stats = {
  total_generated: int;
  total_verified: int;
  cached_proofs: int;
  verification_rate: float;
}

val get_stats : proof_generator -> proof_stats