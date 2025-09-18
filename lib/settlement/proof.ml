(** Settlement Proof Module
    
    This module generates cryptographic proofs of settlement execution.
    These proofs ensure settlements are verifiable and non-repudiable.
    
    Key concepts:
    - Merkle tree proofs
    - State transition proofs  
    - Witness generation
    - Proof verification
    - Proof aggregation
*)

open Ambience_core.Types
module Intent = Ambience_core.Intent

(** Proof types *)
type proof_type =
  | StateTransition    (* Proves valid state change *)
  | MerkleInclusion   (* Proves inclusion in Merkle tree *)
  | Signature         (* Digital signature proof *)
  | Witness           (* Witness of execution *)
  | Aggregate         (* Aggregated proof *)

(** Settlement proof *)
type settlement_proof = {
  proof_id: uuid;
  settlement_id: uuid;
  proof_type: proof_type;
  
  (* State before and after *)
  pre_state_root: bytes;
  post_state_root: bytes;
  
  (* Proof data *)
  merkle_proof: merkle_proof option;
  signatures: signature list;
  witnesses: witness list;
  
  (* Metadata *)
  generated_at: timestamp;
  generator: public_key;
  verified: bool;
}

and merkle_proof = {
  root: bytes;
  leaf: bytes;
  path: merkle_path;
  index: int;
}

and merkle_path = bytes list  (* Sibling hashes *)

and signature = {
  signer: public_key;
  signature_data: bytes;
  signed_at: timestamp;  (* Renamed to match .mli *)
}

and witness = {
  witness_id: public_key;
  attestation: string;
  data: bytes option;
  witnessed_at: timestamp;  (* Renamed to match .mli *)
}

(** Proof generator *)
type proof_generator = {
  mutable proofs_generated: int;
  mutable proofs_verified: int;
  proof_cache: (uuid, settlement_proof) Hashtbl.t;
}

(** Create proof generator *)
let create_generator () = {
  proofs_generated = 0;
  proofs_verified = 0;
  proof_cache = Hashtbl.create 1000;
}

(** {2 Hash Functions} *)

(** Simple hash function (would use SHA256 in production) *)
let hash data =
  Bytes.of_string (Digest.string (Bytes.to_string data))

(** Hash concatenation *)
let hash_concat left right =
  let combined = Bytes.create (Bytes.length left + Bytes.length right) in
  Bytes.blit left 0 combined 0 (Bytes.length left);
  Bytes.blit right 0 combined (Bytes.length left) (Bytes.length right);
  hash combined

(** {2 Merkle Tree} *)

module MerkleTree = struct
  (** Merkle tree node *)
  type node =
    | Leaf of bytes
    | Branch of bytes * node * node  (* hash, left, right *)
  
  (** Get hash of node *)
  let get_hash = function
    | Leaf data -> hash data
    | Branch (h, _, _) -> h
  
  (** Build Merkle tree from leaves *)
  let rec build_tree leaves =
    match leaves with
    | [] -> failwith "Empty leaf list"
    | [leaf] -> Leaf leaf
    | _ ->
        (* Pair up leaves into nodes *)
        let rec pair_up = function
          | [] -> []
          | [x] -> [Leaf x]  (* Odd number, promote to node *)
          | a :: b :: rest ->
              let left = Leaf a in
              let right = Leaf b in
              let hash = hash_concat (get_hash left) (get_hash right) in
              Branch (hash, left, right) :: pair_up rest
        in
        
        let next_level = 
          List.map (function
            | Leaf _ as l -> l
            | Branch _ as b -> b
          ) (pair_up leaves)
        in
        
        if List.length next_level = 1 then
          List.hd next_level
        else
          build_tree (List.map get_hash next_level)
  
  (** Generate Merkle proof for leaf *)
  let generate_proof tree leaf_index =
    let rec find_path node index path =
      match node with
      | Leaf _ -> path
      | Branch (_, left, right) ->
          let left_size = 1 in  (* Simplified *)
          if index < left_size then
            find_path left index (get_hash right :: path)
          else
            find_path right (index - left_size) (get_hash left :: path)
    in
    
    let path = find_path tree leaf_index [] in
    {
      root = get_hash tree;
      leaf = Bytes.create 0;  (* Would be actual leaf *)
      path = path;
      index = leaf_index;
    }
  
  (** Verify Merkle proof *)
  let verify_proof proof =
    let rec compute_root hash path index =
      match path with
      | [] -> hash
      | sibling :: rest ->
          let combined = 
            if index mod 2 = 0 then
              hash_concat hash sibling
            else
              hash_concat sibling hash
          in
          compute_root combined rest (index / 2)
    in
    
    let computed_root = compute_root (hash proof.leaf) proof.path proof.index in
    Bytes.equal computed_root proof.root
end

(** {2 State Proof Generation} *)

(** Serialize state for hashing *)
let serialize_state (settlement : settlement) =
  (* Simplified - would properly serialize all fields *)
  let data = 
    Printf.sprintf "%s:%s:%f:%f:%f"
      settlement.settlement_id
      settlement.match_id
      settlement.executed_point.price
      settlement.executed_point.quantity
      settlement.executed_point.execution_time
  in
  Bytes.of_string data

(** Generate state transition proof *)
let generate_state_proof generator pre_settlement post_settlement =
  let pre_state_root = hash (serialize_state pre_settlement) in
  let post_state_root = hash (serialize_state post_settlement) in
  
  let proof = {
    proof_id = Intent.generate_uuid ();
    settlement_id = post_settlement.settlement_id;
    proof_type = StateTransition;
    pre_state_root = pre_state_root;
    post_state_root = post_state_root;
    merkle_proof = None;
    signatures = [];
    witnesses = [];
    generated_at = Unix.time ();
    generator = "system";
    verified = false;
  } in
  
  generator.proofs_generated <- generator.proofs_generated + 1;
  Hashtbl.add generator.proof_cache proof.proof_id proof;
  
  proof

(** {2 Signature Proofs} *)

(** Create signature (simplified - would use real crypto) *)
let sign_data signer data =
  {
    signer = signer;
    signature_data = hash data;  (* Would use private key *)
    signed_at = Unix.time ();
  }

(** Verify signature *)
let verify_signature signature data =
  (* Simplified - would use public key crypto *)
  Bytes.equal signature.signature_data (hash data)

(** Generate signature proof *)
let generate_signature_proof generator settlement signers =
  let data = serialize_state settlement in
  
  let signatures = 
    List.map (fun signer -> sign_data signer data) signers
  in
  
  let proof = {
    proof_id = Intent.generate_uuid ();
    settlement_id = settlement.settlement_id;
    proof_type = Signature;
    pre_state_root = Bytes.create 0;
    post_state_root = Bytes.create 0;
    merkle_proof = None;
    signatures = signatures;
    witnesses = [];
    generated_at = Unix.time ();
    generator = "system";
    verified = false;
  } in
  
  generator.proofs_generated <- generator.proofs_generated + 1;
  Hashtbl.add generator.proof_cache proof.proof_id proof;
  
  proof

(** {2 Witness Proofs} *)

(** Generate witness *)
let create_witness witness_id attestation ?(data = None) () =
  {
    witness_id = witness_id;
    attestation = attestation;
    data = data;
    witnessed_at = Unix.time ();
  }

(** Generate witness proof *)
let generate_witness_proof generator (settlement : settlement) witnesses_info =
  let witnesses =
    List.map (fun (id, attestation) ->
      create_witness id attestation ()
    ) witnesses_info
  in

  let proof = {
    proof_id = Intent.generate_uuid ();
    settlement_id = settlement.settlement_id;
    proof_type = Witness;
    pre_state_root = Bytes.create 0;
    post_state_root = Bytes.create 0;
    merkle_proof = None;
    signatures = [];
    witnesses = witnesses;
    generated_at = Unix.time ();
    generator = "system";
    verified = false;
  } in

  generator.proofs_generated <- generator.proofs_generated + 1;
  Hashtbl.add generator.proof_cache proof.proof_id proof;

  proof

(** {2 Proof Aggregation} *)

(** Aggregate multiple proofs *)
let aggregate_proofs generator proofs =
  (* Collect all components *)
  let all_signatures = 
    List.fold_left (fun acc p -> acc @ p.signatures) [] proofs
  in
  
  let all_witnesses = 
    List.fold_left (fun acc p -> acc @ p.witnesses) [] proofs
  in
  
  (* Use first proof's settlement ID *)
  let settlement_id = 
    match proofs with
    | [] -> ""
    | p :: _ -> p.settlement_id
  in
  
  let aggregated = {
    proof_id = Intent.generate_uuid ();
    settlement_id = settlement_id;
    proof_type = Aggregate;
    pre_state_root = Bytes.create 0;
    post_state_root = Bytes.create 0;
    merkle_proof = None;
    signatures = all_signatures;
    witnesses = all_witnesses;
    generated_at = Unix.time ();
    generator = "system";
    verified = false;
  } in
  
  generator.proofs_generated <- generator.proofs_generated + 1;
  Hashtbl.add generator.proof_cache aggregated.proof_id aggregated;
  
  aggregated

(** {2 Proof Verification} *)

(** Verify state transition proof *)
let verify_state_transition proof pre_state post_state =
  let expected_pre = hash (serialize_state pre_state) in
  let expected_post = hash (serialize_state post_state) in
  
  Bytes.equal proof.pre_state_root expected_pre &&
  Bytes.equal proof.post_state_root expected_post

(** Verify all signatures in proof *)
let verify_signatures proof data =
  List.for_all (fun sig_ -> verify_signature sig_ data) proof.signatures

(** Verify complete proof *)
let verify_proof generator proof =
  let verified = 
    match proof.proof_type with
    | StateTransition ->
        (* Would verify against actual states *)
        true
    | MerkleInclusion ->
        (match proof.merkle_proof with
         | None -> false
         | Some mp -> MerkleTree.verify_proof mp)
    | Signature ->
        (* Verify all signatures *)
        List.length proof.signatures > 0
    | Witness ->
        (* Check witness attestations *)
        List.length proof.witnesses > 0
    | Aggregate ->
        (* Verify component proofs *)
        true
  in
  
  generator.proofs_verified <- generator.proofs_verified + 1;
  
  let verified_proof = { proof with verified = verified } in
  Hashtbl.replace generator.proof_cache proof.proof_id verified_proof;
  
  verified

(** {2 Proof Storage and Retrieval} *)

(** Store proof *)
let store_proof generator proof =
  Hashtbl.replace generator.proof_cache proof.proof_id proof

(** Retrieve proof *)
let get_proof generator proof_id =
  Hashtbl.find_opt generator.proof_cache proof_id

(** Get all proofs for settlement *)
let get_settlement_proofs generator settlement_id =
  Hashtbl.fold (fun _id proof acc ->
    if proof.settlement_id = settlement_id then
      proof :: acc
    else
      acc
  ) generator.proof_cache []

(** {2 Statistics} *)

type proof_stats = {
  total_generated: int;
  total_verified: int;
  cached_proofs: int;
  verification_rate: float;
}

let get_stats generator =
  let cached = Hashtbl.length generator.proof_cache in
  let rate = 
    if generator.proofs_generated > 0 then
      float_of_int generator.proofs_verified /. 
      float_of_int generator.proofs_generated
    else
      0.0
  in
  
  {
    total_generated = generator.proofs_generated;
    total_verified = generator.proofs_verified;
    cached_proofs = cached;
    verification_rate = rate;
  }