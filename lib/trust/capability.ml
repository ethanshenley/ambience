(** Capability Module
    
    This module manages agent capabilities and permissions in the protocol.
    Capabilities define what actions an agent is authorized to perform.
    
    Key concepts:
    - Capability certificates
    - Capability verification
    - Delegation chains
    - Revocation lists
    - Capability inheritance
*)

open Ambience_core.Types
module Intent = Ambience_core.Intent  (* Import Intent module for generate_uuid *)

(** Capability types *)
type capability = 
  | CanTrade of resource_uri         (* Can trade specific resource *)
  | CanMatch                          (* Can discover matches *)
  | CanSettle                         (* Can execute settlements *)
  | CanArbitrate                      (* Can arbitrate disputes *)
  | CanProvideEscrow                  (* Can act as escrow agent *)
  | CanIssueCredentials              (* Can issue capability credentials *)
  | MaxVolume of float                (* Maximum transaction volume *)
  | MaxFrequency of int * duration    (* Max transactions per time period *)
  | ResourceLimit of resource_uri * float  (* Limit for specific resource *)
  | Custom of string * string         (* Extensible capabilities *)

and duration = Hour | Day | Week | Month

(** Capability certificate *)
type certificate = {
  cert_id: uuid;
  subject: public_key;                (* Who has the capability *)
  issuer: public_key;                 (* Who issued it *)
  capabilities: capability list;      (* What they can do *)
  
  (* Validity *)
  issued_at: timestamp;
  expires_at: timestamp option;
  revoked: bool;
  
  (* Delegation *)
  delegatable: bool;                  (* Can be delegated to others *)
  delegation_depth: int;              (* How many levels of delegation allowed *)
  parent_cert: uuid option;           (* If this is a delegated cert *)
  
  (* Proof *)
  signature: bytes;                   (* Issuer's signature *)
  proof_of_authority: bytes option;   (* Proof issuer can issue this *)
  
  (* Metadata *)
  metadata: (string * string) list;
}

(** Capability manager *)
type capability_manager = {
  certificates: (uuid, certificate) Hashtbl.t;
  by_subject: (public_key, certificate list) Hashtbl.t;
  by_issuer: (public_key, certificate list) Hashtbl.t;
  revocation_list: uuid list ref;
  
  (* Trust anchors - root authorities *)
  trust_anchors: public_key list;
  
  (* Configuration *)
  max_delegation_depth: int;
  require_proof_of_authority: bool;
  
  (* Statistics *)
  mutable certificates_issued: int;
  mutable certificates_revoked: int;
  mutable verifications_performed: int;
}

(** Create capability manager *)
let create_manager ?(trust_anchors = []) ?(max_delegation_depth = 3) 
    ?(require_proof = false) () =
  {
    certificates = Hashtbl.create 1000;
    by_subject = Hashtbl.create 100;
    by_issuer = Hashtbl.create 100;
    revocation_list = ref [];
    trust_anchors = trust_anchors;
    max_delegation_depth = max_delegation_depth;
    require_proof_of_authority = require_proof;
    certificates_issued = 0;
    certificates_revoked = 0;
    verifications_performed = 0;
  }

(** {2 Certificate Creation} *)

(** Sign data (simplified - would use real crypto) *)
let sign_data issuer data =
  (* In production, use private key to sign *)
  Bytes.of_string (Digest.string (issuer ^ Bytes.to_string data))

(** Create capability certificate *)
let issue_certificate manager ~issuer ~subject ~capabilities 
    ?(expires_in = 86400.0 *. 30.0) (* 30 days default *)
    ?(delegatable = false) ?(parent_cert = None) () =
  
  (* Check if issuer can issue these capabilities *)
  let can_issue = 
    (* Check if issuer is trust anchor *)
    List.mem issuer manager.trust_anchors ||
    (* Or check if issuer has CanIssueCredentials capability *)
    (match Hashtbl.find_opt manager.by_subject issuer with
     | None -> false
     | Some certs ->
         List.exists (fun cert ->
           not cert.revoked &&
           List.mem CanIssueCredentials cert.capabilities
         ) certs)
  in
  
  if not can_issue then
    Error "Issuer not authorized to issue certificates"
  else
    let cert_id = Intent.generate_uuid () in
    let current_time = Ambience_core.Time_provider.now () in
    
    (* Calculate delegation depth *)
    let delegation_depth = 
      match parent_cert with
      | None -> manager.max_delegation_depth
      | Some parent_id ->
          match Hashtbl.find_opt manager.certificates parent_id with
          | None -> 0
          | Some parent -> max 0 (parent.delegation_depth - 1)
    in
    
    if delegation_depth <= 0 && parent_cert <> None then
      Error "Maximum delegation depth exceeded"
    else
      (* Create certificate *)
      let cert_data = 
        Bytes.concat Bytes.empty [
          Bytes.of_string cert_id;
          Bytes.of_string subject;
          Bytes.of_string issuer;
          Bytes.of_string (string_of_float current_time);
        ]
      in
      
      let certificate = {
        cert_id = cert_id;
        subject = subject;
        issuer = issuer;
        capabilities = capabilities;
        issued_at = current_time;
        expires_at = Some (current_time +. expires_in);
        revoked = false;
        delegatable = delegatable;
        delegation_depth = delegation_depth;
        parent_cert = parent_cert;
        signature = sign_data issuer cert_data;
        proof_of_authority = None;
        metadata = [];
      } in
      
      (* Store certificate *)
      Hashtbl.add manager.certificates cert_id certificate;
      
      (* Index by subject *)
      let subject_certs = 
        try Hashtbl.find manager.by_subject subject
        with Not_found -> []
      in
      Hashtbl.replace manager.by_subject subject (certificate :: subject_certs);
      
      (* Index by issuer *)
      let issuer_certs = 
        try Hashtbl.find manager.by_issuer issuer
        with Not_found -> []
      in
      Hashtbl.replace manager.by_issuer issuer (certificate :: issuer_certs);
      
      manager.certificates_issued <- manager.certificates_issued + 1;
      
      Ok certificate

(** {2 Capability Verification} *)

(** Check if capability is valid *)
let is_capability_valid cap current_time =
  match cap with
  | MaxFrequency (limit, duration) ->
      (* Would check against actual usage *)
      true
  | MaxVolume volume ->
      volume > 0.0
  | _ -> true

(** Verify certificate chain *)
let rec verify_chain manager cert =
  (* Check if revoked *)
  if List.mem cert.cert_id !(manager.revocation_list) then
    false
  else
    (* Check expiry *)
    let current_time = Ambience_core.Time_provider.now () in
    match cert.expires_at with
    | Some expiry when expiry < current_time -> false
    | _ ->
        (* Check parent if delegated *)
        match cert.parent_cert with
        | None ->
            (* Root certificate - check if issuer is trust anchor *)
            List.mem cert.issuer manager.trust_anchors
        | Some parent_id ->
            match Hashtbl.find_opt manager.certificates parent_id with
            | None -> false
            | Some parent ->
                (* Verify parent recursively *)
                verify_chain manager parent

(** Check if agent has capability *)
let has_capability manager agent capability =
  manager.verifications_performed <- manager.verifications_performed + 1;
  
  match Hashtbl.find_opt manager.by_subject agent with
  | None -> false
  | Some certs ->
      List.exists (fun cert ->
        not cert.revoked &&
        verify_chain manager cert &&
        List.mem capability cert.capabilities
      ) certs

(** Get all capabilities for agent *)
let get_capabilities manager agent =
  match Hashtbl.find_opt manager.by_subject agent with
  | None -> []
  | Some certs ->
      certs
      |> List.filter (fun cert ->
          not cert.revoked && verify_chain manager cert)
      |> List.map (fun cert -> cert.capabilities)
      |> List.flatten
      |> List.sort_uniq compare

(** {2 Delegation} *)

(** Delegate capability to another agent *)
let delegate_capability manager cert_id delegator delegatee 
    ?(capabilities = None) ?(expires_in = 86400.0 *. 7.0) () =
  
  match Hashtbl.find_opt manager.certificates cert_id with
  | None -> Error "Certificate not found"
  | Some cert ->
      if cert.subject <> delegator then
        Error "Only the subject can delegate"
      else if not cert.delegatable then
        Error "Certificate is not delegatable"
      else if cert.delegation_depth <= 0 then
        Error "Maximum delegation depth reached"
      else
        (* Create delegated certificate *)
        let delegated_caps = 
          match capabilities with
          | None -> cert.capabilities  (* Delegate all *)
          | Some caps ->
              (* Only delegate capabilities that delegator has *)
              List.filter (fun cap ->
                List.mem cap cert.capabilities
              ) caps
        in
        
        issue_certificate manager 
          ~issuer:delegator
          ~subject:delegatee
          ~capabilities:delegated_caps
          ~expires_in:expires_in
          ~delegatable:true
          ~parent_cert:(Some cert_id)
          ()

(** {2 Revocation} *)

(** Revoke certificate *)
let rec revoke_certificate manager cert_id revoker reason =
  match Hashtbl.find_opt manager.certificates cert_id with
  | None -> Error "Certificate not found"
  | Some cert ->
      (* Check if revoker can revoke *)
      if cert.issuer <> revoker && 
         not (List.mem revoker manager.trust_anchors) then
        Error "Not authorized to revoke"
      else
        (* Mark as revoked *)
        let revoked_cert = { 
          cert with 
          revoked = true;
          metadata = ("revoked_by", revoker) :: 
                    ("revoked_reason", reason) ::
                    ("revoked_at", string_of_float (Ambience_core.Time_provider.now ())) ::
                    cert.metadata
        } in
        
        Hashtbl.replace manager.certificates cert_id revoked_cert;
        manager.revocation_list := cert_id :: !(manager.revocation_list);
        manager.certificates_revoked <- manager.certificates_revoked + 1;
        
        (* Recursively revoke delegated certificates *)
        Hashtbl.iter (fun _id other_cert ->
          if other_cert.parent_cert = Some cert_id then
            let _ = revoke_certificate manager other_cert.cert_id revoker
                     "Parent certificate revoked" in
            ()
        ) manager.certificates;
        
        Ok ()

(** {2 Capability Checking} *)

(** Check if agent can trade resource *)
let can_trade manager agent resource_uri =
  has_capability manager agent (CanTrade resource_uri) ||
  has_capability manager agent (CanTrade "any")

(** Check if agent can execute settlement *)
let can_settle manager agent =
  has_capability manager agent CanSettle

(** Check if agent can arbitrate *)
let can_arbitrate manager agent =
  has_capability manager agent CanArbitrate

(** Check volume limit *)
let check_volume_limit manager agent volume =
  let caps = get_capabilities manager agent in
  
  let volume_limit = 
    List.fold_left (fun current_limit cap ->
      match cap with
      | MaxVolume limit -> min current_limit limit
      | _ -> current_limit
    ) infinity caps
  in
  
  volume <= volume_limit

(** {2 Trust Anchor Management} *)

(** Add trust anchor *)
let add_trust_anchor manager anchor =
  if not (List.mem anchor manager.trust_anchors) then
    { manager with trust_anchors = anchor :: manager.trust_anchors }
  else
    manager

(** Remove trust anchor *)
let remove_trust_anchor manager anchor =
  { manager with 
    trust_anchors = List.filter (fun a -> a <> anchor) manager.trust_anchors }

(** {2 Statistics} *)

type capability_stats = {
  total_certificates: int;
  active_certificates: int;
  revoked_certificates: int;
  verifications_performed: int;
  average_delegation_depth: float;
}

let get_stats manager =
  let active = 
    Hashtbl.fold (fun _id cert acc ->
      if not cert.revoked then acc + 1 else acc
    ) manager.certificates 0
  in
  
  let total_depth = 
    Hashtbl.fold (fun _id cert acc ->
      if cert.parent_cert <> None then
        acc + (manager.max_delegation_depth - cert.delegation_depth)
      else
        acc
    ) manager.certificates 0
  in
  
  let delegated_count = 
    Hashtbl.fold (fun _id cert acc ->
      if cert.parent_cert <> None then acc + 1 else acc
    ) manager.certificates 0
  in
  
  let avg_depth = 
    if delegated_count > 0 then
      float_of_int total_depth /. float_of_int delegated_count
    else
      0.0
  in
  
  {
    total_certificates = Hashtbl.length manager.certificates;
    active_certificates = active;
    revoked_certificates = List.length !(manager.revocation_list);
    verifications_performed = manager.verifications_performed;
    average_delegation_depth = avg_depth;
  }