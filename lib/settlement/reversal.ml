(** Settlement Reversal Module
    
    This module handles the reversal and cancellation of settlements.
    It ensures reversals are atomic and maintain system consistency.
    
    Key concepts:
    - Reversal windows
    - Partial reversals
    - Cascading reversals
    - Compensation mechanisms
    - Reversal proofs
*)

open Ambience_core.Types
open Proof
module Intent = Ambience_core.Intent  (* Import Intent module for generate_uuid *)

(** Reversal types *)
type reversal_reason =
  | DisputeRaised of string
  | FraudDetected of string
  | InsufficientFunds
  | TechnicalError of string
  | UserRequested of public_key
  | Timeout
  | ConsensusFailure

type reversal_type =
  | Full              (* Complete reversal *)
  | Partial of float  (* Partial reversal with percentage *)
  | Compensated       (* Reversal with compensation *)

type reversal_status =
  | Initiated
  | Approved
  | Executing
  | Completed
  | Rejected of string
  | Failed of string

(** Reversal record *)
type reversal = {
  reversal_id: uuid;
  settlement_id: uuid;
  
  (* Reversal details *)
  reversal_type: reversal_type;
  reason: reversal_reason;
  status: reversal_status;
  
  (* Participants *)
  initiated_by: public_key;
  approved_by: public_key list;
  affected_parties: public_key list;
  
  (* Timing *)
  initiated_at: timestamp;
  deadline: timestamp;
  completed_at: timestamp option;
  
  (* Compensation *)
  compensation: compensation option;
  
  (* Proof *)
  reversal_proof: settlement_proof option;
  
  (* Effects *)
  reversed_transfers: uuid list;
  new_transfers: uuid list;
  
  metadata: (string * string) list;
}

and compensation = {
  compensated_party: public_key;
  compensation_amount: float;
  compensation_resource: resource_field;
  paid_by: public_key;
}

(** Reversal manager *)
type reversal_manager = {
  reversals: (uuid, reversal) Hashtbl.t;
  settlement_reversals: (uuid, uuid list) Hashtbl.t;  (* settlement -> reversals *)
  proof_generator: proof_generator;
  
  (* Configuration *)
  reversal_window: float;  (* Time window for reversals *)
  require_approval: bool;  (* Require approval for reversals *)
  min_approvals: int;      (* Minimum approvals needed *)
  
  (* Statistics *)
  mutable total_reversals: int;
  mutable successful_reversals: int;
  mutable failed_reversals: int;
  mutable compensation_paid: float;
}

(** Create reversal manager *)
let create_manager proof_generator ?(reversal_window = 3600.0) 
    ?(require_approval = true) ?(min_approvals = 1) () =
  {
    reversals = Hashtbl.create 100;
    settlement_reversals = Hashtbl.create 100;
    proof_generator = proof_generator;
    reversal_window = reversal_window;
    require_approval = require_approval;
    min_approvals = min_approvals;
    total_reversals = 0;
    successful_reversals = 0;
    failed_reversals = 0;
    compensation_paid = 0.0;
  }

(** {2 Reversal Initiation} *)

(** Check if settlement can be reversed *)
let can_reverse manager settlement =
  let current_time = Unix.time () in
  let age = current_time -. settlement.executed_at in
  
  (* Check if within reversal window *)
  if age > manager.reversal_window then
    Error "Reversal window expired"
  else
    (* Check if not already reversed *)
    match Hashtbl.find_opt manager.settlement_reversals settlement.settlement_id with
    | Some reversals when List.length reversals > 0 ->
        Error "Settlement already has reversals"
    | _ ->
        (* Check settlement status *)
        match settlement.status with
        | Completed -> Ok ()
        | Reversed _ -> Error "Already reversed"
        | Failed _ -> Error "Cannot reverse failed settlement"
        | _ -> Error "Settlement not in reversible state"

(** Initiate reversal *)
let initiate_reversal manager settlement_id initiated_by reason 
    ?(reversal_type = Full) ?(compensation = None) () =
  
  (* Generate reversal ID *)
  let reversal_id = Intent.generate_uuid () in
  let current_time = Unix.time () in
  
  (* Create reversal record *)
  let reversal = {
    reversal_id = reversal_id;
    settlement_id = settlement_id;
    reversal_type = reversal_type;
    reason = reason;
    status = Initiated;
    initiated_by = initiated_by;
    approved_by = [];
    affected_parties = [];  (* Would extract from settlement *)
    initiated_at = current_time;
    deadline = current_time +. 300.0;  (* 5 minute deadline *)
    completed_at = None;
    compensation = compensation;
    reversal_proof = None;
    reversed_transfers = [];
    new_transfers = [];
    metadata = [];
  } in
  
  (* Add to manager *)
  Hashtbl.add manager.reversals reversal_id reversal;
  
  (* Track by settlement *)
  let existing = 
    try Hashtbl.find manager.settlement_reversals settlement_id
    with Not_found -> []
  in
  Hashtbl.replace manager.settlement_reversals settlement_id 
    (reversal_id :: existing);
  
  manager.total_reversals <- manager.total_reversals + 1;
  
  Ok reversal_id

(** {2 Approval Process} *)

(** Approve reversal *)
let approve_reversal manager reversal_id approver =
  match Hashtbl.find_opt manager.reversals reversal_id with
  | None -> Error "Reversal not found"
  | Some reversal ->
      if reversal.status <> Initiated then
        Error "Reversal not in approvable state"
      else if List.mem approver reversal.approved_by then
        Error "Already approved by this party"
      else
        let updated = {
          reversal with
          approved_by = approver :: reversal.approved_by;
        } in
        
        (* Check if enough approvals *)
        let status = 
          if List.length updated.approved_by >= manager.min_approvals then
            Approved
          else
            reversal.status
        in
        
        let updated = { updated with status = status } in
        Hashtbl.replace manager.reversals reversal_id updated;
        
        Ok ()

(** Reject reversal *)
let reject_reversal manager reversal_id rejector reason =
  match Hashtbl.find_opt manager.reversals reversal_id with
  | None -> Error "Reversal not found"
  | Some reversal ->
      if reversal.status <> Initiated && reversal.status <> Approved then
        Error "Reversal not in rejectable state"
      else
        let updated = {
          reversal with
          status = Rejected reason;
          metadata = ("rejected_by", rejector) :: reversal.metadata;
        } in
        
        Hashtbl.replace manager.reversals reversal_id updated;
        manager.failed_reversals <- manager.failed_reversals + 1;
        
        Ok ()

(** {2 Execution} *)

(** Reverse transfers *)
let reverse_transfers _manager reversal =
  (* Create opposite transfers for each original *)
  let reversed = 
    List.map (fun transfer_id ->
      (* Would create actual reverse transfer *)
      transfer_id
    ) reversal.reversed_transfers
  in
  
  reversed

(** Apply compensation *)
let apply_compensation manager reversal =
  match reversal.compensation with
  | None -> Ok ()
  | Some comp ->
      (* Transfer compensation amount *)
      manager.compensation_paid <- 
        manager.compensation_paid +. comp.compensation_amount;
      Ok ()

(** Execute reversal *)
let execute_reversal manager reversal_id =
  match Hashtbl.find_opt manager.reversals reversal_id with
  | None -> Error "Reversal not found"
  | Some reversal ->
      (* Check status *)
      if reversal.status <> Approved then
        Error "Reversal not approved"
      else if manager.require_approval && 
              List.length reversal.approved_by < manager.min_approvals then
        Error "Insufficient approvals"
      else
        (* Update status *)
        let executing = { reversal with status = Executing } in
        Hashtbl.replace manager.reversals reversal_id executing;
        
        (* Reverse transfers *)
        let reversed_transfers = reverse_transfers manager reversal in
        
        (* Apply compensation if needed *)
        let _ = apply_compensation manager reversal in
        
        (* Generate proof *)
        let proof = 
          generate_witness_proof manager.proof_generator
            { settlement_id = reversal.settlement_id;
              (* Other settlement fields *)
              match_id = "";
              executed_point = { price = 0.0; quantity = 0.0; 
                                execution_time = 0.0; 
                                quality_level = None;
                                additional_terms = [] };
              execution_proof = "";
              pre_state_hash = "";
              post_state_hash = "";
              reversible_until = None;
              status = Reversed "Reversal executed";
              executed_at = Unix.time (); }
            [(reversal.initiated_by, "Reversal executed")]
        in
        
        (* Mark as completed *)
        let completed = {
          reversal with
          status = Completed;
          completed_at = Some (Unix.time ());
          reversed_transfers = reversed_transfers;
          reversal_proof = Some proof;
        } in
        
        Hashtbl.replace manager.reversals reversal_id completed;
        manager.successful_reversals <- manager.successful_reversals + 1;
        
        Ok ()

(** {2 Cascading Reversals} *)

(** Find dependent settlements *)
let find_dependent_settlements _manager _settlement_id =
  (* Would find settlements that depend on this one *)
  []

(** Execute cascading reversal *)
let execute_cascading_reversal manager root_settlement_id reason =
  (* Find all dependent settlements *)
  let dependents = find_dependent_settlements manager root_settlement_id in
  
  (* Reverse in order *)
  let rec reverse_chain = function
    | [] -> Ok ()
    | settlement_id :: rest ->
        match initiate_reversal manager settlement_id "system" reason () with
        | Error e -> Error e
        | Ok reversal_id ->
            (* Auto-approve system reversals *)
            let _ = approve_reversal manager reversal_id "system" in
            match execute_reversal manager reversal_id with
            | Error e -> Error e
            | Ok () -> reverse_chain rest
  in
  
  reverse_chain (root_settlement_id :: dependents)

(** {2 Monitoring} *)

(** Get reversal status *)
let get_status manager reversal_id =
  match Hashtbl.find_opt manager.reversals reversal_id with
  | None -> None
  | Some reversal -> Some reversal.status

(** Get settlement reversals *)
let get_settlement_reversals manager settlement_id =
  try
    let reversal_ids = Hashtbl.find manager.settlement_reversals settlement_id in
    List.filter_map (fun id -> 
      Hashtbl.find_opt manager.reversals id
    ) reversal_ids
  with Not_found -> []

(** Check for expired reversals *)
let check_deadlines manager =
  let current_time = Unix.time () in
  
  Hashtbl.iter (fun reversal_id reversal ->
    if reversal.status = Initiated || reversal.status = Approved then
      if current_time > reversal.deadline then
        (* Timeout - auto-reject *)
        let _ = reject_reversal manager reversal_id "system" "Deadline expired" in
        ()
  ) manager.reversals

(** {2 Statistics} *)

type reversal_stats = {
  total_reversals: int;
  successful_reversals: int;
  failed_reversals: int;
  pending_reversals: int;
  compensation_paid: float;
  reversal_rate: float;
}

let get_stats manager =
  let pending = 
    Hashtbl.fold (fun _id reversal acc ->
      match reversal.status with
      | Initiated | Approved | Executing -> acc + 1
      | _ -> acc
    ) manager.reversals 0
  in
  
  let rate = 
    if manager.total_reversals > 0 then
      float_of_int manager.successful_reversals /. 
      float_of_int manager.total_reversals
    else
      0.0
  in
  
  {
    total_reversals = manager.total_reversals;
    successful_reversals = manager.successful_reversals;
    failed_reversals = manager.failed_reversals;
    pending_reversals = pending;
    compensation_paid = manager.compensation_paid;
    reversal_rate = rate;
  }