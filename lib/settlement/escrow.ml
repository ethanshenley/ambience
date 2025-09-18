(** Escrow Module
    
    This module handles escrow and collateral management for settlements.
    It ensures resources are locked during settlement and released appropriately.
    
    Key concepts:
    - Resource locking and unlocking
    - Collateral requirements and management
    - Time-locked escrow
    - Multi-signature escrow
    - Dispute resolution via escrow
*)

open Ambience_core
open Types

(** Escrow types *)
type escrow_id = uuid

type escrow_state =
  | Created           (* Initial state *)
  | Funded           (* Resources deposited *)
  | Locked           (* Resources locked, settlement in progress *)
  | Released         (* Resources released to recipients *)
  | Disputed         (* Under dispute *)
  | Refunded         (* Refunded to depositors *)
  | Expired          (* Time limit exceeded *)

type escrow_type =
  | Simple           (* Basic two-party escrow *)
  | TimeLocked of timestamp  (* Released after time *)
  | MultiSig of int * public_key list  (* M-of-N signatures required *)
  | Conditional of (State.t -> bool)  (* Released when condition met *)

(** Escrow account *)
type escrow_account = {
  escrow_id: escrow_id;
  match_id: uuid;
  settlement_id: uuid option;
  
  (* Participants *)
  depositor: public_key;
  beneficiary: public_key;
  arbitrator: public_key option;
  
  (* Resources in escrow *)
  deposited_resources: resource_lock list;
  expected_resources: resource_expectation list;
  
  (* Escrow parameters *)
  escrow_type: escrow_type;
  state: escrow_state;
  
  (* Timing *)
  created_at: timestamp;
  funded_at: timestamp option;
  account_locked_at: timestamp option;  (* Renamed to avoid conflict with resource_lock.locked_at *)
  released_at: timestamp option;
  expires_at: timestamp;
  
  (* Collateral *)
  collateral_requirement: float;  (* Percentage over value *)
  collateral_locked: resource_lock list;
  
  (* Metadata *)
  metadata: (string * string) list;
}

and resource_lock = {
  locked_resource: resource_field;  (* Renamed to avoid conflict with resource_expectation.expected_resource *)
  amount_locked: float;
  lock_id: uuid;
  locked_from: public_key;
  lock_timestamp: timestamp;  (* Renamed to avoid conflict with escrow_account.account_locked_at *)
  unlock_condition: unlock_condition;
}

and resource_expectation = {
  expected_resource: resource_field;  (* Renamed for clarity and to avoid conflict *)
  amount_expected: float;
  from_party: public_key;
  deadline: timestamp;
}

and unlock_condition =
  | OnSettlement       (* Unlock when settlement completes *)
  | OnTimeout of timestamp  (* Unlock after timeout *)
  | OnSignature of public_key list  (* Unlock with signatures *)
  | OnCondition of (State.t -> bool)  (* Custom condition *)

(** Escrow manager *)
type escrow_manager = {
  escrows: (escrow_id, escrow_account) Hashtbl.t;
  locks_by_agent: (public_key, resource_lock list) Hashtbl.t;
  mutable total_value_locked: float;
  mutable active_escrows: int;
  mutable completed_escrows: int;
  mutable disputed_escrows: int;
}

(** Create escrow manager *)
let create_manager () = {
  escrows = Hashtbl.create 1000;
  locks_by_agent = Hashtbl.create 100;
  total_value_locked = 0.0;
  active_escrows = 0;
  completed_escrows = 0;
  disputed_escrows = 0;
}

(** {2 Escrow Creation} *)

(** Calculate required collateral *)
let calculate_collateral resource_value collateral_rate =
  resource_value *. collateral_rate

(** Create escrow account *)
let create_escrow manager ~match_id ~depositor ~beneficiary 
    ~resources ~escrow_type ?(collateral_rate = 0.1) 
    ?(timeout = 3600.0) ?(arbitrator = None) () =
  
  let escrow_id = Intent.generate_uuid () in
  let current_time = Ambience_core.Time_provider.now () in
  
  (* Calculate expected resources *)
  let expected = 
    List.map (fun (resource, amount) ->
      {
        expected_resource = resource;
        amount_expected = amount;
        from_party = depositor;
        deadline = current_time +. timeout;
      }
    ) resources
  in
  
  (* Create escrow account *)
  let escrow = {
    escrow_id = escrow_id;
    match_id = match_id;
    settlement_id = None;
    depositor = depositor;
    beneficiary = beneficiary;
    arbitrator = arbitrator;
    deposited_resources = [];
    expected_resources = expected;
    escrow_type = escrow_type;
    state = Created;
    created_at = current_time;
    funded_at = None;
    account_locked_at = None;
    released_at = None;
    expires_at = current_time +. timeout;
    collateral_requirement = collateral_rate;
    collateral_locked = [];
    metadata = [];
  } in
  
  (* Add to manager *)
  Hashtbl.add manager.escrows escrow_id escrow;
  manager.active_escrows <- manager.active_escrows + 1;
  
  Ok escrow_id

(** {2 Resource Locking} *)

(** Lock resources in escrow *)
let lock_resources manager escrow_id agent_id resources =
  match Hashtbl.find_opt manager.escrows escrow_id with
  | None -> Error "Escrow not found"
  | Some escrow ->
      if escrow.state <> Created && escrow.state <> Funded then
        Error "Escrow not in correct state for funding"
      else
        let current_time = Ambience_core.Time_provider.now () in
        
        (* Create resource locks *)
        let locks = 
          List.map (fun (resource, amount) ->
            {
              locked_resource = resource;
              amount_locked = amount;
              lock_id = Intent.generate_uuid ();
              locked_from = agent_id;
              lock_timestamp = current_time;
              unlock_condition = OnSettlement;
            }
          ) resources
        in
        
        (* Update escrow *)
        let updated_escrow = {
          escrow with
          deposited_resources = escrow.deposited_resources @ locks;
          state = if escrow.state = Created then Funded else escrow.state;
          funded_at = if escrow.state = Created then Some current_time else escrow.funded_at;
        } in
        
        Hashtbl.replace manager.escrows escrow_id updated_escrow;
        
        (* Track locks by agent *)
        let agent_locks = 
          try Hashtbl.find manager.locks_by_agent agent_id
          with Not_found -> []
        in
        Hashtbl.replace manager.locks_by_agent agent_id (locks @ agent_locks);
        
        (* Update total value locked *)
        let value = List.fold_left (fun acc (_r, amt) -> acc +. amt) 0.0 resources in
        manager.total_value_locked <- manager.total_value_locked +. value;
        
        Ok ()

(** Check if escrow is fully funded *)
let is_fully_funded escrow =
  (* Check if all expected resources are deposited *)
  List.for_all (fun expectation ->
    List.exists (fun lock ->
      lock.locked_resource = expectation.expected_resource &&
      lock.amount_locked >= expectation.amount_expected &&
      lock.locked_from = expectation.from_party
    ) escrow.deposited_resources
  ) escrow.expected_resources

(** {2 Escrow State Transitions} *)

(** Lock escrow for settlement *)
let lock_escrow manager escrow_id settlement_id =
  match Hashtbl.find_opt manager.escrows escrow_id with
  | None -> Error "Escrow not found"
  | Some escrow ->
      if escrow.state <> Funded then
        Error "Escrow must be funded before locking"
      else if not (is_fully_funded escrow) then
        Error "Escrow not fully funded"
      else
        let updated_escrow = {
          escrow with
          state = Locked;
          settlement_id = Some settlement_id;
          account_locked_at = Some (Ambience_core.Time_provider.now ());
        } in
        
        Hashtbl.replace manager.escrows escrow_id updated_escrow;
        Ok ()

(** Release escrow resources *)
let release_escrow manager escrow_id =
  match Hashtbl.find_opt manager.escrows escrow_id with
  | None -> Error "Escrow not found"
  | Some escrow ->
      if escrow.state <> Locked then
        Error "Escrow must be locked before release"
      else
        (* Check release conditions *)
        let can_release = 
          match escrow.escrow_type with
          | Simple -> true
          | TimeLocked deadline -> Ambience_core.Time_provider.now () >= deadline
          | MultiSig (required, signers) ->
              (* TODO: Check signatures *)
              List.length signers >= required
          | Conditional condition ->
              (* TODO: Evaluate condition *)
              let state = State.create () in
              condition state
        in
        
        if not can_release then
          Error "Release conditions not met"
        else
          (* Transfer resources to beneficiary *)
          let updated_escrow = {
            escrow with
            state = Released;
            released_at = Some (Ambience_core.Time_provider.now ());
          } in
          
          Hashtbl.replace manager.escrows escrow_id updated_escrow;
          
          (* Update statistics *)
          manager.completed_escrows <- manager.completed_escrows + 1;
          manager.active_escrows <- manager.active_escrows - 1;
          
          Ok ()

(** Refund escrow to depositor *)
let refund_escrow manager escrow_id reason =
  match Hashtbl.find_opt manager.escrows escrow_id with
  | None -> Error "Escrow not found"
  | Some escrow ->
      if escrow.state = Released then
        Error "Escrow already released"
      else
        (* Return resources to depositors *)
        let updated_escrow = {
          escrow with
          state = Refunded;
          metadata = ("refund_reason", reason) :: escrow.metadata;
        } in
        
        Hashtbl.replace manager.escrows escrow_id updated_escrow;
        
        (* Unlock resources *)
        List.iter (fun lock ->
          (* Remove from agent locks *)
          let agent_locks = 
            try Hashtbl.find manager.locks_by_agent lock.locked_from
            with Not_found -> []
          in
          let remaining = 
            List.filter (fun l -> l.lock_id <> lock.lock_id) agent_locks
          in
          Hashtbl.replace manager.locks_by_agent lock.locked_from remaining
        ) escrow.deposited_resources;
        
        manager.active_escrows <- manager.active_escrows - 1;
        
        Ok ()

(** {2 Dispute Resolution} *)

(** Raise dispute on escrow *)
let raise_dispute manager escrow_id disputing_party reason =
  match Hashtbl.find_opt manager.escrows escrow_id with
  | None -> Error "Escrow not found"
  | Some escrow ->
      if escrow.state = Released || escrow.state = Refunded then
        Error "Escrow already finalized"
      else
        let updated_escrow = {
          escrow with
          state = Disputed;
          metadata = [
            ("dispute_raised_by", disputing_party);
            ("dispute_reason", reason);
            ("dispute_raised_at", string_of_float (Ambience_core.Time_provider.now ()));
          ] @ escrow.metadata;
        } in
        
        Hashtbl.replace manager.escrows escrow_id updated_escrow;
        manager.disputed_escrows <- manager.disputed_escrows + 1;
        
        Ok ()

(** Resolve dispute *)
let resolve_dispute manager escrow_id resolution =
  match Hashtbl.find_opt manager.escrows escrow_id with
  | None -> Error "Escrow not found"
  | Some escrow ->
      if escrow.state <> Disputed then
        Error "Escrow not under dispute"
      else
        match resolution with
        | `Release ->
            release_escrow manager escrow_id
        | `Refund reason ->
            refund_escrow manager escrow_id reason
        | `Split ratio ->
            (* Split resources between parties *)
            let updated_escrow = {
              escrow with
              state = Released;
              released_at = Some (Ambience_core.Time_provider.now ());
              metadata = ("split_ratio", string_of_float ratio) :: escrow.metadata;
            } in
            
            Hashtbl.replace manager.escrows escrow_id updated_escrow;
            manager.disputed_escrows <- manager.disputed_escrows - 1;
            manager.completed_escrows <- manager.completed_escrows + 1;
            manager.active_escrows <- manager.active_escrows - 1;
            
            Ok ()

(** {2 Collateral Management} *)

(** Lock collateral *)
let lock_collateral manager escrow_id agent_id collateral_amount =
  match Hashtbl.find_opt manager.escrows escrow_id with
  | None -> Error "Escrow not found"
  | Some escrow ->
      let resource_field =
        match Resource.create_field
          ~resource_type:"currency:collateral"
          ~min_quantity:collateral_amount
          ~max_quantity:collateral_amount
          ~quality:Fungible
          ~metadata:[] with
        | Ok field -> field
        | Error _ -> failwith "Failed to create collateral resource field"
      in
      let lock = {
        locked_resource = resource_field;
        amount_locked = collateral_amount;
        lock_id = Intent.generate_uuid ();
        locked_from = agent_id;
        lock_timestamp = Ambience_core.Time_provider.now ();
        unlock_condition = OnSettlement;
      } in
      
      let updated_escrow = {
        escrow with
        collateral_locked = lock :: escrow.collateral_locked;
      } in
      
      Hashtbl.replace manager.escrows escrow_id updated_escrow;
      Ok ()

(** Release collateral *)
let release_collateral manager escrow_id =
  match Hashtbl.find_opt manager.escrows escrow_id with
  | None -> Error "Escrow not found"
  | Some escrow ->
      if escrow.state <> Released && escrow.state <> Refunded then
        Error "Settlement must be finalized to release collateral"
      else begin
        (* Return collateral to owners *)
        List.iter (fun lock ->
          let agent_locks =
            try Hashtbl.find manager.locks_by_agent lock.locked_from
            with Not_found -> []
          in
          let remaining =
            List.filter (fun l -> l.lock_id <> lock.lock_id) agent_locks
          in
          Hashtbl.replace manager.locks_by_agent lock.locked_from remaining
        ) escrow.collateral_locked;
        Ok ()
      end

(** {2 Maintenance} *)

(** Check for expired escrows *)
let check_expirations manager =
  let current_time = Ambience_core.Time_provider.now () in
  
  Hashtbl.iter (fun escrow_id escrow ->
    if escrow.state = Created || escrow.state = Funded then
      if current_time > escrow.expires_at then
        (* Refund expired escrow *)
        let _ = refund_escrow manager escrow_id "Expired" in
        ()
  ) manager.escrows

(** Get escrow statistics *)
type escrow_stats = {
  total_escrows: int;
  active_escrows: int;
  completed_escrows: int;
  disputed_escrows: int;
  total_value_locked: float;
  average_lock_time: float;
}

let get_stats manager =
  let lock_times = ref [] in
  
  Hashtbl.iter (fun _id escrow ->
    match escrow.account_locked_at, escrow.released_at with
    | Some locked, Some released ->
        lock_times := (released -. locked) :: !lock_times
    | _ -> ()
  ) manager.escrows;
  
  let avg_lock_time = 
    match !lock_times with
    | [] -> 0.0
    | times ->
        let sum = List.fold_left (+.) 0.0 times in
        sum /. float_of_int (List.length times)
  in
  
  {
    total_escrows = Hashtbl.length manager.escrows;
    active_escrows = manager.active_escrows;
    completed_escrows = manager.completed_escrows;
    disputed_escrows = manager.disputed_escrows;
    total_value_locked = manager.total_value_locked;
    average_lock_time = avg_lock_time;
  }