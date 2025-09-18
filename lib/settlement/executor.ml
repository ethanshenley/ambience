(** Settlement Executor Module
    
    This module handles the atomic execution of settlements. It coordinates
    the transfer of resources between parties and ensures consistency.
    
    Key concepts:
    - Atomic execution with rollback
    - Two-phase commit protocol
    - Settlement validation
    - Resource transfer
    - Event logging
*)

open Ambience_core
open Types
open Escrow

(** Execution status *)
type execution_status =
  | Pending
  | Validating
  | Preparing
  | Committing
  | Committed
  | RollingBack
  | RolledBack
  | Failed of string

(** Execution context *)
type execution_context = {
  settlement: settlement;
  match_t: match_t;
  escrow_id: escrow_id option;
  participants: participant list;
  transfers: resource_transfer list;
  status: execution_status;
  started_at: timestamp;
  completed_at: timestamp option;
  logs: execution_log list;
}

and participant = {
  agent_id: public_key;
  role: participant_role;
  resources_in: resource_field list;
  resources_out: resource_field list;
  ready: bool;
  committed: bool;
}

and participant_role =
  | Buyer
  | Seller
  | Intermediary
  | MarketMaker

and resource_transfer = {
  transfer_id: uuid;
  from_agent: public_key;
  to_agent: public_key;
  resource: resource_field;
  amount: float;
  transfer_status: transfer_status;
}

and transfer_status =
  | TransferPending
  | TransferLocked
  | TransferCompleted
  | TransferFailed of string

and execution_log = {
  log_timestamp: timestamp;
  level: log_level;
  log_status: execution_status;
  message: string;
  details: (string * string) list;
}

and log_level = Debug | Info | Warning | Error

(** Settlement executor *)
type executor = {
  state: State.t;
  escrow_manager: escrow_manager;
  contexts: (uuid, execution_context) Hashtbl.t;
  mutable settlements_executed: int;
  mutable settlements_failed: int;
  mutable total_volume: float;
  
  (* Configuration *)
  validation_timeout: float;
  commit_timeout: float;
  require_escrow: bool;
  
  (* Callbacks *)
  mutable on_settlement_complete: (settlement -> unit) option;
  mutable on_settlement_failed: (settlement -> string -> unit) option;
}

(** Create executor *)
let create state escrow_manager ?(validation_timeout = 10.0) 
    ?(commit_timeout = 30.0) ?(require_escrow = true) () =
  {
    state = state;
    escrow_manager = escrow_manager;
    contexts = Hashtbl.create 1000;
    settlements_executed = 0;
    settlements_failed = 0;
    total_volume = 0.0;
    validation_timeout = validation_timeout;
    commit_timeout = commit_timeout;
    require_escrow = require_escrow;
    on_settlement_complete = None;
    on_settlement_failed = None;
  }

(** Create execution context *)
let create_context executor settlement match_t =
  (* Extract participants from match *)
  let participants = 
    List.map (fun intent_id ->
      match State.Queries.get_active_intents executor.state
            |> List.find_opt (fun i -> i.intent_id = intent_id) with
      | None -> None
      | Some intent ->
          Some {
            agent_id = intent.agent_id;
            role = Seller; (* TODO: Determine role based on intent vs settlement *)
            resources_in = [intent.want_field];
            resources_out = [intent.offer_field];
            ready = false;
            committed = false;
          }
    ) match_t.intent_ids
    |> List.filter_map (fun x -> x)
  in
  
  (* Create transfers based on settlement point *)
  let transfers = 
    (* Simplified - would create actual transfers *)
    []
  in
  
  {
    settlement = settlement;
    match_t = match_t;
    escrow_id = None;
    participants = participants;
    transfers = transfers;
    status = Pending;
    started_at = Unix.time ();
    completed_at = None;
    logs = [];
  }

(** Add log entry *)
let add_log context level message ?(details = []) () =
  let log_entry = {
    log_timestamp = Unix.time ();
    level = level;
    log_status = context.status;
    message = message;
    details = details;
  } in
  { context with logs = log_entry :: context.logs }

(** {2 Validation Phase} *)

(** Validate participant readiness *)
let validate_participants executor context =
  let all_ready =
    List.for_all (fun p ->
      (* Check agent exists *)
      State.Queries.get_reputation executor.state p.agent_id
      |> Option.is_some
    ) context.participants
  in

  if all_ready then
    Ok (add_log context Info "All participants validated" ())
  else
    Error "Not all participants are valid"

(** Validate resources *)
let validate_resources context =
  (* Check all required resources are available *)
  let all_available = 
    List.for_all (fun transfer ->
      (* Check resource exists and is sufficient *)
      (* Simplified - would check actual balances *)
      true
    ) context.transfers
  in
  
  if all_available then
    Ok (add_log context Info "Resources validated" ())
  else
    Error "Insufficient resources"

(** Validate settlement conditions *)
let validate_conditions context =
  (* Check time windows *)
  let current_time = Unix.time () in
  let within_window = 
    current_time >= context.settlement.executed_point.execution_time -. 60.0 &&
    current_time <= context.settlement.executed_point.execution_time +. 60.0
  in
  
  if not within_window then
    Result.Error "Outside execution time window"
  else
    (* Check other conditions *)
    Ok (add_log context Info "Conditions validated" ())

(** Run validation phase *)
let validate_settlement executor context =
  let updated_context = { context with status = Validating } in
  
  (* Run all validations *)
  match validate_participants executor updated_context with
  | Error e -> Result.Error e
  | Ok ctx1 ->
      match validate_resources ctx1 with
      | Error e -> Result.Error e
      | Ok ctx2 ->
          match validate_conditions ctx2 with
          | Error e -> Result.Error e
          | Ok ctx3 -> Ok { ctx3 with status = Preparing }

(** {2 Preparation Phase} *)

(** Create escrow if required *)
let setup_escrow executor context =
  if not executor.require_escrow then
    Ok context
  else
    (* Create escrow account *)
    match context.participants with
    | [buyer; seller] ->
        let resources = 
          List.map (fun t -> (t.resource, t.amount)) context.transfers
        in
        
        (match create_escrow executor.escrow_manager
                ~match_id:context.match_t.match_id
                ~depositor:seller.agent_id
                ~beneficiary:buyer.agent_id
                ~resources:resources
                ~escrow_type:Simple
                () with
         | Result.Error e -> Result.Error e
         | Ok escrow_id ->
             Ok { context with escrow_id = Some escrow_id })
    | _ ->
        Error "Multi-party escrow not implemented"

(** Lock resources *)
let lock_resources executor context =
  match context.escrow_id with
  | None ->
      (* Direct locking without escrow *)
      Ok context
  | Some escrow_id ->
      (* Lock resources in escrow *)
      List.fold_left (fun acc transfer ->
        match acc with
        | Result.Error e -> Result.Error e
        | Ok ctx ->
            let resources = [(transfer.resource, transfer.amount)] in
            match lock_resources executor.escrow_manager escrow_id
                    transfer.from_agent resources with
            | Result.Error e -> Result.Error e
            | Ok () ->
                (* Update transfer status *)
                let updated_transfers = 
                  List.map (fun t ->
                    if t.transfer_id = transfer.transfer_id then
                      { t with transfer_status = TransferLocked }
                    else t
                  ) ctx.transfers
                in
                Ok { ctx with transfers = updated_transfers }
      ) (Ok context) context.transfers

(** Run preparation phase *)
let prepare_settlement executor context =
  match setup_escrow executor context with
  | Error e -> Result.Error e
  | Ok ctx1 ->
      match lock_resources executor ctx1 with
      | Error e -> Result.Error e
      | Ok ctx2 ->
          Ok { ctx2 with
               status = Committing;
               logs = (add_log ctx2 Info "Preparation complete" ()).logs }

(** {2 Commit Phase} *)

(** Execute transfers *)
let execute_transfers executor context =
  (* Transfer resources between parties *)
  List.fold_left (fun acc transfer ->
    match acc with
    | Result.Error e -> Result.Error e
    | Ok ctx ->
        (* Execute the transfer *)
        (* In production, this would update actual balances *)
        let updated_transfer = { 
          transfer with 
          transfer_status = TransferCompleted 
        } in
        
        let updated_transfers = 
          List.map (fun t ->
            if t.transfer_id = transfer.transfer_id then
              updated_transfer
            else t
          ) ctx.transfers
        in
        
        let updated_ctx = add_log ctx Info
                     (Printf.sprintf "Transfer %s completed" transfer.transfer_id)
                     ~details:[
                       "from", transfer.from_agent;
                       "to", transfer.to_agent;
                       "amount", string_of_float transfer.amount;
                     ] () in
        Ok { updated_ctx with
             transfers = updated_transfers }
  ) (Ok context) context.transfers

(** Commit settlement *)
let commit_settlement executor context =
  (* Execute all transfers *)
  match execute_transfers executor context with
  | Result.Error e -> Result.Error e
  | Ok ctx ->
      (* Update settlement status *)
      let updated_settlement = {
        ctx.settlement with
        status = Completed;
      } in
      
      (* Record in state *)
      (match State.Transitions.execute_settlement executor.state updated_settlement with
       | Result.Error e -> Result.Error e
       | Ok () ->
           (* Release escrow if used *)
           (match ctx.escrow_id with
            | None -> ()
            | Some escrow_id ->
                let _ = release_escrow executor.escrow_manager escrow_id in
                ());
           
           (* Update executor stats *)
           executor.settlements_executed <- executor.settlements_executed + 1;
           executor.total_volume <- 
             executor.total_volume +. ctx.settlement.executed_point.quantity;
           
           (* Trigger callback *)
           (match executor.on_settlement_complete with
            | None -> ()
            | Some callback -> callback updated_settlement);
           
           let updated_ctx = add_log ctx Info "Settlement committed" () in
           Ok { updated_ctx with
                status = Committed;
                completed_at = Some (Unix.time ()) })

(** {2 Rollback} *)

(** Rollback settlement *)
let rollback_settlement executor context reason =
  let ctx = { context with status = RollingBack } in
  
  (* Reverse any completed transfers *)
  let ctx = 
    List.fold_left (fun ctx transfer ->
      match transfer.transfer_status with
      | TransferCompleted ->
          (* Reverse the transfer *)
          add_log ctx Info
                    (Printf.sprintf "Reversing transfer %s" transfer.transfer_id) ()
      | _ -> ctx
    ) ctx context.transfers
  in
  
  (* Refund escrow if exists *)
  (match ctx.escrow_id with
   | None -> ()
   | Some escrow_id ->
       let _ = refund_escrow executor.escrow_manager escrow_id reason in
       ());
  
  (* Update settlement status *)
  let updated_settlement = {
    ctx.settlement with
    status = Failed reason;
  } in
  
  (* Update stats *)
  executor.settlements_failed <- executor.settlements_failed + 1;
  
  (* Trigger callback *)
  (match executor.on_settlement_failed with
   | None -> ()
   | Some callback -> callback updated_settlement reason);
  
  let updated_ctx = add_log ctx Error (Printf.sprintf "Settlement rolled back: %s" reason) () in
  { updated_ctx with
    status = RolledBack;
    completed_at = Some (Unix.time ()) }

(** {2 Main Execution} *)

(** Execute settlement *)
let execute executor settlement match_t =
  (* Create execution context *)
  let context = create_context executor settlement match_t in
  
  (* Store context *)
  Hashtbl.add executor.contexts settlement.settlement_id context;
  
  (* Run execution phases *)
  let result =
    match validate_settlement executor context with
    | Error e -> Result.Error e
    | Ok validated ->
        match prepare_settlement executor validated with
        | Error e -> Result.Error e
        | Ok prepared -> commit_settlement executor prepared
  in
  
  match result with
  | Ok final_context ->
      Hashtbl.replace executor.contexts settlement.settlement_id final_context;
      Ok settlement.settlement_id
  | Result.Error reason ->
      (* Rollback *)
      let rolled_back = rollback_settlement executor context reason in
      Hashtbl.replace executor.contexts settlement.settlement_id rolled_back;
      Error reason

(** {2 Monitoring} *)

(** Get execution status *)
let get_status executor settlement_id =
  match Hashtbl.find_opt executor.contexts settlement_id with
  | None -> None
  | Some context -> Some context.status

(** Get execution logs *)
let get_logs executor settlement_id =
  match Hashtbl.find_opt executor.contexts settlement_id with
  | None -> []
  | Some context -> List.rev context.logs

(** Executor statistics *)
type executor_stats = {
  settlements_executed: int;
  settlements_failed: int;
  settlements_pending: int;
  total_volume: float;
  success_rate: float;
  average_execution_time: float;
}

let get_stats executor =
  let pending = 
    Hashtbl.fold (fun _id ctx acc ->
      match ctx.status with
      | Pending | Validating | Preparing | Committing -> acc + 1
      | _ -> acc
    ) executor.contexts 0
  in
  
  let execution_times = 
    Hashtbl.fold (fun _id ctx acc ->
      match ctx.completed_at with
      | None -> acc
      | Some completed ->
          (completed -. ctx.started_at) :: acc
    ) executor.contexts []
  in
  
  let avg_time = 
    match execution_times with
    | [] -> 0.0
    | times ->
        let sum = List.fold_left (+.) 0.0 times in
        sum /. float_of_int (List.length times)
  in
  
  let total = executor.settlements_executed + executor.settlements_failed in
  let success_rate = 
    if total = 0 then 0.0
    else float_of_int executor.settlements_executed /. float_of_int total
  in
  
  {
    settlements_executed = executor.settlements_executed;
    settlements_failed = executor.settlements_failed;
    settlements_pending = pending;
    total_volume = executor.total_volume;
    success_rate = success_rate;
    average_execution_time = avg_time;
  }