(** Settlement Reversal Module
    
    Handles reversal and cancellation of settlements.
*)

open Ambience_core.Types
open Proof

(** {1 Types} *)

type reversal_reason =
  | DisputeRaised of string
  | FraudDetected of string
  | InsufficientFunds
  | TechnicalError of string
  | UserRequested of public_key
  | Timeout
  | ConsensusFailure

type reversal_type =
  | Full
  | Partial of float
  | Compensated

type reversal_status =
  | Initiated
  | Approved
  | Executing
  | Completed
  | Rejected of string
  | Failed of string

type reversal
type compensation
type reversal_manager

(** {1 Manager} *)

val create_manager :
  proof_generator ->
  ?reversal_window:float ->
  ?require_approval:bool ->
  ?min_approvals:int ->
  unit -> reversal_manager

(** {1 Reversal Operations} *)

val can_reverse : 
  reversal_manager -> settlement -> (unit, string) result

val initiate_reversal :
  reversal_manager -> uuid -> public_key -> reversal_reason ->
  ?reversal_type:reversal_type ->
  ?compensation:compensation option ->
  unit -> (uuid, string) result

val approve_reversal :
  reversal_manager -> uuid -> public_key -> (unit, string) result

val reject_reversal :
  reversal_manager -> uuid -> public_key -> string -> (unit, string) result

val execute_reversal :
  reversal_manager -> uuid -> (unit, string) result

val execute_cascading_reversal :
  reversal_manager -> uuid -> reversal_reason -> (unit, string) result

(** {1 Monitoring} *)

val get_status : reversal_manager -> uuid -> reversal_status option
val get_settlement_reversals : reversal_manager -> uuid -> reversal list
val check_deadlines : reversal_manager -> unit

(** {1 Statistics} *)

type reversal_stats = {
  total_reversals: int;
  successful_reversals: int;
  failed_reversals: int;
  pending_reversals: int;
  compensation_paid: float;
  reversal_rate: float;
}

val get_stats : reversal_manager -> reversal_stats