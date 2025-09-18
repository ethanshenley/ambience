(** Settlement Executor Module
    
    Handles atomic execution of settlements.
*)

open Ambience_core.Types
open Escrow

(** {1 Types} *)

type execution_status =
  | Pending
  | Validating
  | Preparing
  | Committing
  | Committed
  | RollingBack
  | RolledBack
  | Failed of string

type execution_context

(** Execution log entry for tracking settlement execution steps *)
type execution_log = {
  log_timestamp: timestamp;
  level: log_level;
  log_status: execution_status;
  message: string;
  details: (string * string) list;
}

and log_level = Debug | Info | Warning | Error

type executor

(** {1 Executor Creation} *)

val create :
  Ambience_core.State.t -> escrow_manager ->
  ?validation_timeout:float ->
  ?commit_timeout:float ->
  ?require_escrow:bool ->
  unit -> executor

(** {1 Execution} *)

val execute :
  executor -> settlement -> match_t -> (uuid, string) Result.t
(** Execute a settlement atomically *)

val rollback_settlement :
  executor -> execution_context -> string -> execution_context
(** Rollback a settlement *)

(** {1 Monitoring} *)

val get_status : executor -> uuid -> execution_status option
val get_logs : executor -> uuid -> execution_log list

(** {1 Statistics} *)

type executor_stats = {
  settlements_executed: int;
  settlements_failed: int;
  settlements_pending: int;
  total_volume: float;
  success_rate: float;
  average_execution_time: float;
}

val get_stats : executor -> executor_stats

