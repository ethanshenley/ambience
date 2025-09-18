(** Escrow Module
    
    Handles escrow and collateral management for settlements.
*)

open Ambience_core.Types

(** {1 Types} *)

type escrow_id = uuid

type escrow_state =
  | Created
  | Funded
  | Locked
  | Released
  | Disputed
  | Refunded
  | Expired

type escrow_type =
  | Simple
  | TimeLocked of timestamp
  | MultiSig of int * public_key list
  | Conditional of (Ambience_core.State.t -> bool)

type escrow_account
type resource_lock
type escrow_manager

(** {1 Manager} *)

val create_manager : unit -> escrow_manager

(** {1 Escrow Operations} *)

val create_escrow :
  escrow_manager ->
  match_id:uuid ->
  depositor:public_key ->
  beneficiary:public_key ->
  resources:(resource_field * float) list ->
  escrow_type:escrow_type ->
  ?collateral_rate:float ->
  ?timeout:float ->
  ?arbitrator:public_key option ->
  unit -> (escrow_id, string) result

val lock_resources :
  escrow_manager -> escrow_id -> public_key -> 
  (resource_field * float) list -> (unit, string) result

val lock_escrow :
  escrow_manager -> escrow_id -> uuid -> (unit, string) result

val release_escrow :
  escrow_manager -> escrow_id -> (unit, string) result

val refund_escrow :
  escrow_manager -> escrow_id -> string -> (unit, string) result

(** {1 Dispute Resolution} *)

val raise_dispute :
  escrow_manager -> escrow_id -> public_key -> string -> (unit, string) result

val resolve_dispute :
  escrow_manager -> escrow_id -> 
  [`Release | `Refund of string | `Split of float] -> (unit, string) result

(** {1 Collateral} *)

val lock_collateral :
  escrow_manager -> escrow_id -> public_key -> float -> (unit, string) result

val release_collateral :
  escrow_manager -> escrow_id -> (unit, string) result

(** {1 Maintenance} *)

val check_expirations : escrow_manager -> unit

(** {1 Statistics} *)

type escrow_stats = {
  total_escrows: int;
  active_escrows: int;
  completed_escrows: int;
  disputed_escrows: int;
  total_value_locked: float;
  average_lock_time: float;
}

val get_stats : escrow_manager -> escrow_stats