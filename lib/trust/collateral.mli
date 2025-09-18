(** Collateral Module
    
    Manages collateral requirements and staking for the protocol.
*)

open Ambience_core.Types

(** {1 Types} *)

type collateral_type =
  | Fixed of float
  | Percentage of float
  | Dynamic of (float -> float -> float)
  | Tiered of tier list

and tier = {
  min_value: float;
  max_value: float;
  collateral_rate: float;
}

type stake
type slashing_reason =
  | SettlementDefault
  | FraudDetected
  | ProtocolViolation
  | DisputeLost
  | InactivityPenalty

type pool
type collateral_manager

(** {1 Manager} *)

val create_manager :
  ?default_type:collateral_type ->
  ?min_ratio:float ->
  ?unstake_delay:float ->
  unit -> collateral_manager

(** {1 Collateral Calculation} *)

val calculate_required :
  collateral_manager -> float -> float -> collateral_type -> float

val has_sufficient_collateral :
  collateral_manager -> public_key -> float -> bool

(** {1 Pool Management} *)

val create_pool :
  collateral_manager ->
  name:string ->
  min_stake:float ->
  collateral_type:collateral_type ->
  ?max_stake:float option ->
  ?reward_rate:float ->
  unit -> (uuid, string) result

(** {1 Staking Operations} *)

val stake :
  collateral_manager -> public_key -> float -> resource_field ->
  ?pool_id:uuid option ->
  unit -> (uuid, string) result

val lock_stake :
  collateral_manager -> uuid -> uuid -> float -> (unit, string) result

val request_unstake :
  collateral_manager -> public_key -> uuid -> (unit, string) result

val complete_unstake :
  collateral_manager -> public_key -> uuid -> (float, string) result

(** {1 Slashing} *)

val slash_stake :
  collateral_manager -> uuid -> float -> slashing_reason -> public_key ->
  ?settlement_id:uuid option ->
  unit -> (float, string) result

(** {1 Rewards} *)

val calculate_rewards : collateral_manager -> public_key -> float
val pay_rewards : collateral_manager -> public_key -> float

(** {1 Statistics} *)

type collateral_stats = {
  total_staked: float;
  total_slashed: float;
  total_rewards_paid: float;
  active_stakes: int;
  pools_count: int;
  average_stake_duration: float;
}

val get_stats : collateral_manager -> collateral_stats