(** Negotiation Module
    
    Multi-round negotiation between agents to find settlement points.
*)

open Ambience_core.Types

(** {1 Protocol Types} *)

type protocol =
  | AlternatingOffers
  | SimultaneousOffers
  | MediatedNegotiation
  | AscendingAuction
  | DescendingAuction

type strategy =
  | Aggressive of float
  | Moderate
  | Accommodating of float
  | Tit_for_tat
  | Deadline_based

(** {1 Session Types} *)

type session
type participant
type offer

val create_session :
  uuid -> participant list -> settlement_manifold -> protocol ->
  ?deadline:timestamp option -> ?max_rounds:int -> unit -> session

val create_participant :
  public_key -> strategy -> 
  ?reservation_utility:float ->
  ?time_preference:float ->
  ?reputation:float -> unit -> participant

(** {1 Negotiation Execution} *)

val execute_round : session -> session
(** Execute one round of negotiation *)

val run_to_completion : session -> session
(** Run negotiation until agreement or failure *)

(** {1 Offer Generation} *)

val generate_initial_offer : participant -> settlement_manifold -> settlement_point option
val generate_counter_offer : 
  participant -> settlement_point -> settlement_manifold -> session -> settlement_point option

(** {1 Analysis} *)

type outcome = {
  success: bool;
  final_point: settlement_point option;
  rounds_taken: int;
  time_taken: float;
  total_concession: float;
  efficiency: float;
}

val analyze_outcome : session -> outcome

(** {1 Learning} *)

val adapt_strategy : participant -> outcome -> unit
(** Update participant strategy based on outcome *)