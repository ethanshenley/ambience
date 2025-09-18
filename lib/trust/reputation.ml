(** Reputation Module
    
    This module manages reputation scores and trust relationships in the protocol.
    Reputation is earned through successful interactions and lost through failures.
    
    Key concepts:
    - Multi-dimensional reputation
    - Reputation decay over time
    - Trust propagation
    - Reputation aggregation
    - Sybil resistance
*)

open Ambience_core.Types
module Intent = Ambience_core.Intent  (* Import Intent module for generate_uuid *)

(** Reputation dimensions *)
type dimension =
  | Reliability        (* Completes settlements successfully *)
  | Timeliness        (* Meets deadlines *)
  | VolumeHandling    (* Can handle large transactions *)
  | DisputeResolution (* Fair in disputes *)
  | PriceDiscovery    (* Offers fair prices *)
  | Liquidity         (* Provides liquidity *)

(** Reputation score *)
type score = {
  value: float;         (* 0.0 to 1.0 *)
  confidence: float;    (* How confident in this score *)
  samples: int;         (* Number of interactions *)
  last_updated: timestamp;
}

(** Agent reputation *)
type reputation = {
  agent_id: public_key;
  
  (* Multi-dimensional scores *)
  dimensions: (dimension, score) Hashtbl.t;
  mutable overall_score: float;
  
  (* History *)
  mutable interactions: interaction list;
  
  (* Trust relationships *)
  trusted_by: (public_key * float) list;  (* Who trusts them and how much *)
  trusts: (public_key * float) list;      (* Who they trust *)
  
  (* Statistics *)
  mutable total_transactions: int;
  mutable successful_transactions: int;
  mutable failed_transactions: int;
  mutable total_volume: float;
  mutable disputes_raised: int;
  mutable disputes_won: int;
  
  (* Metadata *)
  created_at: timestamp;
  mutable last_active: timestamp;
  verification_level: verification_level;
}

and interaction = {
  interaction_id: uuid;
  counterparty: public_key;
  interaction_type: interaction_type;
  outcome: outcome;
  value: float;
  timestamp: timestamp;
  impact: float;  (* How much this affected reputation *)
}

and interaction_type =
  | Settlement
  | Match
  | Negotiation
  | Arbitration
  | Endorsement

and outcome =
  | Success
  | Failure of string
  | Partial of float  (* Partial success rate *)
  | Disputed

and verification_level =
  | Unverified
  | BasicVerified      (* Email/phone verified *)
  | IdentityVerified   (* KYC completed *)
  | InstitutionBacked  (* Backed by known institution *)

(** Reputation manager *)
type reputation_manager = {
  reputations: (public_key, reputation) Hashtbl.t;
  
  (* Configuration *)
  decay_rate: float;           (* How fast reputation decays *)
  min_confidence: float;       (* Minimum confidence for valid score *)
  max_history: int;           (* Maximum interactions to keep *)
  
  (* Weights for different dimensions *)
  dimension_weights: (dimension, float) Hashtbl.t;
  
  (* Trust network *)
  trust_propagation_depth: int;
  trust_decay_factor: float;   (* How much trust decreases per hop *)
  
  (* Sybil resistance *)
  min_stake_for_reputation: float;
  require_verification: bool;
  
  (* Statistics *)
  mutable total_agents: int;
  mutable total_interactions: int;
  mutable average_reputation: float;
}

(** Create reputation manager *)
let create_manager ?(decay_rate = 0.01) ?(min_confidence = 0.1) 
    ?(max_history = 1000) ?(propagation_depth = 3) () =
  
  let dimension_weights = Hashtbl.create 6 in
  (* Default equal weights *)
  Hashtbl.add dimension_weights Reliability 0.3;
  Hashtbl.add dimension_weights Timeliness 0.2;
  Hashtbl.add dimension_weights VolumeHandling 0.15;
  Hashtbl.add dimension_weights DisputeResolution 0.15;
  Hashtbl.add dimension_weights PriceDiscovery 0.1;
  Hashtbl.add dimension_weights Liquidity 0.1;
  
  {
    reputations = Hashtbl.create 1000;
    decay_rate = decay_rate;
    min_confidence = min_confidence;
    max_history = max_history;
    dimension_weights = dimension_weights;
    trust_propagation_depth = propagation_depth;
    trust_decay_factor = 0.5;
    min_stake_for_reputation = 100.0;
    require_verification = false;
    total_agents = 0;
    total_interactions = 0;
    average_reputation = 0.5;
  }

(** {2 Reputation Initialization} *)

(** Initialize reputation for new agent *)
let initialize_reputation manager agent_id ?(verification = Unverified) () =
  let dimensions = Hashtbl.create 6 in
  
  (* Start with neutral scores *)
  List.iter (fun dim ->
    Hashtbl.add dimensions dim {
      value = 0.5;
      confidence = 0.0;  (* No confidence initially *)
      samples = 0;
      last_updated = Unix.time ();
    }
  ) [Reliability; Timeliness; VolumeHandling; 
     DisputeResolution; PriceDiscovery; Liquidity];
  
  let reputation = {
    agent_id = agent_id;
    dimensions = dimensions;
    overall_score = 0.5;
    interactions = [];
    trusted_by = [];
    trusts = [];
    total_transactions = 0;
    successful_transactions = 0;
    failed_transactions = 0;
    total_volume = 0.0;
    disputes_raised = 0;
    disputes_won = 0;
    created_at = Unix.time ();
    last_active = Unix.time ();
    verification_level = verification;
  } in
  
  Hashtbl.add manager.reputations agent_id reputation;
  manager.total_agents <- manager.total_agents + 1;
  
  reputation

(** {2 Score Calculation} *)

(** Update dimension score *)
let update_dimension_score old_score success =
  let samples = old_score.samples + 1 in
  let success_value = if success then 1.0 else 0.0 in
  
  (* Weighted average with more weight on recent *)
  let weight = 1.0 /. float_of_int samples in
  let new_value = 
    old_score.value *. (1.0 -. weight) +. success_value *. weight
  in
  
  (* Confidence increases with samples *)
  let confidence = 1.0 -. exp (-. float_of_int samples /. 10.0) in
  
  {
    value = new_value;
    confidence = confidence;
    samples = samples;
    last_updated = Unix.time ();
  }

(** Calculate overall score from dimensions *)
let calculate_overall_score manager reputation =
  let weighted_sum = ref 0.0 in
  let total_weight = ref 0.0 in
  
  Hashtbl.iter (fun dim score ->
    match Hashtbl.find_opt manager.dimension_weights dim with
    | None -> ()
    | Some weight ->
        if score.confidence >= manager.min_confidence then begin
          weighted_sum := !weighted_sum +. (score.value *. weight *. score.confidence);
          total_weight := !total_weight +. (weight *. score.confidence)
        end
  ) reputation.dimensions;
  
  if !total_weight > 0.0 then
    !weighted_sum /. !total_weight
  else
    0.5  (* Neutral if no confident scores *)

(** Apply time decay to scores *)
let apply_decay manager reputation =
  let current_time = Unix.time () in
  
  Hashtbl.iter (fun dim score ->
    let age = current_time -. score.last_updated in
    let decay_factor = exp (-. manager.decay_rate *. age /. 86400.0) in  (* Daily decay *)
    
    let decayed_confidence = score.confidence *. decay_factor in
    
    Hashtbl.replace reputation.dimensions dim {
      score with
      confidence = max 0.0 decayed_confidence;
    }
  ) reputation.dimensions

(** {2 Reputation Updates} *)

(** Record interaction *)
let rec record_interaction manager agent_id counterparty interaction_type
    outcome value =
  
  match Hashtbl.find_opt manager.reputations agent_id with
  | None -> 
      let _ = initialize_reputation manager agent_id () in
      record_interaction manager agent_id counterparty interaction_type outcome value
  | Some reputation ->
      (* Create interaction record *)
      let interaction = {
        interaction_id = Intent.generate_uuid ();
        counterparty = counterparty;
        interaction_type = interaction_type;
        outcome = outcome;
        value = value;
        timestamp = Unix.time ();
        impact = 0.0;  (* Will be calculated *)
      } in
      
      (* Update relevant dimensions *)
      let update_dim dim success =
        match Hashtbl.find_opt reputation.dimensions dim with
        | None -> ()
        | Some score ->
            let new_score = update_dimension_score score success in
            Hashtbl.replace reputation.dimensions dim new_score
      in
      
      (* Update based on interaction type and outcome *)
      (match interaction_type, outcome with
       | Settlement, Success ->
           update_dim Reliability true;
           update_dim Timeliness true;
           reputation.successful_transactions <- reputation.successful_transactions + 1
       | Settlement, Failure _ ->
           update_dim Reliability false;
           reputation.failed_transactions <- reputation.failed_transactions + 1
       | Settlement, Disputed ->
           reputation.disputes_raised <- reputation.disputes_raised + 1
       | Negotiation, Success ->
           update_dim PriceDiscovery true
       | Arbitration, Success ->
           update_dim DisputeResolution true;
           reputation.disputes_won <- reputation.disputes_won + 1
       | _ -> ());
      
      (* Update statistics *)
      reputation.total_transactions <- reputation.total_transactions + 1;
      reputation.total_volume <- reputation.total_volume +. value;
      reputation.last_active <- Unix.time ();
      
      (* Calculate impact *)
      let impact = value /. (reputation.total_volume +. 1.0) in
      let interaction = { interaction with impact = impact } in
      
      (* Add to history (limited) *)
      let history = 
        interaction :: reputation.interactions
        |> List.sort (fun a b -> Float.compare b.timestamp a.timestamp)
        |> List.filteri (fun i _ -> i < manager.max_history)
      in
      
      (* Recalculate overall score *)
      let overall = calculate_overall_score manager reputation in
      
      (* Update reputation *)
      let updated = { 
        reputation with 
        interactions = history;
        overall_score = overall;
      } in
      
      Hashtbl.replace manager.reputations agent_id updated;
      manager.total_interactions <- manager.total_interactions + 1

(** {2 Trust Network} *)

(** Add trust relationship *)
let add_trust manager truster trustee trust_level =
  match Hashtbl.find_opt manager.reputations truster,
        Hashtbl.find_opt manager.reputations trustee with
  | Some truster_rep, Some trustee_rep ->
      (* Update truster's trust list *)
      let new_trusts = 
        (trustee, trust_level) :: 
        List.filter (fun (a, _) -> a <> trustee) truster_rep.trusts
      in
      let updated_truster = { truster_rep with trusts = new_trusts } in
      
      (* Update trustee's trusted_by list *)
      let new_trusted_by = 
        (truster, trust_level) ::
        List.filter (fun (a, _) -> a <> truster) trustee_rep.trusted_by
      in
      let updated_trustee = { trustee_rep with trusted_by = new_trusted_by } in
      
      Hashtbl.replace manager.reputations truster updated_truster;
      Hashtbl.replace manager.reputations trustee updated_trustee;
      Ok ()
  | _ -> Error "Agent not found"

(** Calculate propagated trust *)
let rec calculate_trust_score manager source target depth =
  if depth > manager.trust_propagation_depth then
    0.0
  else if source = target then
    1.0
  else
    match Hashtbl.find_opt manager.reputations source with
    | None -> 0.0
    | Some reputation ->
        (* Direct trust *)
        match List.find_opt (fun (a, _) -> a = target) reputation.trusts with
        | Some (_, trust) -> trust
        | None ->
            (* Propagated trust *)
            let propagated = 
              List.fold_left (fun acc (intermediate, trust) ->
                let indirect = 
                  calculate_trust_score manager intermediate target (depth + 1)
                in
                max acc (trust *. indirect *. manager.trust_decay_factor)
              ) 0.0 reputation.trusts
            in
            propagated

(** {2 Queries} *)

(** Get reputation score *)
let get_reputation manager agent_id =
  match Hashtbl.find_opt manager.reputations agent_id with
  | None -> 0.5  (* Default neutral *)
  | Some reputation ->
      (* Apply decay before returning *)
      apply_decay manager reputation;
      calculate_overall_score manager reputation

(** Get dimension score *)
let get_dimension_score manager agent_id dimension =
  match Hashtbl.find_opt manager.reputations agent_id with
  | None -> None
  | Some reputation ->
      Hashtbl.find_opt reputation.dimensions dimension

(** Get trust score between agents *)
let get_trust_score manager source target =
  calculate_trust_score manager source target 0

(** Check if agent meets reputation threshold *)
let meets_threshold manager agent_id threshold =
  get_reputation manager agent_id >= threshold

(** {2 Sybil Resistance} *)

(** Check for sybil attacks *)
let detect_sybil_pattern manager agent_id =
  match Hashtbl.find_opt manager.reputations agent_id with
  | None -> false
  | Some reputation ->
      (* Check for suspicious patterns *)
      let suspicious = 
        (* Too many interactions with same parties *)
        let interaction_counts = Hashtbl.create 10 in
        List.iter (fun i ->
          let count = 
            try Hashtbl.find interaction_counts i.counterparty + 1
            with Not_found -> 1
          in
          Hashtbl.replace interaction_counts i.counterparty count
        ) reputation.interactions;
        
        let max_with_same = 
          Hashtbl.fold (fun _ count acc -> max acc count) interaction_counts 0
        in
        
        (* More than 50% interactions with same party is suspicious *)
        float_of_int max_with_same > 
        float_of_int (List.length reputation.interactions) *. 0.5
      in
      
      suspicious

(** {2 Statistics} *)

type reputation_stats = {
  total_agents: int;
  verified_agents: int;
  average_reputation: float;
  high_reputation_agents: int;  (* > 0.8 *)
  low_reputation_agents: int;   (* < 0.3 *)
  total_trust_relationships: int;
}

let get_stats manager =
  let all_reps = 
    Hashtbl.fold (fun _ rep acc -> rep :: acc) manager.reputations []
  in
  
  let verified = 
    List.filter (fun r -> r.verification_level <> Unverified) all_reps
    |> List.length
  in
  
  let scores = List.map (fun r -> r.overall_score) all_reps in
  let avg_score = 
    if List.length scores > 0 then
      List.fold_left (+.) 0.0 scores /. float_of_int (List.length scores)
    else
      0.5
  in
  
  let high = List.filter (fun s -> s > 0.8) scores |> List.length in
  let low = List.filter (fun s -> s < 0.3) scores |> List.length in
  
  let total_trust = 
    List.fold_left (fun acc r -> acc + List.length r.trusts) 0 all_reps
  in
  
  {
    total_agents = manager.total_agents;
    verified_agents = verified;
    average_reputation = avg_score;
    high_reputation_agents = high;
    low_reputation_agents = low;
    total_trust_relationships = total_trust;
  }