(** Negotiation Module
    
    This module handles negotiation between agents to find mutually acceptable
    settlement points within the settlement manifold.
    
    Key concepts:
    - Multi-round negotiation protocols
    - Concession strategies
    - Time-based pressure
    - Reputation-based trust
*)

open Ambience_core.Types
open Ambience_core

(** Negotiation protocol type *)
type protocol =
  | AlternatingOffers      (* Classic alternating offers *)
  | SimultaneousOffers     (* Both parties offer simultaneously *)
  | MediatedNegotiation    (* Third party mediator *)
  | AscendingAuction       (* Price ascends until one drops *)
  | DescendingAuction      (* Dutch auction *)

(** Negotiation strategy *)
type strategy =
  | Aggressive of float     (* High initial demand, slow concession *)
  | Moderate               (* Balanced approach *)
  | Accommodating of float (* Low initial demand, fast concession *)
  | Tit_for_tat           (* Mirror opponent's concessions *)
  | Deadline_based        (* Increase concessions as deadline approaches *)

(** Negotiation session *)
type session = {
  session_id: uuid;
  match_id: uuid;
  participants: participant list;
  manifold: settlement_manifold;
  protocol: protocol;
  state: negotiation_state;
  history: offer list;
  started_at: timestamp;
  deadline: timestamp option;
  max_rounds: int;
  current_round: int;
}

and participant = {
  agent_id: public_key;
  strategy: strategy;
  reservation_utility: float;  (* Minimum acceptable utility *)
  time_preference: float;      (* Discount factor for delay *)
  reputation: float;
  mutable last_offer: settlement_point option;
  mutable concession_rate: float;
}

and offer = {
  round: int;
  proposer: public_key;
  point: settlement_point;
  utility_for_proposer: float;
  utility_for_receiver: float;
  timestamp: timestamp;
}

(** Create a new negotiation session *)
let create_session match_id participants manifold protocol 
    ?(deadline = None) ?(max_rounds = 20) () =
  {
    session_id = Intent.generate_uuid ();
    match_id = match_id;
    participants = participants;
    manifold = manifold;
    protocol = protocol;
    state = Proposing;
    history = [];
    started_at = Ambience_core.Time_provider.now ();
    deadline = deadline;
    max_rounds = max_rounds;
    current_round = 0;
  }

(** Create a participant *)
let create_participant agent_id strategy ?(reservation_utility = 0.3) 
    ?(time_preference = 0.95) ?(reputation = 0.5) () =
  {
    agent_id = agent_id;
    strategy = strategy;
    reservation_utility = reservation_utility;
    time_preference = time_preference;
    reputation = reputation;
    last_offer = None;
    concession_rate = 0.1;  (* Default 10% concession per round *)
  }

(** {2 Utility Calculation} *)

(** Calculate utility of a point for a participant *)
let calculate_participant_utility participant point manifold =
  (* Find utility from manifold scores *)
  match manifold.optimality_scores with
  | None -> 0.5  (* Default neutral utility *)
  | Some scores ->
      (* Find this point in scores *)
      List.find_map (fun (p, score) ->
        if p.price = point.price && 
           p.quantity = point.quantity &&
           p.execution_time = point.execution_time then
          Some score
        else None
      ) scores
      |> Option.value ~default:0.5

(** Apply time discounting to utility *)
let apply_time_discount participant utility delay =
  utility *. (participant.time_preference ** delay)

(** {2 Offer Generation} *)

(** Generate initial offer based on strategy *)
let generate_initial_offer participant manifold =
  match participant.strategy with
  | Aggressive aspiration ->
      (* Pick point with high utility for self *)
      let candidates = 
        match manifold.optimality_scores with
        | None -> manifold.pareto_frontier
        | Some scores ->
            scores
            |> List.filter (fun (_p, score) -> score >= aspiration)
            |> List.map fst
      in
      List.nth_opt candidates 0
      
  | Moderate ->
      (* Pick middle point of Pareto frontier *)
      let n = List.length manifold.pareto_frontier in
      List.nth_opt manifold.pareto_frontier (n / 2)
      
  | Accommodating target ->
      (* Pick point favorable to opponent *)
      let candidates = 
        match manifold.optimality_scores with
        | None -> manifold.pareto_frontier
        | Some scores ->
            scores
            |> List.filter (fun (_p, score) -> score <= target)
            |> List.map fst
      in
      List.nth_opt candidates 0
      
  | _ ->
      (* Default: first point on frontier *)
      List.nth_opt manifold.pareto_frontier 0

(** Generate counter-offer based on strategy and history *)
let generate_counter_offer participant opponent_offer manifold session =
  match participant.strategy with
  | Tit_for_tat ->
      (* Mirror opponent's concession *)
      let opponent_concession = 
        match session.history with
        | [] -> 0.1
        | last :: _ ->
            let prev_utility = last.utility_for_receiver in
            let curr_utility = 
              calculate_participant_utility participant opponent_offer manifold
            in
            curr_utility -. prev_utility
      in
      
      (* Make similar concession *)
      participant.concession_rate <- abs_float opponent_concession;
      
      (* Find point with adjusted utility *)
      let target_utility = 
        match participant.last_offer with
        | None -> participant.reservation_utility
        | Some last ->
            let last_utility = 
              calculate_participant_utility participant last manifold
            in
            last_utility -. participant.concession_rate
      in
      
      (* Find closest point to target utility *)
      (match manifold.optimality_scores with
      | None -> Some opponent_offer  (* Accept if no scores *)
      | Some scores ->
          scores
          |> List.map (fun (p, s) -> (abs_float (s -. target_utility), p))
          |> List.sort (fun (d1, _) (d2, _) -> Float.compare d1 d2)
          |> List.map snd
          |> (fun lst -> List.nth_opt lst 0))

  | Deadline_based ->
      (* Increase concessions as deadline approaches *)
      let time_pressure = 
        match session.deadline with
        | None -> 0.1
        | Some deadline ->
            let remaining = deadline -. Ambience_core.Time_provider.now () in
            let total = deadline -. session.started_at in
            1.0 -. (remaining /. total)
      in
      
      participant.concession_rate <- 0.05 +. (0.3 *. time_pressure);
      
      (* Apply concession *)
      generate_initial_offer participant manifold
      
  | _ ->
      (* Default concession strategy *)
      participant.concession_rate <- 
        participant.concession_rate *. 1.1;  (* Increase by 10% *)
      
      generate_initial_offer participant manifold

(** {2 Negotiation Protocols} *)

(** Run alternating offers protocol *)
let alternating_offers_round session : negotiation_state =
  match session.participants with
  | [p1; p2] ->
      let proposer, receiver = 
        if session.current_round mod 2 = 0 then (p1, p2) else (p2, p1)
      in
      
      (* Generate offer *)
      let offer_point = 
        if session.current_round = 0 then
          generate_initial_offer proposer session.manifold
        else
          match List.nth_opt session.history 0 with
          | None -> generate_initial_offer proposer session.manifold
          | Some last_offer -> 
              generate_counter_offer proposer last_offer.point 
                session.manifold session
      in
      
      match offer_point with
      | None -> Failed "No valid offer found"
      | Some point ->
          (* Calculate utilities *)
          let u_proposer = 
            calculate_participant_utility proposer point session.manifold
          in
          let u_receiver = 
            calculate_participant_utility receiver point session.manifold
          in
          
          (* Check if acceptable *)
          if u_receiver >= receiver.reservation_utility then
            (* Accept *)
            Agreed point
          else if session.current_round >= session.max_rounds then
            (* Max rounds reached *)
            Failed "Maximum rounds exceeded"
          else
            (* Continue negotiation *)
            let offer = {
              round = session.current_round;
              proposer = proposer.agent_id;
              point = point;
              utility_for_proposer = u_proposer;
              utility_for_receiver = u_receiver;
              timestamp = Ambience_core.Time_provider.now ();
            } in
            
            proposer.last_offer <- Some point;
            Negotiating [point]
            
  | _ -> Failed "Alternating offers requires exactly 2 participants"

(** Run simultaneous offers protocol *)
let simultaneous_offers_round session =
  (* Each participant makes an offer *)
  let offers = 
    List.filter_map (fun p ->
      let offer = 
        if session.current_round = 0 then
          generate_initial_offer p session.manifold
        else
          match p.last_offer with
          | None -> generate_initial_offer p session.manifold
          | Some last -> 
              generate_counter_offer p last session.manifold session
      in
      Option.map (fun point -> (p, point)) offer
    ) session.participants
  in
  
  (* Check for agreement (offers are close enough) *)
  match offers with
  | [(p1, point1); (p2, point2)] ->
      let distance = 
        abs_float (point1.price -. point2.price) +.
        abs_float (point1.quantity -. point2.quantity)
      in
      
      if distance < 0.01 then
        (* Close enough - take midpoint *)
        let midpoint = {
          price = (point1.price +. point2.price) /. 2.0;
          quantity = (point1.quantity +. point2.quantity) /. 2.0;
          execution_time = min point1.execution_time point2.execution_time;
          quality_level = point1.quality_level;
          additional_terms = [];
        } in
        Agreed midpoint
      else
        Negotiating [point1; point2]
  | _ ->
      Failed "Invalid number of offers"

(** {2 Negotiation Execution} *)

(** Execute one round of negotiation *)
let execute_round session =
  (* Check deadline *)
  let deadline_passed = 
    match session.deadline with
    | None -> false
    | Some d -> Ambience_core.Time_provider.now () > d
  in
  
  if deadline_passed then
    { session with state = Failed "Deadline exceeded" }
  else if session.current_round >= session.max_rounds then
    { session with state = Failed "Maximum rounds exceeded" }
  else
    let new_state = 
      match session.protocol with
      | AlternatingOffers -> alternating_offers_round session
      | SimultaneousOffers -> simultaneous_offers_round session
      | MediatedNegotiation -> Failed "Mediated negotiation not implemented"
      | AscendingAuction -> Failed "Ascending auction not implemented"
      | DescendingAuction -> Failed "Descending auction not implemented"
    in
    
    { session with 
      state = new_state;
      current_round = session.current_round + 1;
    }

(** Run negotiation to completion *)
let rec run_to_completion session =
  match session.state with
  | Agreed _ | Failed _ -> session
  | _ ->
      let next_session = execute_round session in
      run_to_completion next_session

(** {2 Analysis} *)

(** Negotiation outcome *)
type outcome = {
  success: bool;
  final_point: settlement_point option;
  rounds_taken: int;
  time_taken: float;
  total_concession: float;
  efficiency: float;  (* Social welfare achieved *)
}

(** Analyze negotiation outcome *)
let analyze_outcome session =
  let success, final_point = 
    match session.state with
    | Agreed point -> (true, Some point)
    | _ -> (false, None)
  in
  
  let time_taken = Ambience_core.Time_provider.now () -. session.started_at in
  
  let total_concession = 
    List.fold_left (fun acc p -> acc +. p.concession_rate) 0.0 
      session.participants
  in
  
  let efficiency = 
    match final_point, session.manifold.optimality_scores with
    | Some point, Some scores ->
        (* Find social welfare of agreed point *)
        List.find_map (fun (p, score) ->
          if p = point then Some score else None
        ) scores
        |> Option.value ~default:0.0
    | _ -> 0.0
  in
  
  {
    success = success;
    final_point = final_point;
    rounds_taken = session.current_round;
    time_taken = time_taken;
    total_concession = total_concession;
    efficiency = efficiency;
  }

(** {2 Learning and Adaptation} *)

(** Update strategy based on outcome *)
let adapt_strategy participant outcome =
  if outcome.success then
    (* Successful negotiation - slightly reduce concession rate *)
    participant.concession_rate <- participant.concession_rate *. 0.9
  else
    (* Failed negotiation - increase concession rate *)
    participant.concession_rate <- participant.concession_rate *. 1.2