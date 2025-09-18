(** Matching Engine
    
    This module implements the core matching algorithm that discovers when
    intents can satisfy each other and computes the space of possible settlements.
    
    Key concepts:
    - Matching is continuous, not event-driven
    - Matches exist in multi-dimensional settlement spaces
    - The engine finds Pareto-optimal settlement points
    - Matching can be bilateral or multilateral
*)

open Ambience_core.Types
module Intent = Ambience_core.Intent
module Resource = Ambience_core.Resource
module Reputation = Ambience_trust.Reputation
module Capability = Ambience_trust.Capability
module Collateral = Ambience_trust.Collateral
module Escrow = Ambience_settlement.Escrow

(** Configuration for the matching engine *)
type config = {
  matching_interval: float;      (* How often to run matching (seconds) *)
  max_intents_per_round: int;    (* Maximum intents to process per round *)
  pareto_samples: int;           (* Number of samples for Pareto frontier *)
  min_match_quality: float;      (* Minimum quality score for a match *)
  enable_multilateral: bool;     (* Enable multi-party matching *)
  max_settlement_dimensions: int; (* Limit complexity of settlement space *)
}

(** Market condition detection *)
type market_conditions =
  | HighLiquidity    (* Many compatible intents *)
  | LowLiquidity     (* Few matches possible *)
  | Volatile         (* Rapid price changes *)
  | Stable          (* Consistent patterns *)

(** Default configuration *)
let default_config = {
  matching_interval = 0.1;       (* 100ms *)
  max_intents_per_round = 1000;
  pareto_samples = 100;
  min_match_quality = 0.5;
  enable_multilateral = false;   (* Start with bilateral only *)
  max_settlement_dimensions = 5;
}

(** The matching engine state *)
type t = {
  config: config;
  mutable running: bool;
  mutable rounds_completed: int;
  mutable total_matches_found: int;
  mutable total_time_spent: float;
  state: Ambience_core.State.t;
  (* Trust layer integration *)
  reputation_mgr: Reputation.reputation_manager option;
  capability_mgr: Capability.capability_manager option;
  collateral_mgr: Collateral.collateral_manager option;
  (* Settlement layer integration *)
  escrow_mgr: Escrow.escrow_manager option;
  (* Network propagation callback *)
  mutable propagate_callback: (match_t -> unit) option;
  (* Learning and adaptation *)
  mutable match_outcomes: (uuid * bool * float) list;  (* match_id, success, utility *)
  (* Market conditions *)
  mutable market_conditions: market_conditions;
}

(** Create a new matching engine *)
let create ?(config = default_config) ?reputation_mgr ?capability_mgr ?collateral_mgr
    ?escrow_mgr state = {
  config = config;
  running = false;
  rounds_completed = 0;
  total_matches_found = 0;
  total_time_spent = 0.0;
  state = state;
  reputation_mgr = reputation_mgr;
  capability_mgr = capability_mgr;
  collateral_mgr = collateral_mgr;
  escrow_mgr = escrow_mgr;
  propagate_callback = None;
  match_outcomes = [];
  market_conditions = Stable;
}

(** {2 Settlement Space Computation} *)

(** Compute the valid price range for a match *)
let compute_price_range intent_a intent_b =
  (* Get price constraints from both intents *)
  let prices_a = Intent.get_price_ranges intent_a in
  let prices_b = Intent.get_price_ranges intent_b in
  
  (* If no price constraints, use quantity ratio *)
  match prices_a, prices_b with
  | [], [] ->
      (* No explicit price constraints - derive from quantities *)
      let (a_offer_min, a_offer_max) = intent_a.offer_field.quantity_range in
      let (b_offer_min, b_offer_max) = intent_b.offer_field.quantity_range in
      if a_offer_max > 0.0 && b_offer_max > 0.0 then
        let min_price = b_offer_min /. a_offer_max in
        let max_price = b_offer_max /. a_offer_min in
        Some (min_price, max_price)
      else
        None
  | (min_a, max_a) :: _, (min_b, max_b) :: _ ->
      (* Find intersection of price ranges *)
      let min_price = max min_a min_b in
      let max_price = min max_a max_b in
      if min_price <= max_price then
        Some (min_price, max_price)
      else
        None
  | _ ->
      (* One has constraints, other doesn't - use the constrained range *)
      match prices_a @ prices_b with
      | (min_p, max_p) :: _ -> Some (min_p, max_p)
      | [] -> None

(** Compute the valid quantity range for a match *)
let compute_quantity_range intent_a intent_b =
  Resource.calculate_tradeable_range 
    intent_a.offer_field 
    intent_b.want_field

(** Compute the valid time window for execution *)
let compute_time_window intent_a intent_b =
  let windows_a = Intent.get_time_windows intent_a in
  let windows_b = Intent.get_time_windows intent_b in
  
  let current_time = Ambience_core.Time_provider.now () in
  
  match windows_a, windows_b with
  | [], [] -> 
      (* No time constraints - execute anytime *)
      Some (current_time, current_time +. 86400.0)  (* 24 hours *)
  | [], (start_b, end_b) :: _ -> Some (max current_time start_b, end_b)
  | (start_a, end_a) :: _, [] -> Some (max current_time start_a, end_a)
  | (start_a, end_a) :: _, (start_b, end_b) :: _ ->
      let start = max (max current_time start_a) start_b in
      let end_ = min end_a end_b in
      if start <= end_ then Some (start, end_) else None

(** Sample points in the settlement space *)
let sample_settlement_points ~price_range ~quantity_range ~time_window ~samples =
  match price_range, quantity_range, time_window with
  | Some (min_p, max_p), Some (min_q, max_q), Some (min_t, max_t) ->
      (* Generate uniform samples across the space *)
      let n = samples in
      let points = ref [] in
      
      for i = 0 to n - 1 do
        let t = float_of_int i /. float_of_int (n - 1) in
        
        (* Linear interpolation for each dimension *)
        let price = min_p +. t *. (max_p -. min_p) in
        let quantity = min_q +. t *. (max_q -. min_q) in
        let time = min_t +. t *. (max_t -. min_t) in
        
        let point = {
          price = price;
          quantity = quantity;
          execution_time = time;
          quality_level = None;
          additional_terms = [];
        } in
        points := point :: !points
      done;
      !points
  | _ -> []

(** Calculate utility score for a settlement point from an agent's perspective *)
let calculate_utility intent point =
  (* Utility function components *)
  
  (* Price utility - closer to preferred price is better *)
  let price_utility = 
    match Intent.get_price_ranges intent with
    | [] -> 1.0  (* No preference *)
    | (min_p, max_p) :: _ ->
        if point.price < min_p then 0.0
        else if point.price > max_p then 0.0
        else
          (* Prefer middle of range *)
          let mid = (min_p +. max_p) /. 2.0 in
          let deviation = abs_float (point.price -. mid) /. (max_p -. min_p) in
          1.0 -. deviation
  in
  
  (* Quantity utility - more is generally better for receiver *)
  let quantity_utility =
    let (min_q, max_q) = intent.want_field.quantity_range in
    if max_q > min_q then
      (point.quantity -. min_q) /. (max_q -. min_q)
    else
      1.0
  in
  
  (* Time utility - sooner is generally better *)
  let time_utility =
    let current = Ambience_core.Time_provider.now () in
    let delay = point.execution_time -. current in
    let max_delay = 86400.0 in  (* 24 hours *)
    max 0.0 (1.0 -. (delay /. max_delay))
  in
  
  (* Combine utilities with weights *)
  let weights = (0.5, 0.3, 0.2) in  (* Price, quantity, time *)
  let (w_p, w_q, w_t) = weights in
  w_p *. price_utility +. w_q *. quantity_utility +. w_t *. time_utility

(** Find Pareto-optimal points in the settlement space *)
let find_pareto_frontier intent_a intent_b points =
  (* A point is Pareto-optimal if no other point is better for both agents *)
  let is_dominated p1 p2 =
    let u1_a = calculate_utility intent_a p1 in
    let u1_b = calculate_utility intent_b p1 in
    let u2_a = calculate_utility intent_a p2 in
    let u2_b = calculate_utility intent_b p2 in
    (* p1 is dominated by p2 if p2 is better for both *)
    u2_a >= u1_a && u2_b >= u1_b && (u2_a > u1_a || u2_b > u1_b)
  in
  
  (* Filter to non-dominated points *)
  List.filter (fun p ->
    not (List.exists (fun q -> p != q && is_dominated p q) points)
  ) points

(** Compute the complete settlement manifold for a match *)
let compute_settlement_manifold config intent_a intent_b =
  (* Compute valid ranges for each dimension *)
  let price_range = compute_price_range intent_a intent_b in
  let quantity_range = compute_quantity_range intent_a intent_b in
  let time_window = compute_time_window intent_a intent_b in
  
  (* Check if match is feasible *)
  match price_range, quantity_range, time_window with
  | None, _, _ | _, None, _ | _, _, None ->
      (* No feasible match *)
      None
  | Some _, Some _, Some _ ->
      (* Sample the settlement space *)
      let sample_points = 
        sample_settlement_points 
          ~price_range ~quantity_range ~time_window 
          ~samples:config.pareto_samples
      in
      
      (* Find Pareto frontier *)
      let pareto_frontier = find_pareto_frontier intent_a intent_b sample_points in
      
      (* Calculate optimality scores *)
      let optimality_scores = 
        List.map (fun point ->
          let score = 
            (calculate_utility intent_a point +. 
             calculate_utility intent_b point) /. 2.0
          in
          (point, score)
        ) pareto_frontier
      in
      
      (* Create manifold *)
      Some {
        dimensions = ["price"; "quantity"; "time"];
        valid_region_constraints = 
          intent_a.constraints @ intent_b.constraints;
        pareto_frontier = pareto_frontier;
        optimality_scores = Some optimality_scores;
      }

(** Find Nash bargaining solution in settlement manifold *)
let find_nash_solution manifold intent_a intent_b =
  match manifold.optimality_scores with
  | None -> None
  | Some scores ->
      (* Calculate reservation utilities (disagreement points) *)
      (* These are the utilities if no agreement is reached *)
      let reservation_a = 0.0 in  (* No trade utility *)
      let reservation_b = 0.0 in

      (* Find point maximizing Nash product *)
      let nash_point =
        List.fold_left (fun best (point, _score) ->
          let utility_a = calculate_utility intent_a point in
          let utility_b = calculate_utility intent_b point in

          (* Nash product: (u_a - d_a) * (u_b - d_b) *)
          let surplus_a = max 0.0 (utility_a -. reservation_a) in
          let surplus_b = max 0.0 (utility_b -. reservation_b) in
          let nash_product = surplus_a *. surplus_b in

          match best with
          | None -> Some (point, nash_product)
          | Some (_, best_product) ->
              if nash_product > best_product then
                Some (point, nash_product)
              else
                best
        ) None scores
      in

      Option.map fst nash_point

(** Find Kalai-Smorodinsky solution in settlement manifold *)
let find_kalai_smorodinsky_solution manifold intent_a intent_b =
  match manifold.optimality_scores with
  | None -> None
  | Some scores ->
      (* Find ideal utilities for each agent *)
      let ideal_a =
        List.fold_left (fun acc (point, _) ->
          max acc (calculate_utility intent_a point)
        ) 0.0 scores
      in

      let ideal_b =
        List.fold_left (fun acc (point, _) ->
          max acc (calculate_utility intent_b point)
        ) 0.0 scores
      in

      (* Find point maintaining proportional utilities *)
      let ks_point =
        List.fold_left (fun best (point, _) ->
          let utility_a = calculate_utility intent_a point in
          let utility_b = calculate_utility intent_b point in

          (* Check if utilities are proportional to ideal utilities *)
          let ratio_a = if ideal_a > 0.0 then utility_a /. ideal_a else 0.0 in
          let ratio_b = if ideal_b > 0.0 then utility_b /. ideal_b else 0.0 in
          let ratio_diff = abs_float (ratio_a -. ratio_b) in

          match best with
          | None -> Some (point, ratio_diff)
          | Some (_, best_diff) ->
              if ratio_diff < best_diff then
                Some (point, ratio_diff)
              else
                best
        ) None scores
      in

      Option.map fst ks_point

(** {2 Match Discovery} *)

(** Calculate match quality with trust and history *)
let calculate_match_quality_with_trust engine intent_a intent_b manifold =
  (* Base quality from manifold characteristics *)
  let base_quality =
    match manifold.optimality_scores with
    | None -> 0.5
    | Some scores ->
        let sum = List.fold_left (fun acc (_, s) -> acc +. s) 0.0 scores in
        sum /. float_of_int (List.length scores)
  in

  (* Factor in reputation *)
  let reputation_factor = match engine.reputation_mgr with
    | None -> 0.5
    | Some mgr ->
        let rep_a = Reputation.get_reputation mgr intent_a.agent_id in
        let rep_b = Reputation.get_reputation mgr intent_b.agent_id in
        (rep_a +. rep_b) /. 2.0
  in

  (* Factor in historical success between these agents *)
  let history_factor =
    (* For now, use average of their reputations as proxy for history *)
    match engine.reputation_mgr with
    | None -> 0.5
    | Some mgr ->
        let rep_a = Reputation.get_reputation mgr intent_a.agent_id in
        let rep_b = Reputation.get_reputation mgr intent_b.agent_id in
        (* Agents with good reputation likely have good history *)
        (rep_a +. rep_b) /. 2.0
  in

  (* Factor in settlement risk *)
  let risk_factor =
    let time_risk =
      (* Longer time windows = higher risk *)
      match Intent.get_time_windows intent_a, Intent.get_time_windows intent_b with
      | (start_a, end_a) :: _, (start_b, end_b) :: _ ->
          let window_a = end_a -. start_a in
          let window_b = end_b -. start_b in
          let max_window = max window_a window_b in
          min 1.0 (max_window /. 86400.0)  (* Normalize to 1 day *)
      | _ -> 0.5
    in
    time_risk
  in

  (* Weighted combination *)
  base_quality *. 0.4 +.
  reputation_factor *. 0.3 +.
  history_factor *. 0.2 +.
  (1.0 -. risk_factor) *. 0.1

(** Legacy calculate_match_quality *)
let calculate_match_quality intent_a intent_b manifold =
  let engine = create (Ambience_core.State.create ()) in
  calculate_match_quality_with_trust engine intent_a intent_b manifold

(** Check if two intents can match with trust integration *)
let can_match_with_trust engine intent_a intent_b =
  (* Quick compatibility check *)
  if not (Intent.are_compatible intent_a intent_b) then
    false
  else
    (* Check capabilities if available *)
    let capability_ok = match engine.capability_mgr with
      | None -> true  (* No capability check if not configured *)
      | Some mgr ->
          Ambience_trust.Capability.can_trade mgr intent_a.agent_id intent_a.offer_field.resource_type &&
          Ambience_trust.Capability.can_trade mgr intent_b.agent_id intent_b.offer_field.resource_type
    in

    if not capability_ok then false
    else
      (* Check reputation requirements *)
      let (rep_a, rep_b) = match engine.reputation_mgr with
        | None -> (0.5, 0.5)  (* Default reputation if not configured *)
        | Some mgr ->
            (Ambience_trust.Reputation.get_reputation mgr intent_a.agent_id,
             Ambience_trust.Reputation.get_reputation mgr intent_b.agent_id)
      in

      (* Check collateral sufficiency *)
      let collateral_ok = match engine.collateral_mgr with
        | None -> true
        | Some mgr ->
            let max_value = intent_a.offer_field.quantity_range |> snd in
            let required = Ambience_trust.Collateral.calculate_required mgr max_value rep_a
              (Ambience_trust.Collateral.Percentage 0.1) in
            Ambience_trust.Collateral.has_sufficient_collateral mgr intent_a.agent_id required &&
            let required_b = Ambience_trust.Collateral.calculate_required mgr max_value rep_b
              (Ambience_trust.Collateral.Percentage 0.1) in
            Ambience_trust.Collateral.has_sufficient_collateral mgr intent_b.agent_id required_b
      in

      if not collateral_ok then false
      else
        (* Check counterparty requirements with actual reputation *)
        Intent.meets_counterparty_requirements intent_a intent_b.agent_id rep_b &&
        Intent.meets_counterparty_requirements intent_b intent_a.agent_id rep_a

(** Legacy can_match for backwards compatibility *)
let can_match config intent_a intent_b =
  (* Create temporary engine with no trust components *)
  let engine = create ~config (Ambience_core.State.create ()) in
  can_match_with_trust engine intent_a intent_b

(** Discover bilateral matches *)
let discover_bilateral_matches config intents =
  (* Group intents by resource for efficiency *)
  let by_resource = Intent.Batch.group_by_resource intents in
  
  let matches = ref [] in
  
  (* For each resource type, find matches *)
  Hashtbl.iter (fun _resource_type intent_list ->
    (* Find all compatible pairs *)
    let pairs = Intent.Batch.find_compatible_pairs intent_list in
    
    (* For each pair, try to create a match *)
    List.iter (fun (intent_a, intent_b) ->
      if can_match config intent_a intent_b then
        match compute_settlement_manifold config intent_a intent_b with
        | None -> ()  (* No feasible settlement *)
        | Some manifold ->
            let quality = calculate_match_quality intent_a intent_b manifold in
            
            if quality >= config.min_match_quality then
              let match_t = {
                match_id = Intent.generate_uuid ();
                intent_ids = [intent_a.intent_id; intent_b.intent_id];
                settlement_space = manifold;
                discovered_at = Ambience_core.Time_provider.now ();
                discovered_by = "matching_engine";
                expires_at = Ambience_core.Time_provider.now () +. 300.0;  (* 5 minute expiry *)
              } in
              matches := match_t :: !matches
    ) pairs
  ) by_resource;
  
  !matches

(** Discover multilateral matches (3+ party) *)
let discover_multilateral_matches _config _intents =
  (* TODO: Implement circular/triangular trades *)
  (* Example: A offers X wants Y, B offers Y wants Z, C offers Z wants X *)
  []

(** {2 Market Adaptation} *)

(** Detect current market conditions *)
let detect_market_conditions engine intents matches =
  let intent_count = List.length intents in
  let match_count = List.length matches in
  let match_rate = if intent_count > 0 then
    float_of_int match_count /. float_of_int intent_count
  else 0.0 in

  (* Calculate price volatility from recent matches *)
  let price_volatility =
    if List.length matches < 2 then 0.0
    else
      let prices = List.filter_map (fun m ->
        match m.settlement_space.pareto_frontier with
        | [] -> None
        | points -> Some (List.hd points).price
      ) matches in
      if List.length prices < 2 then 0.0
      else
        let avg = List.fold_left (+.) 0.0 prices /. float_of_int (List.length prices) in
        let variance = List.fold_left (fun acc p ->
          acc +. (p -. avg) ** 2.0
        ) 0.0 prices /. float_of_int (List.length prices) in
        sqrt variance /. avg  (* Coefficient of variation *)
  in

  (* Determine conditions based on metrics *)
  match match_rate, price_volatility with
  | rate, _ when rate > 0.5 -> HighLiquidity
  | rate, _ when rate < 0.1 -> LowLiquidity
  | _, vol when vol > 0.3 -> Volatile
  | _ -> Stable

(** Adapt matching strategy to market conditions *)
let adapt_to_market_conditions engine conditions =
  match conditions with
  | HighLiquidity ->
      (* Many matches available - be more selective *)
      engine.config <- { engine.config with
        min_match_quality = min 0.8 (engine.config.min_match_quality *. 1.2);
        matching_interval = 1.0;  (* Can afford slower matching *)
      }
  | LowLiquidity ->
      (* Few matches - be less selective and enable multilateral *)
      engine.config <- { engine.config with
        min_match_quality = max 0.3 (engine.config.min_match_quality *. 0.8);
        enable_multilateral = true;
        matching_interval = 0.5;  (* Match more frequently *)
      }
  | Volatile ->
      (* Prices changing rapidly - match quickly *)
      engine.config <- { engine.config with
        matching_interval = 0.1;  (* Very fast matching *)
        pareto_samples = 50;  (* Lower resolution for speed *)
      }
  | Stable ->
      (* Normal conditions *)
      engine.config <- { engine.config with
        min_match_quality = 0.5;
        matching_interval = 1.0;
        pareto_samples = 100;
      }

(** Propagate high-quality matches via callback *)
let propagate_matches engine matches =
  match engine.propagate_callback with
  | None -> ()
  | Some callback ->
      matches
      |> List.filter (fun m -> m.quality > 0.7)  (* Only propagate high-quality matches *)
      |> List.iter callback

(** Set match propagation callback *)
let set_propagate_callback engine callback =
  engine.propagate_callback <- Some callback

(** Run one matching round *)
let run_matching_round engine =
  let start_time = Ambience_core.Time_provider.now () in
  
  (* Get active intents *)
  let intents = 
    Ambience_core.State.Queries.get_active_intents engine.state
    |> Intent.Batch.sort_by_priority
    |> fun list ->
        (* Limit number of intents per round *)
        let len = min (List.length list) engine.config.max_intents_per_round in
        List.filteri (fun i _ -> i < len) list
  in
  
  (* Discover matches *)
  let bilateral_matches = discover_bilateral_matches engine.config intents in
  
  let multilateral_matches = 
    if engine.config.enable_multilateral then
      discover_multilateral_matches engine.config intents
    else
      []
  in
  
  let all_matches = bilateral_matches @ multilateral_matches in
  
  (* Record matches in state *)
  List.iter (fun match_t ->
    match Ambience_core.State.Transitions.record_match engine.state match_t with
    | Ok () -> engine.total_matches_found <- engine.total_matches_found + 1
    | Error e -> Printf.eprintf "Failed to record match: %s\n" e
  ) all_matches;
  
  (* Update statistics *)
  let elapsed = Ambience_core.Time_provider.now () -. start_time in
  engine.total_time_spent <- engine.total_time_spent +. elapsed;
  engine.rounds_completed <- engine.rounds_completed + 1;

  (* Propagate high-quality matches through gossip network *)
  propagate_matches engine all_matches;

  (* Detect and adapt to market conditions *)
  let conditions = detect_market_conditions engine intents all_matches in
  if conditions <> engine.market_conditions then begin
    engine.market_conditions <- conditions;
    adapt_to_market_conditions engine conditions
  end;

  all_matches

(** {2 Engine Control} *)

(** Start the matching engine *)
let start engine =
  engine.running <- true;
  
  let rec loop () =
    if engine.running then begin
      (* Run a matching round *)
      let _matches = run_matching_round engine in
      
      (* Sleep before next round *)
      Unix.sleepf engine.config.matching_interval;
      
      (* Continue *)
      loop ()
    end
  in
  
  (* Run in separate thread in production *)
  loop ()

(** Stop the matching engine *)
let stop engine =
  engine.running <- false

(** Get engine statistics *)
type stats = {
  rounds_completed: int;
  total_matches_found: int;
  total_time_spent: float;
  average_time_per_round: float;
  matches_per_second: float;
  is_running: bool;
}

let get_stats (engine : t) = {
  rounds_completed = engine.rounds_completed;
  total_matches_found = engine.total_matches_found;
  total_time_spent = engine.total_time_spent;
  average_time_per_round = 
    if engine.rounds_completed > 0 then
      engine.total_time_spent /. float_of_int engine.rounds_completed
    else 0.0;
  matches_per_second = 
    if engine.total_time_spent > 0.0 then
      float_of_int engine.total_matches_found /. engine.total_time_spent
    else 0.0;
  is_running = engine.running;
}

(** {2 Advanced Matching Algorithms} *)

module Advanced = struct
  (** Stable matching using Gale-Shapley algorithm *)
  let stable_matching intents_a intents_b =
    (* TODO: Implement deferred acceptance algorithm *)
    []
  
  (** Optimal matching using Hungarian algorithm *)
  let optimal_matching intents_a intents_b cost_matrix =
    (* TODO: Implement Hungarian algorithm for optimal assignment *)
    []
  
  (** Market clearing prices using tatonnement *)
  let find_clearing_prices intents =
    (* TODO: Implement price adjustment to clear market *)
    []
end

(** {2 Match Ranking and Filtering} *)

(** Rank matches by various criteria *)
let rank_matches matches =
  let score_match match_t =
    (* Score based on multiple factors *)
    let quality_score = 
      match match_t.settlement_space.optimality_scores with
      | None -> 0.5
      | Some scores ->
          let sum = List.fold_left (fun acc (_, s) -> acc +. s) 0.0 scores in
          sum /. float_of_int (List.length scores)
    in
    
    let urgency_score =
      (* Matches expiring soon get higher priority *)
      let time_until_expiry = match_t.expires_at -. Ambience_core.Time_provider.now () in
      let max_expiry = 300.0 in  (* 5 minutes *)
      1.0 -. (time_until_expiry /. max_expiry)
    in
    
    let frontier_size_score =
      (* Larger Pareto frontier means more flexibility *)
      let size = List.length match_t.settlement_space.pareto_frontier in
      float_of_int (min size 10) /. 10.0
    in
    
    (* Weighted combination *)
    0.5 *. quality_score +. 0.3 *. urgency_score +. 0.2 *. frontier_size_score
  in
  
  matches
  |> List.map (fun m -> (score_match m, m))
  |> List.sort (fun (s1, _) (s2, _) -> Float.compare s2 s1)
  |> List.map snd

(** Filter matches to avoid conflicts *)
let filter_non_conflicting matches =
  (* An intent can only be in one active match at a time *)
  let used_intents = Hashtbl.create 100 in
  
  List.filter (fun match_t ->
    let conflicting = 
      List.exists (fun intent_id ->
        Hashtbl.mem used_intents intent_id
      ) match_t.intent_ids
    in
    
    if not conflicting then begin
      (* Mark intents as used *)
      List.iter (fun id -> Hashtbl.add used_intents id true) match_t.intent_ids;
      true
    end else
      false
  ) matches