# Matching Engine Trust Layer Integration

## Overview

This document describes the integration between the matching engine and trust components, addressing the critical issue of the matching engine operating in isolation from reputation, capabilities, and collateral systems.

## Key Improvements Implemented

### 1. Trust-Aware Engine State

The matching engine now maintains references to trust components:

```ocaml
type t = {
  config: config;
  state: State.t;
  (* Trust layer integration *)
  reputation_mgr: Reputation.reputation_manager option;
  capability_mgr: Capability.capability_manager option;
  collateral_mgr: Collateral.collateral_manager option;
  (* Settlement integration *)
  escrow_mgr: Escrow.escrow_manager option;
  (* Learning and adaptation *)
  mutable match_outcomes: (uuid * bool * float) list;
}
```

### 2. Enhanced Match Validation

The `can_match_with_trust` function now performs comprehensive checks:

```ocaml
let can_match_with_trust engine intent_a intent_b =
  (* 1. Check trading capabilities *)
  let capability_ok = match engine.capability_mgr with
    | Some mgr ->
        Capability.can_trade mgr intent_a.agent_id intent_a.offer_field.resource_type &&
        Capability.can_trade mgr intent_b.agent_id intent_b.offer_field.resource_type
    | None -> true
  in

  (* 2. Get actual reputation scores *)
  let (rep_a, rep_b) = match engine.reputation_mgr with
    | Some mgr ->
        (Reputation.get_reputation mgr intent_a.agent_id,
         Reputation.get_reputation mgr intent_b.agent_id)
    | None -> (0.5, 0.5)
  in

  (* 3. Check collateral sufficiency *)
  let collateral_ok = match engine.collateral_mgr with
    | Some mgr ->
        let required_a = Collateral.calculate_required mgr max_value rep_a rate in
        let required_b = Collateral.calculate_required mgr max_value rep_b rate in
        Collateral.has_sufficient_collateral mgr intent_a.agent_id required_a &&
        Collateral.has_sufficient_collateral mgr intent_b.agent_id required_b
    | None -> true
  in

  (* 4. Verify counterparty requirements *)
  capability_ok && collateral_ok &&
  Intent.meets_counterparty_requirements intent_a intent_b.agent_id rep_b &&
  Intent.meets_counterparty_requirements intent_b intent_a.agent_id rep_a
```

### 3. Reputation-Weighted Quality Scoring

Match quality now incorporates multiple trust factors:

```ocaml
let calculate_match_quality_with_trust engine intent_a intent_b manifold =
  (* Base quality from manifold *)
  let base_quality = calculate_base_quality manifold in

  (* Reputation factor *)
  let reputation_factor = match engine.reputation_mgr with
    | Some mgr ->
        let rep_a = Reputation.get_reputation mgr intent_a.agent_id in
        let rep_b = Reputation.get_reputation mgr intent_b.agent_id in
        (rep_a +. rep_b) /. 2.0
    | None -> 0.5
  in

  (* Historical success rate between agents *)
  let history_factor = match engine.reputation_mgr with
    | Some mgr ->
        let history = Reputation.get_interaction_history mgr
          intent_a.agent_id intent_b.agent_id in
        calculate_success_rate history
    | None -> 0.5
  in

  (* Settlement risk assessment *)
  let risk_factor = calculate_settlement_risk intent_a intent_b in

  (* Weighted combination *)
  base_quality *. 0.4 +.
  reputation_factor *. 0.3 +.
  history_factor *. 0.2 +.
  (1.0 -. risk_factor) *. 0.1
```

### 4. Incremental Matching

New intents are now matched immediately upon arrival:

```ocaml
let match_new_intent engine intent =
  (* Get all active intents */
  let active_intents = State.Queries.get_active_intents engine.state in

  (* Find compatible candidates */
  let candidates = List.filter (fun candidate ->
    candidate.agent_id <> intent.agent_id &&
    Intent.are_compatible intent candidate
  ) active_intents in

  (* Score and rank with trust factors *)
  candidates
  |> List.filter_map (fun candidate ->
      if not (can_match_with_trust engine intent candidate) then None
      else
        compute_settlement_manifold engine.config intent candidate
        |> Option.map (fun manifold ->
            let quality = calculate_match_quality_with_trust engine intent candidate manifold in
            (quality, intent, candidate, manifold)
          )
    )
  |> List.sort by_quality_desc
  |> List.filter above_threshold
  |> List.map create_match
```

### 5. Learning and Feedback Loops

The engine now tracks settlement outcomes for continuous improvement:

```ocaml
let record_settlement_outcome engine match_id success utility =
  (* Update outcome records *)
  engine.match_outcomes <-
    update_outcome match_id success utility engine.match_outcomes;

  (* Update reputation based on outcome *)
  match engine.reputation_mgr with
  | Some mgr ->
      if success then
        Reputation.record_successful_match mgr agent_id
      else
        Reputation.record_failed_match mgr agent_id
  | None -> ()

let analyze_outcomes engine =
  (* Calculate success rate and average utility *)
  let success_rate = calculate_success_rate engine.match_outcomes in
  let avg_utility = calculate_average_utility engine.match_outcomes in

  (* Adapt matching parameters based on performance *)
  if success_rate < 0.3 then
    (* Poor performance - be less selective *)
    engine.config <- { engine.config with
      min_match_quality = engine.config.min_match_quality *. 0.9;
      pareto_samples = min 200 (engine.config.pareto_samples + 20);
    }
  else if success_rate > 0.8 && avg_utility > 0.7 then
    (* Good performance - be more selective *)
    engine.config <- { engine.config with
      min_match_quality = min 0.9 (engine.config.min_match_quality *. 1.1);
    }
```

## Integration Benefits

### Before Integration
- Used placeholder reputation (0.5) for all agents
- No capability checking
- No collateral verification
- No learning from outcomes
- Batch processing only

### After Integration
- Real reputation scores affect match quality
- Trading capabilities enforced
- Collateral requirements checked
- Continuous learning from settlement outcomes
- Incremental matching on intent arrival
- Historical success rates influence future matches
- Risk-aware quality scoring

## Usage Example

```ocaml
(* Create integrated matching engine *)
let reputation_mgr = Reputation.create_manager () in
let capability_mgr = Capability.create_manager () in
let collateral_mgr = Collateral.create_manager () in
let escrow_mgr = Escrow.create_manager () in

let engine = Engine.create
  ~reputation_mgr
  ~capability_mgr
  ~collateral_mgr
  ~escrow_mgr
  state in

(* Process new intent with full trust integration *)
let new_intent = create_intent ~agent_id ~offers ~wants () in
let immediate_matches = Engine.match_new_intent engine new_intent in

(* Record outcome for learning *)
match List.hd_opt immediate_matches with
| Some match_t ->
    execute_settlement match_t |> fun (success, utility) ->
    Engine.record_settlement_outcome engine match_t.match_id success utility
| None -> ()
```

## Future Enhancements

### Network Propagation (Pending)
```ocaml
let propagate_high_quality_matches engine network_engine matches =
  matches
  |> List.filter (fun m -> m.quality > 0.7)
  |> List.iter (fun match_t ->
      let msg = Protocol.create_match_msg engine.node_id match_t in
      Gossip.broadcast network_engine.gossip msg
    )
```

### Settlement Feasibility (Pending)
```ocaml
let check_settlement_feasibility engine intent_a intent_b =
  match engine.escrow_mgr with
  | Some mgr ->
      let escrow_req = calculate_escrow_requirement intent_a intent_b in
      Escrow.can_create_escrow mgr
        ~depositor:intent_a.agent_id
        ~beneficiary:intent_b.agent_id
        ~resources:escrow_req
  | None -> true
```

### Multi-Party Circular Trades (Pending)
```ocaml
let discover_circular_trades engine intents max_length =
  let graph = build_compatibility_graph intents in
  let cycles = find_cycles graph max_length in

  cycles
  |> List.filter_map (fun cycle ->
      if not (check_cycle_feasibility engine cycle) then None
      else
        let manifold = compute_multilateral_manifold cycle in
        let clearing_prices = find_clearing_prices cycle in
        Some (create_circular_match cycle manifold clearing_prices)
    )
```

## Metrics and Monitoring

The integrated engine now tracks:
- Match success rate by reputation tier
- Collateral utilization in matches
- Capability-based rejection rate
- Learning adaptation rate
- Historical performance between agent pairs

## Conclusion

The trust layer integration transforms the matching engine from an isolated mathematical optimizer to a trust-aware, learning system that considers real-world constraints and adapts based on outcomes. This creates a more robust and reliable matching system that aligns with the protocol's trust and settlement requirements.