# Matching Engine Documentation

## Overview

The Matching Engine is the core component of the Ambient Commerce Protocol responsible for discovering compatible intents and computing optimal settlements. It implements sophisticated algorithms for bilateral and multilateral matching, leveraging game-theoretic solution concepts to find mutually beneficial trades.

## Architecture

### Pipeline Stages

1. **Discovery Phase**
   - Continuously scans active intents for compatibility
   - Uses indexing and caching for O(n log n) performance
   - Filters by resource type, price range, and time windows

2. **Manifold Computation**
   - Calculates the multi-dimensional settlement space
   - Dimensions: price, quantity, time, quality
   - Identifies feasible region based on combined constraints

3. **Optimization**
   - Finds Pareto-optimal points using dominance checking
   - Eliminates dominated solutions
   - Builds the Pareto frontier

4. **Solution Selection**
   - Applies game-theoretic solution concepts
   - Nash Bargaining: maximizes product of surplus utilities
   - Kalai-Smorodinsky: maintains proportional utilities
   - Egalitarian: maximizes minimum utility (maximin)

5. **Quality Scoring**
   - Ranks matches by multiple quality metrics
   - Considers feasible region size, utility scores, urgency

## Key Algorithms

### Bilateral Matching

```ocaml
(* Core bilateral matching algorithm *)
let discover_bilateral_matches config intents =
  let matches = ref [] in

  (* Group intents by resource type for efficiency *)
  let by_resource = Intent.Batch.group_by_resource intents in

  Hashtbl.iter (fun resource_type intent_list ->
    (* Find all compatible pairs *)
    let pairs = Intent.Batch.find_compatible_pairs intent_list in

    List.iter (fun (intent_a, intent_b) ->
      (* Compute settlement manifold *)
      match compute_settlement_manifold config intent_a intent_b with
      | Some manifold when List.length manifold.pareto_frontier > 0 ->
          let quality = calculate_match_quality intent_a intent_b manifold in
          if quality >= config.min_match_quality then
            matches := (create_match intent_a intent_b manifold) :: !matches
      | _ -> ()
    ) pairs
  ) by_resource;

  (* Sort by quality *)
  List.sort (fun a b -> compare b.quality a.quality) !matches
```

### Settlement Manifold Computation

The settlement manifold represents all possible ways two intents can settle:

```ocaml
let compute_settlement_manifold config intent_a intent_b =
  (* Extract feasible ranges *)
  let price_range = compute_price_range intent_a intent_b in
  let quantity_range = compute_quantity_range intent_a intent_b in
  let time_window = compute_time_window intent_a intent_b in

  (* Sample the multi-dimensional space *)
  let sample_points = sample_settlement_space
    ~price_range
    ~quantity_range
    ~time_window
    ~samples:config.pareto_samples in

  (* Find Pareto-optimal points *)
  let pareto_frontier = find_pareto_frontier intent_a intent_b sample_points in

  (* Create manifold with optimality scores *)
  {
    dimensions = ["price"; "quantity"; "time"];
    valid_region_constraints = intent_a.constraints @ intent_b.constraints;
    pareto_frontier = pareto_frontier;
    optimality_scores = calculate_scores pareto_frontier;
  }
```

### Nash Bargaining Solution

Finds the point that maximizes the product of surplus utilities:

```ocaml
let find_nash_solution manifold intent_a intent_b =
  let reservation_a = 0.0 in  (* No-trade utility *)
  let reservation_b = 0.0 in

  List.fold_left (fun best point ->
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
        else best
  ) None manifold.pareto_frontier
  |> Option.map fst
```

## Configuration

The matching engine behavior can be tuned via configuration:

```ocaml
type config = {
  matching_interval: float;          (* Seconds between rounds *)
  max_intents_per_round: int;        (* Processing limit *)
  pareto_samples: int;                (* Manifold resolution *)
  min_match_quality: float;           (* Quality threshold 0-1 *)
  enable_multilateral: bool;          (* Enable 3+ party trades *)
  max_settlement_dimensions: int;     (* Dimension limit *)
}

let default_config = {
  matching_interval = 1.0;
  max_intents_per_round = 1000;
  pareto_samples = 100;
  min_match_quality = 0.3;
  enable_multilateral = false;
  max_settlement_dimensions = 4;
}
```

## Performance Characteristics

- **Bilateral Matching**: O(n²) worst case, O(n log n) with indexing
- **Manifold Computation**: O(s × d) where s = samples, d = dimensions
- **Pareto Finding**: O(s² × d) for s sample points in d dimensions
- **Memory Usage**: O(n + s × m) for n intents, s samples, m matches

## Quality Metrics

Match quality is calculated based on:

1. **Feasible Region Size**: Larger regions offer more flexibility
2. **Pareto Frontier Size**: More Pareto points mean more options
3. **Average Utility**: Higher utilities indicate better matches
4. **Constraint Margins**: How well constraints are satisfied
5. **Time Urgency**: Matches expiring soon get priority

## Multi-party Matching (Future)

The engine will support circular trades like A→B→C→A:

```ocaml
let discover_multilateral_matches config intents =
  (* Build directed graph of compatible trades *)
  let graph = build_compatibility_graph intents in

  (* Find cycles in the graph *)
  let cycles = find_cycles graph ~max_length:config.max_cycle_length in

  (* Verify cycle feasibility *)
  List.filter_map (fun cycle ->
    compute_cycle_settlement cycle
  ) cycles
```

## Testing

The matching engine includes comprehensive tests:

- **Unit Tests**: Test individual functions like utility calculation
- **Integration Tests**: Test full matching pipeline
- **Property Tests**: Verify invariants (Pareto optimality, etc.)
- **Performance Tests**: Benchmark with various intent counts
- **Fuzz Tests**: Random intent generation to find edge cases

## Current Implementation Status

### Completed (100%)
- ✅ Bilateral matching algorithm
- ✅ Settlement manifold computation
- ✅ Pareto frontier finding
- ✅ Nash bargaining solution
- ✅ Kalai-Smorodinsky solution
- ✅ Quality scoring
- ✅ Batch operations

### In Progress (20%)
- 🚧 Multi-party matching
- 🚧 Advanced indexing structures
- 🚧 Incremental matching
- 🚧 Parallel processing

### Not Started (0%)
- ❌ Machine learning quality prediction
- ❌ Historical match analysis
- ❌ Automated parameter tuning

## Usage Examples

### Basic Usage

```ocaml
(* Create and start engine *)
let engine = Engine.create state in
Engine.start engine;

(* Manual match discovery *)
let intents = State.Queries.get_active_intents state in
let matches = Engine.discover_bilateral_matches
  Engine.default_config intents in

(* Process best match *)
match List.hd_opt matches with
| Some best_match ->
    execute_settlement best_match
| None ->
    (* No matches found *)
```

### Advanced Usage

```ocaml
(* Custom configuration *)
let config = {
  Engine.default_config with
  matching_interval = 0.5;      (* Faster matching *)
  pareto_samples = 1000;        (* Higher resolution *)
  min_match_quality = 0.5;      (* More selective *)
} in

(* Compute specific manifold *)
match Engine.compute_settlement_manifold config intent_a intent_b with
| Some manifold ->
    (* Find optimal solutions *)
    let nash = Engine.find_nash_solution manifold intent_a intent_b in
    let ks = Engine.find_kalai_smorodinsky_solution manifold intent_a intent_b in

    (* Choose based on fairness criteria *)
    let chosen = match nash, ks with
      | Some n, Some k when fair_score n > fair_score k -> n
      | _, Some k -> k
      | Some n, _ -> n
      | _ -> failwith "No solution found"
    in

    execute_at_point chosen
| None ->
    (* Intents incompatible *)
```

## Future Enhancements

1. **Machine Learning Integration**
   - Learn optimal matching parameters from historical data
   - Predict match success probability
   - Personalized utility functions

2. **Advanced Algorithms**
   - Stable marriage for two-sided markets
   - Hungarian algorithm for optimal assignment
   - Auction mechanisms for price discovery

3. **Performance Optimizations**
   - GPU acceleration for manifold computation
   - Distributed matching across multiple nodes
   - Incremental Pareto frontier updates

4. **Enhanced Features**
   - Time-decay preferences
   - Reputation-weighted matching
   - Cross-chain settlement support