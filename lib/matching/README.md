# Matching Module - Discovery Through Field Intersection

```
╔══════════════════════════════════════════════════════════════════╗
║                    AMBIENCE MATCHING MODULE                      ║
║              Continuous Discovery & Settlement Spaces            ║
╚══════════════════════════════════════════════════════════════════╝
```

## Theory: Markets as Continuous Fields, Not Discrete Events

Traditional markets work through order books - discrete bids and asks that match at specific price points. This works for humans who make deliberate, point-in-time decisions. But agents exist continuously in economic space.

The matching engine doesn't "search" for matches - it detects field intersections:

```
Traditional: ASK($100) meets BID($100) → TRADE
Agent Field: Offer(GPU, $80-120) ∩ Want(GPU, $90-110) → MANIFOLD($90-110)
```

The intersection isn't a point but a **settlement manifold** - a space of possible trades, each with different optimality properties.

## Module Structure

```
matching/
├── engine.ml       # Core matching algorithm with trust integration
├── manifold.ml     # Settlement space computation
├── discovery.ml    # Continuous background discovery
└── negotiation.ml  # Multi-round settlement negotiation
```

## Key Components

### 1. Matching Engine (`engine.ml`)

The heart of economic field detection:

```ocaml
type t = {
  config: config;
  state: State.t;
  reputation_mgr: Reputation.reputation_manager option;
  capability_mgr: Capability.capability_manager option;
  collateral_mgr: Collateral.collateral_manager option;
  mutable market_conditions: market_conditions;
  mutable match_outcomes: (uuid * bool * float) list;
}

(* Discover compatible intent pairs *)
let discover_bilateral_matches engine intents =
  intents
  |> Intent.Batch.filter_active
  |> find_compatible_pairs
  |> compute_settlement_manifolds
  |> filter_by_trust_requirements
  |> rank_by_optimality
```

**Key Features**:
- **Trust Integration**: Matches filtered by reputation, capabilities, collateral
- **Learning**: Tracks outcome history to improve future matching
- **Market Adaptation**: Adjusts parameters based on liquidity and volatility
- **Multi-party Support**: Discovers circular trades (A→B→C→A)

### 2. Settlement Manifolds (`manifold.ml`)

Each match produces a multidimensional settlement space:

```ocaml
type settlement_manifold = {
  dimensions: string list;           (* price, quantity, time, quality *)
  pareto_frontier: settlement_point list;
  nash_solution: settlement_point option;
  kalai_smorodinsky_solution: settlement_point option;
  core_region: constraint_region option;
}
```

**Optimality Concepts**:

#### Pareto Frontier
Points where no party can improve without making another worse off:
```
P = {s ∈ M | ¬∃s' ∈ M : ∀i U_i(s') ≥ U_i(s) ∧ ∃j U_j(s') > U_j(s)}
```

#### Nash Bargaining Solution
Maximizes the product of surplus utilities:
```
N = argmax_{s ∈ M} ∏(U_i(s) - d_i)
where d_i = disagreement payoff
```

#### Kalai-Smorodinsky Solution
Maintains proportional gains relative to ideal points:
```
KS: (U_1(s) - d_1)/(I_1 - d_1) = (U_2(s) - d_2)/(I_2 - d_2)
where I_i = ideal utility for agent i
```

### 3. Discovery Process (`discovery.ml`)

Continuous background matching:

```ocaml
type discovery_engine = {
  state: State.t;
  engine: Engine.t;
  mutable running: bool;
  config: discovery_config;
}

(* Runs continuously *)
let rec discovery_loop engine =
  let intents = State.get_active_intents engine.state in

  (* Check bilateral matches *)
  let bilateral = Engine.discover_bilateral_matches intents in

  (* Check circular trades *)
  let circular = Engine.discover_multilateral_matches intents in

  (* Broadcast high-quality matches *)
  broadcast_matches (bilateral @ circular);

  Unix.sleepf engine.config.discovery_interval;
  if engine.running then discovery_loop engine
```

### 4. Multi-Party Circular Trades

The engine can discover complex circular economies:

```ocaml
(* Build directed graph of trading relationships *)
type trade_node = {
  intent: intent;
  offers_resource: string;
  wants_resource: string;
}

(* Find cycles in trade graph *)
let find_trade_cycles nodes max_length =
  (* DFS with cycle detection *)
  detect_cycles graph |> filter_valid_cycles |> compute_circular_manifolds

(* Example 3-party cycle:
   Alice: GPU → USD
   Bob: USD → ETH
   Charlie: ETH → GPU
   Creates circular flow with no bilateral match! *)
```

## Market Adaptation

The engine adapts to market conditions:

```ocaml
type market_conditions =
  | HighLiquidity    (* Many compatible intents *)
  | LowLiquidity     (* Few matches possible *)
  | Volatile         (* Rapid price changes *)
  | Stable          (* Consistent patterns *)

let adapt_to_conditions engine = function
  | HighLiquidity ->
      engine.config.min_match_quality <- 0.8;  (* Be selective *)
      engine.config.matching_interval <- 1.0   (* Can be slower *)
  | LowLiquidity ->
      engine.config.min_match_quality <- 0.3;  (* Be inclusive *)
      engine.config.enable_multilateral <- true (* Try harder *)
  | Volatile ->
      engine.config.matching_interval <- 0.1;  (* Match quickly *)
      engine.config.pareto_samples <- 50       (* Lower precision *)
```

## Negotiation Protocol (`negotiation.ml`)

Agents explore the settlement manifold:

```ocaml
type negotiation_state =
  | Proposing          (* Initial proposals *)
  | Exploring          (* Manifold exploration *)
  | Converging         (* Approaching agreement *)
  | Finalizing         (* Last adjustments *)
  | Agreed of point    (* Consensus reached *)
  | Failed of reason   (* No agreement possible *)

(* Agents use different strategies *)
type strategy =
  | Competitive      (* Maximize own utility *)
  | Cooperative      (* Maximize joint utility *)
  | Fair            (* Seek balanced outcomes *)
  | Fast            (* Prioritize quick agreement *)
```

## Usage Example

```ocaml
open Ambience_matching

(* Create matching engine with trust integration *)
let engine = Engine.create
  ~reputation_mgr
  ~capability_mgr
  ~collateral_mgr
  state

(* Discover all compatible matches *)
let matches = Engine.run_matching_round engine

(* Examine a match's settlement space *)
match List.hd matches with
| Some m ->
    Printf.printf "Pareto frontier has %d points\n"
      (List.length m.settlement_space.pareto_frontier);

    (* Get optimal settlement points *)
    let nash = Manifold.nash_bargaining_solution m.settlement_space in
    let ks = Manifold.kalai_smorodinsky_solution m.settlement_space
```

## Performance Optimizations

1. **Spatial Indexing**: Intents organized by resource type for O(log n) lookup
2. **Incremental Matching**: New intents matched against existing pool
3. **Lazy Manifold Computation**: Only computed for promising matches
4. **Parallel Discovery**: Multiple threads explore different intent combinations
5. **Caching**: Recent non-matches cached to avoid recomputation

## Try It Yourself

```bash
# Interactive matching exploration
dune utop lib/matching
```

```ocaml
#require "ambience.matching";;
open Ambience_matching;;

(* Create test intents *)
let alice = create_test_intent ~offers:"GPU" ~wants:"USD";;
let bob = create_test_intent ~offers:"USD" ~wants:"ETH";;
let charlie = create_test_intent ~offers:"ETH" ~wants:"GPU";;

(* Discover circular trade *)
let matches = Engine.discover_multilateral_matches
  Engine.default_config [alice; bob; charlie];;

(* Explore settlement manifold *)
match matches with
| m :: _ ->
    let points = m.settlement_space.pareto_frontier in
    List.iter (fun p ->
      Printf.printf "Price: %.2f, Quantity: %.2f\n" p.price p.quantity
    ) points
```

## Mathematical Foundations

### Match Quality Scoring

```
Q(m) = w₁·U_pareto + w₂·R_trust + w₃·H_history + w₄·(1 - T_risk)

Where:
- U_pareto = Average Pareto optimality
- R_trust = Combined reputation score
- H_history = Historical success rate
- T_risk = Settlement risk factor
```

### Circular Trade Detection

Uses modified Tarjan's algorithm for strongly connected components:
```
For graph G = (V, E) where V = intents, E = compatible trades
Find all simple cycles C where |C| ≥ 3 and Σ balances = 0
```

## Related Modules

- **[Core](../core/)**: Provides intent and resource types
- **[Trust](../trust/)**: Filters matches by reputation
- **[Settlement](../settlement/)**: Executes matched trades
- **[Network](../network/)**: Propagates discovered matches

## Key Insights

1. **Continuous vs Discrete**: Matching happens continuously, not on-demand
2. **Spaces vs Points**: Matches produce possibility spaces, not single prices
3. **Trust Integration**: Reputation directly affects match quality
4. **Learning System**: Past outcomes improve future matching
5. **Circular Economies**: Multi-party trades emerge automatically

---

*"In the space between intention and execution lies a manifold of possibilities."*