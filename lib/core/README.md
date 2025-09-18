# Core Module - Economic Primitives

```
╔══════════════════════════════════════════════════════════════════╗
║                     AMBIENCE CORE MODULE                         ║
║                   Economic Primitives & State                    ║
╚══════════════════════════════════════════════════════════════════╝
```

## Theory: Intents as Persistent Economic Fields

Traditional commerce operates through discrete transactions - you place an order, it executes, done. Agent commerce is fundamentally different. Agents broadcast **intents** - persistent declarations of economic desire that create standing waves in economic space.

Think of it like this:
- **Human commerce**: "I want to buy X for Y" (point event)
- **Agent commerce**: "I continuously emit a field that attracts X and repels up to Y" (persistent field)

These intent fields interact continuously, discovering matches not through active search but through field intersection. When fields overlap constructively, economic value crystallizes.

## Module Structure

```
core/
├── types.ml         # Core type definitions
├── intent.ml        # Intent lifecycle management
├── resource.ml      # Resource field operations
├── registry.ml      # Dynamic resource registry
├── state.ml         # Global protocol state
└── time_provider.ml # Deterministic time abstraction
```

## Key Components

### 1. Types (`types.ml`)

Defines the fundamental economic primitives:

```ocaml
type intent = {
  intent_id: uuid;
  agent_id: public_key;
  offer_field: resource_field;    (* What I'm radiating *)
  want_field: resource_field;     (* What I'm attracting *)
  constraints: constraint_t list;  (* Boundary conditions *)
  lifecycle: intent_lifecycle;     (* Birth → Death states *)
  ...
}
```

**Key Insight**: Intents aren't orders - they're economic force fields that persist until satisfied or expired.

### 2. Intent (`intent.ml`)

Manages the intent lifecycle:

```ocaml
(* Create an intent field *)
let my_intent = Intent.create
  ~agent_id:"alice"
  ~offers:(gpu_compute_field)
  ~wants:(currency_field)
  ~constraints:[TimeWindow (now, now +. 3600.)]
  ()

(* Intents evolve through states *)
Created → Broadcast → Discovered → Matched → Settled → Completed
```

**Lifecycle States**:
- `Created`: Local intent, not yet broadcast
- `Broadcast`: Propagating through network
- `Discovered`: Found compatible match candidates
- `Matched`: Negotiating settlement terms
- `Settled`: Executing atomic swap
- `Completed`: Successfully terminated

### 3. Resource (`resource.ml`)

Resources are now **completely dynamic** - any URI is valid:

```ocaml
(* Traditional resources *)
let usd = create_resource ~uri:"currency:fiat:USD" ~quantity:100.0
let btc = create_resource ~uri:"currency:crypto:BTC" ~quantity:0.1

(* Novel resources - no validation! *)
let karma = create_resource ~uri:"social:reddit:karma" ~quantity:1000.0
let attention = create_resource ~uri:"human:attention:minutes" ~quantity:30.0
```

**Design Philosophy**: The protocol doesn't decide what has value - agents do.

### 4. Registry (`registry.ml`)

The dynamic registry discovers and tracks resource types:

```ocaml
(* Auto-discovery when agents use new resources *)
Registry.discover registry
  ~uri:"quantum:compute:qubits:IBM"
  ~agent_id:"researcher"
  ~description:"IBM quantum computing time"

(* Community endorsement builds trust *)
Registry.endorse registry
  ~uri:"quantum:compute:qubits:IBM"
  ~agent_id:"mit_lab"
  ~stake:10.0  (* Reputation points at risk *)

(* Query resource credibility *)
let credibility = Registry.get_credibility_score registry uri
```

**Key Features**:
- Permissionless resource creation
- Reputation-based endorsements
- Usage tracking and analytics
- Similarity detection

### 5. State (`state.ml`)

Manages the global protocol state:

```ocaml
type t = {
  intents: IntentPool.t;
  matches: MatchRegistry.t;
  settlements: SettlementLog.t;
  agents: AgentRegistry.t;
}

(* State transitions are atomic *)
State.Transitions.post_intent state intent
State.Transitions.record_match state match
State.Transitions.execute_settlement state settlement
```

### 6. Time Provider (`time_provider.ml`)

Abstracts time for deterministic testing:

```ocaml
(* Production: real time *)
Time_provider.set_mode Real
let now = Time_provider.now ()  (* Unix.time() *)

(* Testing: deterministic time *)
Time_provider.set_mode Mock
Time_provider.set_time 1000.0
Time_provider.advance 60.0  (* Jump forward 1 minute *)
```

## Usage Example

```ocaml
open Ambience_core

(* Initialize protocol *)
let state = State.create ()
let registry = Resource.get_registry ()

(* Create a novel resource type *)
let my_resource = Resource.create_field
  ~resource_type:"compute:quantum:simulator:GPU"
  ~min_quantity:100.0
  ~max_quantity:1000.0
  ~quality:(Graded 0.95)
  ~metadata:[("provider", "nvidia")]

(* Create an intent *)
let intent = Intent.create
  ~agent_id:"quantum_researcher"
  ~offers:my_resource
  ~wants:(Resource.create_field
    ~resource_type:"currency:crypto:ETH"
    ~min_quantity:0.5
    ~max_quantity:2.0
    ~quality:Fungible
    ~metadata:[])
  ~constraints:[
    PriceRange (0.001, 0.01);
    TimeWindow (now, now +. 86400.0);
  ]
  ()

(* Post to global state *)
State.Transitions.post_intent state intent
```

## Mathematical Foundation

### Settlement Manifolds

Each match produces not a single price but a **settlement manifold** - a multidimensional space of valid settlements:

```
M = {(p, q, t, μ) | C(p,q,t,μ) ∧ P(p,q,t,μ)}

Where:
- p = price vector
- q = quantity vector
- t = execution time
- μ = quality parameters
- C = constraint satisfaction
- P = Pareto optimality condition
```

### Intent Field Interaction

Intents create economic fields that interact:

```
F_total = Σ F_i(x, t)

Where each intent contributes:
F_i = A_i × exp(-||x - x_i||² / σ_i²) × exp(-(t - t_i) / τ_i)
```

## Key Concepts

1. **Dynamic Resources**: Any string can be a resource - no central registry
2. **Intent Persistence**: Intents exist until satisfied, not just at creation time
3. **Field-Based Discovery**: Matches emerge from field overlap, not active search
4. **Deterministic Time**: All time operations go through provider for testing
5. **Atomic State Transitions**: State changes are transactional and reversible

## Try It Yourself

```bash
# Run the intent creation example
dune utop lib/core
```

```ocaml
#require "ambience.core";;
open Ambience_core;;

(* Create your own resource type! *)
let my_resource_uri = "magic:unicorn:tears:grade-a";;
let field = Resource.create_field
  ~resource_type:my_resource_uri
  ~min_quantity:1.0
  ~max_quantity:10.0
  ~quality:Fungible
  ~metadata:[];;

(* Check if it was auto-discovered *)
Registry.exists (Resource.get_registry()) my_resource_uri;;
```

## Related Modules

- **[Matching](../matching/)**: Discovers compatible intents
- **[Trust](../trust/)**: Reputation and capabilities
- **[Settlement](../settlement/)**: Executes matched intents
- **[Network](../network/)**: Propagates intents via gossip

## Design Principles

1. **Unopinionated**: Protocol doesn't validate resource types
2. **Continuous**: Intents persist and evolve over time
3. **Composable**: Outputs of one intent can be inputs to another
4. **Deterministic**: All operations must be reproducible
5. **Decentralized**: No central authority or registry

---

*"In agent commerce, transactions don't happen - they precipitate from supersaturated economic fields."*