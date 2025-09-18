# Ambient Commerce Protocol (ACP)

> *Economic fields for autonomous agent interaction*

```
╔══════════════════════════════════════════════════════════════════╗
║                                                                  ║
║   ┌─────┐    ╔═══════╗    ┌─────┐                                ║
║   │Agent│───▶║ INTENT ║◀──│Agent│      Continuous                ║
║   └─────┘    ╚═══╤═══╝    └─────┘     Economic                   ║
║                  │                      Fields                   ║
║                  ▼                                               ║
║            ╔═══════════╗                                         ║
║            ║  MATCHING ║  ← Discovery through field intersection ║
║            ╚═══╤═══╤═══╝                                         ║
║                │   └──────┐                                      ║
║                ▼          ▼                                      ║
║         ╔══════════╗ ╔══════════╗                                ║
║         ║SETTLEMENT║ ║ REVERSAL ║ ← Cryptographic truth          ║
║         ╚══════════╝ ╚══════════╝                                ║
║                                                                  ║
╚══════════════════════════════════════════════════════════════════╝
```

## What is ACP?

The Ambient Commerce Protocol is economic infrastructure for autonomous agents. Unlike traditional commerce systems designed for human interaction, ACP treats economic activity as **continuous fields** rather than discrete events.
 We see **Agent Commerce** as an optimization problem for intent matching. The faster we can compute an event chain: Emit intent field → Continuous matching → Crystallize settlement, the more commerce can occur. 

So agents don't really "shop" - they emit persistent economic fields that automatically intersect with compatible fields, discovering trades through mathematical optimization rather than active search.

## Quick Start

```bash
# Clone the repository
git clone https://github.com/ethanshenley/ambience.git
cd ambience

# Set up OCaml environment (requires OCaml >= 5.1.0)
opam switch create . 5.1.0
eval $(opam env)

# Install dependencies
opam install -y dune yojson ppx_deriving_yojson lwt alcotest \
                cohttp-lwt-unix uri threads unix

# Build the project
dune build

# Run examples
dune exec examples/circular_marketplace/main.exe
dune exec examples/novel_resources/main.exe

# Run tests
dune test
```

## Key Features

### 🌐 Dynamic Resource Registry
Any economic value can be tokenized and traded. The protocol doesn't define what has value; agents do.

```ocaml
(* Traditional resources *)
let usd = create_resource ~uri:"currency:fiat:USD" ~quantity:100.0
let gpu = create_resource ~uri:"compute:gpu:hours" ~quantity:10.0

(* Novel resources - no validation required! *)
let attention = create_resource ~uri:"human:attention:focused:minutes" ~quantity:30.0
let carbon = create_resource ~uri:"carbon:offset:verified:tons" ~quantity:1.0
let reputation = create_resource ~uri:"social:twitter:followers:verified" ~quantity:10000.0
```

### 🔄 Multi-Party Circular Trades
Automatically discovers circular economies where no bilateral trades exist:

```
Alice: Offers GPU → Wants USD
Bob:   Offers USD → Wants ETH
Charlie: Offers ETH → Wants GPU Compute

Result: Circular trade (Alice→Charlie→Bob→Alice) discovered automatically
```

### 📊 Settlement Manifolds
Matches produce multidimensional possibility spaces, not single prices:

```ocaml
type settlement_manifold = {
  pareto_frontier: point list;      (* Optimal trade-offs *)
  nash_solution: point option;       (* Game-theoretic optimum *)
  kalai_smorodinsky: point option;   (* Proportional fairness *)
}
```

### 🤝 Trust Integration
Reputation, capabilities, and collateral create trust without central authority:

- **Reputation**: Multi-dimensional scoring from trade history
- **Capabilities**: Fine-grained permissions for economic actions
- **Collateral**: Economic stake ensuring good behavior

### ⏮️ Reversible Settlements
Settlements can be reversed within time windows, creating tentative finality:

```ocaml
let settlement = execute_trade ~reversal_window:3600.0  (* 1 hour *)

(* If something goes wrong... *)
request_reversal ~reason:NonDelivery ~evidence ~bond
```

## Architecture

```
lib/
├── core/       # Economic primitives (intents, resources, state)
├── matching/   # Discovery engine and settlement manifolds
├── trust/      # Reputation, capabilities, collateral
├── settlement/ # Execution, proofs, reversals
└── network/    # Gossip protocol and peer management

examples/
├── circular_marketplace/  # Multi-party circular trades demo
├── novel_resources/      # Dynamic resource types demo
├── compute_market/       # GPU marketplace
└── energy_trading/       # Renewable energy exchange
```

## Core Concepts

### Intents as Economic Fields

Intents are not orders - they're persistent economic force fields:

```ocaml
let intent = Intent.create
  ~agent_id:"alice"
  ~offers:(compute_field ~quality:0.95)
  ~wants:(currency_field ~range:(10.0, 30.0))
  ~constraints:[TimeWindow (now, now +. 3600.0)]
```

This intent continuously attracts currency and radiates compute availability until satisfied or expired.

### Continuous Discovery

Matching happens continuously in the background:

```ocaml
(* The discovery engine runs perpetually *)
let rec discovery_loop engine =
  let intents = get_active_intents () in
  let matches = discover_compatible_fields intents in
  broadcast_high_quality_matches matches;
  sleep 0.1; discovery_loop engine
```

### Trust Without Authority

Trust emerges from economic behavior:

```ocaml
(* Endorse a new resource type by staking reputation *)
Registry.endorse ~uri:"quantum:qubits:IBM" ~stake:25.0

(* If the resource proves fake, endorsers lose reputation *)
(* If legitimate, they gain reputation from successful trades *)
```

## Examples

### Running the Examples

```bash
# Circular marketplace with trust integration
make run-circular-marketplace

# Novel resource types (social capital, quantum compute, etc.)
dune exec examples/novel_resources/main.exe

# GPU compute marketplace
make run-gpu-provider &
make run-gpu-consumer

# Renewable energy trading
make run-energy-producer &
make run-energy-consumer
```

## Development

### Project Structure

- **`lib/`** - Core protocol implementation
- **`examples/`** - Demonstration applications
- **`test/`** - Comprehensive test suite
- **`docs/`** - Additional documentation
- **`bin/`** - Command-line tools

### Building

```bash
# Development build with watch mode
make watch

# Run tests
dune test

# Build documentation
make doc

# Format code
make format
```

### Testing

The protocol uses deterministic time for reproducible testing:

```ocaml
(* In tests *)
Time_provider.set_mode Mock;
Time_provider.set_time 1000.0;
(* ... run test ... *)
Time_provider.advance 60.0;  (* Jump forward 1 minute *)
```

## Theory

The protocol is based on treating commerce as field interactions rather than discrete transactions. Key papers and concepts:

- [Theory Document](docs/Theory.txt) - Foundational concepts
- [Matching Engine Design](docs/MATCHING_ENGINE_IMPROVEMENTS.md) - Algorithm details
- Settlement manifolds inspired by game theory and mechanism design
- Gossip protocols adapted from epidemic algorithms
- Trust system based on web-of-trust and capability-based security

## Implementation Status

Current implementation: **~90% complete**

✅ Completed:
- Core type system with full serialization
- Intent lifecycle management
- Dynamic resource registry
- Matching engine with trust integration
- Multi-party circular trade discovery
- Settlement execution with proofs
- Reputation and capability systems
- Basic gossip network

🚧 In Progress:
- Manifold gradient optimization
- Advanced negotiation strategies
- Network resilience improvements

## Contributing

There is much to do and I welcome contributions! Areas of particular interest:

1. **Novel resource types** - Create examples with unique economic value
2. **Matching algorithms** - Optimize discovery for specific domains
3. **Settlement mechanisms** - New types of cryptographic proofs
4. **Network protocols** - Alternative propagation strategies

## License

MIT License - See [LICENSE](LICENSE) file for details

## Contact

- GitHub Issues: [Report bugs or request features](https://github.com/ethanshenley/ambience/issues)
- Discussions: [Join the conversation](https://github.com/ethanshenley/ambience/discussions)

---

Built with OCaml 🐫 for the age of autonomous agents 🤖