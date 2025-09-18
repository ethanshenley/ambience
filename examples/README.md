# Ambient Commerce Protocol Examples

This directory contains example applications demonstrating the Ambient Commerce Protocol in various real-world scenarios.

## Examples

### 1. Circular Marketplace (`circular_marketplace/`)

**Comprehensive demonstration of all protocol features:**
- Multi-party circular trades (3-party and 4-party examples)
- Trust layer integration with reputation and collateral
- Market condition detection and adaptation
- Settlement with learning feedback
- Energy/Compute/Currency circular economy

**Run it:**
```bash
cd circular_marketplace
dune exec ./main.exe
```

**Key Features Demonstrated:**
- Alice → Charlie → Bob → Alice circular trade with GPUs, USD, and ETH
- Trust-aware matching that blocks untrusted traders
- Dynamic parameter adjustment based on market liquidity
- Learning from settlement outcomes to improve future matching
- 4-party circle: Solar Energy → USD → ETH → GPU Compute → Solar Energy

### 2. Compute Market (`compute_market/`)

**GPU compute marketplace with bilateral trading:**
- GPU providers offering compute hours
- Consumers requesting specific GPU models
- Price discovery through continuous matching
- Quality-based resource matching

**Run it:**
```bash
cd compute_market
dune exec ./main.exe
```

### 3. Energy Trading (`energy_trading/`)

**Decentralized energy grid trading:**
- Solar/wind producers with excess capacity
- Consumers with energy demands
- Time-window constrained matching
- Location-aware settlement

**Run it:**
```bash
cd energy_trading
dune exec ./main.exe
```

## Protocol Features

Each example showcases different aspects of the protocol:

| Feature | Circular Marketplace | Compute Market | Energy Trading |
|---------|---------------------|----------------|----------------|
| Bilateral Matching | ✓ | ✓ | ✓ |
| Circular Trades | ✓ | - | - |
| Trust Integration | ✓ | Partial | - |
| Market Adaptation | ✓ | - | - |
| Settlement Learning | ✓ | - | - |
| Time Constraints | - | ✓ | ✓ |
| Location Constraints | - | - | ✓ |
| Quality Matching | ✓ | ✓ | - |
| Gossip Network | ✓ | ✓ | ✓ |

## Building and Running

All examples follow the same build pattern:

```bash
# Build all examples
dune build examples/

# Run a specific example
dune exec examples/circular_marketplace/main.exe

# Or navigate to the example directory
cd examples/circular_marketplace
dune exec ./main.exe
```

## Architecture

Each example follows a similar structure:
1. **Initialize Protocol Stack** - Set up state, trust layer, matching engine, and network
2. **Create Agents** - Define participants with intents, reputation, and capabilities
3. **Post Intents** - Agents broadcast their offers and wants
4. **Discovery** - Matching engine finds compatible trades (bilateral or circular)
5. **Settlement** - Execute trades with cryptographic proofs
6. **Learning** - System adapts based on outcomes

## Customization

You can modify the examples to test different scenarios:
- Adjust resource quantities and types
- Change trust parameters (reputation thresholds, collateral requirements)
- Modify market conditions (number of participants, matching intervals)
- Add new resource types to the ontology
- Implement custom constraints

## Key Concepts Demonstrated

### Intents as Economic Fields
Unlike traditional order books, intents create persistent fields that continuously interact, discovering matches as new intents arrive.

### Settlement Manifolds
Each match produces a multi-dimensional space of possible settlements, allowing agents to negotiate within the Pareto-optimal frontier.

### Trust-Aware Matching
The system respects trust boundaries, requiring sufficient reputation, capabilities, and collateral before allowing matches.

### Circular Trade Discovery
The protocol automatically discovers multi-party circular trades where no bilateral match exists, enabling complex economic cycles.

### Adaptive Market Making
The matching engine adapts its parameters based on detected market conditions (high/low liquidity, volatility).

## Next Steps

To create your own example:
1. Copy one of the existing examples as a template
2. Modify the resource types and agent behaviors
3. Add custom constraints or matching logic
4. Test with different market conditions

The protocol is designed to be extensible - you can add new resource types, trust models, and settlement mechanisms as needed for your use case.