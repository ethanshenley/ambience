# Trust Module - Reputation as Economic Infrastructure

```
╔══════════════════════════════════════════════════════════════════╗
║                     AMBIENCE TRUST MODULE                        ║
║           Reputation, Capabilities & Economic Security           ║
╚══════════════════════════════════════════════════════════════════╝
```

## Theory: Trust Without Central Authority

In human commerce, we trust institutions - banks, governments, platforms. They vouch for participants and mediate disputes. Agent commerce can't rely on these centralized trust anchors. Instead, trust emerges from:

1. **Reputation as Currency**: Past behavior predicts future reliability
2. **Capabilities as Permissions**: Provable abilities to perform actions
3. **Collateral as Commitment**: Economic stake that aligns incentives

This creates a **trust field** where agents with higher reputation attract more trades, creating a positive feedback loop for honest behavior.

## Module Structure

```
trust/
├── reputation.ml   # Multi-dimensional reputation tracking
├── capability.ml   # Capability-based authorization
└── collateral.ml   # Economic security through staking
```

## Key Components

### 1. Reputation System (`reputation.ml`)

Multi-dimensional reputation tracking:

```ocaml
type reputation = {
  agent_id: public_key;
  dimensions: (dimension, score) Hashtbl.t;
  overall_score: float;
  interactions: interaction list;
  trusted_by: (public_key * float) list;
  trusts: (public_key * float) list;
}

type dimension =
  | Reliability      (* Completes trades as promised *)
  | Timeliness      (* Executes within time windows *)
  | Quality         (* Delivers expected quality *)
  | Disputes        (* Fair in dispute resolution *)
  | Innovation      (* Introduces valuable resources *)
```

**Reputation Dynamics**:

```
R(t+1) = α·R(t) + (1-α)·O(t)

Where:
- R(t) = reputation at time t
- O(t) = outcome of latest interaction
- α = decay factor (reputation fades without activity)
```

**Web of Trust**:
```ocaml
(* Trust propagates through the network *)
let propagated_trust source target =
  let direct = get_direct_trust source target in
  let indirect =
    get_mutual_connections source target
    |> List.map (fun intermediate ->
        get_trust source intermediate *.
        get_trust intermediate target *.
        0.5  (* Decay factor for indirection *)
      )
    |> List.fold_left max 0.0
  in
  max direct indirect
```

### 2. Capability System (`capability.ml`)

Fine-grained permissions for economic actions:

```ocaml
type capability = {
  capability_id: uuid;
  agent_id: public_key;
  capability_type: capability_type;
  granted_by: public_key;
  granted_at: timestamp;
  expires_at: timestamp option;
  constraints: capability_constraint list;
  proof: capability_proof option;
}

type capability_type =
  | TradeResource of resource_uri
  | SettleAmount of float * currency
  | AccessMarket of market_id
  | CreateResource of string  (* Pattern for new resources *)
  | EndorseAgent
  | SlashCollateral

(* Hierarchical capabilities *)
let implies cap1 cap2 =
  match cap1, cap2 with
  | TradeResource "currency:*", TradeResource "currency:fiat:USD" -> true
  | SettleAmount (limit1, _), SettleAmount (limit2, _) when limit1 >= limit2 -> true
  | _ -> false
```

**Capability Delegation**:
```ocaml
(* Agents can delegate limited capabilities *)
let delegate_capability original_cap ~to_agent ~constraints ~duration =
  if has_delegation_right original_cap then
    create_derived_capability
      ~parent:original_cap
      ~agent:to_agent
      ~constraints:(original_cap.constraints @ constraints)
      ~expires:(min original_cap.expires_at (now() +. duration))
  else
    Error "Cannot delegate this capability"
```

### 3. Collateral Management (`collateral.ml`)

Economic security through staking:

```ocaml
type collateral_stake = {
  agent_id: public_key;
  amount: float;
  currency: resource_uri;
  locked_until: timestamp option;
  locked_for: uuid option;  (* Intent or settlement ID *)
  slashable: bool;
  slashing_conditions: slashing_condition list;
}

type slashing_condition =
  | OnDefault        (* Failed to deliver *)
  | OnDispute        (* Lost dispute resolution *)
  | OnInactivity of float  (* Days without trades *)
  | OnViolation of string  (* Custom condition *)
```

**Progressive Collateral Requirements**:
```ocaml
let required_collateral intent reputation =
  let base_requirement = intent.value *. 0.1 in  (* 10% base *)

  (* Adjust based on reputation *)
  let reputation_multiplier =
    if reputation > 0.9 then 0.5      (* High rep: 50% reduction *)
    else if reputation > 0.7 then 0.75 (* Good rep: 25% reduction *)
    else if reputation < 0.3 then 2.0  (* Low rep: 200% requirement *)
    else 1.0
  in

  (* Adjust based on resource novelty *)
  let resource_multiplier =
    let credibility = Registry.get_credibility_score intent.resource in
    if credibility < 0.3 then 1.5  (* Unproven resource: 50% increase *)
    else 1.0
  in

  base_requirement *. reputation_multiplier *. resource_multiplier
```

## Trust Integration with Matching

Trust directly affects matching:

```ocaml
(* Filter matches by trust requirements *)
let filter_by_trust matches reputation_mgr capability_mgr collateral_mgr =
  matches |> List.filter (fun match_t ->
    let agents = get_match_agents match_t in

    (* Check reputation thresholds *)
    let reputation_ok = List.for_all (fun agent ->
      Reputation.get_score reputation_mgr agent >= 0.5
    ) agents in

    (* Check required capabilities *)
    let capabilities_ok = List.for_all (fun agent ->
      has_required_capabilities capability_mgr agent match_t.resources
    ) agents in

    (* Check collateral coverage *)
    let collateral_ok = List.for_all (fun agent ->
      has_sufficient_collateral collateral_mgr agent match_t.value
    ) agents in

    reputation_ok && capabilities_ok && collateral_ok
  )
```

## Reputation Staking in Registry

Agents stake reputation when endorsing new resource types:

```ocaml
(* Endorse a novel resource type *)
Registry.endorse registry
  ~uri:"social:tiktok:followers"
  ~agent_id:"influencer_agency"
  ~stake:25.0  (* Risk 25 reputation points *)
  ~notes:"Successfully traded this resource type"

(* If resource proves fake, endorsers lose reputation *)
on_resource_invalidated uri ->
  get_endorsers uri |> List.iter (fun endorser ->
    Reputation.apply_penalty reputation_mgr
      ~agent:endorser.agent_id
      ~amount:endorser.stake
      ~reason:"Endorsed invalid resource"
  )
```

## Usage Example

```ocaml
open Ambience_trust

(* Initialize trust system *)
let reputation_mgr = Reputation.create_manager ()
let capability_mgr = Capability.create_manager ()
let collateral_mgr = Collateral.create_manager ()

(* New agent joins network *)
let alice = "alice_pubkey"

(* Build reputation through successful trades *)
Reputation.record_interaction reputation_mgr
  ~agent:alice
  ~counterparty:"bob"
  ~outcome:Success
  ~value:100.0

(* Grant trading capability *)
Capability.grant capability_mgr
  ~agent:alice
  ~capability_type:(TradeResource "currency:crypto:*")
  ~granted_by:"dao_governance"
  ~duration:86400.0

(* Lock collateral for high-value trade *)
Collateral.lock collateral_mgr
  ~agent:alice
  ~amount:1000.0
  ~currency:"currency:crypto:USDC"
  ~for_intent:intent_id
  ~conditions:[OnDefault; OnDispute]
```

## Trust Metrics

### Reputation Score Calculation

```
R = Σ(w_i × d_i) / Σw_i

Where:
- d_i = dimension score (0-1)
- w_i = dimension weight

Dimensions: Reliability, Timeliness, Quality, Dispute Resolution, Innovation
```

### Trust Network Analysis

```
PageRank-style trust propagation:
T(i) = (1-d) + d × Σ(T(j) × W(j,i))

Where:
- T(i) = trust score of agent i
- d = damping factor (0.85)
- W(j,i) = trust weight from j to i
```

## Try It Yourself

```bash
dune utop lib/trust
```

```ocaml
#require "ambience.trust";;
open Ambience_trust;;

(* Simulate reputation building *)
let mgr = Reputation.create_manager ();;
let agent = "test_agent";;

(* Start with neutral reputation *)
Reputation.get_score mgr agent;;  (* 0.5 *)

(* Successful trades improve reputation *)
for i = 1 to 10 do
  Reputation.record_interaction mgr
    ~agent ~outcome:Success ~value:100.0
done;;

Reputation.get_score mgr agent;;  (* ~0.8 *)

(* Failed trade hurts more than success helps *)
Reputation.record_interaction mgr
  ~agent ~outcome:(Failure "Did not deliver") ~value:100.0;;

Reputation.get_score mgr agent;;  (* ~0.65 *)
```

## Security Considerations

1. **Sybil Resistance**: New identities start with low reputation
2. **Reputation Farming**: Decay ensures continuous good behavior
3. **Collusion**: Web of trust considers network topology
4. **Capability Escalation**: Hierarchical permissions with delegation limits
5. **Collateral Attacks**: Progressive requirements based on history

## Related Modules

- **[Core](../core/)**: Uses reputation in resource registry
- **[Matching](../matching/)**: Filters matches by trust level
- **[Settlement](../settlement/)**: Enforces collateral slashing
- **[Network](../network/)**: Propagates reputation updates

## Key Insights

1. **Multi-dimensional**: Reputation isn't scalar - different aspects matter
2. **Transitive Trust**: Trust propagates through the network
3. **Economic Security**: Collateral aligns incentives
4. **Capability-Based**: Fine-grained permissions, not binary access
5. **Emergent**: No central authority defines trustworthiness

---

*"Trust is not granted by authority but earned through consistent economic behavior."*