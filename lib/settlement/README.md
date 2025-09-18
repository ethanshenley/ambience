# Settlement Module - Deterministic Economic Crystallization

```
╔══════════════════════════════════════════════════════════════════╗
║                   AMBIENCE SETTLEMENT MODULE                     ║
║         Atomic Execution, Cryptographic Proofs & Reversals       ║
╚══════════════════════════════════════════════════════════════════╝
```

## Theory: From Possibility to Reality

The matching engine discovers **possibility spaces** - manifolds where trades could occur. The settlement module **crystallizes** these possibilities into reality through deterministic execution. But unlike traditional settlement which is final, agent settlement can be **reversible** within time windows, creating a new economic primitive: tentative finality.

Think of it as quantum collapse:
- **Before**: Superposition of possible settlements
- **During**: Measurement collapses to specific state
- **After**: State is real but can tunnel back within time window

## Module Structure

```
settlement/
├── executor.ml    # Deterministic settlement execution
├── escrow.ml      # Multi-party escrow management
├── proof.ml       # Cryptographic proof generation
└── reversal.ml    # Time-windowed settlement reversal
```

## Key Components

### 1. Settlement Executor (`executor.ml`)

Deterministic execution engine:

```ocaml
type settlement = {
  settlement_id: uuid;
  match_id: uuid;
  participants: participant list;
  transfers: resource_transfer list;
  executed_at: timestamp;
  state_root: hash;
  proof: settlement_proof;
  reversal_window: float;
  status: settlement_status;
}

type settlement_status =
  | Pending           (* Awaiting execution *)
  | Executed          (* Successfully completed *)
  | Reversed          (* Unwound within window *)
  | Finalized         (* Past reversal window *)
  | Failed of string  (* Execution failed *)

(* Atomic execution *)
let execute_settlement settlement escrow_mgr =
  (* Begin atomic transaction *)
  let tx = Transaction.begin () in

  try
    (* Lock all resources *)
    List.iter (lock_resource escrow_mgr) settlement.transfers;

    (* Validate pre-conditions *)
    validate_balances settlement;
    validate_capabilities settlement;
    validate_constraints settlement;

    (* Execute transfers atomically *)
    List.iter (execute_transfer tx) settlement.transfers;

    (* Generate proof *)
    let proof = Proof.generate_settlement_proof settlement tx in

    (* Commit transaction *)
    Transaction.commit tx;
    Ok { settlement with status = Executed; proof }

  with e ->
    Transaction.rollback tx;
    Error (Failed e.message)
```

**Determinism Requirements**:
- Same inputs → same outputs
- No external dependencies during execution
- All randomness from agreed seeds
- Floating point via fixed-point arithmetic

### 2. Escrow Management (`escrow.ml`)

Multi-party resource locking:

```ocaml
type escrow_account = {
  escrow_id: uuid;
  settlement_id: uuid;
  participants: public_key list;
  deposits: resource_deposit list;
  release_conditions: release_condition list;
  timeout: timestamp;
  status: escrow_status;
}

type release_condition =
  | AllSignatures       (* All parties must sign *)
  | MajoritySignatures  (* >50% must sign *)
  | TimeoutRelease      (* Auto-release after timeout *)
  | ProofSubmission     (* Valid proof triggers release *)
  | OracleConfirmation  (* External oracle validates *)

(* Multi-party escrow flow *)
let create_escrow ~participants ~resources ~conditions =
  let escrow = {
    escrow_id = generate_uuid ();
    participants;
    deposits = [];
    release_conditions = conditions;
    timeout = Time_provider.now () +. 3600.0;
    status = AwaitingDeposits;
  } in

  (* Notify participants to deposit *)
  List.iter (notify_deposit_required escrow) participants;
  escrow

(* Atomic release *)
let release_escrow escrow signatures =
  if validate_release_conditions escrow signatures then
    (* Distribute resources according to settlement *)
    List.iter (transfer_to_recipient escrow) escrow.deposits;
    { escrow with status = Released }
  else
    Error "Release conditions not met"
```

### 3. Cryptographic Proofs (`proof.ml`)

Verifiable settlement evidence:

```ocaml
type settlement_proof = {
  proof_type: proof_type;
  state_transition: state_transition;
  merkle_root: hash;
  witness_data: witness list;
  signature: multi_signature;
}

type proof_type =
  | SimpleTransfer      (* Basic A→B transfer *)
  | MultiParty          (* Complex multi-party *)
  | Circular            (* Circular trade proof *)
  | Conditional         (* Constraint satisfaction *)

(* Generate Merkle proof of state transition *)
let generate_merkle_proof settlement =
  let leaves = settlement.transfers |> List.map (fun transfer ->
    hash_transfer transfer
  ) in

  let tree = MerkleTree.build leaves in
  {
    root = MerkleTree.root tree;
    inclusion_proofs = List.map (MerkleTree.proof tree) leaves;
  }

(* Verify settlement independently *)
let verify_settlement_proof settlement proof =
  (* Recompute state transition *)
  let expected_transition = compute_state_transition settlement in

  (* Verify Merkle tree *)
  let merkle_valid = MerkleTree.verify proof.merkle_root proof.inclusion_proofs in

  (* Verify signatures *)
  let signatures_valid = List.for_all (verify_signature settlement) proof.signatures in

  (* Verify constraint satisfaction *)
  let constraints_valid = List.for_all (evaluate_constraint settlement) settlement.constraints in

  merkle_valid && signatures_valid && constraints_valid
```

### 4. Settlement Reversal (`reversal.ml`)

Time-windowed settlement unwinding:

```ocaml
type reversal_request = {
  settlement_id: uuid;
  requester: public_key;
  reason: reversal_reason;
  evidence: evidence list;
  bond: collateral_stake;
  submitted_at: timestamp;
}

type reversal_reason =
  | NonDelivery         (* Resource not delivered as promised *)
  | QualityDispute      (* Resource quality below specification *)
  | ConstraintViolation (* Settlement violated constraints *)
  | MutualAgreement     (* All parties agree to reverse *)
  | FraudProof          (* Cryptographic proof of fraud *)

(* Reversal with economic consequences *)
let request_reversal ~settlement ~reason ~evidence ~bond =
  let now = Time_provider.now () in

  (* Check if within reversal window *)
  if now > settlement.executed_at +. settlement.reversal_window then
    Error "Reversal window expired"

  (* Require bond to prevent griefing *)
  else if bond.amount < settlement.value *. 0.1 then
    Error "Insufficient reversal bond"

  else
    let request = {
      settlement_id = settlement.id;
      requester = caller;
      reason;
      evidence;
      bond;
      submitted_at = now;
    } in

    (* Notify all parties *)
    broadcast_reversal_request request;

    (* Start dispute timer *)
    schedule_reversal_decision request (now +. 3600.0);
    Ok request

(* Execute reversal if approved *)
let execute_reversal settlement request =
  (* Create inverse transfers *)
  let reversal_transfers = settlement.transfers |> List.map (fun transfer ->
    { transfer with
      from = transfer.to;
      to = transfer.from;
      memo = "Reversal: " ^ request.reason;
    }
  ) in

  (* Execute atomically *)
  execute_settlement {
    settlement_id = generate_uuid ();
    match_id = settlement.match_id;
    transfers = reversal_transfers;
    reversal_window = 0.0;  (* Reversals can't be reversed *)
  }
```

## Settlement Lifecycle

```
   ┌─────────┐
   │ Pending │
   └────┬────┘
        │ Lock resources
        ▼
   ┌──────────┐
   │ Escrowed │
   └────┬─────┘
        │ Execute transfers
        ▼
   ┌──────────┐     Reversal requested
   │ Executed │ ◄──────────────────┐
   └────┬─────┘                    │
        │                          │
        │ Time passes              ▼
        │                   ┌──────────────┐
        │                   │ Under Dispute │
        │                   └──────┬───────┘
        │                          │
        ▼                          ▼
   ┌───────────┐            ┌──────────┐
   │ Finalized │            │ Reversed │
   └───────────┘            └──────────┘
```

## Usage Example

```ocaml
open Ambience_settlement

(* Create settlement from match *)
let settlement = {
  settlement_id = generate_uuid ();
  match_id = match.match_id;
  participants = get_match_participants match;
  transfers = [
    { from = "alice"; to = "bob";
      resource = gpu_hours; amount = 10.0 };
    { from = "bob"; to = "alice";
      resource = usdc; amount = 100.0 };
  ];
  reversal_window = 3600.0;  (* 1 hour reversal window *)
  status = Pending;
}

(* Set up escrow *)
let escrow = Escrow.create
  ~participants:settlement.participants
  ~resources:settlement.transfers
  ~conditions:[AllSignatures; TimeoutRelease]

(* Execute when ready *)
match Executor.execute settlement escrow with
| Ok executed ->
    (* Generate and store proof *)
    let proof = Proof.generate executed in
    store_settlement_proof proof;

    (* Wait for finalization or reversal *)
    schedule_finalization executed

| Error reason ->
    (* Unlock escrowed resources *)
    Escrow.abort escrow
```

## Security Properties

### Atomicity
All transfers execute or none do:
```
∀ s ∈ Settlement: execute(s) ∈ {Success(s'), Failure(s)}
where s' has all transfers complete, s unchanged
```

### Determinism
Same inputs always produce same result:
```
∀ s, e: execute(s, e) = execute(s, e)
```

### Verifiability
Anyone can verify settlement correctness:
```
verify(settlement, proof) = true ⟺ settlement executed correctly
```

## Try It Yourself

```bash
dune utop lib/settlement
```

```ocaml
#require "ambience.settlement";;
open Ambience_settlement;;

(* Create a test settlement *)
let transfers = [
  { from = "alice"; to = "bob";
    resource_type = "compute:gpu"; amount = 1.0 };
];;

let settlement = create_settlement ~transfers ~reversal_window:600.0;;

(* Generate proof *)
let proof = Proof.generate_merkle_proof settlement;;

(* Verify independently *)
Proof.verify_settlement_proof settlement proof;;  (* true *)

(* Request reversal within window *)
let reversal = Reversal.request
  ~settlement
  ~reason:NonDelivery
  ~evidence:["GPU hours not provided"]
  ~bond:{ amount = 10.0; currency = "USDC" };;
```

## Performance Optimizations

1. **Batch Settlement**: Multiple matches settled in one transaction
2. **Proof Compression**: ZK-SNARKs for succinct proofs
3. **Parallel Validation**: Independent constraint checking
4. **State Channels**: Off-chain settlement with on-chain finality
5. **Optimistic Execution**: Execute assuming success, rollback if needed

## Related Modules

- **[Core](../core/)**: Provides resource and intent types
- **[Matching](../matching/)**: Generates settlements from matches
- **[Trust](../trust/)**: Manages collateral and slashing
- **[Network](../network/)**: Propagates settlement proofs

## Key Insights

1. **Tentative Finality**: Settlements can be reversed within windows
2. **Cryptographic Truth**: Proofs replace institutional trust
3. **Atomic Complexity**: Multi-party settlements execute atomically
4. **Economic Security**: Bonds prevent frivolous reversals
5. **Deterministic Execution**: Enables independent verification

---

*"Settlement is not the end of a trade but the beginning of economic reality."*