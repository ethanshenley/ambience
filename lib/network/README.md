# Network Module - Economic Field Propagation

```
╔══════════════════════════════════════════════════════════════════╗
║                    AMBIENCE NETWORK MODULE                       ║
║            Gossip Protocols & Distributed Intent Pool            ║
╚══════════════════════════════════════════════════════════════════╝
```

## Theory: Networks as Economic Substrates

Traditional financial networks are hub-and-spoke - exchanges at the center, traders at the edges. Agent networks are **rhizomatic** - every node is simultaneously client, server, and router. Intents don't travel through networks; they **permeate** them like fields through space.

The network isn't infrastructure for commerce - it IS commerce. Every packet carries economic intent, every connection is a potential trade route, every node maintains a local view of the global economic field.

## Module Structure

```
network/
├── transport.ml   # Low-level network transport
├── protocol.ml    # Wire protocol & message types
├── peer.ml        # Peer discovery & management
└── gossip.ml      # Intent propagation via gossip
```

## Key Components

### 1. Transport Layer (`transport.ml`)

Multi-protocol network abstraction:

```ocaml
type endpoint = {
  transport: transport_type;
  host: string;
  port: int;
  peer_id: peer_id option;
}

type transport_type =
  | TCP           (* Reliable, ordered *)
  | UDP           (* Fast, unordered *)
  | WebSocket     (* Browser-compatible *)
  | Libp2p        (* DHT-based P2P *)
  | Memory        (* In-process for testing *)

(* Unified connection interface *)
module type TRANSPORT = sig
  type connection
  val connect : endpoint -> connection
  val send : connection -> message -> unit
  val receive : connection -> message option
  val broadcast : connection list -> message -> unit
end

(* Adaptive transport selection *)
let select_transport intent =
  match intent.urgency, intent.size with
  | High, Small -> UDP      (* Time-critical small intents *)
  | High, Large -> TCP      (* Time-critical large intents *)
  | Low, _ -> Libp2p       (* Background propagation *)
```

### 2. Protocol Messages (`protocol.ml`)

Economic communication primitives:

```ocaml
type message =
  | IntentBroadcast of intent_broadcast
  | MatchAnnouncement of match_announcement
  | SettlementProof of settlement_proof_msg
  | PeerDiscovery of peer_discovery
  | ReputationUpdate of reputation_update
  | ResourceDiscovery of resource_discovery
  | GossipSync of gossip_sync

type intent_broadcast = {
  intent: intent;
  ttl: int;               (* Hops remaining *)
  propagation_path: peer_id list;
  signature: signature;
}

(* Message routing decisions *)
let should_propagate msg peer_state =
  match msg with
  | IntentBroadcast ib ->
      (* Don't propagate if we've seen it *)
      not (BloomFilter.contains peer_state.seen_intents ib.intent.id) &&
      (* Don't send back to sender *)
      not (List.mem peer_state.peer_id ib.propagation_path) &&
      (* Resource is relevant to peer *)
      is_interested peer_state ib.intent.resources &&
      (* TTL not exhausted *)
      ib.ttl > 0

  | MatchAnnouncement ma ->
      (* High-quality matches propagate further *)
      ma.match_quality > peer_state.min_match_quality
```

### 3. Peer Management (`peer.ml`)

Dynamic peer discovery and scoring:

```ocaml
type peer = {
  peer_id: peer_id;
  endpoint: endpoint;
  reputation: float;
  capabilities: capability list;
  interests: resource_pattern list;
  last_seen: timestamp;
  connection_quality: connection_stats;
  mutable score: float;
}

type peer_discovery_strategy =
  | Bootstrap of endpoint list    (* Known seeds *)
  | DHT                           (* Distributed hash table *)
  | Gossip                        (* Learn from peers *)
  | Rendezvous of string          (* Meeting point *)
  | Local                        (* mDNS/local broadcast *)

(* Peer scoring for preferential propagation *)
let score_peer peer =
  let base_score =
    peer.reputation *. 0.3 +.
    peer.connection_quality.success_rate *. 0.2 +.
    peer.connection_quality.latency_score *. 0.2 +.
    (if is_online peer then 0.3 else 0.0)
  in

  (* Boost for peers with complementary resources *)
  let interest_overlap = calculate_interest_overlap local_interests peer.interests in
  base_score *. (1.0 +. interest_overlap)

(* Adaptive peer selection *)
let select_propagation_peers peers intent k =
  peers
  |> List.filter (is_interested_in intent)
  |> List.map (fun p -> (score_peer p, p))
  |> List.sort (fun (s1, _) (s2, _) -> Float.compare s2 s1)
  |> List.take k
  |> List.map snd
```

### 4. Gossip Protocol (`gossip.ml`)

Epidemic intent propagation:

```ocaml
type gossip_state = {
  local_intents: intent_pool;
  peer_manager: Peer.manager;
  mutable vector_clock: vector_clock;
  seen_filter: bloom_filter;
  propagation_buffer: (message * float) Queue.t;
}

(* Gossip algorithm with economic bias *)
let gossip_round state =
  (* Select fanout peers *)
  let peers = Peer.select_active state.peer_manager state.config.fanout in

  (* Prepare intent batch *)
  let intents_to_send =
    IntentPool.get_recent state.local_intents
    |> filter_unseen_by_peers peers
    |> prioritize_by_economic_value
    |> take state.config.max_batch_size
  in

  (* Send to each peer with probability *)
  List.iter (fun peer ->
    if Random.float 1.0 < gossip_probability peer then
      send_intent_batch peer intents_to_send
  ) peers;

  (* Pull from random peer *)
  match select_random_peer peers with
  | Some peer -> pull_intents peer
  | None -> ()

(* Anti-entropy: periodic full synchronization *)
let anti_entropy_round state =
  let peer = select_sync_peer state.peer_manager in

  (* Exchange vector clocks *)
  let their_clock = exchange_vector_clocks peer state.vector_clock in

  (* Determine missing intents *)
  let missing = VectorClock.diff state.vector_clock their_clock in

  (* Request missing intents *)
  request_intents peer missing
```

## Network Topology

```
   Traditional Exchange               Agent Network

       ┌──────────┐                 ○────○────○
       │ Exchange │                  │    │    │
       └────┬─────┘                  ○────●────○
      ╱  ╱  │  ╲  ╲                  │    │    │
     ○  ○   ○   ○  ○                 ○────○────○

   Hub & Spoke: O(1) hops      Mesh: O(log n) hops
   Single point of failure      No single point of failure
   Exchange controls flow       Intents flow freely
```

## Economic Routing

Intents route based on economic gradients:

```ocaml
(* Route intents toward likely matches *)
let economic_routing intent peer_list =
  (* Calculate match probability for each peer *)
  let scores = List.map (fun peer ->
    let resource_relevance =
      estimate_resource_availability peer intent.wants in
    let peer_reputation = peer.reputation in
    let network_distance = 1.0 /. (1.0 +. peer.latency) in

    (peer, resource_relevance *. peer_reputation *. network_distance)
  ) peer_list in

  (* Route to top-k peers *)
  scores
  |> List.sort (fun (_, s1) (_, s2) -> Float.compare s2 s1)
  |> List.take config.routing_fanout
  |> List.map fst
```

## Usage Example

```ocaml
open Ambience_network

(* Initialize network stack *)
let transport = Transport.create TCP in
let peer_mgr = Peer.create_manager ~bootstrap_peers in
let gossip = Gossip.create ~transport ~peer_mgr

(* Start network services *)
Gossip.start gossip;
Peer.start_discovery peer_mgr;

(* Broadcast an intent *)
let intent = create_test_intent () in
Gossip.broadcast gossip (IntentBroadcast {
  intent;
  ttl = 6;
  propagation_path = [local_peer_id];
  signature = sign_intent intent;
})

(* Handle incoming messages *)
Gossip.set_handler gossip (function
  | IntentBroadcast ib ->
      process_incoming_intent ib.intent
  | MatchAnnouncement ma ->
      evaluate_match ma.match
  | _ -> ()
)
```

## Consensus Properties

The network achieves **eventual consistency** without consensus:

### Intent Pool Convergence
```
∀ nodes n₁, n₂: lim(t→∞) |Pool(n₁) ∩ Pool(n₂)| / |Pool(n₁) ∪ Pool(n₂)| → 1
```

### Probabilistic Broadcast
```
P(intent reaches all nodes) = 1 - (1 - p)^(f×log(n))
where p = gossip probability, f = fanout
```

### Byzantine Tolerance
Up to f < n/3 Byzantine nodes cannot prevent intent propagation:
```
Honest nodes form connected component with high probability
```

## Try It Yourself

```bash
dune utop lib/network
```

```ocaml
#require "ambience.network";;
open Ambience_network;;

(* Create in-memory transport for testing *)
let transport = Transport.create Memory;;

(* Set up two peers *)
let peer1 = Peer.create ~id:"alice" ~transport;;
let peer2 = Peer.create ~id:"bob" ~transport;;

(* Connect them *)
Peer.connect peer1 peer2.endpoint;;

(* Send message *)
Protocol.send peer1 peer2.id (IntentBroadcast {
  intent = create_test_intent ();
  ttl = 3;
  propagation_path = ["alice"];
  signature = "test_sig";
});;

(* Check if received *)
Protocol.receive peer2;;
```

## Performance Characteristics

| Metric | Value | Notes |
|--------|-------|-------|
| Message Propagation | O(log n) hops | Gossip with fanout |
| Network Overhead | O(n × fanout) | Per gossip round |
| Convergence Time | O(log n) rounds | For n nodes |
| Failure Recovery | O(1) rounds | Detect failed peers |
| State Size | O(intents + peers) | Bloom filters for seen |

## Security Considerations

1. **Eclipse Attacks**: Peer diversity requirements
2. **Spam Prevention**: Proof-of-work or stake for broadcasts
3. **Privacy**: Onion routing for sensitive intents
4. **Sybil Resistance**: Reputation-weighted propagation
5. **Censorship**: Multiple routing paths per intent

## Related Modules

- **[Core](../core/)**: Provides intent types
- **[Matching](../matching/)**: Processes received intents
- **[Trust](../trust/)**: Peer reputation scoring
- **[Settlement](../settlement/)**: Proof propagation

## Key Insights

1. **Field Propagation**: Intents spread like fields, not packets
2. **Economic Routing**: Messages flow along value gradients
3. **No Global View**: Each node has partial, eventual knowledge
4. **Emergent Behavior**: Global matching from local propagation
5. **Resilient Topology**: No single point of failure

---

*"The network doesn't carry commerce - it becomes commerce."*