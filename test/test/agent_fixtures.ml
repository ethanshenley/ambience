(** Agent Fixtures for Testing *)

open Ambience_core.Types
open Test_helpers

(** Standard test agents *)
let alice = make_test_agent ~name:"alice" ~id:"agent-alice-001" ()
let bob = make_test_agent ~name:"bob" ~id:"agent-bob-001" ()
let charlie = make_test_agent ~name:"charlie" ~id:"agent-charlie-001" ()
let dave = make_test_agent ~name:"dave" ~id:"agent-dave-001" ()

(** Agents with different reputation levels *)
let trusted_agent =
  let id = make_test_agent ~name:"trusted" ~id:"agent-trusted-001" () in
  (id, make_test_reputation ~score:0.95 ~volume:10000.0 ())

let new_agent =
  let id = make_test_agent ~name:"newbie" ~id:"agent-new-001" () in
  (id, make_test_reputation ~score:0.5 ~volume:0.0 ())

let suspicious_agent =
  let id = make_test_agent ~name:"suspicious" ~id:"agent-sus-001" () in
  (id, {
    agent_id = id;
    score = 0.2;
    total_settlements = 25;
    successful_settlements = 5;
    failed_settlements = 20;
    total_volume = 500.0;
    last_updated = get_test_time ();
    domain_scores = Some [("resource://general", 0.2)];
  })

(** Market maker agents *)
let market_makers = List.init 5 (fun i ->
  let id = make_test_agent
    ~name:(Printf.sprintf "mm%d" i)
    ~id:(Printf.sprintf "agent-mm-%03d" i)
    () in
  let reputation = make_test_reputation
    ~score:(0.7 +. 0.05 *. float_of_int i)
    ~volume:(5000.0 *. float_of_int (i + 1))
    () in
  (id, reputation)
)

(** Agents with capabilities *)
let compute_provider =
  let id = make_test_agent ~name:"gpu-provider" ~id:"agent-gpu-001" () in
  let capabilities = [
    "compute.gpu.nvidia.a100";
    "compute.gpu.nvidia.v100";
    "compute.cpu.amd.epyc";
  ] in
  (id, capabilities)

let storage_provider =
  let id = make_test_agent ~name:"storage-provider" ~id:"agent-storage-001" () in
  let capabilities = [
    "storage.ssd.nvme";
    "storage.hdd.sata";
    "storage.distributed.ipfs";
  ] in
  (id, capabilities)

let service_provider =
  let id = make_test_agent ~name:"service-provider" ~id:"agent-service-001" () in
  let capabilities = [
    "service.api.rest";
    "service.api.graphql";
    "service.compute.serverless";
  ] in
  (id, capabilities)

(** Agent network for trust testing *)
type agent_network = {
  agents: (public_key * reputation) list;
  trust_edges: (public_key * public_key * float) list; (* from, to, trust_score *)
}

let create_trust_network () =
  let agents = [
    (alice, make_test_reputation ~score:0.8 ());
    (bob, make_test_reputation ~score:0.75 ());
    (charlie, make_test_reputation ~score:0.7 ());
    (dave, make_test_reputation ~score:0.6 ());
  ] in
  let trust_edges = [
    (alice, bob, 0.9);
    (alice, charlie, 0.7);
    (bob, charlie, 0.8);
    (bob, dave, 0.5);
    (charlie, dave, 0.6);
  ] in
  { agents; trust_edges }

(** Byzantine agents for adversarial testing *)
let byzantine_agents = List.init 10 (fun i ->
  make_test_agent
    ~name:(Printf.sprintf "byzantine%d" i)
    ~id:(Printf.sprintf "agent-byz-%03d" i)
    ()
)

(** Agent collateral information *)
let collateralized_agents = [
  (alice, 1000.0);  (* $1000 collateral *)
  (bob, 5000.0);    (* $5000 collateral *)
  (charlie, 500.0); (* $500 collateral *)
]

(** Agent metadata for different scenarios *)
let kyc_verified_agent =
  let id = make_test_agent ~name:"kyc-verified" ~id:"agent-kyc-001" () in
  let metadata = [
    ("kyc_status", "verified");
    ("kyc_date", "2024-01-01");
    ("jurisdiction", "US");
    ("entity_type", "individual");
  ] in
  (id, metadata)

let institutional_agent =
  let id = make_test_agent ~name:"institution" ~id:"agent-inst-001" () in
  let metadata = [
    ("entity_type", "corporation");
    ("registration", "Delaware C-Corp");
    ("tax_id", "XX-XXXXXXX");
    ("authorized_traders", "5");
  ] in
  (id, metadata)

(** Agent groups for batch testing *)
let small_agent_pool = [alice; bob; charlie; dave]

let medium_agent_pool = List.init 50 (fun i ->
  make_test_agent
    ~name:(Printf.sprintf "agent%d" i)
    ~id:(Printf.sprintf "agent-pool-%03d" i)
    ()
)

let large_agent_pool = List.init 1000 (fun i ->
  make_test_agent
    ~name:(Printf.sprintf "large%d" i)
    ~id:(Printf.sprintf "agent-large-%04d" i)
    ()
)

(** Helper functions for agent testing *)
let get_agent_reputation agents agent_id =
  try
    List.assoc agent_id agents
  with Not_found ->
    make_test_reputation () (* Default reputation *)

let update_agent_reputation agents agent_id new_reputation =
  (agent_id, new_reputation) ::
  List.filter (fun (id, _) -> id <> agent_id) agents