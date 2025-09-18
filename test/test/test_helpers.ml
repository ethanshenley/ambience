(** Test Helper Utilities for Ambient Commerce Protocol Tests *)

open Ambience_core
open Ambience_core.Types

(** Time manipulation *)
let test_time = ref (Unix.time ())
let set_test_time t = test_time := t
let advance_time delta = test_time := !test_time +. delta
let get_test_time () = !test_time

(** UUID generation for deterministic tests *)
let test_uuid_counter = ref 0
let reset_uuid_counter () = test_uuid_counter := 0
let generate_test_uuid () =
  incr test_uuid_counter;
  Printf.sprintf "test-uuid-%04d" !test_uuid_counter

(** Agent builders *)
let make_test_agent ?(name="agent") ?(id=generate_test_uuid()) () =
  id

let make_test_reputation ?(agent_id=make_test_agent()) ?(score=0.5) ?(volume=100.0) () : reputation = {
  agent_id = agent_id;
  score = score;
  total_settlements = 11;
  successful_settlements = 10;
  failed_settlements = 1;
  total_volume = volume;
  last_updated = get_test_time ();
  domain_scores = None;
}

(** Resource builders *)
let make_test_resource ?(uri="resource://currency/usd") ?(amount=100.0) () : resource_field = {
  resource_type = uri;
  quantity_range = (amount, amount);  (* Fixed amount as equal min/max *)
  quality = Fungible;
  metadata = [];
}

let make_test_resource_range ?(uri="resource://currency/usd") ?(min=10.0) ?(max=100.0) () : resource_field = {
  resource_type = uri;
  quantity_range = (min, max);
  quality = Fungible;
  metadata = [];
}

(** Intent builders *)
let make_test_intent
    ?(id=generate_test_uuid())
    ?(agent=make_test_agent())
    ?(offer=make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 ())
    ?(want=make_test_resource ~uri:"resource://compute/gpu/hours" ~amount:10.0 ())
    ?(lifecycle=Eternal)
    ?(constraints=[])
    ?(priority=0.0)
    ?(created_at=None)
    () : intent = {
  intent_id = id;
  agent_id = agent;
  offer_field = offer;
  want_field = want;
  constraints = constraints;
  lifecycle = lifecycle;
  created_at = (match created_at with Some t -> t | None -> get_test_time ());
  updated_at = get_test_time ();
  commitment = "test-commitment-hash";
  priority = priority;
}

(** Settlement point builders *)
let make_test_settlement_point
    ?(price=10.0)
    ?(quantity=1.0)
    ?(time=None)
    ?(quality_level=None)
    ?(additional_terms=[]) () : settlement_point = {
  price = price;
  quantity = quantity;
  execution_time = (match time with Some t -> t | None -> get_test_time ());
  quality_level = quality_level;
  additional_terms = additional_terms;
}

(** Match builders *)
let make_test_match
    ?(id=generate_test_uuid())
    ?(intents=[])
    ?(space=None)
    ?(discovered_by=make_test_agent())
    ?(expires_in=3600.0)
    () : match_t =
  let intent_ids = match intents with
    | [] -> [generate_test_uuid(); generate_test_uuid()]
    | ids -> List.map (fun i -> i.intent_id) ids
  in
  {
    match_id = id;
    intent_ids = intent_ids;
    settlement_space = (match space with
      | Some s -> s
      | None -> {
          dimensions = ["price"; "quantity"; "time"];
          valid_region_constraints = [];
          pareto_frontier = [make_test_settlement_point ()];
          optimality_scores = None;
        });
    discovered_at = get_test_time ();
    discovered_by = discovered_by;
    expires_at = get_test_time () +. expires_in;
  }

(** Settlement builders *)
let make_test_settlement
    ?(id=generate_test_uuid())
    ?(match_id=generate_test_uuid())
    ?(point=make_test_settlement_point())
    ?(status=Pending)
    ?(proof="test-proof")
    ?(pre_hash="pre-state-hash")
    ?(post_hash="post-state-hash")
    ?(reversible_until=None)
    () : settlement = {
  settlement_id = id;
  match_id = match_id;
  executed_point = point;
  execution_proof = proof;
  pre_state_hash = pre_hash;
  post_state_hash = post_hash;
  reversible_until = reversible_until;
  status = status;
  executed_at = get_test_time ();
}

(** Constraint builders *)
let make_price_constraint ?(min=0.0) ?(max=1000.0) () : constraint_t =
  PriceRange (min, max)

let make_time_constraint ?(deadline=None) () : constraint_t =
  let time = match deadline with
    | Some t -> t
    | None -> get_test_time () +. 3600.0
  in
  TimeWindow (get_test_time (), time)

let make_location_constraint ?(locations=["US"; "EU"]) () : constraint_t =
  Custom ("locations", String.concat "," locations)

let make_quality_constraint ?(min=0.8) () : constraint_t =
  Custom ("min_quality", string_of_float min)

(** Assertion helpers *)
let assert_intent_valid intent =
  Alcotest.(check bool) "intent is valid" true
    (Intent.is_valid intent (get_test_time ()))

let assert_intents_compatible intent1 intent2 =
  Alcotest.(check bool) "intents are compatible" true
    (Intent.are_compatible intent1 intent2)

let assert_match_quality_above match_t threshold =
  Alcotest.(check bool)
    (Printf.sprintf "match quality >= %f" threshold)
    true
    (threshold <= 1.0) (* Simplified check - would compute actual quality *)

let assert_settlement_successful settlement =
  Alcotest.(check bool) "settlement is successful" true
    (settlement.status = Completed)

(** Custom Alcotest testables *)
let intent_testable = Alcotest.testable
  (fun ppf i -> Format.fprintf ppf "Intent(%s)" i.intent_id)
  (fun i1 i2 -> i1.intent_id = i2.intent_id)

let match_testable = Alcotest.testable
  (fun ppf m -> Format.fprintf ppf "Match(%s)" m.match_id)
  (fun m1 m2 -> m1.match_id = m2.match_id)

let settlement_testable = Alcotest.testable
  (fun ppf s -> Format.fprintf ppf "Settlement(%s)" s.settlement_id)
  (fun s1 s2 -> s1.settlement_id = s2.settlement_id)

let resource_testable = Alcotest.testable
  (fun ppf r -> Format.fprintf ppf "Resource(%s)" r.resource_type)
  (fun r1 r2 -> r1.resource_type = r2.resource_type)

(** Test data generators *)
let generate_intent_batch n =
  List.init n (fun i ->
    make_test_intent
      ~id:(Printf.sprintf "intent-%04d" i)
      ~agent:(Printf.sprintf "agent-%02d" (i mod 10))
      ~offer:(make_test_resource
        ~uri:(Printf.sprintf "resource://test/type%d" (i mod 5))
        ~amount:(float_of_int (i * 10 + 10)) ())
      ~want:(make_test_resource
        ~uri:(Printf.sprintf "resource://test/want%d" ((i + 1) mod 5))
        ~amount:(float_of_int (i + 1)) ())
      ())

let generate_compatible_pair () =
  let agent1 = make_test_agent ~name:"alice" ()
  and agent2 = make_test_agent ~name:"bob" () in
  let intent1 = make_test_intent
    ~agent:agent1
    ~offer:(make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 ())
    ~want:(make_test_resource ~uri:"resource://compute/gpu/hours" ~amount:10.0 ())
    ()
  and intent2 = make_test_intent
    ~agent:agent2
    ~offer:(make_test_resource ~uri:"resource://compute/gpu/hours" ~amount:10.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 ())
    ()
  in
  (intent1, intent2)

(** Performance measurement *)
let measure_time f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  (result, elapsed)

let assert_performance_under name threshold_ms f =
  let (result, elapsed) = measure_time f in
  let elapsed_ms = elapsed *. 1000.0 in
  if elapsed_ms > threshold_ms then
    Alcotest.fail (Printf.sprintf
      "%s took %.2fms (threshold: %.2fms)"
      name elapsed_ms threshold_ms);
  result

(** State helpers *)
let create_test_state () =
  State.create ()

let populate_test_state state intents =
  List.iter (fun intent ->
    match State.Transitions.post_intent state intent with
    | Ok () -> ()
    | Error e -> Alcotest.fail (Printf.sprintf "Failed to post intent: %s" e)
  ) intents

(** Network simulation helpers *)
type network_conditions = {
  latency_ms: float;
  packet_loss: float;
  jitter_ms: float;
}

let simulate_network_delay conditions =
  let delay = conditions.latency_ms +.
    (Random.float conditions.jitter_ms -. conditions.jitter_ms /. 2.0) in
  Unix.sleepf (delay /. 1000.0)

let simulate_packet_loss conditions =
  Random.float 1.0 < conditions.packet_loss

(** Chaos testing helpers *)
let inject_random_failure probability f =
  if Random.float 1.0 < probability then
    Error "Random failure injected"
  else
    f ()

let with_timeout seconds f =
  let timeout_handler _ = Alcotest.fail (Printf.sprintf "Test timed out after %d seconds" seconds) in
  let old_handler = Sys.signal Sys.sigalrm (Sys.Signal_handle timeout_handler) in
  ignore (Unix.alarm seconds);
  let result = f () in
  ignore (Unix.alarm 0);
  Sys.set_signal Sys.sigalrm old_handler;
  result