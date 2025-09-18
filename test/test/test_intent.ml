(** Intent Module Test Suite *)

open Alcotest
open Ambience_core
open Ambience_core.Types
open Test_helpers
open Fixtures.Intent_fixtures

(** Test intent creation *)
let test_create_valid_intent () =
  let offer = make_test_resource ~uri:"resource://compute/gpu/hours" ~amount:10.0 () in
  let want = make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 () in

  match Intent.create
    ~agent_id:"test-agent-001"
    ~offers:offer
    ~wants:want
    () with
  | Ok intent ->
      check string "Agent ID" "test-agent-001" intent.agent_id;
      check string "Resource URI" "resource://compute/gpu/hours" intent.offer_field.resource_type;
      let (min, max) = intent.offer_field.quantity_range in
      check (float 0.01) "Offer amount" 10.0 min
  | Error msg -> fail ("Intent creation failed: " ^ msg)

let test_create_with_constraints () =
  let offer = make_test_resource ~uri:"resource://service/consulting" ~amount:1.0 () in
  let want = make_test_resource ~uri:"resource://currency/usd" ~amount:500.0 () in
  let constraints = [
    make_price_constraint ~min:400.0 ~max:600.0 ();
    make_time_constraint ~deadline:(Some (Unix.time () +. 3600.0)) ();
    make_location_constraint ~locations:["US"; "EU"] ();
  ] in

  match Intent.create
    ~agent_id:"test-agent-002"
    ~offers:offer
    ~wants:want
    ~constraints
    () with
  | Ok intent ->
      check int "Constraints count" 3 (List.length intent.constraints);
      check bool "Has price constraint" true (
        List.exists (function PriceRange _ -> true | _ -> false) intent.constraints
      )
  | Error msg -> fail ("Intent creation with constraints failed: " ^ msg)

let test_create_with_lifecycle () =
  let offer = make_test_resource ~uri:"resource://inventory/items" ~amount:10.0 () in
  let want = make_test_resource ~uri:"resource://currency/usd" ~amount:25.0 () in

  match Intent.create
    ~agent_id:"test-agent-003"
    ~offers:offer
    ~wants:want
    ~lifecycle:(Consumable 5)
    () with
  | Ok intent ->
      check bool "Is consumable" true (intent.lifecycle = Consumable 5)
  | Error msg -> fail ("Intent creation with lifecycle failed: " ^ msg)

(** Test intent validation *)
let test_is_valid () =
  set_test_time 1000.0;

  (* Test eternal intent *)
  let eternal_intent = make_test_intent ~lifecycle:Eternal () in
  check bool "Eternal intent always valid" true
    (Intent.is_valid eternal_intent (get_test_time () +. 10000.0));

  (* Test expiring intent *)
  let expiry_time = get_test_time () +. 3600.0 in
  let expiring_intent = make_test_intent ~lifecycle:(Expiring expiry_time) () in
  check bool "Expiring intent valid before expiry" true
    (Intent.is_valid expiring_intent (get_test_time () +. 1800.0));
  check bool "Expiring intent invalid after expiry" false
    (Intent.is_valid expiring_intent (expiry_time +. 1.0));

  (* Test consumable intent *)
  let consumable_intent = make_test_intent ~lifecycle:(Consumable 0) () in
  check bool "Exhausted consumable invalid" false
    (Intent.is_valid consumable_intent (get_test_time ()))

let test_constraints_satisfied () =
  set_test_time 1000.0;

  (* Test time constraint *)
  let time_constraint = make_time_constraint ~deadline:(Some 2000.0) () in
  let intent = make_test_intent ~constraints:[time_constraint] () in

  check bool "Constraint satisfied within window" true
    (Intent.constraints_satisfied intent 1500.0);
  check bool "Constraint not satisfied outside window" false
    (Intent.constraints_satisfied intent 2500.0)

(** Test intent compatibility *)
let test_are_compatible () =
  (* Compatible intents - one offers what the other wants *)
  let intent_a = make_test_intent
    ~offer:(make_test_resource ~uri:"resource://compute/gpu/hours" ~amount:10.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 ())
    () in
  let intent_b = make_test_intent
    ~offer:(make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 ())
    ~want:(make_test_resource ~uri:"resource://compute/gpu/hours" ~amount:10.0 ())
    () in

  check bool "Compatible intents" true
    (Intent.are_compatible intent_a intent_b);

  (* Incompatible intents - both want the same thing *)
  let intent_c = make_test_intent
    ~offer:(make_test_resource ~uri:"resource://service/a" ~amount:1.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 ())
    () in
  let intent_d = make_test_intent
    ~offer:(make_test_resource ~uri:"resource://service/b" ~amount:1.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 ())
    () in

  check bool "Incompatible intents" false
    (Intent.are_compatible intent_c intent_d)

let test_priority_score () =
  let low_priority = make_test_intent ~priority:0.1 () in
  let high_priority = make_test_intent ~priority:0.9 () in

  let low_score = Intent.priority_score low_priority in
  let high_score = Intent.priority_score high_priority in

  check bool "Higher priority has higher score" true (high_score > low_score)

let test_meets_counterparty_requirements () =
  let req = {
    min_reputation = Some 0.8;
    required_capabilities = [];
    excluded_agents = [];
    preferred_agents = [];
  } in
  let constraints = [
    Counterparty req;
  ] in
  let intent = make_test_intent ~constraints () in

  (* Test with high reputation *)
  check bool "High reputation meets requirements" true
    (Intent.meets_counterparty_requirements intent "agent-001" 0.9);

  (* Test with low reputation *)
  check bool "Low reputation fails requirements" false
    (Intent.meets_counterparty_requirements intent "agent-002" 0.5)

(** Test intent manipulation *)
let test_consume_match () =
  let consumable = make_test_intent ~lifecycle:(Consumable 5) () in

  match Intent.consume_match consumable with
  | Ok consumed ->
      check bool "Consumable count reduced" true
        (consumed.lifecycle = Consumable 4)
  | Error msg -> fail ("Failed to consume match: " ^ msg);

  (* Test exhausted consumable *)
  let exhausted = make_test_intent ~lifecycle:(Consumable 0) () in
  match Intent.consume_match exhausted with
  | Ok _ -> fail "Should not be able to consume exhausted intent"
  | Error _ -> check bool "Cannot consume exhausted" true true

let test_cancel () =
  let intent = make_test_intent ~lifecycle:Eternal () in
  let cancelled = Intent.cancel intent in

  (* Check that cancelled intent is marked as expired *)
  check bool "Cancelled intent not valid" false
    (Intent.is_valid cancelled (Unix.time ()))

let test_extend_expiry () =
  set_test_time 1000.0;
  let original_expiry = get_test_time () +. 3600.0 in
  let intent = make_test_intent ~lifecycle:(Expiring original_expiry) () in

  match Intent.extend_expiry intent 7200.0 with
  | Ok extended ->
      (match extended.lifecycle with
      | Expiring new_expiry ->
          check (float 0.01) "Extended expiry"
            (original_expiry +. 7200.0) new_expiry
      | _ -> fail "Expected Expiring lifecycle")
  | Error msg -> fail ("Failed to extend expiry: " ^ msg)

(** Test information extraction *)
let test_get_time_windows () =
  let constraints = [
    TimeWindow (1000.0, 2000.0);
    TimeWindow (3000.0, 4000.0);
  ] in
  let intent = make_test_intent ~constraints () in

  let windows = Intent.get_time_windows intent in
  check int "Time windows count" 2 (List.length windows);

  let (start1, end1) = List.hd windows in
  check (float 0.01) "First window start" 1000.0 start1;
  check (float 0.01) "First window end" 2000.0 end1

let test_get_price_ranges () =
  let constraints = [
    PriceRange (100.0, 200.0);
    PriceRange (300.0, 400.0);
  ] in
  let intent = make_test_intent ~constraints () in

  let ranges = Intent.get_price_ranges intent in
  check int "Price ranges count" 2 (List.length ranges);

  let (min1, max1) = List.hd ranges in
  check (float 0.01) "First range min" 100.0 min1;
  check (float 0.01) "First range max" 200.0 max1

let test_get_counterparty_requirements () =
  let req = {
    min_reputation = Some 0.8;
    required_capabilities = ["certified"; "verified"];
    excluded_agents = [];
    preferred_agents = [];
  } in
  let constraints = [
    Counterparty req;
  ] in
  let intent = make_test_intent ~constraints () in

  let reqs = Intent.get_counterparty_requirements intent in
  check int "Requirements count" 1 (List.length reqs)

let test_get_stats () =
  (* Use mock time for deterministic testing *)
  Time_provider.Test.with_mock_time 1000.0 (fun () ->
    let intent = make_test_intent
      ~lifecycle:(Expiring 4600.0)  (* Expires at t=4600 *)
      ~created_at:(Some 400.0)      (* Created at t=400 *)
      () in

    let stats = Intent.get_stats intent in
    (* Check age is exactly 600 seconds (1000 - 400) *)
    check (float 0.01) "Age" 600.0 stats.age;

    match stats.time_remaining with
    | Some remaining ->
        (* Check time remaining is exactly 3600 seconds (4600 - 1000) *)
        check (float 0.01) "Time remaining" 3600.0 remaining
    | None -> fail "Expected time remaining"
  )

(** Test UUID generation *)
let test_generate_uuid () =
  let uuid1 = Intent.generate_uuid () in
  let uuid2 = Intent.generate_uuid () in

  check bool "UUID not empty" true (String.length uuid1 > 0);
  check (neg string) "UUIDs unique" uuid1 uuid2

(** Export test suite for main runner *)
let tests = [
  "Creation", [
    test_case "Valid intent" `Quick test_create_valid_intent;
    test_case "With constraints" `Quick test_create_with_constraints;
    test_case "With lifecycle" `Quick test_create_with_lifecycle;
  ];
  "Validation", [
    test_case "Is valid" `Quick test_is_valid;
    test_case "Constraints satisfied" `Quick test_constraints_satisfied;
  ];
  "Compatibility", [
    test_case "Are compatible" `Quick test_are_compatible;
    test_case "Priority score" `Quick test_priority_score;
    test_case "Counterparty requirements" `Quick test_meets_counterparty_requirements;
  ];
  "Manipulation", [
    test_case "Consume match" `Quick test_consume_match;
    test_case "Cancel" `Quick test_cancel;
    test_case "Extend expiry" `Quick test_extend_expiry;
  ];
  "Information", [
    test_case "Get time windows" `Quick test_get_time_windows;
    test_case "Get price ranges" `Quick test_get_price_ranges;
    test_case "Get counterparty requirements" `Quick test_get_counterparty_requirements;
    test_case "Get stats" `Quick test_get_stats;
  ];
  "Utilities", [
    test_case "Generate UUID" `Quick test_generate_uuid;
  ];
]

(** Standalone runner for this test module *)
let () =
  if Array.length Sys.argv > 1 && Sys.argv.(0) = "test_intent" then
    run "Intent Module Tests" tests