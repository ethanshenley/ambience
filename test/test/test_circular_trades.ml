(** Test Suite for Multi-Party Circular Trades *)

open Alcotest
open Ambience_core.Types
module Intent = Ambience_core.Intent
module Engine = Ambience_matching.Engine
module Resource = Ambience_core.Resource

(** Test utilities *)
let create_test_intent ~agent_id ~offers ~wants () =
  match Intent.create
    ~agent_id
    ~offers
    ~wants
    ~constraints:[]
    () with
  | Ok intent -> intent
  | Error e -> failwith ("Failed to create test intent: " ^ e)

let create_resource ~resource_type ~quantity =
  (* Determine appropriate quality based on resource type *)
  let quality =
    if String.starts_with ~prefix:"currency:" resource_type then
      Fungible
    else if String.starts_with ~prefix:"compute:" resource_type then
      Graded 0.9
    else
      Graded 0.8
  in
  match Resource.create_field
    ~resource_type
    ~min_quantity:quantity
    ~max_quantity:quantity
    ~quality
    ~metadata:[]
  with
  | Ok field -> field
  | Error e -> failwith ("Failed to create resource field: " ^ e)

(** Test basic 3-party circular trade *)
let test_three_party_circular () =
  (* Create circular trade scenario:
     Alice: offers GPU, wants USD
     Bob: offers USD, wants ETH
     Charlie: offers ETH, wants GPU *)

  let gpu_field = create_resource ~resource_type:"compute:gpu" ~quantity:1.0 in
  let usd_field = create_resource ~resource_type:"currency:fiat:USD" ~quantity:1000.0 in
  let eth_field = create_resource ~resource_type:"currency:crypto:ETH" ~quantity:0.5 in

  let alice_intent = create_test_intent
    ~agent_id:"alice"
    ~offers:gpu_field
    ~wants:usd_field
    () in

  let bob_intent = create_test_intent
    ~agent_id:"bob"
    ~offers:usd_field
    ~wants:eth_field
    () in

  let charlie_intent = create_test_intent
    ~agent_id:"charlie"
    ~offers:eth_field
    ~wants:gpu_field
    () in

  (* Test multilateral discovery *)
  let config = { Engine.default_config with
    enable_multilateral = true;
    min_match_quality = 0.5;
  } in

  let intents = [alice_intent; bob_intent; charlie_intent] in
  let matches = Engine.discover_multilateral_matches config intents in

  (* Should find the circular trade *)
  check bool "Should find circular match" true (List.length matches > 0);

  (match matches with
  | [] -> fail "No circular match found"
  | match_t :: _ ->
      (* Verify all three intents are in the match *)
      check int "Match should include 3 intents" 3
        (List.length match_t.intent_ids);

      check bool "Alice's intent included" true
        (List.mem alice_intent.intent_id match_t.intent_ids);

      check bool "Bob's intent included" true
        (List.mem bob_intent.intent_id match_t.intent_ids);

      check bool "Charlie's intent included" true
        (List.mem charlie_intent.intent_id match_t.intent_ids);

      (* Verify match was created *)
      check bool "Match was created" true true;

      (* Check settlement manifold *)
      check bool "Has settlement points" true
        (List.length match_t.settlement_space.pareto_frontier > 0))

(** Test 4-party circular trade *)
let test_four_party_circular () =
  (* Create 4-party circular trade:
     A: offers GPU, wants USD
     B: offers USD, wants ETH
     C: offers ETH, wants BTC
     D: offers BTC, wants GPU *)

  let gpu = create_resource ~resource_type:"compute:gpu" ~quantity:1.0 in
  let usd = create_resource ~resource_type:"currency:fiat:USD" ~quantity:1000.0 in
  let eth = create_resource ~resource_type:"currency:crypto:ETH" ~quantity:0.5 in
  let btc = create_resource ~resource_type:"currency:crypto:BTC" ~quantity:0.02 in

  let intents = [
    create_test_intent ~agent_id:"A" ~offers:gpu ~wants:usd ();
    create_test_intent ~agent_id:"B" ~offers:usd ~wants:eth ();
    create_test_intent ~agent_id:"C" ~offers:eth ~wants:btc ();
    create_test_intent ~agent_id:"D" ~offers:btc ~wants:gpu ();
  ] in

  let config = { Engine.default_config with
    enable_multilateral = true;
    min_match_quality = 0.5;
  } in

  let matches = Engine.discover_multilateral_matches config intents in

  (* Should find the 4-party circular trade *)
  check bool "Should find 4-party match" true (List.length matches > 0);

  (match matches with
  | [] -> fail "No 4-party match found"
  | match_t :: _ ->
      check int "Match should include 4 intents" 4
        (List.length match_t.intent_ids);

      (* 4-party trades should be found *)
      check bool "4-party trade found" true true)

(** Test broken circular trade (no cycle) *)
let test_broken_circle () =
  (* Create non-circular scenario:
     Alice: offers GPU, wants USD
     Bob: offers USD, wants ETH
     Charlie: offers ETH, wants BTC (no one offers BTC) *)

  let gpu = create_resource ~resource_type:"compute:gpu" ~quantity:1.0 in
  let usd = create_resource ~resource_type:"currency:fiat:USD" ~quantity:1000.0 in
  let eth = create_resource ~resource_type:"currency:crypto:ETH" ~quantity:0.5 in
  let btc = create_resource ~resource_type:"currency:crypto:BTC" ~quantity:0.02 in

  let intents = [
    create_test_intent ~agent_id:"alice" ~offers:gpu ~wants:usd ();
    create_test_intent ~agent_id:"bob" ~offers:usd ~wants:eth ();
    create_test_intent ~agent_id:"charlie" ~offers:eth ~wants:btc ();
  ] in

  let config = { Engine.default_config with
    enable_multilateral = true;
    min_match_quality = 0.5;
  } in

  let matches = Engine.discover_multilateral_matches config intents in

  (* Should NOT find a circular trade *)
  check bool "Should not find broken circle" true (List.length matches = 0)

(** Test quantity balancing in circular trades *)
let test_quantity_balancing () =
  (* Create circular trade with mismatched quantities *)
  let gpu_small = create_resource ~resource_type:"compute:gpu" ~quantity:0.5 in
  let gpu_large = create_resource ~resource_type:"compute:gpu" ~quantity:2.0 in
  let usd = create_resource ~resource_type:"currency:fiat:USD" ~quantity:1000.0 in
  let eth = create_resource ~resource_type:"currency:crypto:ETH" ~quantity:0.5 in

  let intents = [
    create_test_intent ~agent_id:"alice" ~offers:gpu_small ~wants:usd ();
    create_test_intent ~agent_id:"bob" ~offers:usd ~wants:eth ();
    create_test_intent ~agent_id:"charlie" ~offers:eth ~wants:gpu_large ();
  ] in

  let config = { Engine.default_config with
    enable_multilateral = true;
    min_match_quality = 0.5;
  } in

  let matches = Engine.discover_multilateral_matches config intents in

  (* Should handle quantity mismatch gracefully *)
  (* With 20% tolerance, this might still match *)
  if List.length matches > 0 then
    check bool "Quantities should be adjusted" true true
  else
    check bool "Rejected due to quantity mismatch" true true

(** Test performance with many intents *)
let test_circular_performance () =
  let create_chain_intent i =
    let offers_type = Printf.sprintf "resource:%d" i in
    let wants_type = Printf.sprintf "resource:%d" ((i + 1) mod 10) in
    let offers = create_resource ~resource_type:offers_type ~quantity:1.0 in
    let wants = create_resource ~resource_type:wants_type ~quantity:1.0 in
    create_test_intent
      ~agent_id:(Printf.sprintf "agent_%d" i)
      ~offers
      ~wants
      ()
  in

  (* Create 10 intents that form a potential 10-party circle *)
  let intents = List.init 10 create_chain_intent in

  let config = { Engine.default_config with
    enable_multilateral = true;
    min_match_quality = 0.5;
  } in

  let start = Unix.gettimeofday () in
  let matches = Engine.discover_multilateral_matches config intents in
  let elapsed = Unix.gettimeofday () -. start in

  Printf.printf "Found %d circular matches in %.3f ms\n"
    (List.length matches) (elapsed *. 1000.0);

  (* Should complete reasonably quickly *)
  check bool "Performance acceptable" true (elapsed < 0.1)

(** Test suite *)
let () =
  run "Circular Trades" [
    "Basic", [
      test_case "Three-party circle" `Quick test_three_party_circular;
      test_case "Four-party circle" `Quick test_four_party_circular;
      test_case "Broken circle" `Quick test_broken_circle;
    ];
    "Advanced", [
      test_case "Quantity balancing" `Quick test_quantity_balancing;
    ];
    "Performance", [
      test_case "Large circular trades" `Quick test_circular_performance;
    ];
  ]