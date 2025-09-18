(** Match Fixtures for Testing *)

open Ambience_core.Types
open Test_helpers
open Intent_fixtures

(** Simple bilateral match *)
let simple_match = make_test_match
  ~id:"match-001"
  ~intents:[simple_buy_intent; simple_sell_intent]
  
  ()

(** Match with complex settlement manifold *)
let manifold_match =
  let points = [
    make_test_settlement_point ~price:9.5 ~quantity:10.0 ();
    make_test_settlement_point ~price:10.0 ~quantity:9.5 ();
    make_test_settlement_point ~price:10.5 ~quantity:9.0 ();
  ] in
  let manifold = {
    dimensions = ["price"; "quantity"; "time"; "quality"];
    valid_region_constraints = [
      PriceRange (9.0, 11.0);
      make_time_constraint ();
    ];
    pareto_frontier = points;
    optimality_scores = Some [
      (List.hd points, 0.95);
      (List.nth points 1, 0.92);
      (List.nth points 2, 0.88);
    ];
  } in
  make_test_match
    ~id:"match-manifold-001"
    ~intents:[range_buy_intent; range_sell_intent]
    ~space:(Some manifold)
    
    ()

(** Multilateral match (for future support) *)
let trilateral_match =
  let intent_a = make_test_intent
    ~id:"tri-a"
    ~offer:(make_test_resource ~uri:"resource://a" ~amount:100.0 ())
    ~want:(make_test_resource ~uri:"resource://b" ~amount:100.0 ())
    () in
  let intent_b = make_test_intent
    ~id:"tri-b"
    ~offer:(make_test_resource ~uri:"resource://b" ~amount:100.0 ())
    ~want:(make_test_resource ~uri:"resource://c" ~amount:100.0 ())
    () in
  let intent_c = make_test_intent
    ~id:"tri-c"
    ~offer:(make_test_resource ~uri:"resource://c" ~amount:100.0 ())
    ~want:(make_test_resource ~uri:"resource://a" ~amount:100.0 ())
    () in
  {
    match_id = "match-trilateral-001";
    intent_ids = ["tri-a"; "tri-b"; "tri-c"];
    settlement_space = {
      dimensions = ["flow_a_to_b"; "flow_b_to_c"; "flow_c_to_a"];
      valid_region_constraints = [];
      pareto_frontier = [
        make_test_settlement_point ~price:1.0 ~quantity:100.0 ();
      ];
      optimality_scores = None;
    };
    discovered_at = get_test_time ();
    discovered_by = "test-discoverer";
    expires_at = get_test_time () +. 3600.0;
  }

(** Low quality match *)
let poor_match = make_test_match
  ~id:"match-poor-001"
  
  ()

(** Time-sensitive match *)
let urgent_match =
  let urgent_buy = make_test_intent
    ~id:"urgent-buy"
    ~lifecycle:Eternal
    ~offer:(make_test_resource ~uri:"resource://currency/usd" ~amount:150.0 ())
    ~want:(make_test_resource ~uri:"resource://service/immediate" ~amount:1.0 ())
    () in
  let urgent_sell = make_test_intent
    ~id:"urgent-sell"
    ~offer:(make_test_resource ~uri:"resource://service/immediate" ~amount:1.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:150.0 ())
    () in
  make_test_match
    ~id:"match-urgent-001"
    ~intents:[urgent_buy; urgent_sell]
     (* Perfect match due to urgency *)
    ()

(** Match with consumable intents *)
let consumable_match =
  let consumable_seller = make_test_intent
    ~id:"consumable-seller"
    ~lifecycle:(Consumable 10)
    ~offer:(make_test_resource ~uri:"resource://inventory/items" ~amount:1.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:25.0 ())
    () in
  let buyer = make_test_intent
    ~id:"consumable-buyer"
    ~offer:(make_test_resource ~uri:"resource://currency/usd" ~amount:25.0 ())
    ~want:(make_test_resource ~uri:"resource://inventory/items" ~amount:1.0 ())
    () in
  make_test_match
    ~id:"match-consumable-001"
    ~intents:[consumable_seller; buyer]
    
    ()

(** Match batch for testing matching engine *)
let generate_match_batch n =
  List.init n (fun i ->
    let intent1 = make_test_intent
      ~id:(Printf.sprintf "batch-intent-a-%04d" i)
      ~offer:(make_test_resource
        ~uri:(Printf.sprintf "resource://type/%d" (i mod 10))
        ~amount:(100.0 +. float_of_int i) ())
      ~want:(make_test_resource
        ~uri:(Printf.sprintf "resource://type/%d" ((i + 1) mod 10))
        ~amount:(10.0 +. float_of_int (i mod 20)) ())
      () in
    let intent2 = make_test_intent
      ~id:(Printf.sprintf "batch-intent-b-%04d" i)
      ~offer:(make_test_resource
        ~uri:(Printf.sprintf "resource://type/%d" ((i + 1) mod 10))
        ~amount:(10.0 +. float_of_int (i mod 20)) ())
      ~want:(make_test_resource
        ~uri:(Printf.sprintf "resource://type/%d" (i mod 10))
        ~amount:(100.0 +. float_of_int i) ())
      () in
    make_test_match
      ~id:(Printf.sprintf "batch-match-%04d" i)
      ~intents:[intent1; intent2]
      ()
  )

(** Match with settlement constraints *)
let constrained_match =
  let manifold = {
    dimensions = ["price"; "quantity"; "delivery_time"];
    valid_region_constraints = [
      PriceRange (95.0, 105.0);
      Custom ("validator", "always_true")
    ];
    pareto_frontier = [
      make_test_settlement_point ~price:95.0 ~quantity:10.0 ();
      make_test_settlement_point ~price:100.0 ~quantity:9.8 ();
      make_test_settlement_point ~price:105.0 ~quantity:9.5 ();
    ];
    optimality_scores = None;
  } in
  make_test_match
    ~id:"match-constrained-001"
    ~space:(Some manifold)
    
    ()

(** Invalid matches for negative testing *)
let empty_match = {
  match_id = "match-empty-001";
  intent_ids = [];
  settlement_space = {
    dimensions = [];
    valid_region_constraints = [];
    pareto_frontier = [];
    optimality_scores = None;
  };
  discovered_at = get_test_time ();
  discovered_by = "test-discoverer";
  expires_at = get_test_time () +. 3600.0;
}

let single_intent_match = make_test_match
  ~id:"match-single-001"
  ~intents:[simple_buy_intent] (* Only one intent - invalid for bilateral *)
  
  ()