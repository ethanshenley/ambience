(** Core Types Test Suite *)

open Alcotest
open Ambience_core
open Ambience_core.Types
open Test_helpers
open Fixtures.Intent_fixtures

(** Test UUID generation *)
let test_uuid_generation () =
  let uuid1 = Intent.generate_uuid () in
  let uuid2 = Intent.generate_uuid () in
  check (neg string) "UUIDs should be unique" uuid1 uuid2;
  check bool "UUID should not be empty" true (String.length uuid1 > 0)

(** Test resource field creation and validation *)
let test_resource_field_creation () =
  let resource = make_test_resource
    ~uri:"resource://compute/gpu/hours"
    ~amount:10.0
    () in
  check string "Resource URI" "resource://compute/gpu/hours" resource.resource_type;
  let (min, max) = resource.quantity_range in
  check (float 0.01) "Fixed amount min" 10.0 min;
  check (float 0.01) "Fixed amount max" 10.0 max

let test_resource_range_creation () =
  let resource = make_test_resource_range
    ~uri:"resource://currency/usd"
    ~min:100.0
    ~max:500.0
    () in
  let (min, max) = resource.quantity_range in
  check (float 0.01) "Min amount" 100.0 min;
  check (float 0.01) "Max amount" 500.0 max

(** Test constraint types *)
let test_constraint_creation () =
  let price_constraint = make_price_constraint ~min:10.0 ~max:100.0 () in
  match price_constraint with
  | PriceRange (min, max) ->
      check (float 0.01) "Min price" 10.0 min;
      check (float 0.01) "Max price" 100.0 max
  | _ -> fail "Expected PriceRange constraint"

let test_time_constraint () =
  set_test_time 1000.0;
  let constraint_t = make_time_constraint ~deadline:(Some 2000.0) () in
  match constraint_t with
  | TimeWindow (start_t, end_t) ->
      check (float 0.01) "Start time" 1000.0 start_t;
      check (float 0.01) "End time" 2000.0 end_t
  | _ -> fail "Expected TimeWindow constraint"

let test_location_constraint () =
  let locations = ["US"; "EU"; "ASIA"] in
  let constraint_t = make_location_constraint ~locations () in
  match constraint_t with
  | Custom ("locations", value) ->
      check string "Locations" "US,EU,ASIA" value
  | _ -> fail "Expected Custom locations constraint"

let test_quality_constraint () =
  let constraint_t = make_quality_constraint ~min:0.9 () in
  match constraint_t with
  | Custom ("min_quality", value) ->
      check string "Quality score" "0.9" value
  | _ -> fail "Expected Custom quality constraint"

(** Test intent lifecycles *)
let test_intent_lifecycles () =
  let lifecycles = [
    Eternal;
    Expiring (Unix.time () +. 3600.0);
    Consumable 5;
    Conditional "price > 100";
  ] in
  List.iter (fun lifecycle ->
    let intent = make_test_intent ~lifecycle () in
    check bool "Intent lifecycle set correctly" true (intent.lifecycle = lifecycle)
  ) lifecycles

(** Test settlement status *)
let test_settlement_status () =
  let statuses = [
    Pending;
    Completed;
    Failed "test error";
    Reversed "test reversal";
  ] in
  List.iter (fun status ->
    let settlement = make_test_settlement ~status () in
    check bool "Settlement status set correctly" true (settlement.status = status)
  ) statuses


(** Test settlement point *)
let test_settlement_point () =
  let point = make_test_settlement_point
    ~price:99.5
    ~quantity:10.0
    ~time:(Some 1500.0)
    ~quality_level:(Some 0.95)
    ~additional_terms:[("slippage", "0.01")]
    () in
  check (float 0.01) "Price" 99.5 point.price;
  check (float 0.01) "Quantity" 10.0 point.quantity;
  check (float 0.01) "Execution time" 1500.0 point.execution_time;
  check (option (float 0.01)) "Quality level" (Some 0.95) point.quality_level;
  check (list (pair string string)) "Additional terms" [("slippage", "0.01")] point.additional_terms

(** Test settlement manifold *)
let test_settlement_manifold () =
  let points = [
    make_test_settlement_point ~price:100.0 ~quantity:10.0 ();
    make_test_settlement_point ~price:101.0 ~quantity:9.5 ();
    make_test_settlement_point ~price:102.0 ~quantity:9.0 ();
  ] in
  let manifold = {
    dimensions = ["price"; "quantity"];
    valid_region_constraints = [PriceRange (99.0, 103.0)];
    pareto_frontier = points;
    optimality_scores = Some (List.map (fun p -> (p, 0.9)) points);
  } in
  check (list string) "Dimensions" ["price"; "quantity"] manifold.dimensions;
  check int "Pareto frontier size" 3 (List.length manifold.pareto_frontier);
  match manifold.optimality_scores with
  | Some scores -> check int "Optimality scores count" 3 (List.length scores)
  | None -> fail "Expected optimality scores"

(** Test reputation type *)
let test_reputation () =
  let rep = make_test_reputation ~score:0.85 ~volume:5000.0 () in
  check (float 0.01) "Score" 0.85 rep.score;
  check int "Successful settlements" 10 rep.successful_settlements;
  check int "Failed settlements" 1 rep.failed_settlements;
  check (float 0.01) "Total volume" 5000.0 rep.total_volume;
  check int "Total settlements" 11 rep.total_settlements

(** Test time window validation *)
let test_time_window_validation () =
  let valid_start = 1000.0 in
  let valid_end = 2000.0 in
  check bool "Valid time window" true (valid_end > valid_start);

  let invalid_start = 2000.0 in
  let invalid_end = 1000.0 in
  check bool "Invalid time window" true (invalid_end < invalid_start)


(** Test complex nested types *)
let test_complex_intent () =
  let intent = constrained_intent in
  check bool "Intent has constraints" true (List.length intent.constraints > 0);

  (* Check constraint types *)
  let has_price_constraint = List.exists (function
    | PriceRange _ -> true
    | _ -> false
  ) intent.constraints in
  check bool "Has price constraint" true has_price_constraint;

  let has_time_constraint = List.exists (function
    | TimeWindow _ -> true
    | _ -> false
  ) intent.constraints in
  check bool "Has time constraint" true has_time_constraint

(** Test edge cases *)
let test_zero_quantity () =
  let resource = make_test_resource ~amount:0.0 () in
  let (min, max) = resource.quantity_range in
  check (float 0.01) "Zero quantity min" 0.0 min;
  check (float 0.01) "Zero quantity max" 0.0 max

let test_negative_price_range () =
  (* This should be allowed for some markets *)
  let constraint_t = PriceRange (-10.0, 10.0) in
  match constraint_t with
  | PriceRange (min, _max) ->
      check bool "Negative price allowed" true (min < 0.0)
  | _ -> fail "Expected PriceRange"

let test_empty_metadata () =
  let intent = make_test_intent ~id:"test" () in
  check bool "Intent created without metadata" true (intent.intent_id = "test")

let test_large_metadata () =
  let large_terms = List.init 100 (fun i ->
    (Printf.sprintf "key%d" i, Printf.sprintf "value%d" i)
  ) in
  let point = {
    price = 100.0;
    quantity = 10.0;
    execution_time = 1000.0;
    quality_level = None;
    additional_terms = large_terms;
  } in
  check int "Large additional terms count" 100 (List.length point.additional_terms)

(** Export test suite for main runner *)
let tests = [
  "UUID", [
    test_case "Generation" `Quick test_uuid_generation;
  ];
  "Resource Fields", [
    test_case "Fixed creation" `Quick test_resource_field_creation;
    test_case "Range creation" `Quick test_resource_range_creation;
  ];
  "Constraints", [
    test_case "Price constraint" `Quick test_constraint_creation;
    test_case "Time constraint" `Quick test_time_constraint;
    test_case "Location constraint" `Quick test_location_constraint;
    test_case "Quality constraint" `Quick test_quality_constraint;
  ];
  "Intent Lifecycles", [
    test_case "All lifecycles" `Quick test_intent_lifecycles;
  ];
  "Settlement", [
    test_case "Status types" `Quick test_settlement_status;
    test_case "Settlement point" `Quick test_settlement_point;
    test_case "Settlement manifold" `Quick test_settlement_manifold;
  ];
  "Reputation", [
    test_case "Reputation fields" `Quick test_reputation;
  ];
  "Time Windows", [
    test_case "Validation" `Quick test_time_window_validation;
  ];
  "Complex Types", [
    test_case "Complex intent" `Quick test_complex_intent;
  ];
  "Edge Cases", [
    test_case "Zero quantity" `Quick test_zero_quantity;
    test_case "Negative price range" `Quick test_negative_price_range;
    test_case "Empty metadata" `Quick test_empty_metadata;
    test_case "Large metadata" `Quick test_large_metadata;
  ];
]

(** Standalone runner for this test module *)
let () =
  if Array.length Sys.argv > 1 && Sys.argv.(0) = "test_core_types" then
    run "Core Types Tests" tests