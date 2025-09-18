(** Intent Fixtures for Testing *)

open Ambience_core.Types
open Test_helpers

(** Standard test intents *)
let simple_buy_intent = make_test_intent
  ~id:"intent-buy-001"
  ~offer:(make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 ())
  ~want:(make_test_resource ~uri:"resource://compute/gpu/hours" ~amount:10.0 ())
  ()

let simple_sell_intent = make_test_intent
  ~id:"intent-sell-001"
  ~offer:(make_test_resource ~uri:"resource://compute/gpu/hours" ~amount:10.0 ())
  ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:100.0 ())
  ()

(** Intents with ranges *)
let range_buy_intent = make_test_intent
  ~id:"intent-range-buy-001"
  ~offer:(make_test_resource_range ~uri:"resource://currency/usd" ~min:50.0 ~max:150.0 ())
  ~want:(make_test_resource_range ~uri:"resource://compute/gpu/hours" ~min:5.0 ~max:15.0 ())
  ()

let range_sell_intent = make_test_intent
  ~id:"intent-range-sell-001"
  ~offer:(make_test_resource_range ~uri:"resource://compute/gpu/hours" ~min:5.0 ~max:20.0 ())
  ~want:(make_test_resource_range ~uri:"resource://currency/usd" ~min:80.0 ~max:200.0 ())
  ()

(** Consumable intents *)
let consumable_intent = make_test_intent
  ~id:"intent-consumable-001"
  ~lifecycle:(Consumable 5)
  ~offer:(make_test_resource ~uri:"resource://energy/kwh" ~amount:100.0 ())
  ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:20.0 ())
  ()

(** Intents with constraints *)
let constrained_intent = make_test_intent
  ~id:"intent-constrained-001"
  ~offer:(make_test_resource ~uri:"resource://storage/gb" ~amount:1000.0 ())
  ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:50.0 ())
  ~constraints:[
    make_price_constraint ~min:0.04 ~max:0.06 ();
    make_time_constraint ();
    make_location_constraint ~locations:["US-EAST"; "EU-WEST"] ();
    make_quality_constraint ~min:0.9 ();
  ]
  ()

(** Multi-resource intents *)
let multi_offer_intent = make_test_intent
  ~id:"intent-multi-001"
  ~offer:(make_test_resource ~uri:"resource://bundle/compute-storage" ~amount:1.0 ())
  ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:500.0 ())
  ()

(** Time-sensitive intents *)
let urgent_intent =
  let current = get_test_time () in
  make_test_intent
    ~id:"intent-urgent-001"
    ~lifecycle:Eternal
    ~offer:(make_test_resource ~uri:"resource://service/urgent" ~amount:1.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:200.0 ())
    ()

let future_intent =
  let current = get_test_time () in
  make_test_intent
    ~id:"intent-future-001"
    ~lifecycle:Eternal
    ~offer:(make_test_resource ~uri:"resource://service/scheduled" ~amount:1.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:150.0 ())
    ()

(** Invalid intents for negative testing *)
let expired_intent =
  let current = get_test_time () in
  make_test_intent
    ~id:"intent-expired-001"
    ~lifecycle:Eternal
    ~offer:(make_test_resource ~uri:"resource://test/expired" ~amount:1.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:10.0 ())
    ()

let zero_quantity_intent = make_test_intent
  ~id:"intent-zero-001"
  ~offer:(make_test_resource ~uri:"resource://test/zero" ~amount:0.0 ())
  ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:10.0 ())
  ()

let invalid_time_window_intent =
  let current = get_test_time () in
  make_test_intent
    ~id:"intent-invalid-time-001"
    ~lifecycle:Eternal
    ~offer:(make_test_resource ~uri:"resource://test/invalid" ~amount:1.0 ())
    ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:10.0 ())
    ()

(** Intent collections for batch testing *)
let compatible_intent_pairs = [
  (simple_buy_intent, simple_sell_intent);
  (range_buy_intent, range_sell_intent);
]

let market_maker_intents =
  List.init 10 (fun i ->
    let spread = 0.02 in
    let mid_price = 100.0 in
    let buy_price = mid_price *. (1.0 -. spread) in
    let sell_price = mid_price *. (1.0 +. spread) in

    if i mod 2 = 0 then
      make_test_intent
        ~id:(Printf.sprintf "mm-buy-%03d" i)
        ~offer:(make_test_resource ~uri:"resource://currency/usd" ~amount:buy_price ())
        ~want:(make_test_resource ~uri:"resource://asset/token" ~amount:1.0 ())
        ()
    else
      make_test_intent
        ~id:(Printf.sprintf "mm-sell-%03d" i)
        ~offer:(make_test_resource ~uri:"resource://asset/token" ~amount:1.0 ())
        ~want:(make_test_resource ~uri:"resource://currency/usd" ~amount:sell_price ())
        ()
  )

let large_intent_set =
  List.init 1000 (fun i ->
    let resource_types = [|
      "resource://currency/usd";
      "resource://currency/eur";
      "resource://compute/gpu/hours";
      "resource://storage/gb";
      "resource://bandwidth/mbps";
      "resource://energy/kwh";
    |] in

    let offer_type = resource_types.(i mod Array.length resource_types) in
    let want_type = resource_types.((i + 1) mod Array.length resource_types) in

    make_test_intent
      ~id:(Printf.sprintf "bulk-intent-%04d" i)
      ~agent:(Printf.sprintf "agent-%03d" (i mod 100))
      ~offer:(make_test_resource ~uri:offer_type ~amount:(float_of_int (100 + i mod 900)) ())
      ~want:(make_test_resource ~uri:want_type ~amount:(float_of_int (10 + i mod 90)) ())
      ~lifecycle:(if i mod 10 = 0 then Consumable (i mod 5 + 1) else Eternal)
      ()
  )