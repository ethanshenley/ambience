(** Main test runner for the Ambient Commerce Protocol *)

open Alcotest
open Ambience_core

let () =
  (* Always start tests with real time to ensure clean state *)
  Time_provider.reset ();

  (* Run all test suites *)
  run "Ambient Commerce Protocol" (
    (* Core functionality *)
    List.map (fun (name, tests) -> ("Core.TimeProvider." ^ name, tests)) Test_time_provider.tests @
    List.map (fun (name, tests) -> ("Core.Types." ^ name, tests)) Test_core_types.tests @
    List.map (fun (name, tests) -> ("Core.Intent." ^ name, tests)) Test_intent.tests
    (* Future test modules:
       @ List.map (fun (name, tests) -> ("Core.State." ^ name, tests)) Test_state.tests
       @ List.map (fun (name, tests) -> ("Matching.Engine." ^ name, tests)) Test_matching.tests
       @ List.map (fun (name, tests) -> ("Settlement." ^ name, tests)) Test_settlement.tests
       @ List.map (fun (name, tests) -> ("Network." ^ name, tests)) Test_network.tests
       @ List.map (fun (name, tests) -> ("Trust." ^ name, tests)) Test_trust.tests
    *)
  )