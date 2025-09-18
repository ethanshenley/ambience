(** Time Provider Test Suite *)

open Alcotest
open Ambience_core

(** Test real time mode *)
let test_real_time_mode () =
  Time_provider.reset ();  (* Ensure clean state *)
  Time_provider.use_real_time ();

  (* Get two timestamps with a small delay *)
  let t1 = Time_provider.now () in
  Unix.sleepf 0.1;  (* Sleep 100ms to ensure measurable difference *)
  let t2 = Time_provider.now () in

  (* In real time mode, t2 should be greater than or equal to t1 *)
  check bool "Real time advances" true (t2 >= t1);

  (* Check mode is RealTime *)
  match Time_provider.get_mode () with
  | Time_provider.RealTime -> ()
  | _ -> fail "Expected RealTime mode"

(** Test mock time mode *)
let test_mock_time_mode () =
  Time_provider.use_mock_time 1000.0;

  (* Get multiple timestamps - should all be the same *)
  let t1 = Time_provider.now () in
  let t2 = Time_provider.now () in
  let t3 = Time_provider.now () in

  check (float 0.0) "Mock time stays fixed" 1000.0 t1;
  check (float 0.0) "Mock time consistent" t1 t2;
  check (float 0.0) "Mock time consistent" t2 t3;

  (* Check mode is MockTime *)
  match Time_provider.get_mode () with
  | Time_provider.MockTime t -> check (float 0.0) "Mock time value" 1000.0 t
  | _ -> fail "Expected MockTime mode"

(** Test setting mock time *)
let test_set_time () =
  Time_provider.use_mock_time 1000.0;

  (* Set to new time *)
  Time_provider.set_time 2000.0;
  let t1 = Time_provider.now () in
  check (float 0.0) "Time set correctly" 2000.0 t1;

  (* Set to another time *)
  Time_provider.set_time 500.0;
  let t2 = Time_provider.now () in
  check (float 0.0) "Time set again" 500.0 t2

(** Test advancing mock time *)
let test_advance_time () =
  Time_provider.use_mock_time 1000.0;

  (* Advance by 100 *)
  Time_provider.advance_time 100.0;
  let t1 = Time_provider.now () in
  check (float 0.0) "Time advanced" 1100.0 t1;

  (* Advance by negative (go back) *)
  Time_provider.advance_time (-50.0);
  let t2 = Time_provider.now () in
  check (float 0.0) "Time went back" 1050.0 t2

(** Test auto-advance *)
let test_auto_advance () =
  Time_provider.use_mock_time 1000.0;
  Time_provider.set_auto_advance (Some 10.0);

  (* Each call should advance by 10 *)
  let t1 = Time_provider.now () in
  let t2 = Time_provider.now () in
  let t3 = Time_provider.now () in

  check (float 0.0) "First call" 1000.0 t1;
  check (float 0.0) "Second call advances" 1010.0 t2;
  check (float 0.0) "Third call advances" 1020.0 t3;

  (* Disable auto-advance *)
  Time_provider.set_auto_advance None;
  let t4 = Time_provider.now () in
  let t5 = Time_provider.now () in
  check (float 0.0) "No more advance" t4 t5

(** Test error handling in real time mode *)
let test_real_time_errors () =
  Time_provider.use_real_time ();

  (* Should fail to set time in real mode *)
  check bool "Cannot set time in real mode" true
    (try
       Time_provider.set_time 1000.0;
       false
     with Failure _ -> true);

  (* Should fail to advance time in real mode *)
  check bool "Cannot advance time in real mode" true
    (try
       Time_provider.advance_time 100.0;
       false
     with Failure _ -> true)

(** Test utility functions *)
let test_utils () =
  Time_provider.use_mock_time 1000.0;

  (* Test is_past *)
  check bool "500 is past" true (Time_provider.Utils.is_past 500.0);
  check bool "1500 is not past" false (Time_provider.Utils.is_past 1500.0);

  (* Test is_future *)
  check bool "1500 is future" true (Time_provider.Utils.is_future 1500.0);
  check bool "500 is not future" false (Time_provider.Utils.is_future 500.0);

  (* Test time_until *)
  check (float 0.01) "Time until 1500" 500.0 (Time_provider.Utils.time_until 1500.0);
  check (float 0.01) "Time until past is 0" 0.0 (Time_provider.Utils.time_until 500.0);

  (* Test time_since *)
  check (float 0.01) "Time since 500" 500.0 (Time_provider.Utils.time_since 500.0);
  check (float 0.01) "Time since future is 0" 0.0 (Time_provider.Utils.time_since 1500.0);

  (* Test in_window *)
  check bool "1000 in window [500, 1500]" true
    (Time_provider.Utils.in_window 500.0 1500.0);
  check bool "1000 not in window [100, 900]" false
    (Time_provider.Utils.in_window 100.0 900.0)

(** Test ISO 8601 formatting *)
let test_iso8601 () =
  (* Test known timestamp *)
  let timestamp = 1609459200.0 in  (* 2021-01-01 00:00:00 UTC *)
  let iso = Time_provider.Utils.to_iso8601 timestamp in
  check string "ISO 8601 format" "2021-01-01T00:00:00Z" iso;

  (* Test round-trip *)
  let parsed = Time_provider.Utils.from_iso8601 iso in
  match parsed with
  | Some t -> check (float 0.01) "Round-trip timestamp" timestamp t
  | None -> fail "Failed to parse ISO 8601"

(** Test configuration save/restore *)
let test_save_restore () =
  (* Start with real time *)
  Time_provider.use_real_time ();

  (* Save config *)
  let saved = Time_provider.Test.save_config () in

  (* Change to mock time *)
  Time_provider.use_mock_time 5000.0;
  Time_provider.set_auto_advance (Some 5.0);

  (* Verify changed *)
  let t1 = Time_provider.now () in
  check (float 0.0) "Mock time active" 5000.0 t1;

  (* Restore *)
  Time_provider.Test.restore_config saved;

  (* Should be back to real time *)
  match Time_provider.get_mode () with
  | Time_provider.RealTime -> ()
  | _ -> fail "Should be restored to RealTime"

(** Test with_mock_time helper *)
let test_with_mock_time () =
  Time_provider.use_real_time ();

  (* Run function with mock time *)
  let result = Time_provider.Test.with_mock_time 3000.0 (fun () ->
    let t = Time_provider.now () in
    check (float 0.0) "Mock time in function" 3000.0 t;
    "test_result"
  ) in

  check string "Function result" "test_result" result;

  (* Should be back to real time *)
  match Time_provider.get_mode () with
  | Time_provider.RealTime -> ()
  | _ -> fail "Should be back to RealTime"

(** Test with_auto_advance helper *)
let test_with_auto_advance () =
  Time_provider.use_real_time ();

  (* Run function with auto-advancing mock time *)
  let times = Time_provider.Test.with_auto_advance 1000.0 10.0 (fun () ->
    let t1 = Time_provider.now () in
    let t2 = Time_provider.now () in
    let t3 = Time_provider.now () in
    [t1; t2; t3]
  ) in

  check (float 0.0) "First time" 1000.0 (List.nth times 0);
  check (float 0.0) "Second time" 1010.0 (List.nth times 1);
  check (float 0.0) "Third time" 1020.0 (List.nth times 2);

  (* Should be back to real time *)
  match Time_provider.get_mode () with
  | Time_provider.RealTime -> ()
  | _ -> fail "Should be back to RealTime"

(** Test reset function *)
let test_reset () =
  (* Set to mock time with auto-advance *)
  Time_provider.use_mock_time 2000.0;
  Time_provider.set_auto_advance (Some 5.0);

  (* Reset *)
  Time_provider.reset ();

  (* Should be real time, no auto-advance *)
  match Time_provider.get_mode () with
  | Time_provider.RealTime -> ()
  | _ -> fail "Should be RealTime after reset"

(** Export test suite for main runner *)
let tests = [
  "Basic", [
    test_case "Real time mode" `Quick test_real_time_mode;
    test_case "Mock time mode" `Quick test_mock_time_mode;
    test_case "Set time" `Quick test_set_time;
    test_case "Advance time" `Quick test_advance_time;
  ];
  "Advanced", [
    test_case "Auto-advance" `Quick test_auto_advance;
    test_case "Real time errors" `Quick test_real_time_errors;
  ];
  "Utilities", [
    test_case "Time utilities" `Quick test_utils;
    test_case "ISO 8601" `Quick test_iso8601;
  ];
  "Test Helpers", [
    test_case "Save/restore config" `Quick test_save_restore;
    test_case "with_mock_time" `Quick test_with_mock_time;
    test_case "with_auto_advance" `Quick test_with_auto_advance;
    test_case "Reset" `Quick test_reset;
  ];
]

(** Standalone runner for this test module *)
let () =
  if Array.length Sys.argv > 1 && Sys.argv.(0) = "test_time_provider" then
    run "Time Provider Tests" tests