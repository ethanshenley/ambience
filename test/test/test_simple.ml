(** Simple test to verify test framework works *)

open Alcotest

let test_basic () =
  check int "one plus one" 2 (1 + 1)

let test_string () =
  check string "hello" "hello" "hello"

let () =
  run "Simple Tests" [
    "Basic", [
      test_case "Addition" `Quick test_basic;
      test_case "String" `Quick test_string;
    ];
  ]