open Alcotest

let () =
  run "Bindings" (Test_ints.tests ())