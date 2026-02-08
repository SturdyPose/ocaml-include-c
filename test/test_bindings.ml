open Alcotest
open Cpp_generator
open CInteract
open Alcotest_helpers

let test_ptrs () =
  let pValStart = ptr_alloc ~size: 10n in
  let pVal = ref pValStart in
  for i = 0 to 4 do
    poke_i16 ~p:!pVal ~value:(Int16.of_int i);
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;
  pVal := pValStart;
  for i = 0 to 4 do
    let v = peek_i16 ~p: !pVal in
    Alcotest.(check int16) "Expecting same value as in loop" (Int16.of_int i) v;
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;
  ptr_free pValStart


let pointerTests () = [
    "Array test", [
      test_case "zero" `Quick test_ptrs;
    ];
]


let () =
  run "Bindings" (List.concat [Test_ints.tests (); pointerTests ()])