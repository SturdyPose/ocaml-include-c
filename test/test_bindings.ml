open Alcotest
open Cpp_generator
open CInteract
open Alcotest_helpers
open CArray

let test_ptrs () =
  let pValStart = ptr_alloc ~size: 10n in
  let pVal = ref pValStart in
  for i = 0 to 4 do
    poke_i16 ~ptr:!pVal ~value:(Int16.of_int i);
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;
  pVal := pValStart;
  for i = 0 to 4 do
    let v = peek_i16 ~ptr: !pVal in
    Alcotest.(check int16) "Expecting same value as in loop" (Int16.of_int i) v;
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;
  ptr_free ~ptr: pValStart

let test_foreach() =
  let pValStart = ptr_alloc ~size: 10n in
  let arr : Int16.t carray = {
    length = 5;
    size_of_element = 2;
    ptr = pValStart
  } in
  let pVal = ref pValStart in
  for i = 0 to 4 do
    poke_i16 ~ptr:!pVal ~value:(Int16.of_int i);
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;

  for_each (function pi16 -> let x = peek_i16 ~ptr:pi16 in poke_i16 ~ptr:pi16 ~value:(Int16.succ x)) arr;
  pVal := pValStart;
  for i = 0 to 4 do
    let v = peek_i16 ~ptr: !pVal in
    Alcotest.(check int16) "Expecting +1 value as in loop" (Int16.of_int (i + 1)) v;
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;

  ptr_free ~ptr:pValStart;
  ()


let test_foreach_n() =
  let pValStart = ptr_alloc ~size: 10n in
  let arr : Int16.t carray = {
    length = 5;
    size_of_element = 2;
    ptr = pValStart
  } in
  let pVal = ref pValStart in
  for i = 0 to arr.length - 1 do
    poke_i16 ~ptr:!pVal ~value:(Int16.of_int i);
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;
  for_each (function pi16 -> let x = peek_n ~ptr:pi16 ~size:2n in poke_i16 ~ptr:pi16 ~value:(Int16.succ x)) arr;
  pVal := pValStart;
  for i = 0 to arr.length - 1 do
    let v = peek_i16 ~ptr: !pVal in
    Alcotest.(check int16) "Expecting +1 value as in loop" (Int16.of_int (i + 1)) v;
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;

  ptr_free ~ptr:pValStart;
  ()


let test_foreach_ref() =
  try
  begin
  let pValStart = ptr_alloc ~size: 10n in
  let arr : Int16.t carray = {
    length = 5;
    size_of_element = 2;
    ptr = pValStart
  } in
  let pVal = ref pValStart in
  for i = 0 to arr.length-1 do
    poke_i16 ~ptr:!pVal ~value:(Int16.of_int i);
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;
  for_each_ref (function i16 -> i16 := Int16.succ !i16;) arr;
  pVal := pValStart;
  for i = 0 to arr.length-1 do
    let v = peek_i16 ~ptr: !pVal in
    Alcotest.(check int16) "Expecting +1 value" (Int16.of_int (i + 1)) v;
    pVal := ptr_add ~ptr:!pVal ~offset:2n
  done;

  ptr_free ~ptr:pValStart;
  ()
  end
  with exc -> "Got exception: " ^ Printexc.to_string exc |> Alcotest.fail

let test_peek_poke_n_unboxed () =
  let inputVal = 420 in
  let word_size_native = Nativeint.of_int word_size in
  let pVal = ptr_alloc ~size:word_size_native in
  poke_n ~ptr:pVal ~value:inputVal ~size:word_size_native;
  let resVal = peek_n ~ptr:pVal ~size:word_size_native in
  Alcotest.(check int) "Expecting same input value as read value" inputVal resVal;
  ptr_free ~ptr:pVal;
  ()

let test_peek_poke_n_boxed () =
  let inputVal = Int32.of_int 420 in
  let pVal = ptr_alloc ~size:4n in
  poke_n ~ptr:pVal ~value:inputVal ~size:4n;
  let resVal = peek_n ~ptr:pVal ~size:4n in
  Alcotest.(check int32) "Expecting same input value as read value" inputVal resVal;
  ptr_free ~ptr:pVal;
  ()


let pointerTests () = [
    "Ptr_test", [
      test_case "poke n peek unboxed" `Quick test_peek_poke_n_unboxed;
      test_case "poke n peek boxed" `Quick test_peek_poke_n_boxed;
    ];
    "Array_test", [
      test_case "fill array" `Quick test_ptrs;
      test_case "use for_each" `Quick test_foreach;
      test_case "use for_each_n" `Quick test_foreach_n;
      test_case "use for_each_ref" `Quick test_foreach_ref;
    ];
]


let () =
  Testing.hook_fail_functions ();
  run "Bindings" (List.concat [Test_ints.tests (); pointerTests ()])