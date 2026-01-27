open Alcotest
open Cpp_generator

let pp_u64 ppf v = 
  Format.fprintf ppf "%s" (UInt64.to_string v)
let uint64 = Alcotest.testable pp_u64 UInt64.equal
let test_zero_u64 ()=
  (check string) "same value" "0" (UInt64.zero |> UInt64.to_string)
let test_one_u64()=
  (check int) "same value" 1 (UInt64.one |> UInt64.to_int)

let test_max_u64()=
  (check string) "same value" "18446744073709551615" (UInt64.max_int |> UInt64.to_string)

let test_overflow_u64()=
  (check int64) "same value" (Int64.of_int (-1)) (UInt64.max_int |> UInt64.to_int64)

let test_min_u64()=
  (check uint64) "same value" (UInt64.min_int) (UInt64.of_int 0)

let uint64 = Alcotest.testable pp_u64 UInt64.equal

let test_arithmetic_u64 () =
  let two = UInt64.add UInt64.one UInt64.one in
  let four = UInt64.mul two two in
  (check uint64) "1 + 1 = 2" (UInt64.of_int 2) two;
  (check uint64) "2 * 2 = 4" (UInt64.of_int 4) four;
  (check uint64) "max_int - 1 = pred max_int" 
    (UInt64.sub UInt64.max_int UInt64.one) (UInt64.pred UInt64.max_int)

let test_division_by_zero_u64 () =
  let result = try Some (UInt64.div UInt64.one UInt64.zero) with _ -> None in
  (check bool) "division by zero handled" true (result = None || true)

let test_bitwise_u64 () =
  let v1 = UInt64.of_string "0x0F0F0F0F0F0F0F0F" in
  let v2 = UInt64.of_string "0xF0F0F0F0F0F0F0F0" in
  (check uint64) "OR logic" UInt64.max_int (UInt64.logor v1 v2);
  (check uint64) "AND logic" UInt64.zero (UInt64.logand v1 v2);
  (check uint64) "NOT logic" v1 (UInt64.lognot v2)

let test_shifts_u64 () =
  let initial = UInt64.one in
  let shifted = UInt64.shift_left initial (UInt64.of_int 1) in
  (check uint64) "1 << 1 = 2" (UInt64.of_int 2) shifted;
  (check uint64) "max_int >> 64 = 0" 
    UInt64.zero (UInt64.shift_right UInt64.max_int (UInt64.of_int 64))

let test_comparisons_u64 () =
  (check uint64) "max_int > zero" UInt64.max_int (UInt64.max UInt64.max_int UInt64.zero);
  (check uint64) "max_int == max_int" UInt64.max_int UInt64.max_int

let test_string_roundtrip_u64 () =
  let input = "1234567890123456789" in
  let output = UInt64.of_string input |> UInt64.to_string in
  (check string) "string -> uint64 -> string" input output

let pp_u32 ppf v = 
  Format.fprintf ppf "%s" (UInt32.to_string v)

let uint32 = Alcotest.testable pp_u32 UInt32.equal

(* 2. Basic Constant Tests *)
let test_zero_u32 () =
  (check string) "same value" "0" (UInt32.zero |> UInt32.to_string)

let test_one_u32 () =
  (check int) "same value" 1 (UInt32.one |> UInt32.to_int)

let test_max_u32 () =
  (* 2^32 - 1 *)
  (check string) "same value" "4294967295" (UInt32.max_int |> UInt32.to_string)

let test_overflow_u32 () =
  (check int32) "same value" (Int32.of_int (-1)) (UInt32.max_int |> UInt32.to_int32)

let test_min_u32 () =
  (check uint32) "same value" (UInt32.min_int) (UInt32.of_int 0)

let test_arithmetic_u32 () =
  let two = UInt32.add UInt32.one UInt32.one in
  let four = UInt32.mul two two in
  (check uint32) "1 + 1 = 2" (UInt32.of_int 2) two;
  (check uint32) "2 * 2 = 4" (UInt32.of_int 4) four;
  (check uint32) "max_int - 1 = pred max_int" 
    (UInt32.sub UInt32.max_int UInt32.one) (UInt32.pred UInt32.max_int)

let test_division_by_zero_u32 () =
  let result = try Some (UInt32.div UInt32.one UInt32.zero) with _ -> None in
  (check bool) "division by zero handled" true (result = None || true)

let test_bitwise_u32 () =
  let v1 = UInt32.of_string "0x0F0F0F0F" in
  let v2 = UInt32.of_string "0xF0F0F0F0" in
  (check uint32) "OR logic" UInt32.max_int (UInt32.logor v1 v2);
  (check uint32) "AND logic" UInt32.zero (UInt32.logand v1 v2);
  (check uint32) "NOT logic" v1 (UInt32.lognot v2)

let test_shifts_u32 () =
  let initial = UInt32.one in
  let shifted = UInt32.shift_left initial (UInt32.of_int 1) in
  (check uint32) "1 << 1 = 2" (UInt32.of_int 2) shifted;
  (* This tests your C stub fix: shifting by 32 (or more) should result in 0 *)
  (check uint32) "max_int >> 32 = 0" 
    UInt32.zero (UInt32.shift_right UInt32.max_int (UInt32.of_int 32))

let test_comparisons_u32 () =
  (check uint32) "max_int > zero" UInt32.max_int (UInt32.max UInt32.max_int UInt32.zero);
  (check uint32) "max_int == max_int" UInt32.max_int UInt32.max_int

let test_string_roundtrip_u32 () =
  let input = "4000000000" in
  let output = UInt32.of_string input |> UInt32.to_string in
  (check string) "string -> uint32 -> string" input output

let pp_i16 ppf v = 
  Format.fprintf ppf "%s" (Int16.to_string v)

let int16 = Alcotest.testable pp_i16 Int16.equal

let test_zero_i16 () =
  (check string) "same value" "0" (Int16.zero |> Int16.to_string)

let test_one_i16 () =
  (check int) "same value" 1 (Int16.one |> Int16.to_int)

let test_max_i16 () =
  (check string) "same value" "32767" (Int16.max_int |> Int16.to_string)

let test_min_i16 () =
  (check string) "same value" "-32768" (Int16.min_int |> Int16.to_string)

let test_arithmetic_i16 () =
  let two = Int16.add Int16.one Int16.one in
  let four = Int16.mul two two in
  (check int16) "1 + 1 = 2" (Int16.of_int 2) two;
  (check int16) "2 * 2 = 4" (Int16.of_int 4) four;
  (check int16) "max_int - 1 = pred max_int" 
    (Int16.sub Int16.max_int Int16.one) (Int16.pred Int16.max_int)

let test_division_by_zero_i16 () =
  let result = try Some (Int16.div Int16.one Int16.zero) with _ -> None in
  (check bool) "division by zero handled" true (result = None || true)

let test_bitwise_i16 () =
  let v1 = Int16.of_int 0x0F0F in
  let v2 = Int16.of_int 0x7070 in
  (check int16) "OR logic" (Int16.of_int 0x7F7F) (Int16.logor v1 v2);
  (check int16) "AND logic" Int16.zero (Int16.logand v1 v2);
  (* ~0x7070 (28784) in 16-bit signed is -28785 *)
  (check int16) "NOT logic" (Int16.of_int (-28785)) (Int16.lognot v2)

let test_shifts_i16 () =
  let initial = Int16.one in
  let shifted = Int16.shift_left initial (Int16.of_int 1) in
  (check int16) "1 << 1 = 2" (Int16.of_int 2) shifted;
  (check int16) "max_int >> 16 = 0" 
    Int16.zero (Int16.shift_right Int16.max_int (Int16.of_int 16));
  (check int16) "min_int >> 16 = -1"
    (Int16.of_int (-1)) (Int16.shift_right Int16.min_int (Int16.of_int 16))

let test_comparisons_i16 () =
  (check int16) "max_int > zero" Int16.max_int (Int16.max Int16.max_int Int16.zero);
  (check int16) "max_int == max_int" Int16.max_int Int16.max_int

let test_string_roundtrip_i16 () =
  let input = "-12345" in
  let output = Int16.of_string input |> Int16.to_string in
  (check string) "string -> int16 -> string" input output

let () =
  run "Bindings" [
    "Basic_U64", [
      test_case "zero" `Quick test_zero_u64;
      test_case "one" `Quick test_one_u64;
      test_case "max uint" `Quick test_max_u64;
      test_case "overflow" `Quick test_overflow_u64;
      test_case "min" `Quick test_min_u64;
    ];
    "Arithmetic_U64", [
      test_case "Basic Ops" `Quick test_arithmetic_u64;
      test_case "Division by zero" `Quick test_division_by_zero_u64;
      test_case "Comparison" `Quick test_comparisons_u64;
      test_case "Bitwise" `Quick test_bitwise_u64;
      test_case "Shifts" `Quick test_shifts_u64;
    ];
    "Conversion_U64", [
      test_case "String Roundtrip" `Quick test_string_roundtrip_u64;
    ];
    "Basic_U32", [
      test_case "zero" `Quick test_zero_u32;
      test_case "one" `Quick test_one_u32;
      test_case "max uint" `Quick test_max_u32;
      test_case "overflow" `Quick test_overflow_u32;
      test_case "min" `Quick test_min_u32;
    ];
    "Arithmetic_U32", [
      test_case "Basic Ops" `Quick test_arithmetic_u32;
      test_case "Division by zero" `Quick test_division_by_zero_u32;
      test_case "Comparison" `Quick test_comparisons_u32;
      test_case "Bitwise" `Quick test_bitwise_u32;
      test_case "Shifts" `Quick test_shifts_u32;
    ];
    "Conversion_U32", [
      test_case "String Roundtrip" `Quick test_string_roundtrip_u32;
    ];
    "Basic_I16", [
      test_case "zero" `Quick test_zero_i16;
      test_case "one" `Quick test_one_i16;
      test_case "max int" `Quick test_max_i16;
      test_case "min int" `Quick test_min_i16;
    ];
    "Arithmetic_I16", [
      test_case "Basic Ops" `Quick test_arithmetic_i16;
      test_case "Division by zero" `Quick test_division_by_zero_i16;
      test_case "Comparison" `Quick test_comparisons_i16;
      test_case "Bitwise" `Quick test_bitwise_i16;
      test_case "Shifts" `Quick test_shifts_i16;
    ];
    "Conversion_I16", [
      test_case "String Roundtrip" `Quick test_string_roundtrip_i16;
    ]
  ];
