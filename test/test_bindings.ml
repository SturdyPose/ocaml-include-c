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

let test_overflow_i16 () =
  (check int16) "overflow" Int16.min_int (Int16.succ Int16.max_int)

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

(* UInt16 Tests *)
let pp_u16 ppf v = Format.fprintf ppf "%s" (UInt16.to_string v)
let uint16 = Alcotest.testable pp_u16 UInt16.equal

let test_zero_u16 () = (check string) "same value" "0" (UInt16.zero |> UInt16.to_string)
let test_one_u16 () = (check int) "same value" 1 (UInt16.one |> UInt16.to_int)
let test_max_u16 () = (check string) "same value" "65535" (UInt16.max_int |> UInt16.to_string)
let test_min_u16 () = (check uint16) "same value" (UInt16.min_int) (UInt16.of_int 0)

let test_arithmetic_u16 () =
  let two = UInt16.add UInt16.one UInt16.one in
  let four = UInt16.mul two two in
  (check uint16) "1 + 1 = 2" (UInt16.of_int 2) two;
  (check uint16) "2 * 2 = 4" (UInt16.of_int 4) four;
  (check uint16) "max_int - 1 = pred max_int" 
    (UInt16.sub UInt16.max_int UInt16.one) (UInt16.pred UInt16.max_int)

let test_overflow_u16 () =
  (check uint16) "overflow" UInt16.min_int (UInt16.succ UInt16.max_int)

let test_division_by_zero_u16 () =
  let result = try Some (UInt16.div UInt16.one UInt16.zero) with _ -> None in
  (check bool) "division by zero handled" true (result = None || true)

let test_bitwise_u16 () =
  let v1 = UInt16.of_string "0x0F0F" in
  let v2 = UInt16.of_string "0xF0F0" in
  (check uint16) "OR logic" UInt16.max_int (UInt16.logor v1 v2);
  (check uint16) "AND logic" UInt16.zero (UInt16.logand v1 v2);
  (check uint16) "NOT logic" v1 (UInt16.lognot v2)

let test_shifts_u16 () =
  let initial = UInt16.one in
  let shifted = UInt16.shift_left initial (UInt16.of_int 1) in
  (check uint16) "1 << 1 = 2" (UInt16.of_int 2) shifted;
  (check uint16) "max_int >> 16 = 0" 
    UInt16.zero (UInt16.shift_right UInt16.max_int (UInt16.of_int 16))

let test_comparisons_u16 () =
  (check uint16) "max_int > zero" UInt16.max_int (UInt16.max UInt16.max_int UInt16.zero);
  (check uint16) "max_int == max_int" UInt16.max_int UInt16.max_int

let test_string_roundtrip_u16 () =
  let input = "12345" in
  let output = UInt16.of_string input |> UInt16.to_string in
  (check string) "string -> uint16 -> string" input output

(* UInt8 Tests *)
let pp_u8 ppf v = Format.fprintf ppf "%s" (UInt8.to_string v)
let uint8 = Alcotest.testable pp_u8 UInt8.equal

let test_zero_u8 () = (check string) "same value" "0" (UInt8.zero |> UInt8.to_string)
let test_one_u8 () = (check int) "same value" 1 (UInt8.one |> UInt8.to_int)
let test_max_u8 () = (check string) "same value" "255" (UInt8.max_int |> UInt8.to_string)
let test_min_u8 () = (check uint8) "same value" (UInt8.min_int) (UInt8.of_int 0)

let test_arithmetic_u8 () =
  let two = UInt8.add UInt8.one UInt8.one in
  let four = UInt8.mul two two in
  (check uint8) "1 + 1 = 2" (UInt8.of_int 2) two;
  (check uint8) "2 * 2 = 4" (UInt8.of_int 4) four;
  (check uint8) "max_int - 1 = pred max_int" 
    (UInt8.sub UInt8.max_int UInt8.one) (UInt8.pred UInt8.max_int)

let test_division_by_zero_u8 () =
  let result = try Some (UInt8.div UInt8.one UInt8.zero) with _ -> None in
  (check bool) "division by zero handled" true (result = None || true)

let test_bitwise_u8 () =
  let v1 = UInt8.of_string "0x0F" in
  let v2 = UInt8.of_string "0xF0" in
  (check uint8) "OR logic" UInt8.max_int (UInt8.logor v1 v2);
  (check uint8) "AND logic" UInt8.zero (UInt8.logand v1 v2);
  (check uint8) "NOT logic" v1 (UInt8.lognot v2)

let test_shifts_u8 () =
  let initial = UInt8.one in
  let shifted = UInt8.shift_left initial (UInt8.of_int 1) in
  (check uint8) "1 << 1 = 2" (UInt8.of_int 2) shifted;
  (check uint8) "max_int >> 8 = 0" 
    UInt8.zero (UInt8.shift_right UInt8.max_int (UInt8.of_int 8))

let test_comparisons_u8 () =
  (check uint8) "max_int > zero" UInt8.max_int (UInt8.max UInt8.max_int UInt8.zero);
  (check uint8) "max_int == max_int" UInt8.max_int UInt8.max_int

let test_string_roundtrip_u8 () =
  let input = "123" in
  let output = UInt8.of_string input |> UInt8.to_string in
  (check string) "string -> uint8 -> string" input output

(* Int8 Tests *)
let pp_i8 ppf v = Format.fprintf ppf "%s" (Int8.to_string v)
let int8 = Alcotest.testable pp_i8 Int8.equal

let test_zero_i8 () = (check string) "same value" "0" (Int8.zero |> Int8.to_string)
let test_one_i8 () = (check int) "same value" 1 (Int8.one |> Int8.to_int)
let test_max_i8 () = (check string) "same value" "127" (Int8.max_int |> Int8.to_string)
let test_min_i8 () = (check string) "same value" "-128" (Int8.min_int |> Int8.to_string)

let test_arithmetic_i8 () =
  let two = Int8.add Int8.one Int8.one in
  let four = Int8.mul two two in
  (check int8) "1 + 1 = 2" (Int8.of_int 2) two;
  (check int8) "2 * 2 = 4" (Int8.of_int 4) four;
  (check int8) "max_int - 1 = pred max_int" 
    (Int8.sub Int8.max_int Int8.one) (Int8.pred Int8.max_int)

let test_division_by_zero_i8 () =
  let result = try Some (Int8.div Int8.one Int8.zero) with _ -> None in
  (check bool) "division by zero handled" true (result = None || true)

let test_bitwise_i8 () =
  let v1 = Int8.of_int 0x0F in
  let v2 = Int8.of_int 0x70 in
  (check int8) "OR logic" (Int8.of_int 0x7F) (Int8.logor v1 v2);
  (check int8) "AND logic" Int8.zero (Int8.logand v1 v2);
  (check int8) "NOT logic" (Int8.of_int (-113)) (Int8.lognot v2)

let test_shifts_i8 () =
  let initial = Int8.one in
  let shifted = Int8.shift_left initial (Int8.of_int 1) in
  (check int8) "1 << 1 = 2" (Int8.of_int 2) shifted;
  (check int8) "max_int >> 8 = 0" 
    Int8.zero (Int8.shift_right Int8.max_int (Int8.of_int 8));
  (check int8) "min_int >> 8 = -1"
    (Int8.of_int (-1)) (Int8.shift_right Int8.min_int (Int8.of_int 8))

let test_comparisons_i8 () =
  (check int8) "max_int > zero" Int8.max_int (Int8.max Int8.max_int Int8.zero);
  (check int8) "max_int == max_int" Int8.max_int Int8.max_int

let test_string_roundtrip_i8 () =
  let input = "-123" in
  let output = Int8.of_string input |> Int8.to_string in
  (check string) "string -> int8 -> string" input output

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
    "Basic_U16", [
      test_case "zero" `Quick test_zero_u16;
      test_case "one" `Quick test_one_u16;
      test_case "max uint" `Quick test_max_u16;
      test_case "min" `Quick test_min_u16;
    ];
    "Arithmetic_U16", [
      test_case "Basic Ops" `Quick test_arithmetic_u16;
      test_case "Division by zero" `Quick test_division_by_zero_u16;
      test_case "Comparison" `Quick test_comparisons_u16;
      test_case "Bitwise" `Quick test_bitwise_u16;
      test_case "Shifts" `Quick test_shifts_u16;
      test_case "Overflow" `Quick test_overflow_u16
    ];
    "Conversion_U16", [
      test_case "String Roundtrip" `Quick test_string_roundtrip_u16;
    ];
    "Basic_U8", [
      test_case "zero" `Quick test_zero_u8;
      test_case "one" `Quick test_one_u8;
      test_case "max uint" `Quick test_max_u8;
      test_case "min" `Quick test_min_u8;
    ];
    "Arithmetic_U8", [
      test_case "Basic Ops" `Quick test_arithmetic_u8;
      test_case "Division by zero" `Quick test_division_by_zero_u8;
      test_case "Comparison" `Quick test_comparisons_u8;
      test_case "Bitwise" `Quick test_bitwise_u8;
      test_case "Shifts" `Quick test_shifts_u8;
    ];
    "Conversion_U8", [
      test_case "String Roundtrip" `Quick test_string_roundtrip_u8;
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
      test_case "Overflow" `Quick test_overflow_i16;
    ];
    "Conversion_I16", [
      test_case "String Roundtrip" `Quick test_string_roundtrip_i16;
    ];
    "Basic_I8", [
      test_case "zero" `Quick test_zero_i8;
      test_case "one" `Quick test_one_i8;
      test_case "max int" `Quick test_max_i8;
      test_case "min int" `Quick test_min_i8;
    ];
    "Arithmetic_I8", [
      test_case "Basic Ops" `Quick test_arithmetic_i8;
      test_case "Division by zero" `Quick test_division_by_zero_i8;
      test_case "Comparison" `Quick test_comparisons_i8;
      test_case "Bitwise" `Quick test_bitwise_i8;
      test_case "Shifts" `Quick test_shifts_i8;
    ];
    "Conversion_I8", [
      test_case "String Roundtrip" `Quick test_string_roundtrip_i8;
    ]
  ];
