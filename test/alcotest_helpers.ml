open Cpp_generator

let pp_u64 ppf v = 
  Format.fprintf ppf "%s" (UInt64.to_string v)
let uint64 = Alcotest.testable pp_u64 UInt64.equal

let pp_u32 ppf v = 
  Format.fprintf ppf "%s" (UInt32.to_string v)
let uint32 = Alcotest.testable pp_u32 UInt32.equal

let pp_i16 ppf v = 
  Format.fprintf ppf "%s" (Int16.to_string v)
let int16 = Alcotest.testable pp_i16 Int16.equal

let pp_u16 ppf v = 
  Format.fprintf ppf "%s" (UInt16.to_string v)
let uint16 = Alcotest.testable pp_u16 UInt16.equal

let pp_u8 ppf v = 
  Format.fprintf ppf "%s" (UInt8.to_string v)
let uint8 = Alcotest.testable pp_u8 UInt8.equal

let pp_i8 ppf v = 
  Format.fprintf ppf "%s" (Int8.to_string v)
let int8 = Alcotest.testable pp_i8 Int8.equal
