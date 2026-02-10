
type 'a ptr

module Int8 = struct
  type t
  external _zero : unit -> t = "caml_zero_int8"
  let zero = _zero ()
  external _one: unit -> t = "caml_one_int8"
  let one = _one ()
  external _max_int: unit -> t = "caml_max_int_int8"
  external _min_int: unit -> t = "caml_min_int_int8"
  let max_int = _max_int ()
  let min_int = _min_int ()
  external add: t -> t -> t = "caml_add_int8"
  external sub: t -> t -> t = "caml_sub_int8"
  external mul: t -> t -> t = "caml_mul_int8"
  external div: t -> t -> t = "caml_div_int8"
  external rem: t -> t -> t = "caml_rem_int8"
  external equal: t -> t -> bool = "caml_equal_int8"
  external min: t -> t -> t = "caml_min_int8"
  external max: t -> t -> t = "caml_max_int8"
  external of_int: int -> t = "caml_int_to_int8"
  external of_int32: int32 -> t = "caml_int32_to_int8"
  external of_int64: int64 -> t = "caml_int64_to_int8"
  external of_int64_opt: int64 -> t option = "caml_int64_to_int8_opt"
  external to_int32: t -> int32 = "caml_int8_to_int32"
  (* Doesn't do bound checking *)
  external to_int: t -> int = "caml_int8_to_int"
  external to_float: t -> float = "caml_int8_to_float"
  external of_nativeint: nativeint -> t = "caml_int8_to_nativeint"
  external to_nativeint: t -> nativeint = "caml_nativeint_to_int8"
  external to_nativeint_opt: t -> nativeint = "caml_nativeint_to_int8_opt"
  let succ a = add a one
  let pred a = sub a one
  external seeded_hash_param :
    int -> int -> int -> t -> int = "caml_hash" [@@noalloc]
  let seeded_hash seed x = seeded_hash_param 10 100 seed x
  external of_string : string -> t = "caml_string_to_int8"
  external of_stringopt : string -> t option = "caml_string_to_int8_opt"
  external format_int8: string -> t -> string = "caml_int8_to_string"
  external _formatter: unit -> string = "caml_int8_formatter"
  let formatter = "%" ^ _formatter ()
  let to_string x = format_int8 formatter x
  external logand: t -> t -> t = "caml_int8_logand"
  external logor: t -> t -> t = "caml_int8_logor"
  external logxor: t -> t -> t = "caml_int8_logxor"
  external lognot: t -> t = "caml_int8_lognot"
  external shift_left: t -> t -> t = "caml_int8_shift_left"
  external shift_right: t -> t -> t = "caml_int8_shift_right"
end


module Int16 = struct
  type t
  external _zero : unit -> t = "caml_zero_int16"
  let zero = _zero ()
  external _one: unit -> t = "caml_one_int16"
  let one = _one ()
  external _max_int: unit -> t = "caml_max_int_int16"
  external _min_int: unit -> t = "caml_min_int_int16"
  let max_int = _max_int ()
  let min_int = _min_int ()
  external add: t -> t -> t = "caml_add_int16"
  external sub: t -> t -> t = "caml_sub_int16"
  external mul: t -> t -> t = "caml_mul_int16"
  external div: t -> t -> t = "caml_div_int16"
  external rem: t -> t -> t = "caml_rem_int16"
  external equal: t -> t -> bool = "caml_equal_int16"
  external min: t -> t -> t = "caml_min_int16"
  external max: t -> t -> t = "caml_max_int16"
  external of_int: int -> t = "caml_int_to_int16"
  external of_int32: int32 -> t = "caml_int32_to_int16"
  external of_int64: int64 -> t = "caml_int64_to_int16"
  external of_int64_opt: int64 -> t option = "caml_int64_to_int16_opt"
  external to_int32: t -> int32 = "caml_int16_to_int32"
  (* Doesn't do bound checking *)
  external to_int: t -> int = "caml_int16_to_int"
  external to_float: t -> float = "caml_int16_to_float"
  external of_nativeint: nativeint -> t = "caml_int16_to_nativeint"
  external to_nativeint: t -> nativeint = "caml_nativeint_to_int16"
  external to_nativeint_opt: t -> nativeint = "caml_nativeint_to_int16_opt"
  let succ a = add a one
  let pred a = sub a one
  external seeded_hash_param :
    int -> int -> int -> t -> int = "caml_hash" [@@noalloc]
  let seeded_hash seed x = seeded_hash_param 10 100 seed x
  external of_string : string -> t = "caml_string_to_int16"
  external of_stringopt : string -> t option = "caml_string_to_int16_opt"
  external format_int16: string -> t -> string = "caml_int16_to_string"
  external _formatter: unit -> string = "caml_int16_formatter"
  let formatter = "%" ^ _formatter ()
  let to_string x = format_int16 formatter x
  external logand: t -> t -> t = "caml_int16_logand"
  external logor: t -> t -> t = "caml_int16_logor"
  external logxor: t -> t -> t = "caml_int16_logxor"
  external lognot: t -> t = "caml_int16_lognot"
  external shift_left: t -> t -> t = "caml_int16_shift_left"
  external shift_right: t -> t -> t = "caml_int16_shift_right"
end

module UInt8 = struct
  type t
  external _zero : unit -> t = "caml_zero_uint8"
  let zero = _zero ()
  external _one: unit -> t = "caml_one_uint8"
  let one = _one ()
  external _max_int: unit -> t = "caml_max_int_uint8"
  let max_int = _max_int ()
  let min_int: t = zero
  external add: t -> t -> t = "caml_add_uint8"
  external sub: t -> t -> t = "caml_sub_uint8"
  external mul: t -> t -> t = "caml_mul_uint8"
  external div: t -> t -> t = "caml_div_uint8"
  external rem: t -> t -> t = "caml_rem_uint8"
  external equal: t -> t -> bool = "caml_equal_uint8"
  external min: t -> t -> t = "caml_min_uint8"
  external max: t -> t -> t = "caml_max_uint8"
  external of_int: int -> t = "caml_int_to_uint8"
  external of_int32: int32 -> t = "caml_int32_to_uint8"
  external of_int64: int64 -> t = "caml_int64_to_uint8"
  external of_int64_opt: int64 -> t option = "caml_int64_to_uint8_opt"
  external to_int32: t -> int32 = "caml_uint8_to_int32"
  (* Doesn't do bound checking *)
  external to_int: t -> int = "caml_uint8_to_int"
  external to_float: t -> float = "caml_uint8_to_float"
  external of_nativeint: nativeint -> t = "caml_uint8_to_nativeint"
  external to_nativeint: t -> nativeint = "caml_nativeint_to_uint8"
  external to_nativeint_opt: t -> nativeint = "caml_nativeint_to_uint8_opt"
  let succ a = add a one
  let pred a = sub a one
  external seeded_hash_param :
    int -> int -> int -> t -> int = "caml_hash" [@@noalloc]
  let seeded_hash seed x = seeded_hash_param 10 100 seed x
  external of_string : string -> t = "caml_string_to_uint8"
  external of_stringopt : string -> t option = "caml_string_to_uint8_opt"
  external format_uint8: string -> t -> string = "caml_uint32_to_string"
  external _formatter: unit -> string = "caml_uint8_formatter"
  let formatter = "%" ^ _formatter ()
  let to_string x = format_uint8 formatter x
  external logand: t -> t -> t = "caml_uint8_logand"
  external logor: t -> t -> t = "caml_uint8_logor"
  external logxor: t -> t -> t = "caml_uint8_logxor"
  external lognot: t -> t = "caml_uint8_lognot"
  external shift_left: t -> t -> t = "caml_uint8_shift_left"
  external shift_right: t -> t -> t = "caml_uint8_shift_right"
end

module UInt16 = struct
  type t
  external _zero : unit -> t = "caml_zero_uint16"
  let zero = _zero ()
  external _one: unit -> t = "caml_one_uint16"
  let one = _one ()
  external _max_int: unit -> t = "caml_max_int_uint16"
  let max_int = _max_int ()
  let min_int: t = zero
  external add: t -> t -> t = "caml_add_uint16"
  external sub: t -> t -> t = "caml_sub_uint16"
  external mul: t -> t -> t = "caml_mul_uint16"
  external div: t -> t -> t = "caml_div_uint16"
  external rem: t -> t -> t = "caml_rem_uint16"
  external equal: t -> t -> bool = "caml_equal_uint16"
  external min: t -> t -> t = "caml_min_uint16"
  external max: t -> t -> t = "caml_max_uint16"
  external of_int: int -> t = "caml_int_to_uint16"
  external of_int32: int32 -> t = "caml_int32_to_uint16"
  external of_int64: int64 -> t = "caml_int64_to_uint16"
  external of_int64_opt: int64 -> t option = "caml_int64_to_uint16_opt"
  external to_int32: t -> int32 = "caml_uint16_to_int32"
  (* Doesn't do bound checking *)
  external to_int: t -> int = "caml_uint16_to_int"
  external to_float: t -> float = "caml_uint16_to_float"
  external of_nativeint: nativeint -> t = "caml_uint16_to_nativeint"
  external to_nativeint: t -> nativeint = "caml_nativeint_to_uint16"
  external to_nativeint_opt: t -> nativeint = "caml_nativeint_to_uint16_opt"
  let succ a = add a one
  let pred a = sub a one
  external seeded_hash_param :
    int -> int -> int -> t -> int = "caml_hash" [@@noalloc]
  let seeded_hash seed x = seeded_hash_param 10 100 seed x
  external of_string : string -> t = "caml_string_to_uint16"
  external of_stringopt : string -> t option = "caml_string_to_uint16_opt"
  external format_uint16: string -> t -> string = "caml_uint32_to_string"
  external _formatter: unit -> string = "caml_uint16_formatter"
  let formatter = "%" ^ _formatter ()
  let to_string x = format_uint16 formatter x
  external logand: t -> t -> t = "caml_uint16_logand"
  external logor: t -> t -> t = "caml_uint16_logor"
  external logxor: t -> t -> t = "caml_uint16_logxor"
  external lognot: t -> t = "caml_uint16_lognot"
  external shift_left: t -> t -> t = "caml_uint16_shift_left"
  external shift_right: t -> t -> t = "caml_uint16_shift_right"
end

module UInt32 = struct
  type t
  external _zero : unit -> t = "caml_zero_uint32"
  let zero = _zero ()
  external _one: unit -> t = "caml_one_uint32"
  let one = _one ()
  external _max_int: unit -> t = "caml_max_int_uint32"
  let max_int = _max_int ()
  let min_int: t = zero
  external add: t -> t -> t = "caml_add_uint32"
  external sub: t -> t -> t = "caml_sub_uint32"
  external mul: t -> t -> t = "caml_mul_uint32"
  external div: t -> t -> t = "caml_div_uint32"
  external rem: t -> t -> t = "caml_rem_uint32"
  external equal: t -> t -> bool = "caml_equal_uint32"
  external min: t -> t -> t = "caml_min_uint32"
  external max: t -> t -> t = "caml_max_uint32"
  external of_int: int -> t = "caml_int_to_uint32"
  external of_int32: int32 -> t = "caml_int32_to_uint32"
  external of_int64: int64 -> t = "caml_int64_to_uint32"
  external of_int64_opt: int64 -> t option = "caml_int64_to_uint32_opt"
  external to_int32: t -> int32 = "caml_uint32_to_int32"
  (* Doesn't do bound checking *)
  external to_int: t -> int = "caml_uint32_to_int"
  external to_float: t -> float = "caml_uint32_to_float"
  external of_nativeint: nativeint -> t = "caml_uint32_to_nativeint"
  external to_nativeint: t -> nativeint = "caml_nativeint_to_uint32"
  external to_nativeint_opt: t -> nativeint = "caml_nativeint_to_uint32_opt"
  let succ a = add a one
  let pred a = sub a one
  external seeded_hash_param :
    int -> int -> int -> t -> int = "caml_hash" [@@noalloc]
  let seeded_hash seed x = seeded_hash_param 10 100 seed x
  external of_string : string -> t = "caml_string_to_uint32"
  external of_stringopt : string -> t option = "caml_string_to_uint32_opt"
  external format_uint32: string -> t -> string = "caml_uint32_to_string"
  external _formatter: unit -> string = "caml_uint32_formatter"
  let formatter = "%" ^ _formatter ()
  let to_string x = format_uint32 formatter x
  external logand: t -> t -> t = "caml_uint32_logand"
  external logor: t -> t -> t = "caml_uint32_logor"
  external logxor: t -> t -> t = "caml_uint32_logxor"
  external lognot: t -> t = "caml_uint32_lognot"
  external shift_left: t -> t -> t = "caml_uint32_shift_left"
  external shift_right: t -> t -> t = "caml_uint32_shift_right"
end

module UInt64 = struct
  type t
  external _zero : unit -> t = "caml_zero_uint64"
  let zero = _zero ()
  external _one: unit -> t = "caml_one_uint64"
  let one = _one ()
  external _max_int: unit -> t = "caml_max_int_uint64"
  let max_int = _max_int ()
  let min_int = zero

  external add: t -> t -> t = "caml_add_uint64"
  external sub: t -> t -> t = "caml_sub_uint64"
  external mul: t -> t -> t = "caml_mul_uint64"
  external div: t -> t -> t = "caml_div_uint64"
  external rem: t -> t -> t = "caml_rem_uint64"
  external equal: t -> t -> bool = "caml_equal_uint64"
  external min: t -> t -> t = "caml_min_uint64"
  external max: t -> t -> t = "caml_max_uint64"
  external of_int: int -> t = "caml_int_to_uint64"
  external of_int32: int32 -> t = "caml_int32_to_uint64"
  external of_int64: int64 -> t = "caml_int64_to_uint64"
  external to_int64: t -> int64 = "caml_uint64_to_int64"
  external to_int: t -> int = "caml_uint64_to_int"
  external to_float: t -> float = "caml_uint64_to_float"
  external of_nativeint: nativeint -> t = "caml_nativeint_to_uint64"
  external to_nativeint: t -> nativeint = "caml_uint64_to_nativeint"
  let succ a = add a one
  let pred a = sub a one
  external seeded_hash_param :
    int -> int -> int -> t -> int = "caml_hash" [@@noalloc]
  let seeded_hash seed x = seeded_hash_param 10 100 seed x

  external of_string : string -> t = "caml_string_to_uint64"
  external of_stringopt : string -> t option = "caml_string_to_uint64_opt"
  external _formatter: unit -> string = "caml_uint64_formatter"
  let formatter = "%" ^ (_formatter ())
  external format_uint64: string -> t -> string = "caml_uint64_to_string"
  let to_string x = format_uint64 formatter x
  external logand: t -> t -> t = "caml_uint64_logand"
  external logor: t -> t -> t = "caml_uint64_logor"
  external logxor: t -> t -> t = "caml_uint64_logxor"
  external lognot: t -> t = "caml_uint64_lognot"
  external shift_left: t -> t -> t = "caml_uint64_shift_left"
  external shift_right: t -> t -> t = "caml_uint64_shift_right"
end

type 'a primitive =
| Bool   : bool primitive
| Int    : int primitive
| Int16  : int32 primitive
| UInt16 : int32 primitive
| Int32  : int32 primitive
| UInt32 : int32 primitive
| Int64  : int64 primitive
| UInt64 : int64 primitive
| Float  : float primitive
| Double : float primitive
| Char   : char primitive
| Ptr    : 'b primitive


let sizeof: type a. a primitive -> Nativeint.t = function
  | Bool   -> 1n
  | Int    -> 4n
  | Int16  -> 2n
  | UInt16 -> 2n
  | Int32  -> 4n
  | UInt32 -> 4n
  | UInt64 -> 8n
  | Int64  -> 8n
  | Float  -> 4n
  | Double -> 8n
  | Char   -> 1n
  | Ptr    -> Nativeint.of_int Nativeint.size

module CInteract = struct
  external ptr_alloc: size: nativeint -> 'a ptr = "caml_ptr_alloc"
  external ptr_free: ptr:'a ptr -> unit = "caml_ptr_free"
  external is_null: ptr: 'a ptr -> bool = "caml_ptr_is_null"
  external ptr_add: ptr:'a ptr -> offset:nativeint -> 'a ptr = "caml_ptr_add"
  external ptr_sub: ptr:'a ptr -> offset:nativeint -> 'a ptr = "caml_ptr_sub"

  let alloc (p: 'a primitive) : ('a ptr) = ptr_alloc ~size:(sizeof p)

  external poke_bool: ptr: bool ptr -> value: bool -> unit = "caml_poke_bool"
  external poke_i8: ptr: Int8.t ptr -> value: Int8.t -> unit = "caml_poke_i8"
  external poke_i16: ptr: Int16.t ptr -> value: Int16.t -> unit = "caml_poke_i16"
  external poke_i32: ptr: Int32.t ptr -> value: Int32.t -> unit = "caml_poke_i32"
  (* let poke_int: ptr: int ptr -> value: int -> unit = poke_i32  *)
  external poke_i64: ptr: Int64.t ptr -> value: Int64.t -> unit = "caml_poke_i64"

  external poke_u8: ptr: UInt8.t ptr -> value: UInt8.t -> unit = "caml_poke_u8"
  external poke_u16: ptr: UInt16.t ptr -> value: UInt16.t -> unit = "caml_poke_u16"
  external poke_u32: ptr: UInt32.t ptr -> value: UInt32.t -> unit = "caml_poke_u32"
  (* let poke_uint: ptr: UInt32.t ptr -> value: UInt32.t -> unit = poke_u32  *)
  external poke_u64: ptr: UInt64.t ptr -> value: UInt64.t -> unit = "caml_poke_u64"
  external poke_n: ptr: 't ptr -> value: 't -> size: Nativeint.t -> unit = "caml_poke_n"

  external peek_bool: ptr: bool ptr -> bool = "caml_peek_bool"
  external peek_i8: ptr: Int8.t ptr -> Int8.t = "caml_peek_i8"
  external peek_i16: ptr: Int16.t ptr -> Int16.t = "caml_peek_i16"
  external peek_i32: ptr: Int32.t ptr -> Int32.t = "caml_peek_i32"
  (* let peek_int: ptr: int ptr -> value: int -> unit = peek_i32  *)
  external peek_i64: ptr: Int64.t ptr -> Int64.t = "caml_peek_i64"

  external peek_u8: ptr: UInt8.t ptr -> UInt8.t = "caml_peek_u8"
  external peek_u16: ptr: UInt16.t ptr -> UInt16.t = "caml_peek_u16"
  external peek_u32: ptr: UInt32.t ptr -> UInt32.t = "caml_peek_u32"
  (* let peek_uint: ptr: UInt32.t ptr -> value: UInt32.t -> unit = peek_u32  *)
  external peek_u64: ptr: UInt64.t ptr -> UInt64.t = "caml_peek_u64"

  external peek_n: ptr: 't ptr -> size: nativeint -> 't = "caml_peek_n"
  (* external peek_n_unboxed: ptr: 't ptr -> size: nativeint -> 't = "caml_peek_n_unboxed" *)
  external cast: 'a -> 'b = "%identity"

  (* external peek_int16: int32 ptr -> int32 = "caml_peek_ptr_int16"
  external peek_uint16: int32 ptr -> int32 = "caml_peek_ptr_uint16"
  external peek_int32: int32 ptr -> int32 = "caml_peek_ptr_int32"
  external peek_uint32: int32 ptr -> int32 = "caml_peek_ptr_uint32"
  external peek_int: int ptr -> int = "caml_peek_ptr_int32"
  external peek_int64: int64 ptr -> int64 = "caml_peek_ptr_int64"
  external peek_uint64: int64 ptr -> int64 = "caml_peek_ptr_uint64"

  external peek_bool: bool ptr -> bool = "caml_peek_ptr_bool"
  external peek_float: float ptr -> float= "caml_peek_ptr_float"
  external peek_double: float ptr -> float= "caml_peek_ptr_double" *)

  (* let peek : 'a ptr -> 'b = function
  | Ptr (Bool   addr)  -> 1n
  | Ptr (Int    addr)  -> 4n
  | Ptr (Int16  addr)  -> 2n
  | Ptr (UInt16 addr)  -> 2n
  | Ptr (Int32  addr)  -> 4n
  | Ptr (UInt32 addr)  -> 4n
  | Ptr (UInt64 addr)  -> 8n
  | Ptr (Int64  addr)  -> 8n
  | Ptr (Float  addr)  -> 4n
  | Ptr (Double addr)  -> 8n
  | Ptr (Char   addr)  -> 1n
  | Ptr _ -> failwith "Not implemented" *)

end

module CArray = struct
open CInteract

type 't carray = {
  length: int;
  size_of_element: int;
  ptr: 't ptr
}

(* external memset = ptr: 't tpr *)

let alloc_array len initValue = 
  let size = sizeof initValue in
  let pVal = ptr_alloc ~size:size in

  {
    length = len;
    size_of_element = Nativeint.to_int size;
    ptr = pVal
  }

let for_each callback carray =
  let {length; size_of_element; ptr} = carray in
  let rec for_each_help offset =
    let currentPtr = ptr_add ~ptr:ptr ~offset:(Nativeint.of_int offset) in
    (* let currentVal = peek_n ~ptr:currentPtr ~size:(Nativeint.of_int size_of_element) in *)
    callback currentPtr;
    let nextOffset = offset + size_of_element in
    if nextOffset >= (length * size_of_element) 
      then () 
    else for_each_help (offset + size_of_element)
  in
  for_each_help 0

let for_each_ref callback carray =
  let {length; size_of_element; ptr} = carray in
  let rec for_each_help offset =
    let currentPtr = ptr_add ~ptr:ptr ~offset:(Nativeint.of_int offset) in
    let currentVal = peek_n ~ptr:currentPtr ~size:(Nativeint.of_int size_of_element) in
    let currentValRef = ref currentVal in
    callback (currentValRef);
    let nextOffset = offset + size_of_element in
    if nextOffset >= (length * size_of_element) 
      then () 
    else for_each_help (offset + size_of_element)
  in
  for_each_help 0

(* let fill array =  *)


end


