// caml headers
#include <caml/alloc.h>
#include <caml/config.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/intext.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

// C headers
#include <inttypes.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include "shared.h"

static const struct custom_fixed_length uint8_length = {sizeof(uint8_t),
                                                         sizeof(uint8_t)};

static int uint8_cmp(value v1, value v2) {
  uint8_t i1 = UInt8_val(v1);
  uint8_t i2 = UInt8_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intptr_t uint8_hash(value v) {
  return UInt8_val(v);
}

static void uint8_serialize(value v, uintnat *bsize_32, uintnat *bsize_64) {
  caml_serialize_int_1(UInt8_val(v));
  *bsize_32 = *bsize_64 = sizeof(uint8_t);
}

static uintptr_t uint8_deserialize(void *dst) {
  *((uint8_t *)dst) = caml_deserialize_uint_1();
  return sizeof(uint8_t);
}

CAMLexport const struct custom_operations caml_uint8_ops = {
    "_u8",
    custom_finalize_default,
    uint8_cmp,
    uint8_hash,
    uint8_serialize,
    uint8_deserialize,
    custom_compare_ext_default,
    &uint8_length};

CAMLexport value caml_copy_uint8(uint8_t i) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_custom(&caml_uint8_ops, sizeof(uint8_t), 0, 1);
  UInt8_val(res) = i;
  CAMLreturn(res);
}

CAMLprim value caml_zero_uint8() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint8(0));
}

CAMLprim value caml_one_uint8() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint8(1));
}

CAMLprim value caml_max_int_uint8() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint8(UINT8_MAX));
}

CAMLprim value caml_min_int_uint8() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint8(0));
}

CAMLprim value caml_equal_uint8(value a, value b) {
  CAMLparam2(a, b);
  uint8_t a1 = (uint8_t)UInt8_val(a);
  uint8_t b2 = (uint8_t)UInt8_val(b);
  CAMLreturn(Val_bool(a1 == b2));
}

CAMLprim value caml_max_uint8(value a, value b) {
  CAMLparam2(a, b);
  uint8_t a1 = (uint8_t)UInt8_val(a);
  uint8_t b2 = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(a1 > b2 ? a1 : b2));
}

CAMLprim value caml_min_uint8(value a, value b) {
  CAMLparam2(a, b);
  uint8_t a1 = (uint8_t)UInt8_val(a);
  uint8_t b2 = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(a1 < b2 ? a1 : b2));
}

CAMLprim value caml_int_to_uint8(value intvalue) {
  CAMLparam1(intvalue);
  int v = (int)Int_val(intvalue);
  if(v > UINT8_MAX || v < 0)
  {
    caml_failwith("int_to_uint8 fail, can't fit into uint8");
  }
  CAMLreturn(caml_copy_uint8((uint8_t)v));
}

CAMLprim value caml_int_to_uint8_opt(value intvalue) {
  CAMLparam1(intvalue);
  int v = (int)Int_val(intvalue);
  if(v > UINT8_MAX || v < 0)
  {
    CAMLreturn(Val_none);
  }
  CAMLreturn(caml_alloc_some(caml_copy_uint8((uint8_t)v)));
}

CAMLprim value caml_int32_to_uint8(value intvalue) {
  CAMLparam1(intvalue);
  int32_t v = (int32_t)Int32_val(intvalue);
  if(v > UINT8_MAX || v < 0)
  {
    caml_failwith("int_to_uint8 fail, can't fit into uint8");
  }
  CAMLreturn(caml_copy_uint8((uint8_t)v));
}

CAMLprim value caml_int32_to_uint8_opt(value intvalue) {
  CAMLparam1(intvalue);
  int32_t v = (int32_t)Int32_val(intvalue);
  if(v > UINT8_MAX || v < 0)
  {
    CAMLreturn(Val_none);
  }
  CAMLreturn(caml_alloc_some(caml_copy_uint8((uint8_t)v)));
}

CAMLprim value caml_uint8_to_int32(value uint8value) {
  CAMLparam1(uint8value);
  uint8_t v = (uint8_t)UInt8_val(uint8value);
  CAMLreturn(caml_copy_int32((int32_t)v));
}

CAMLprim value caml_uint8_to_int(value uint8value) {
  CAMLparam1(uint8value);
  uint8_t v = (uint8_t)UInt8_val(uint8value);
  CAMLreturn(Val_int((int)v));
}

CAMLprim value caml_int64_to_uint8(value intvalue) {
  CAMLparam1(intvalue);
  int64_t v = Int64_val(intvalue);
  if (v <= UINT8_MAX && v >= 0)
    CAMLreturn(caml_copy_uint8((uint8_t)v));
  else
    caml_failwith("Int64 can't fit into uint8");
}

CAMLprim value caml_int64_to_uint8_opt(value intvalue) {
  CAMLparam1(intvalue);
  int64_t val = Int64_val(intvalue);
  if (val <= UINT8_MAX && val >= 0)
    CAMLreturn(caml_alloc_some(caml_copy_uint8((uint8_t)val)));
  else
    CAMLreturn(Val_none);
}

CAMLprim value caml_uint8_to_float(value uint8value) {
  CAMLparam1(uint8value);
  uint8_t val = UInt8_val(uint8value);
  CAMLreturn(caml_copy_double((double)val));
}

CAMLprim value caml_nativeint_to_uint8(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  CAMLreturn(caml_int32_to_uint8(nativeint));
#elif BIT64_PLAT
  CAMLreturn(caml_int64_to_uint8(nativeint));
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_nativeint_to_uint8_opt(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  CAMLreturn(caml_int32_to_uint8_opt(nativeint));
#elif BIT64_PLAT
  return caml_int64_to_uint8_opt(nativeint);
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_uint8_to_nativeint(value uint8value) {
  CAMLparam1(uint8value);
  uint8_t v = UInt8_val(uint8value);
  CAMLreturn(caml_copy_nativeint((intnat) v));
}

CAMLprim value caml_add_uint8(value a, value b) {
  CAMLparam2(a, b);
  uint8_t a1 = (uint8_t)UInt8_val(a);
  uint8_t b2 = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(a1 + b2));
}

CAMLprim value caml_sub_uint8(value a, value b) {
  CAMLparam2(a, b);
  uint8_t a1 = (uint8_t)UInt8_val(a);
  uint8_t b2 = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(a1 - b2));
}

CAMLprim value caml_mul_uint8(value a, value b) {
  CAMLparam2(a, b);
  uint8_t a1 = (uint8_t)UInt8_val(a);
  uint8_t b2 = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(a1 * b2));
}

CAMLprim value caml_div_uint8(value a, value b) {
  CAMLparam2(a, b);
  uint8_t b2 = (uint8_t)UInt8_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  uint8_t a1 = (uint8_t)UInt8_val(a);
  CAMLreturn(caml_copy_uint8((uint8_t)(a1 / b2)));
}

CAMLprim value caml_rem_uint8(value a, value b) {
  CAMLparam2(a, b);
  uint8_t b2 = (uint8_t)UInt8_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  uint8_t a1 = (uint8_t)UInt8_val(a);
  CAMLreturn(caml_copy_uint8((uint8_t)(a1 % b2)));
}

CAMLprim value caml_uint8_to_string(value format, value uint8val) {
    CAMLparam2(format, uint8val);
    uint8_t val = (uint8_t)UInt8_val(uint8val);
    const char* pFormat = String_val(format);
    char buffer[6];
    sprintf(buffer, pFormat, val);
    CAMLreturn(caml_copy_string(buffer));
}

CAMLprim value caml_uint8_formatter()
{
    CAMLparam0();
    CAMLreturn(caml_copy_string(PRIu8));
}

CAMLprim value caml_string_to_uint8(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) {
     if (res == PARSE_INVALID_INPUT) caml_failwith("uint8_of_string fail, empty string");
     if (res == PARSE_OVERFLOW) caml_failwith("uint8_of_string fail, can't fit into uint8");
     caml_failwith("uint8_of_string fail");
  }
  
  if (sign == -1) caml_failwith("uint8_of_string fail, negative sign");
  if (val > UINT8_MAX) caml_failwith("uint8_of_string fail, can't fit into uint8");

  CAMLreturn(caml_copy_uint8((uint8_t)val));
}

CAMLprim value caml_string_to_uint8_opt(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) CAMLreturn(Val_none);
  if (sign == -1) CAMLreturn(Val_none);
  if (val > UINT8_MAX) CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(caml_copy_uint8((uint8_t)val)));
}

CAMLprim value caml_uint8_logand(value a, value b) {
  CAMLparam2(a, b);
  uint8_t ua = (uint8_t)UInt8_val(a);
  uint8_t ub = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(ua & ub));
}

CAMLprim value caml_uint8_logor(value a, value b) {
  CAMLparam2(a, b);
  uint8_t ua = (uint8_t)UInt8_val(a);
  uint8_t ub = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(ua | ub));
}

CAMLprim value caml_uint8_logxor(value a, value b) {
  CAMLparam2(a, b);
  uint8_t ua = (uint8_t)UInt8_val(a);
  uint8_t ub = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(ua ^ ub));
}

CAMLprim value caml_uint8_lognot(value a) {
  CAMLparam1(a);
  uint8_t ua = (uint8_t)UInt8_val(a);
  CAMLreturn(caml_copy_uint8(~ua));
}

CAMLprim value caml_uint8_shift_left(value a, value b) {
  CAMLparam2(a, b);
  uint8_t ua = (uint8_t)UInt8_val(a);
  uint8_t ub = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(ua << ub));
}

CAMLprim value caml_uint8_shift_right(value a, value b) {
  CAMLparam2(a, b);
  uint8_t ua = (uint8_t)UInt8_val(a);
  uint8_t ub = (uint8_t)UInt8_val(b);
  CAMLreturn(caml_copy_uint8(ua >> ub));
}
