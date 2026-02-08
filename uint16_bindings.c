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

static const struct custom_fixed_length uint16_length = {sizeof(uint16_t),
                                                         sizeof(uint16_t)};

static int uint16_cmp(value v1, value v2) {
  uint16_t i1 = UInt16_val(v1);
  uint16_t i2 = UInt16_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intptr_t uint16_hash(value v) {
  return UInt16_val(v);
}

static void uint16_serialize(value v, uintnat *bsize_32, uintnat *bsize_64) {
  caml_serialize_int_2(UInt16_val(v));
  *bsize_32 = *bsize_64 = sizeof(uint16_t);
}

static uintptr_t uint16_deserialize(void *dst) {
  *((uint16_t *)dst) = caml_deserialize_uint_2();
  return sizeof(uint16_t);
}

CAMLexport const struct custom_operations caml_uint16_ops = {
    "_u16",
    custom_finalize_default,
    uint16_cmp,
    uint16_hash,
    uint16_serialize,
    uint16_deserialize,
    custom_compare_ext_default,
    &uint16_length};

CAMLexport value caml_copy_uint16(uint16_t i) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_custom(&caml_uint16_ops, sizeof(uint16_t), 0, 1);
  UInt16_val(res) = i;
  CAMLreturn(res);
}

CAMLprim value caml_zero_uint16() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint16(0));
}

CAMLprim value caml_one_uint16() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint16(1));
}

CAMLprim value caml_max_int_uint16() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint16(UINT16_MAX));
}

CAMLprim value caml_min_int_uint16() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint16(0));
}

CAMLprim value caml_equal_uint16(value a, value b) {
  CAMLparam2(a, b);
  uint16_t a1 = (uint16_t)UInt16_val(a);
  uint16_t b2 = (uint16_t)UInt16_val(b);
  CAMLreturn(Val_bool(a1 == b2));
}

CAMLprim value caml_max_uint16(value a, value b) {
  CAMLparam2(a, b);
  uint16_t a1 = (uint16_t)UInt16_val(a);
  uint16_t b2 = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(a1 > b2 ? a1 : b2));
}

CAMLprim value caml_min_uint16(value a, value b) {
  CAMLparam2(a, b);
  uint16_t a1 = (uint16_t)UInt16_val(a);
  uint16_t b2 = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(a1 < b2 ? a1 : b2));
}

CAMLprim value caml_int_to_uint16(value intvalue) {
  CAMLparam1(intvalue);
  int v = (int)Int_val(intvalue);
  if(v > UINT16_MAX || v < 0)
  {
    caml_failwith("int_to_uint16 fail, can't fit into uint16");
  }
  CAMLreturn(caml_copy_uint16((uint16_t)v));
}

CAMLprim value caml_int_to_uint16_opt(value intvalue) {
  CAMLparam1(intvalue);
  int v = (int)Int_val(intvalue);
  if(v > UINT16_MAX || v < 0)
  {
    CAMLreturn(Val_none);
  }
  CAMLreturn(caml_alloc_some(caml_copy_uint16((uint16_t)v)));
}

CAMLprim value caml_int32_to_uint16(value intvalue) {
  CAMLparam1(intvalue);
  int32_t v = (int32_t)Int32_val(intvalue);
  if(v > UINT16_MAX || v < 0)
  {
    caml_failwith("int_to_uint16 fail, can't fit into uint16");
  }
  CAMLreturn(caml_copy_uint16((uint16_t)v));
}

CAMLprim value caml_int32_to_uint16_opt(value intvalue) {
  CAMLparam1(intvalue);
  int32_t v = (int32_t)Int32_val(intvalue);
  if(v > UINT16_MAX || v < 0)
  {
    CAMLreturn(Val_none);
  }
  CAMLreturn(caml_alloc_some(caml_copy_uint16((uint16_t)v)));
}

CAMLprim value caml_uint16_to_int32(value uint16value) {
  CAMLparam1(uint16value);
  uint16_t v = (uint16_t)UInt16_val(uint16value);
  CAMLreturn(caml_copy_int32((int32_t)v));
}

CAMLprim value caml_uint16_to_int(value uint16value) {
  CAMLparam1(uint16value);
  uint16_t v = (uint16_t)UInt16_val(uint16value);
  CAMLreturn(Val_int((int)v));
}

CAMLprim value caml_int64_to_uint16(value intvalue) {
  CAMLparam1(intvalue);
  int64_t v = Int64_val(intvalue);
  if (v <= UINT16_MAX && v >= 0)
    CAMLreturn(caml_copy_uint16((uint16_t)v));
  else
    caml_failwith("Int64 can't fit into uint16");
}

CAMLprim value caml_int64_to_uint16_opt(value intvalue) {
  CAMLparam1(intvalue);
  int64_t val = Int64_val(intvalue);
  if (val <= UINT16_MAX && val >= 0)
    CAMLreturn(caml_alloc_some(caml_copy_uint16((uint16_t)val)));
  else
    CAMLreturn(Val_none);
}

CAMLprim value caml_uint16_to_float(value uint16value) {
  CAMLparam1(uint16value);
  uint16_t val = UInt16_val(uint16value);
  CAMLreturn(caml_copy_double((double)val));
}

CAMLprim value caml_nativeint_to_uint16(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  CAMLreturn(caml_int32_to_uint16(nativeint));
#elif BIT64_PLAT
  CAMLreturn(caml_int64_to_uint16(nativeint));
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_nativeint_to_uint16_opt(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  CAMLreturn(caml_int32_to_uint16_opt(nativeint));
#elif BIT64_PLAT
  return caml_int64_to_uint16_opt(nativeint);
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_uint16_to_nativeint(value uint16value) {
  CAMLparam1(uint16value);
  uint16_t v = UInt16_val(uint16value);
  CAMLreturn(caml_copy_nativeint((intnat) v));
}

CAMLprim value caml_add_uint16(value a, value b) {
  CAMLparam2(a, b);
  uint16_t a1 = (uint16_t)UInt16_val(a);
  uint16_t b2 = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(a1 + b2));
}

CAMLprim value caml_sub_uint16(value a, value b) {
  CAMLparam2(a, b);
  uint16_t a1 = (uint16_t)UInt16_val(a);
  uint16_t b2 = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(a1 - b2));
}

CAMLprim value caml_mul_uint16(value a, value b) {
  CAMLparam2(a, b);
  uint16_t a1 = (uint16_t)UInt16_val(a);
  uint16_t b2 = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(a1 * b2));
}

CAMLprim value caml_div_uint16(value a, value b) {
  CAMLparam2(a, b);
  uint16_t b2 = (uint16_t)UInt16_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  uint16_t a1 = (uint16_t)UInt16_val(a);
  CAMLreturn(caml_copy_uint16((uint16_t)(a1 / b2)));
}

CAMLprim value caml_rem_uint16(value a, value b) {
  CAMLparam2(a, b);
  uint16_t b2 = (uint16_t)UInt16_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  uint16_t a1 = (uint16_t)UInt16_val(a);
  CAMLreturn(caml_copy_uint16((uint16_t)(a1 % b2)));
}

CAMLprim value caml_uint16_to_string(value format, value uint16val) {
    CAMLparam2(format, uint16val);
    uint16_t val = (uint16_t)UInt16_val(uint16val);
    const char* pFormat = String_val(format);
    char buffer[6];
    sprintf(buffer, pFormat, val);
    CAMLreturn(caml_copy_string(buffer));
}

CAMLprim value caml_uint16_formatter()
{
    CAMLparam0();
    CAMLreturn(caml_copy_string(PRIu16));
}

CAMLprim value caml_string_to_uint16(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) {
    switch (res) {
    case PARSE_INVALID_INPUT:
      caml_failwith("uint16_of_string fail, empty string");
    case PARSE_OVERFLOW:
      caml_failwith("uint16_of_string fail, can't fit into uint16");
    default:
      caml_failwith("uint16_of_string fail");
    }
  }
  
  if (sign == -1) caml_failwith("uint16_of_string fail, negative sign");
  if (val > UINT16_MAX) caml_failwith("uint16_of_string fail, can't fit into uint16");

  CAMLreturn(caml_copy_uint16((uint16_t)val));
}

CAMLprim value caml_string_to_uint16_opt(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) CAMLreturn(Val_none);
  if (sign == -1) CAMLreturn(Val_none);
  if (val > UINT16_MAX) CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(caml_copy_uint16((uint16_t)val)));
}

CAMLprim value caml_uint16_logand(value a, value b) {
  CAMLparam2(a, b);
  uint16_t ua = (uint16_t)UInt16_val(a);
  uint16_t ub = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(ua & ub));
}

CAMLprim value caml_uint16_logor(value a, value b) {
  CAMLparam2(a, b);
  uint16_t ua = (uint16_t)UInt16_val(a);
  uint16_t ub = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(ua | ub));
}

CAMLprim value caml_uint16_logxor(value a, value b) {
  CAMLparam2(a, b);
  uint16_t ua = (uint16_t)UInt16_val(a);
  uint16_t ub = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(ua ^ ub));
}

CAMLprim value caml_uint16_lognot(value a) {
  CAMLparam1(a);
  uint16_t ua = (uint16_t)UInt16_val(a);
  CAMLreturn(caml_copy_uint16(~ua));
}

CAMLprim value caml_uint16_shift_left(value a, value b) {
  CAMLparam2(a, b);
  uint16_t ua = (uint16_t)UInt16_val(a);
  uint16_t ub = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(ua << ub));
}

CAMLprim value caml_uint16_shift_right(value a, value b) {
  CAMLparam2(a, b);
  uint16_t ua = (uint16_t)UInt16_val(a);
  uint16_t ub = (uint16_t)UInt16_val(b);
  CAMLreturn(caml_copy_uint16(ua >> ub));
}
