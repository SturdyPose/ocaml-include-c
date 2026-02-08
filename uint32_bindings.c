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

#include "shared.h"

static const struct custom_fixed_length uint32_length = {sizeof(uint32_t),
                                                         sizeof(uint32_t)};

static int uint32_cmp(value v1, value v2) {
  uint32_t i1 = UInt32_val(v1);
  uint32_t i2 = UInt32_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intptr_t uint32_hash(value v) {
  uint32_t x = UInt32_val(v);
  uint32_t lo = (uint32_t)x, hi = (uint32_t)(x >> 16);
  return hi ^ lo;
}

static void uint32_serialize(value v, uintnat *bsize_32, uintnat *bsize_64) {
  caml_serialize_int_4(UInt32_val(v));
  *bsize_32 = *bsize_64 = sizeof(uint32_t);
}

static uintptr_t uint32_deserialize(void *dst) {
  *((uint32_t *)dst) = caml_deserialize_uint_4();
  return sizeof(uint32_t);
}

CAMLexport const struct custom_operations caml_uint32_ops = {
    "_u32",
    custom_finalize_default,
    uint32_cmp,
    uint32_hash,
    uint32_serialize,
    uint32_deserialize,
    custom_compare_ext_default,
    &uint32_length};

CAMLexport value caml_copy_uint32(uint32_t i) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_custom(&caml_uint32_ops, sizeof(uint32_t), 0, 1);
  UInt32_val(res) = i;
  CAMLreturn(res);
}

CAMLprim value caml_zero_uint32() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint32(0));
}

CAMLprim value caml_one_uint32() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint32(1));
}

CAMLprim value caml_max_int_uint32() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint32((uint32_t)(UINT32_MAX)));
}

CAMLprim value caml_equal_uint32(value a, value b) {
  CAMLparam2(a, b);
  uint32_t a1 = (uint32_t)UInt32_val(a);
  uint32_t b2 = (uint32_t)UInt32_val(b);
  CAMLreturn(Val_bool(a1 == b2));
}

CAMLprim value caml_max_uint32(value a, value b) {
  CAMLparam2(a, b);
  uint32_t a1 = (uint32_t)UInt32_val(a);
  uint32_t b2 = (uint32_t)UInt32_val(b);
  CAMLreturn(caml_copy_uint32(a1 > b2 ? a1 : b2));
}

CAMLprim value caml_min_uint32(value a, value b) {
  CAMLparam2(a, b);
  uint32_t a1 = (uint32_t)UInt32_val(a);
  uint32_t b2 = (uint32_t)UInt32_val(b);
  CAMLreturn(caml_copy_uint32(a1 < b2 ? a1 : b2));
}

CAMLprim value caml_int_to_uint32(value intvalue) {
  CAMLparam1(intvalue);
  uint32_t v = (uint32_t)Int_val(intvalue);
  CAMLreturn(caml_copy_uint32(v));
}

CAMLprim value caml_int32_to_uint32(value intvalue) {
  CAMLparam1(intvalue);
  uint32_t v = (uint32_t)Int32_val(intvalue);
  CAMLreturn(caml_copy_uint32(v));
}

CAMLprim value caml_uint32_to_int32(value uintvalue) {
  CAMLparam1(uintvalue);
  uint32_t v = (uint32_t)UInt32_val(uintvalue);
  CAMLreturn(caml_copy_int32((int32_t)v));
}

CAMLprim value caml_uint32_to_int(value uintvalue) {
  CAMLparam1(uintvalue);
  uint32_t v = (uint32_t)UInt32_val(uintvalue);
  if(v > INT32_MAX >> 1) caml_failwith("Can't fit uint64 to int");
  CAMLreturn(Val_int((int)v));
}

CAMLprim value caml_int64_to_uint32(value intvalue) {
  CAMLparam1(intvalue);
  int64_t v = Int64_val(intvalue);
  if (v <= UINT32_MAX && v >= -UINT32_MAX)
    CAMLreturn(caml_copy_uint32((uint32_t)v));
  else
    caml_failwith("Int64 can't fit into uint32");
}

CAMLprim value caml_int64_to_uint32_opt(value intvalue) {
  CAMLparam1(intvalue);
  int64_t val = Int64_val(intvalue);
  if (val <= UINT32_MAX && val >= -UINT32_MAX)
    CAMLreturn(caml_alloc_some(caml_copy_uint32((uint32_t)val)));
  else
    CAMLreturn(Val_none);
}

CAMLprim value caml_uint32_to_float(value uintvalue) {
  CAMLparam1(uintvalue);
  uint32_t val = UInt32_val(uintvalue);
  CAMLreturn(caml_copy_double((double)val));
}

CAMLprim value caml_nativeint_to_uint32(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  uint32_t v = (uint32_t)Native_val(nativeint);
  CAMLreturn(caml_copy_uint32(v));
#elif BIT64_PLAT
  CAMLreturn(caml_int64_to_uint32(nativeint));
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_nativeint_to_uint32_opt(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  uint32_t v = (uint32_t)Native_val(nativeint);
  CAMLreturn(caml_alloc_some(caml_copy_uint32(v)));
#elif BIT64_PLAT
  return caml_int64_to_uint32_opt(nativeint);
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_uint32_to_nativeint(value uintvalue) {
  CAMLparam1(uintvalue);
  uint32_t v = UInt32_val(uintvalue);
  CAMLreturn(caml_copy_nativeint((intnat) v));
}

CAMLprim value caml_add_uint32(value a, value b) {
  CAMLparam2(a, b);
  uint32_t a1 = (uint32_t)UInt32_val(a);
  uint32_t b2 = (uint32_t)UInt32_val(b);
  CAMLreturn(caml_copy_uint32(a1 + b2));
}

CAMLprim value caml_sub_uint32(value a, value b) {
  CAMLparam2(a, b);
  uint32_t a1 = (uint32_t)UInt32_val(a);
  uint32_t b2 = (uint32_t)UInt32_val(b);
  CAMLreturn(caml_copy_uint32(a1 - b2));
}

CAMLprim value caml_mul_uint32(value a, value b) {
  CAMLparam2(a, b);
  uint32_t a1 = (uint32_t)UInt32_val(a);
  uint32_t b2 = (uint32_t)UInt32_val(b);
  CAMLreturn(caml_copy_uint32(a1 * b2));
}

CAMLprim value caml_div_uint32(value a, value b) {
  CAMLparam2(a, b);
  uint32_t b2 = (uint32_t)UInt32_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  uint32_t a1 = (uint32_t)UInt32_val(a);
  CAMLreturn(caml_copy_uint32((uint32_t)(a1 / b2)));
}

CAMLprim value caml_rem_uint32(value a, value b) {
  CAMLparam2(a, b);
  uint32_t b2 = (uint32_t)UInt32_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  uint32_t a1 = (uint32_t)UInt32_val(a);
  CAMLreturn(caml_copy_uint32((uint32_t)(a1 % b2)));
}

CAMLprim value caml_uint32_to_string(value format, value uintval) {
    CAMLparam2(format, uintval);
    uint32_t val = (uint32_t)UInt32_val(uintval);
    const char* pFormat = String_val(format);
    char buffer[11];
    sprintf(buffer, pFormat, val);
    CAMLreturn(caml_copy_string(buffer));
}

CAMLprim value caml_uint32_formatter()
{
    CAMLparam0();
    CAMLreturn(caml_copy_string(PRIu32));
}

CAMLprim value caml_string_to_uint32(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) {
    switch (res) {
    case PARSE_INVALID_INPUT:
      caml_failwith("uint32_of_string fail, empty string");
    case PARSE_OVERFLOW:
      caml_failwith("uint32_of_string fail, can't fit into uint32");
    default:
      caml_failwith("uint32_of_string fail");
    }
  }
  
  if (sign == -1) caml_failwith("uint32_of_string fail, negative sign");
  if (val > UINT32_MAX) caml_failwith("uint32_of_string fail, can't fit into uint32");

  CAMLreturn(caml_copy_uint32((uint32_t)val));
}

CAMLprim value caml_string_to_uint32_opt(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) CAMLreturn(Val_none);
  if (sign == -1) CAMLreturn(Val_none);
  if (val > UINT32_MAX) CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(caml_copy_uint32((uint32_t)val)));
}

CAMLprim value caml_uint32_logand(value a, value b) {
  CAMLparam2(a, b);
  uint32_t ua = (uint64_t)UInt32_val(a);
  uint32_t ub = (uint64_t)UInt32_val(b);
  CAMLreturn(caml_copy_uint32(ua & ub));
}

CAMLprim value caml_uint32_logor(value a, value b) {
  CAMLparam2(a, b);
  uint32_t ua = (uint64_t)UInt32_val(a);
  uint32_t ub = (uint64_t)UInt32_val(b);
  CAMLreturn(caml_copy_uint32(ua | ub));
}

CAMLprim value caml_uint32_logxor(value a, value b) {
  CAMLparam2(a, b);
  uint32_t ua = (uint64_t)UInt32_val(a);
  uint32_t ub = (uint64_t)UInt32_val(b);
  CAMLreturn(caml_copy_uint32(ua ^ ub));
}

CAMLprim value caml_uint32_lognot(value a) {
  CAMLparam1(a);
  uint32_t ua = (uint64_t)UInt32_val(a);
  CAMLreturn(caml_copy_uint32(~ua));
}

CAMLprim value caml_uint32_shift_left(value a, value b) {
  CAMLparam2(a, b);
  uint32_t ua = (uint64_t)UInt32_val(a);
  uint32_t ub = (uint64_t)UInt32_val(b);
  uint32_t res = (ub >= 32) ? 0 : (ua << ub);
  CAMLreturn(caml_copy_uint32(res));
}

CAMLprim value caml_uint32_shift_right(value a, value b) {
  CAMLparam2(a, b);
  uint32_t ua = (uint64_t)UInt32_val(a);
  uint32_t ub = (uint64_t)UInt32_val(b);
  uint32_t res = (ub >= 32) ? 0 : (ua >> ub);
  CAMLreturn(caml_copy_uint32(res));
}