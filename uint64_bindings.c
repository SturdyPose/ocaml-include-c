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

static const struct custom_fixed_length uint64_length = {sizeof(uint64_t),
                                                         sizeof(uint64_t)};

static int uint64_cmp(value v1, value v2) {
  CAMLparam2(v1, v2);
  uint64_t i1 = UInt64_val(v1);
  uint64_t i2 = UInt64_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intptr_t uint64_hash(value v) {
  CAMLparam1(v);
  uint64_t x = UInt64_val(v);
  uint64_t lo = (uint64_t)x, hi = (uint64_t)(x >> 16);
  return hi ^ lo;
}

static void uint64_serialize(value v, uintnat *bsize_32, uintnat *bsize_64) {
  CAMLparam1(v);
  caml_serialize_int_8(UInt64_val(v));
  *bsize_32 = *bsize_64 = sizeof(uint64_t);
}

static uintptr_t uint64_deserialize(void *dst) {
  *((uint64_t *)dst) = caml_deserialize_uint_8();
  return sizeof(uint64_t);
}

CAMLexport const struct custom_operations caml_uint64_ops = {
    "_u64",
    custom_finalize_default,
    uint64_cmp,
    uint64_hash,
    uint64_serialize,
    uint64_deserialize,
    custom_compare_ext_default,
    &uint64_length};

CAMLexport value caml_copy_uint64(uint64_t i) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_custom(&caml_uint64_ops, sizeof(uint64_t), 0, 1);
  UInt64_val(res) = i;
  CAMLreturn(res);
}

CAMLprim value caml_zero_uint64() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint64(0));
}

CAMLprim value caml_one_uint64() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint64(1));
}

CAMLprim value caml_max_int_uint64() {
  CAMLparam0();
  CAMLreturn(caml_copy_uint64((uint64_t)(UINT64_MAX)));
}

CAMLprim value caml_equal_uint64(value a, value b) {
  CAMLparam2(a, b);
  uint64_t a1 = (uint64_t)UInt64_val(a);
  uint64_t b2 = (uint64_t)UInt64_val(b);
  CAMLreturn(Val_bool(a1 == b2));
}

CAMLprim value caml_max_uint64(value a, value b) {
  CAMLparam2(a, b);
  uint64_t a1 = (uint64_t)UInt64_val(a);
  uint64_t b2 = (uint64_t)UInt64_val(b);
  CAMLreturn(caml_copy_uint64(a1 > b2 ? a1 : b2));
}

CAMLprim value caml_min_uint64(value a, value b) {
  CAMLparam2(a, b);
  uint64_t a1 = (uint64_t)UInt64_val(a);
  uint64_t b2 = (uint64_t)UInt64_val(b);
  CAMLreturn(caml_copy_uint64(a1 < b2 ? a1 : b2));
}

CAMLprim value caml_int_to_uint64(value intvalue) {
  CAMLparam1(intvalue);
  uint64_t v = (uint64_t)Int_val(intvalue);
  CAMLreturn(caml_copy_uint64(v));
}

CAMLprim value caml_uint64_to_int(value uintvalue) {
  CAMLparam1(uintvalue);
  uint64_t v = (uint64_t)UInt64_val(uintvalue);
  if(v > INT32_MAX >> 1) caml_failwith("Can't fit uint64 to int");
  CAMLreturn(Val_int(v));
}

CAMLprim value caml_int32_to_uint64(value intvalue) {
  CAMLparam1(intvalue);
  uint64_t v = (uint64_t)Int32_val(intvalue);
  CAMLreturn(caml_copy_uint64(v));
}

CAMLprim value caml_int64_to_uint64(value intvalue) {
  CAMLparam1(intvalue);
  int64_t v = (uint64_t)Int64_val(intvalue);
  CAMLreturn(caml_copy_uint64(v));
}

CAMLprim value caml_uint64_to_int64(value uintvalue) {
  CAMLparam1(uintvalue);
  int64_t v = (int64_t)UInt64_val(uintvalue);
  CAMLreturn(caml_copy_int64(v));
}

CAMLprim value caml_uint64_to_float(value uintvalue) {
  CAMLparam1(uintvalue);
  uint64_t v = UInt64_val(uintvalue);
  CAMLreturn(caml_copy_double((double)v));
}

CAMLprim value caml_nativeint_to_uint64(value nativeint) {
  CAMLparam1(nativeint);
  uint64_t val = (uint64_t)Nativeint_val(nativeint);
  CAMLreturn(caml_copy_uint64(val));
}

CAMLprim value caml_uint64_to_nativeint(value uintvalue) {
  CAMLparam1(uintvalue);
  intptr_t val = (intptr_t )UInt64_val(uintvalue);
  CAMLreturn(caml_copy_nativeint(val));
}

CAMLprim value caml_add_uint64(value a, value b) {
  CAMLparam2(a, b);
  uint64_t a1 = (uint64_t)UInt64_val(a);
  uint64_t b2 = (uint64_t)UInt64_val(b);
  CAMLreturn(caml_copy_uint64(a1 + b2));
}

CAMLprim value caml_sub_uint64(value a, value b) {
  CAMLparam2(a, b);
  uint64_t a1 = (uint64_t)UInt64_val(a);
  uint64_t b2 = (uint64_t)UInt64_val(b);
  CAMLreturn(caml_copy_uint64(a1 - b2));
}

CAMLprim value caml_mul_uint64(value a, value b) {
  CAMLparam2(a, b);
  uint64_t a1 = (uint64_t)UInt64_val(a);
  uint64_t b2 = (uint64_t)UInt64_val(b);
  CAMLreturn(caml_copy_uint64(a1 * b2));
}

CAMLprim value caml_div_uint64(value a, value b) {
  CAMLparam2(a, b);
  uint64_t b2 = (uint64_t)UInt64_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  uint64_t a1 = (uint64_t)UInt64_val(a);
  CAMLreturn(caml_copy_uint64((uint64_t)(a1 / b2)));
}

CAMLprim value caml_rem_uint64(value a, value b) {
  CAMLparam2(a, b);
  uint64_t b2 = (uint64_t)UInt64_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  uint64_t a1 = (uint64_t)UInt64_val(a);
  CAMLreturn(caml_copy_uint64((uint64_t)(a1 % b2)));
}

CAMLprim value caml_uint64_formatter()
{
    CAMLparam0();
    CAMLreturn(caml_copy_string(PRIu64));
}

CAMLprim value caml_uint64_to_string(value format, value uintval) {
    CAMLparam2(format, uintval);
    uint64_t val = (uint64_t)UInt64_val(uintval);
    const char* pFormat = String_val(format);
    char buffer[20];
    sprintf(buffer, pFormat, val);
    CAMLreturn(caml_copy_string(buffer));
}

CAMLprim value caml_string_to_uint64(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) {
     if (res == PARSE_INVALID_INPUT) caml_failwith("uint64_of_string fail, empty string");
     if (res == PARSE_OVERFLOW) caml_failwith("uint64_of_string fail, can't fit into uint64");
     caml_failwith("uint64_of_string fail");
  }
  
  if (sign == -1) caml_failwith("uint64_of_string fail, negative sign");

  CAMLreturn(caml_copy_uint64(val));
}

CAMLprim value caml_string_to_uint64_opt(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) CAMLreturn(Val_none);
  if (sign == -1) CAMLreturn(Val_none);

  CAMLreturn(caml_alloc_some(caml_copy_uint64(val)));
}

CAMLprim value caml_uint64_logand(value a, value b) {
  CAMLparam2(a, b);
  uint64_t ua = (uint64_t)UInt64_val(a);
  uint64_t ub = (uint64_t)UInt64_val(b);
  CAMLreturn(caml_copy_uint64(ua & ub));
}

CAMLprim value caml_uint64_logor(value a, value b) {
  CAMLparam2(a, b);
  uint64_t ua = (uint64_t)UInt64_val(a);
  uint64_t ub = (uint64_t)UInt64_val(b);
  CAMLreturn(caml_copy_uint64(ua | ub));
}

CAMLprim value caml_uint64_logxor(value a, value b) {
  CAMLparam2(a, b);
  uint64_t ua = (uint64_t)UInt64_val(a);
  uint64_t ub = (uint64_t)UInt64_val(b);
  CAMLreturn(caml_copy_uint64(ua ^ ub));
}

CAMLprim value caml_uint64_lognot(value a) {
  CAMLparam1(a);
  uint64_t ua = (uint64_t)UInt64_val(a);
  CAMLreturn(caml_copy_uint64(~ua));
}

CAMLprim value caml_uint64_shift_left(value a, value b) {
	CAMLparam2(a, b);
    uint64_t ua = (uint64_t)UInt64_val(a);
    uint64_t ub = (uint64_t)UInt64_val(b);
    uint64_t res = (ub >= 64) ? 0 : (ua << ub);
    CAMLreturn(caml_copy_uint64(res));
}

CAMLprim value caml_uint64_shift_right(value a, value b) {
	CAMLparam2(a, b);
    uint64_t ua = (uint64_t)UInt64_val(a);
    uint64_t ub = (uint64_t)UInt64_val(b);
    uint64_t res = (ub >= 64) ? 0 : (ua >> ub);
    CAMLreturn(caml_copy_uint64(res));
}