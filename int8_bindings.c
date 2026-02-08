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

static const struct custom_fixed_length int8_length = {sizeof(int8_t),
                                                         sizeof(int8_t)};

static int int8_cmp(value v1, value v2) {
  int8_t i1 = Int8_val(v1);
  int8_t i2 = Int8_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intptr_t int8_hash(value v) {
  return Int8_val(v);
}

static void int8_serialize(value v, uintnat *bsize_32, uintnat *bsize_64) {
  caml_serialize_int_1(Int8_val(v));
  *bsize_32 = *bsize_64 = sizeof(int8_t);
}

static uintptr_t int8_deserialize(void *dst) {
  *((int8_t *)dst) = caml_deserialize_sint_1();
  return sizeof(int8_t);
}

CAMLexport const struct custom_operations caml_int8_ops = {
    "_i8",
    custom_finalize_default,
    int8_cmp,
    int8_hash,
    int8_serialize,
    int8_deserialize,
    custom_compare_ext_default,
    &int8_length};

CAMLexport value caml_copy_int8(int8_t i) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_custom(&caml_int8_ops, sizeof(int8_t), 0, 1);
  Int8_val(res) = i;
  CAMLreturn(res);
}

CAMLprim value caml_zero_int8() {
  CAMLparam0();
  CAMLreturn(caml_copy_int8(0));
}

CAMLprim value caml_one_int8() {
  CAMLparam0();
  CAMLreturn(caml_copy_int8(1));
}

CAMLprim value caml_max_int_int8() {
  CAMLparam0();
  CAMLreturn(caml_copy_int8(INT8_MAX));
}

CAMLprim value caml_min_int_int8() {
  CAMLparam0();
  CAMLreturn(caml_copy_int8(INT8_MIN));
}

CAMLprim value caml_equal_int8(value a, value b) {
  CAMLparam2(a, b);
  int8_t a1 = (int8_t)Int8_val(a);
  int8_t b2 = (int8_t)Int8_val(b);
  CAMLreturn(Val_bool(a1 == b2));
}

CAMLprim value caml_max_int8(value a, value b) {
  CAMLparam2(a, b);
  int8_t a1 = (int8_t)Int8_val(a);
  int8_t b2 = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(a1 > b2 ? a1 : b2));
}

CAMLprim value caml_min_int8(value a, value b) {
  CAMLparam2(a, b);
  int8_t a1 = (int8_t)Int8_val(a);
  int8_t b2 = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(a1 < b2 ? a1 : b2));
}

CAMLprim value caml_int_to_int8(value intvalue) {
  CAMLparam1(intvalue);
  int v = (int)Int_val(intvalue);
  if(v > INT8_MAX || v < INT8_MIN)
  {
    caml_failwith("int_to_int8 fail, can't fit into int8");
  }
  CAMLreturn(caml_copy_int8((int8_t)v));
}

CAMLprim value caml_int_to_int8_opt(value intvalue) {
  CAMLparam1(intvalue);
  int v = (int)Int_val(intvalue);
  if(v > INT8_MAX || v < INT8_MIN)
  {
    CAMLreturn(Val_none);
  }
  CAMLreturn(caml_alloc_some(caml_copy_int8((int8_t)v)));
}

CAMLprim value caml_int32_to_int8(value intvalue) {
  CAMLparam1(intvalue);
  int32_t v = (int32_t)Int32_val(intvalue);
  if(v > INT8_MAX || v < INT8_MIN)
  {
    caml_failwith("int_to_int8 fail, can't fit into int8");
  }
  CAMLreturn(caml_copy_int8((int8_t)v));
}

CAMLprim value caml_int32_to_int8_opt(value intvalue) {
  CAMLparam1(intvalue);
  int32_t v = (int32_t)Int32_val(intvalue);
  if(v > INT8_MAX || v < INT8_MIN)
  {
    CAMLreturn(Val_none);
  }
  CAMLreturn(caml_alloc_some(caml_copy_int8((int8_t)v)));
}

CAMLprim value caml_int8_to_int32(value int8value) {
  CAMLparam1(int8value);
  int8_t v = (int8_t)Int8_val(int8value);
  CAMLreturn(caml_copy_int32((int32_t)v));
}

CAMLprim value caml_int8_to_int(value int8value) {
  CAMLparam1(int8value);
  int8_t v = (int8_t)Int8_val(int8value);
  CAMLreturn(Val_int((int)v));
}

CAMLprim value caml_int64_to_int8(value intvalue) {
  CAMLparam1(intvalue);
  int64_t v = Int64_val(intvalue);
  if (v <= INT8_MAX && v >= INT8_MIN)
    CAMLreturn(caml_copy_int8((int8_t)v));
  else
    caml_failwith("Int64 can't fit into int8");
}

CAMLprim value caml_int64_to_int8_opt(value intvalue) {
  CAMLparam1(intvalue);
  int64_t val = Int64_val(intvalue);
  if (val <= INT8_MAX && val >= INT8_MIN)
    CAMLreturn(caml_alloc_some(caml_copy_int8((int8_t)val)));
  else
    CAMLreturn(Val_none);
}

CAMLprim value caml_int8_to_float(value int8value) {
  CAMLparam1(int8value);
  int8_t val = Int8_val(int8value);
  CAMLreturn(caml_copy_double((double)val));
}

CAMLprim value caml_nativeint_to_int8(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  CAMLreturn(caml_int32_to_int8(nativeint));
#elif BIT64_PLAT
  CAMLreturn(caml_int64_to_int8(nativeint));
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_nativeint_to_int8_opt(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  CAMLreturn(caml_int32_to_int8_opt(nativeint));
#elif BIT64_PLAT
  return caml_int64_to_int8_opt(nativeint);
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_int8_to_nativeint(value int8value) {
  CAMLparam1(int8value);
  int8_t v = Int8_val(int8value);
  CAMLreturn(caml_copy_nativeint((intnat) v));
}

CAMLprim value caml_add_int8(value a, value b) {
  CAMLparam2(a, b);
  int8_t a1 = (int8_t)Int8_val(a);
  int8_t b2 = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(a1 + b2));
}

CAMLprim value caml_sub_int8(value a, value b) {
  CAMLparam2(a, b);
  int8_t a1 = (int8_t)Int8_val(a);
  int8_t b2 = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(a1 - b2));
}

CAMLprim value caml_mul_int8(value a, value b) {
  CAMLparam2(a, b);
  int8_t a1 = (int8_t)Int8_val(a);
  int8_t b2 = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(a1 * b2));
}

CAMLprim value caml_div_int8(value a, value b) {
  CAMLparam2(a, b);
  int8_t b2 = (int8_t)Int8_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  int8_t a1 = (int8_t)Int8_val(a);
  CAMLreturn(caml_copy_int8((int8_t)(a1 / b2)));
}

CAMLprim value caml_rem_int8(value a, value b) {
  CAMLparam2(a, b);
  int8_t b2 = (int8_t)Int8_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  int8_t a1 = (int8_t)Int8_val(a);
  CAMLreturn(caml_copy_int8((int8_t)(a1 % b2)));
}

CAMLprim value caml_int8_to_string(value format, value int8val) {
    CAMLparam2(format, int8val);
    int8_t val = (int8_t)Int8_val(int8val);
    const char* pFormat = String_val(format);
    char buffer[6];
    sprintf(buffer, pFormat, val);
    CAMLreturn(caml_copy_string(buffer));
}

CAMLprim value caml_int8_formatter()
{
    CAMLparam0();
    CAMLreturn(caml_copy_string(PRId8));
}

CAMLprim value caml_string_to_int8(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) {
    switch (res) {
    case PARSE_INVALID_INPUT:
      caml_failwith("int8_of_string fail, empty string");
    case PARSE_OVERFLOW:
      caml_failwith("int8_of_string fail, can't fit into int8");
    default:
      caml_failwith("int8_of_string fail");
    }
  }

  if (sign == 1) {
      if (val > INT8_MAX) caml_failwith("int8_of_string fail, can't fit into int8");
      CAMLreturn(caml_copy_int8((int8_t)val));
  } else {
      if (val > (uint64_t)(-(int64_t)INT8_MIN)) caml_failwith("int8_of_string fail, can't fit into int8");
      CAMLreturn(caml_copy_int8((int8_t)(-((int64_t)val))));
  }
}

CAMLprim value caml_string_to_int8_opt(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) CAMLreturn(Val_none);

  if (sign == 1) {
      if (val > INT8_MAX) CAMLreturn(Val_none);
      CAMLreturn(caml_alloc_some(caml_copy_int8((int8_t)val)));
  } else {
      if (val > (uint64_t)(-(int64_t)INT8_MIN)) CAMLreturn(Val_none);
      CAMLreturn(caml_alloc_some(caml_copy_int8((int8_t)(-((int64_t)val)))));
  }
}

CAMLprim value caml_int8_logand(value a, value b) {
  CAMLparam2(a, b);
  int8_t ua = (int8_t)Int8_val(a);
  int8_t ub = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(ua & ub));
}

CAMLprim value caml_int8_logor(value a, value b) {
  CAMLparam2(a, b);
  int8_t ua = (int8_t)Int8_val(a);
  int8_t ub = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(ua | ub));
}

CAMLprim value caml_int8_logxor(value a, value b) {
  CAMLparam2(a, b);
  int8_t ua = (int8_t)Int8_val(a);
  int8_t ub = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(ua ^ ub));
}

CAMLprim value caml_int8_lognot(value a) {
  CAMLparam1(a);
  int8_t ua = (int8_t)Int8_val(a);
  CAMLreturn(caml_copy_int8(~ua));
}

CAMLprim value caml_int8_shift_left(value a, value b) {
  CAMLparam2(a, b);
  int8_t ua = (int8_t)Int8_val(a);
  int8_t ub = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(ua << ub));
}

CAMLprim value caml_int8_shift_right(value a, value b) {
  CAMLparam2(a, b);
  int8_t ua = (int8_t)Int8_val(a);
  int8_t ub = (int8_t)Int8_val(b);
  CAMLreturn(caml_copy_int8(ua >> ub));
}
