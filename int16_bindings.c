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

static const struct custom_fixed_length int16_length = {sizeof(int16_t),
                                                         sizeof(int16_t)};

static int int16_cmp(value v1, value v2) {
  int16_t i1 = Int16_val(v1);
  int16_t i2 = Int16_val(v2);
  return (i1 > i2) - (i1 < i2);
}

static intptr_t int16_hash(value v) {
  return Int16_val(v);
}

static void int16_serialize(value v, uintnat *bsize_32, uintnat *bsize_64) {
  caml_serialize_int_2(Int16_val(v));
  *bsize_32 = *bsize_64 = sizeof(int16_t);
}

static uintptr_t int16_deserialize(void *dst) {
  *((int16_t *)dst) = caml_deserialize_sint_2();
  return sizeof(int16_t);
}

CAMLexport const struct custom_operations caml_int16_ops = {
    "_i16",
    custom_finalize_default,
    int16_cmp,
    int16_hash,
    int16_serialize,
    int16_deserialize,
    custom_compare_ext_default,
    &int16_length};

CAMLexport value caml_copy_int16(int16_t i) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_custom(&caml_int16_ops, sizeof(int16_t), 0, 1);
  Int16_val(res) = i;
  CAMLreturn(res);
}

CAMLprim value caml_zero_int16() {
  CAMLparam0();
  CAMLreturn(caml_copy_int16(0));
}

CAMLprim value caml_one_int16() {
  CAMLparam0();
  CAMLreturn(caml_copy_int16(1));
}

CAMLprim value caml_max_int_int16() {
  CAMLparam0();
  CAMLreturn(caml_copy_int16(INT16_MAX));
}

CAMLprim value caml_min_int_int16() {
  CAMLparam0();
  CAMLreturn(caml_copy_int16(INT16_MIN));
}

CAMLprim value caml_equal_int16(value a, value b) {
  CAMLparam2(a, b);
  int16_t a1 = (int16_t)Int16_val(a);
  int16_t b2 = (int16_t)Int16_val(b);
  CAMLreturn(Val_bool(a1 == b2));
}

CAMLprim value caml_max_int16(value a, value b) {
  CAMLparam2(a, b);
  int16_t a1 = (int16_t)Int16_val(a);
  int16_t b2 = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(a1 > b2 ? a1 : b2));
}

CAMLprim value caml_min_int16(value a, value b) {
  CAMLparam2(a, b);
  int16_t a1 = (int16_t)Int16_val(a);
  int16_t b2 = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(a1 < b2 ? a1 : b2));
}

CAMLprim value caml_int_to_int16(value intvalue) {
  CAMLparam1(intvalue);
  int v = (int)Int_val(intvalue);
  if(v > INT16_MAX || v < INT16_MIN)
  {
    caml_failwith("int_to_int16 fail, can't fit into int16");
  }
  CAMLreturn(caml_copy_int16((int16_t)v));
}

CAMLprim value caml_int_to_int16_opt(value intvalue) {
  CAMLparam1(intvalue);
  int v = (int)Int_val(intvalue);
  if(v > INT16_MAX || v < INT16_MIN)
  {
    CAMLreturn(Val_none);
  }
  CAMLreturn(caml_alloc_some(caml_copy_int16((int16_t)v)));
}

CAMLprim value caml_int32_to_int16(value intvalue) {
  CAMLparam1(intvalue);
  int32_t v = (int32_t)Int32_val(intvalue);
  if(v > INT16_MAX || v < INT16_MIN)
  {
    caml_failwith("int_to_int16 fail, can't fit into int16");
  }
  CAMLreturn(caml_copy_int16((int16_t)v));
}

CAMLprim value caml_int32_to_int16_opt(value intvalue) {
  CAMLparam1(intvalue);
  int32_t v = (int32_t)Int32_val(intvalue);
  if(v > INT16_MAX || v < INT16_MIN)
  {
    CAMLreturn(Val_none);
  }
  CAMLreturn(caml_alloc_some(caml_copy_int16((int16_t)v)));
}

CAMLprim value caml_int16_to_int32(value int16value) {
  CAMLparam1(int16value);
  int16_t v = (int16_t)Int16_val(int16value);
  CAMLreturn(caml_copy_int32((int32_t)v));
}

CAMLprim value caml_int16_to_int(value int16value) {
  CAMLparam1(int16value);
  int16_t v = (int16_t)Int16_val(int16value);
  CAMLreturn(Val_int((int)v));
}

CAMLprim value caml_int64_to_int16(value intvalue) {
  CAMLparam1(intvalue);
  int64_t v = Int64_val(intvalue);
  if (v <= INT16_MAX && v >= INT16_MIN)
    CAMLreturn(caml_copy_int16((int16_t)v));
  else
    caml_failwith("Int64 can't fit into int16");
}

CAMLprim value caml_int64_to_int16_opt(value intvalue) {
  CAMLparam1(intvalue);
  int64_t val = Int64_val(intvalue);
  if (val <= INT16_MAX && val >= INT16_MIN)
    CAMLreturn(caml_alloc_some(caml_copy_int16((int16_t)val)));
  else
    CAMLreturn(Val_none);
}

CAMLprim value caml_int16_to_float(value int16value) {
  CAMLparam1(int16value);
  int16_t val = Int16_val(int16value);
  CAMLreturn(caml_copy_double((double)val));
}

CAMLprim value caml_nativeint_to_int16(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  CAMLreturn(caml_int32_to_int16(nativeint));
#elif BIT64_PLAT
  CAMLreturn(caml_int64_to_int16(nativeint));
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_nativeint_to_int16_opt(value nativeint) {
  CAMLparam1(nativeint);
#if BIT32_PLAT
  CAMLreturn(caml_int32_to_int16_opt(nativeint));
#elif BIT64_PLAT
  return caml_int64_to_int16_opt(nativeint);
#else
#error UNDEFINED
#endif
}

CAMLprim value caml_int16_to_nativeint(value int16value) {
  CAMLparam1(int16value);
  int16_t v = Int16_val(int16value);
  CAMLreturn(caml_copy_nativeint((intnat) v));
}

CAMLprim value caml_add_int16(value a, value b) {
  CAMLparam2(a, b);
  int16_t a1 = (int16_t)Int16_val(a);
  int16_t b2 = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(a1 + b2));
}

CAMLprim value caml_sub_int16(value a, value b) {
  CAMLparam2(a, b);
  int16_t a1 = (int16_t)Int16_val(a);
  int16_t b2 = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(a1 - b2));
}

CAMLprim value caml_mul_int16(value a, value b) {
  CAMLparam2(a, b);
  int16_t a1 = (int16_t)Int16_val(a);
  int16_t b2 = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(a1 * b2));
}

CAMLprim value caml_div_int16(value a, value b) {
  CAMLparam2(a, b);
  int16_t b2 = (int16_t)Int16_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  int16_t a1 = (int16_t)Int16_val(a);
  CAMLreturn(caml_copy_int16((int16_t)(a1 / b2)));
}

CAMLprim value caml_rem_int16(value a, value b) {
  CAMLparam2(a, b);
  int16_t b2 = (int16_t)Int16_val(b);
  if (b2 == 0)
    caml_failwith("Division by zero!");
  int16_t a1 = (int16_t)Int16_val(a);
  CAMLreturn(caml_copy_int16((int16_t)(a1 % b2)));
}

CAMLprim value caml_int16_to_string(value format, value int16val) {
    CAMLparam2(format, int16val);
    int16_t val = (int16_t)Int16_val(int16val);
    const char* pFormat = String_val(format);
    char buffer[6];
    sprintf(buffer, pFormat, val);
    CAMLreturn(caml_copy_string(buffer));
}

CAMLprim value caml_int16_formatter()
{
    CAMLparam0();
    CAMLreturn(caml_copy_string(PRId16));
}

CAMLprim value caml_string_to_int16(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) {
     if (res == PARSE_INVALID_INPUT) caml_failwith("int16_of_string fail, empty string");
     if (res == PARSE_OVERFLOW) caml_failwith("int16_of_string fail, can't fit into int16");
     caml_failwith("int16_of_string fail");
  }

  if (sign == 1) {
      if (val > INT16_MAX) caml_failwith("int16_of_string fail, can't fit into int16");
      CAMLreturn(caml_copy_int16((int16_t)val));
  } else {
      if (val > (uint64_t)(-(int64_t)INT16_MIN)) caml_failwith("int16_of_string fail, can't fit into int16");
      CAMLreturn(caml_copy_int16((int16_t)(-((int64_t)val))));
  }
}

CAMLprim value caml_string_to_int16_opt(value inputString) {
  CAMLparam1(inputString);
  const char *str = String_val(inputString);
  size_t strLen = caml_string_length(inputString);

  uint64_t val;
  int sign;
  ParseResult res = parse_number(str, strLen, &val, &sign);

  if (res != PARSE_SUCCESS) CAMLreturn(Val_none);

  if (sign == 1) {
      if (val > INT16_MAX) CAMLreturn(Val_none);
      CAMLreturn(caml_alloc_some(caml_copy_int16((int16_t)val)));
  } else {
      if (val > (uint64_t)(-(int64_t)INT16_MIN)) CAMLreturn(Val_none);
      CAMLreturn(caml_alloc_some(caml_copy_int16((int16_t)(-((int64_t)val)))));
  }
}

CAMLprim value caml_int16_logand(value a, value b) {
  CAMLparam2(a, b);
  int16_t ua = (int16_t)Int16_val(a);
  int16_t ub = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(ua & ub));
}

CAMLprim value caml_int16_logor(value a, value b) {
  CAMLparam2(a, b);
  int16_t ua = (int16_t)Int16_val(a);
  int16_t ub = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(ua | ub));
}

CAMLprim value caml_int16_logxor(value a, value b) {
  CAMLparam2(a, b);
  int16_t ua = (int16_t)Int16_val(a);
  int16_t ub = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(ua ^ ub));
}

CAMLprim value caml_int16_lognot(value a) {
  CAMLparam1(a);
  int16_t ua = (int16_t)Int16_val(a);
  CAMLreturn(caml_copy_int16(~ua));
}

CAMLprim value caml_int16_shift_left(value a, value b) {
  CAMLparam2(a, b);
  int16_t ua = (int16_t)Int16_val(a);
  int16_t ub = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(ua << ub));
}

CAMLprim value caml_int16_shift_right(value a, value b) {
  CAMLparam2(a, b);
  int16_t ua = (int16_t)Int16_val(a);
  int16_t ub = (int16_t)Int16_val(b);
  CAMLreturn(caml_copy_int16(ua >> ub));
}
