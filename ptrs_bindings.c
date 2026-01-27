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
#include <stdint.h>
#include <stdlib.h>


#include "shared.h"

// TODO: Add support for 32bit and 16bit platforms

#define Ptr_val(v) (*(void **)Data_custom_val(v))

static void finalize_ptr(value v) {
  void *ptr = Ptr_val(v);
  if (ptr != NULL) {
    free(ptr);
  }
}

static struct custom_operations alloc_ops = {"alloc_ops",
                                             finalize_ptr,
                                             custom_compare_default,
                                             custom_hash_default,
                                             custom_serialize_default,
                                             custom_deserialize_default,
                                             custom_compare_ext_default,
                                             custom_fixed_length_default};

static struct custom_operations derived_ptr_ops = {
    "derived_ptr_ops",          custom_finalize_default,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value caml_ptr_alloc(value size) {
  CAMLparam1(size);
  CAMLlocal1(p);

  size_t s = GET_SIZE(size);
  void *pValue = malloc(s);
  if (pValue == NULL)
    caml_failwith("Malloc failed in caml_ptr_alloc");

  p = caml_alloc_custom(&alloc_ops, sizeof(void *), 0, 1);
  Ptr_val(p) = pValue;
  CAMLreturn(p);
}

CAMLprim value caml_ptr_add(value ptr, value offset) {
  CAMLparam2(ptr, offset);
  CAMLlocal1(ptrRes);
  void *p = Ptr_val(ptr);
  size_t off = GET_SIZE(offset);

  // TODO: Maybe utilize small block allocations instead of this?
  void *newPtr = (void *)(p + off);
  ptrRes = caml_alloc_custom(&derived_ptr_ops, sizeof(void *), 0, 1);
  Ptr_val(ptrRes) = newPtr;

  CAMLreturn(ptrRes);
}

CAMLprim value caml_ptr_sub(value ptr, value offset) {
  CAMLparam2(ptr, offset);
  CAMLlocal1(ptrRes);
  void *p = Ptr_val(ptr);
  size_t off = GET_SIZE(offset);

  // TODO: Maybe utilize small block allocations instead of this?
  void *newPtr = (void *)(p - off);
  ptrRes = caml_alloc_custom(&derived_ptr_ops, sizeof(void *), 0, 1);
  Ptr_val(ptrRes) = newPtr;

  CAMLreturn(ptrRes);
}

#define CREATE_PRIMITIVE(name, type, default_val)                              \
  CAMLprim value caml_create_ptr_##name() {                                    \
    CAMLparam0();                                                              \
    CAMLlocal1(p);                                                             \
    p = caml_ptr_alloc(caml_copy_nativeint(sizeof(type)));                     \
    *(type *)Ptr_val(p) = default_val;                                         \
    CAMLreturn(p);                                                             \
  }

CAMLprim value caml_peek_bool(value ptrAddr) {
  CAMLparam1(ptrAddr);
  bool *pBool = (bool *)Ptr_val(ptrAddr);
  CAMLreturn(Val_bool(*pBool));
}

CAMLprim value caml_peek_int(value ptrAddr) {
  CAMLparam1(ptrAddr);
  int *pInt = (int *)Ptr_val(ptrAddr);
  CAMLreturn(Val_int(*pInt));
}

CAMLprim value caml_peek_int64(value ptrAddr) {
  CAMLparam1(ptrAddr);
  int64_t *pInt = (int64_t *)Ptr_val(ptrAddr);
  CAMLreturn(caml_copy_int64(*pInt));
}

// Based on https://en.wikipedia.org/wiki/C_data_types
CREATE_PRIMITIVE(bool, bool, false)

// CREATE_PRIMITIVE(char, char, 0)
// CREATE_PRIMITIVE(schar, signed char, 0)
// CREATE_PRIMITIVE(uchar, unsigned char, 0)

// CREATE_PRIMITIVE(short, short, 0)
// CREATE_PRIMITIVE(shortint, short int, 0)
// CREATE_PRIMITIVE(sshor, signed short, 0)
// CREATE_PRIMITIVE(sshortint, signed short int, 0)

// CREATE_PRIMITIVE(ushort, unsigned short, 0)
// CREATE_PRIMITIVE(ushortint, unsigned short int, 0)

// CREATE_PRIMITIVE(long, long, 0)
// CREATE_PRIMITIVE(longint, long int, 0)
// CREATE_PRIMITIVE(slong, signed long, 0)
// CREATE_PRIMITIVE(slongint, signed long int, 0)

// CREATE_PRIMITIVE(ulong, unsigned long, 0)
// CREATE_PRIMITIVE(ulongint, unsigned long int, 0)

// CREATE_PRIMITIVE(llong, long long, 0)
// CREATE_PRIMITIVE(llongint, long long int, 0)
// CREATE_PRIMITIVE(sllong, signed long long, 0)
// CREATE_PRIMITIVE(sllongint, signed long long int, 0)

CREATE_PRIMITIVE(uint8, uint8_t, 0)
CREATE_PRIMITIVE(int8, int8_t, 0)
CREATE_PRIMITIVE(uint16, uint16_t, 0)
CREATE_PRIMITIVE(int16, int16_t, 0)
CREATE_PRIMITIVE(uint32, uint32_t, 0)
CREATE_PRIMITIVE(int32, int32_t, 0)
CREATE_PRIMITIVE(uint64, uint64_t, 0)
CREATE_PRIMITIVE(int64, int64_t, 0)

CREATE_PRIMITIVE(float, float, 0.0)
CREATE_PRIMITIVE(double, double, 0.0)
