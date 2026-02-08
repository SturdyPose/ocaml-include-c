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
#include <cstddef>
#include <stdint.h>
#include <stdlib.h>


#include "shared.h"

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




static struct custom_operations manual_ptr_ops = {
    "manual_ptr_ops",           custom_finalize_default,
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

  p = caml_alloc_custom(&manual_ptr_ops, sizeof(void *), 0, 1);
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
  ptrRes = caml_alloc_custom(&manual_ptr_ops, sizeof(void *), 0, 1);
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
  ptrRes = caml_alloc_custom(&manual_ptr_ops, sizeof(void *), 0, 1);
  Ptr_val(ptrRes) = newPtr;

  CAMLreturn(ptrRes);
}

CAMLprim value caml_ptr_is_null(value ptr) {
  CAMLparam1(ptr);
  void* p = Ptr_val(ptr);
  CAMLreturn(Val_bool(p == NULL));
}

CAMLprim value caml_ptr_free(value ptr) {
  CAMLparam1(ptr);
  void* p = Ptr_val(ptr);
  free(p);
}

#define CREATE_PRIMITIVE(name, type, default_val)                              \
  CAMLprim value caml_create_ptr_##name() {                                    \
    CAMLparam0();                                                              \
    CAMLlocal1(p);                                                             \
    p = caml_ptr_alloc(caml_copy_nativeint(sizeof(type)));                     \
    *(type *)Ptr_val(p) = default_val;                                         \
    CAMLreturn(p);                                                             \
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


CAMLprim value caml_poke_bool(value ptr, value val) {
  CAMLparam2(ptr, val);
  bool* p = (bool*)Ptr_val(ptr);
  *p = Bool_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_poke_i8(value ptr, value val) {
  CAMLparam2(ptr, val);
  int8_t* p = (int8_t*)Ptr_val(ptr);
  *p = (int8_t)Int8_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_poke_i16(value ptr, value val) {
  CAMLparam2(ptr, val);
  int16_t* p = (int16_t*)Ptr_val(ptr);
  *p = (int16_t)Int16_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_poke_i32(value ptr, value val) {
  CAMLparam2(ptr, val);
  int32_t* p = (int32_t*)Ptr_val(ptr);
  *p = (int32_t)Int32_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_poke_i64(value ptr, value val) {
  CAMLparam2(ptr, val);
  int64_t* p = (int64_t*)Ptr_val(ptr);
  *p = (int64_t)Int64_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_poke_u8(value ptr, value val) {
  CAMLparam2(ptr, val);
  uint8_t* p = (uint8_t*)Ptr_val(ptr);
  *p = (uint8_t)UInt8_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_poke_u16(value ptr, value val) {
  CAMLparam2(ptr, val);
  uint16_t* p = (uint16_t*)Ptr_val(ptr);
  *p = (uint16_t)UInt16_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_poke_u32(value ptr, value val) {
  CAMLparam2(ptr, val);
  uint32_t* p = (uint32_t*)Ptr_val(ptr);
  *p = (uint32_t)UInt32_val(val);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_poke_u64(value ptr, value val) {
  CAMLparam2(ptr, val);
  uint64_t* p = (uint64_t*)Ptr_val(ptr);
  *p = (uint64_t)UInt64_val(val);
  CAMLreturn(Val_unit);
}

/************
Peeking values
*************/
CAMLprim value caml_peek_bool(value ptr) {
  CAMLparam1(ptr);
  bool* p = (bool*)Ptr_val(ptr);
  CAMLreturn(Val_bool(*p));
}

CAMLprim value caml_peek_i8(value ptr) {
  CAMLparam1(ptr);
  int8_t* p = (int8_t*)Ptr_val(ptr);
  CAMLreturn(caml_copy_int8(*p));
}

CAMLprim value caml_peek_i16(value ptr) {
  CAMLparam1(ptr);
  int16_t* p = (int16_t*)Ptr_val(ptr);
  CAMLreturn(caml_copy_int16(*p));
}

CAMLprim value caml_peek_i32(value ptr) {
  CAMLparam1(ptr);
  int32_t* p = (int32_t*)Ptr_val(ptr);
  CAMLreturn(caml_copy_int32(*p));
}

CAMLprim value caml_peek_i64(value ptr) {
  CAMLparam1(ptr);
  int64_t* p = (int64_t*)Ptr_val(ptr);
  CAMLreturn(caml_copy_int64(*p));
}

CAMLprim value caml_peek_u8(value ptr) {
  CAMLparam1(ptr);
  uint8_t* p = (uint8_t*)Ptr_val(ptr);
  CAMLreturn(caml_copy_uint8(*p));
}

CAMLprim value caml_peek_u16(value ptr) {
  CAMLparam1(ptr);
  uint16_t* p = (uint16_t*)Ptr_val(ptr);
  CAMLreturn(caml_copy_uint16(*p));
}

CAMLprim value caml_peek_u32(value ptr) {
  CAMLparam1(ptr);
  uint32_t* p = (uint32_t*)Ptr_val(ptr);
  CAMLreturn(caml_copy_uint32(*p));
}

CAMLprim value caml_peek_u64(value ptr) {
  CAMLparam1(ptr);
  uint64_t* p = (uint64_t*)Ptr_val(ptr);
  CAMLreturn(caml_copy_uint64(*p));
}


// Read arbitrary value
CAMLprim value caml_peek_n(value ptr, value sizeVal) {
  CAMLparam2(ptr, sizeVal);
  CAMLlocal1(allocated);
  void* p = Ptr_val(ptr);
  size_t size = (size_t)Nativeint_val(sizeVal);

  struct custom_operations n_alloc = {"n_alloc",
                                      custom_finalize_default,
                                      custom_compare_default,
                                      custom_hash_default,
                                      custom_serialize_default,
                                      custom_deserialize_default,
                                      custom_compare_ext_default,
                                      custom_fixed_length_default};

  allocated = caml_alloc_custom(&n_alloc, size, 0, 1);
  memcpy(Data_custom_val(allocated), src, size);
  CAMLreturn(allocated);
}
