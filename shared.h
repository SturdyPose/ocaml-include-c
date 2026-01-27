#pragma once

#if UINTPTR_MAX == 0xFFFFFFFFFFFFFFFFu
  #define BIT64_PLAT true
  #define BIT32_PLAT false 
  #define GET_SIZE Int64_val
  #define GET_PTR_ADDR Int64_val
//   #define Val_nativeint(x) caml_copy_int64(uint64_t x)
#elif UINTPTR_MAX == 0xFFFFFFFF
  #define BIT64_PLAT false 
  #define BIT32_PLAT true 
  #define GET_SIZE Int32_val
  #define GET_PTR_ADDR Int32_val
//   #define Val_nativeint caml_copy_int32(uint32_t x)
#elif UINTPTR_MAX == 0xFFFF
  #error TO BE ADDED
#else
  #error TBD pointer size
#endif

#define Int16_val(v) (*((int16_t *)Data_custom_val(v)))
#define UInt32_val(v) (*((uint32_t *)Data_custom_val(v)))
#define UInt64_val(v) (*((uint64_t *)Data_custom_val(v)))

static int parseDigit(char c) {
  switch (c) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    return c - '0';
  case 'A':
  case 'B':
  case 'C':
  case 'D':
  case 'E':
  case 'F':
    return c - 'A' + 10;
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
  case 'f':
    return c - 'a' + 10;
  default:
    return -1;
  }
}
