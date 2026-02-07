#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>

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

#define Int8_val(v) (*((int8_t *)Data_custom_val(v)))
#define Int16_val(v) (*((int16_t *)Data_custom_val(v)))
#define UInt8_val(v) (*((uint8_t *)Data_custom_val(v)))
#define UInt16_val(v) (*((uint16_t *)Data_custom_val(v)))
#define UInt32_val(v) (*((uint32_t *)Data_custom_val(v)))
#define UInt64_val(v) (*((uint64_t *)Data_custom_val(v)))

static inline int parseDigit(char c) {
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

typedef enum {
    PARSE_SUCCESS = 0,
    PARSE_FAILURE = 1,
    PARSE_OVERFLOW = 2,
    PARSE_INVALID_INPUT = 3
} ParseResult;

static inline ParseResult parse_number(const char* str, size_t strLen, uint64_t* out_val, int* out_sign) {
    if (str == NULL || strLen == 0) return PARSE_INVALID_INPUT;
    
    const char *it = str;
    int sign = 1;
    
    if (*it == '-') {
        sign = -1;
        it++;
    } else if (*it == '+') {
        it++;
    }

    if (*it == '\0') {
        return PARSE_FAILURE; // Empty after sign
    }
    
    *out_sign = sign;

    int32_t base = 10;
    if (*it == '0') {
        char next = *(it + 1);
        if (next == 'x' || next == 'X') {
            base = 16;
            it += 2;
        } else if (next == 'o' || next == 'O') {
            base = 8;
            it += 2;
        } else if (next == 'b' || next == 'B') {
            base = 2;
            it += 2;
        } else {
             if ((next >= '0' && next <= '9') || next == 'u' || next == 'U') {
                 it++;
             }
        }
    }

    // Check if string became empty after prefix
    if (*it == '\0' && (it > str) && base != 10) {
        // e.g. "0x"
        return PARSE_FAILURE; 
    }

    uint64_t result = 0;
    while (*it) {
        int val = parseDigit(*it);
        if (val == -1 || val >= base) return PARSE_FAILURE;

        // Check overflow: (UINT64_MAX - val) / base < result
        if ((UINT64_MAX - (uint64_t)val) / (uint64_t)base < result) {
            return PARSE_OVERFLOW;
        }

        result = result * (uint64_t)base + (uint64_t)val;
        it++;
    }

    *out_val = result;
    return PARSE_SUCCESS;
}
