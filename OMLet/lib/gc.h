#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint8_t tag_t;
enum {
  T_UNBOXED = 0,
  T_CLOSURE = 1,
};

void *omlet_malloc(size_t size, tag_t tag);
