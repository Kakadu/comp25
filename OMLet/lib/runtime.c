#include "gc.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void *callf(void *code, uint64_t argc, void **argv);

#ifdef ENABLE_GC
extern void *omlet_malloc(size_t size, tag_t type_tag);
#endif

static inline void *my_alloc(size_t size, tag_t type_tag) {
#ifdef ENABLE_GC
  return omlet_malloc(size, type_tag);
#else
  (void)type_tag;
  return malloc(size);
#endif
}

typedef struct {
  void *code;       // function pointer
  int64_t arity;    // total number of arguments
  int64_t received; // how many arguments are already applied
  void *args[];     // flexible array of applied arguments
} closure;

// allocate a new closure
closure *alloc_closure(void *code, int64_t arity) {
  closure *c = my_alloc(sizeof(closure) + sizeof(void *) * arity, T_CLOSURE);
  c->code = code;
  c->arity = arity;
  c->received = 0;
  memset(c->args, 0, sizeof(void *) * arity);
  return c;
}

int8_t is_pointer(int64_t arg) { return !(arg & 1); };

// apply arguments to a closure
void *apply(closure *tagged_f, int64_t arity, void **args, int64_t argc) {
  closure *f = tagged_f;

  int64_t total = f->received + argc;

  // full application
  if (total == f->arity) {
    void **all_args = my_alloc(sizeof(void *) * f->arity, T_UNBOXED);

    for (int i = 0; i < f->received; i++)
      all_args[i] = f->args[i];
    for (int i = 0; i < argc; i++)
      all_args[f->received + i] = args[i];

    void *result = callf(f->code, f->arity, all_args);
    return result;
  }

  closure *partial =
      my_alloc(sizeof(closure) + sizeof(void *) * f->arity, T_CLOSURE);
  partial->code = f->code;
  partial->arity = f->arity;
  partial->received = total;

  for (int i = 0; i < f->received; i++) {
    partial->args[i] = f->args[i];
  }
  for (int i = 0; i < argc; i++)
    partial->args[f->received + i] = args[i];

  return partial;
}

void print_int(int64_t a) {
  int64_t res = a >> 1;
  printf("%ld", res);
  fflush(stdout);
}
