#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <ffi.h>
#include <stdlib.h>

#define MEM 65536
#define STACK 16384

typedef long int64_t;

__attribute__((aligned(16)))
int64_t mem[MEM];
__attribute__((aligned(16)))
int64_t stack[STACK];

int64_t *stack_end = stack + STACK;

static int64_t ptr = 0;
static int64_t *xmalloc(int64_t size) {
  int64_t *res = &(mem[ptr]);
  ptr += size;
  return res;
}

typedef int64_t *tuple_t;

tuple_t create_tuple(int64_t size, int64_t init) {
  tuple_t tuple = xmalloc(size + 1);
  tuple[0] = size;
  for (int64_t i = 0; i < size; i++)
    tuple[i + 1] = ((int64_t*) init)[i];

  return tuple;
}

int64_t tuple_nth(int64_t tuple, int64_t i) {
  return ((int64_t*) tuple)[i + 1];
}

typedef int64_t *closure_t;

#define debugf printf

int64_t create_closure(int64_t callee, int64_t arity, int64_t argc, int64_t argv_) {
  assert(argc < arity);

  debugf("> create_closure(%ld, %ld, %ld, %ld)\n", callee, arity, argc, argv_);

  int64_t *argv = (int64_t*) argv_;

  closure_t closure = xmalloc(arity + 3);
  closure[0] = callee;
  closure[1] = arity;
  closure[2] = argc;

  for (int64_t i = 0; i < argc; i++) {
    closure[i + 3] = argv[i];
  }
  debugf("< create_closure() -> %ld\n", (int64_t) closure);

  return (int64_t) closure;
}

int64_t copy_closure(int64_t closure_) {
  debugf("> copy_closure(%ld)\n", closure_);

  closure_t closure = (closure_t) closure_;
  int64_t arity = closure[1];

  closure_t closure2 = xmalloc(arity + 3);
  for (int64_t i = 0; i < arity + 3; i++) {
    closure2[i] = closure[i];
  }

  debugf("< copy_closure() -> %ld\n", (int64_t) closure2);

  return (int64_t) closure2;
}

int64_t closure_apply(int64_t closure_, int64_t argc, int64_t argv_) {
  debugf("> closure_apply(%ld, %ld, %ld)\n", closure_, argc, argv_);

  int64_t *argv = (int64_t*) argv_;
  closure_t closure = (closure_t) (copy_closure (closure_));
  debugf("  closure_apply: closure stats %ld %ld %ld\n", closure[0], closure[1], closure[2]);
  int64_t current = closure[2];
  for (int64_t i = 0; i < argc; i++) {
    debugf("  closure_apply: arg %ld %ld is %ld in the orig\n", i, argv[i], i + current);
    closure[i + current + 3] = argv[i];
    closure[2]++;
  }

  if (closure[2] >= closure[1]) {
    debugf("  closure_apply: calling %ld %ld %ld\n", closure[0], closure[1], (int64_t) &(closure[3]));

    ffi_cif func;

    int64_t arity = closure[1];
    ffi_type *args_t[arity];
    void *args_v[arity];
    for (int64_t i = 0; i < arity; i++) {
        args_t[i] = &ffi_type_sint64;
        args_v[i] = &closure[i + 3];
    }

    if (ffi_prep_cif(&func, FFI_DEFAULT_ABI, arity, &ffi_type_sint64, args_t) != FFI_OK) {
        debugf("closure call failed");
        exit(1);
    }

    ffi_sarg ret;
    ffi_call(&func, FFI_FN(closure[0]), &ret, args_v);

    return ret;
  } else {
    debugf("  closure_apply: returning a new closure %ld %ld %ld\n", closure[0], closure[1], (int64_t) &(closure[3]));
    return (int64_t) closure;
  }
}

int64_t print_int(int64_t n) {
  return printf("%ld\n", n);
}

void exit2(void) {
  exit(0);
}
