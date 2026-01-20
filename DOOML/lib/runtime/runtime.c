#include "call-runtime.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#define MEM 65536

typedef long int64_t;

typedef struct {
    uint32_t size;
    uint32_t allocated_size;
    uint8_t bank_idx;
    uint64_t runs;
} GCStats;

typedef struct {
    GCStats stats;

    int64_t *from_bank_start;
    uint32_t from_bank_size;
    int64_t *to_bank_start;
    uint32_t to_bank_size;

    int64_t *free_space_start;
} GC;

typedef enum {
    Tuple,
    Closure,
    Forward,
    ENUM_SIZE_FORCE_32BIT = UINT8_MAX
} GCObjTag;

__attribute__((aligned(8)))
typedef struct {
    uint32_t size;
    GCObjTag tag;
} GCObjHeader;

typedef struct {
    GCObjHeader header;
    int64_t callee;
    int64_t arity;
    int64_t argc;
    int64_t args[];
} GCClosure;

typedef struct {
    GCObjHeader header;
    int64_t size;
    int64_t fields[];
} GCTuple;

typedef struct {
    GCObjHeader header;
    int64_t ptr;
} GCForward;

static const uint64_t GC_BANK_SIZE = MEM;
static GC gc;

#define debugf printf

void init_gc() {
    GCStats init_stats = {
        .runs = 0,
        .bank_idx = 0,
        .size = 0,
        .allocated_size = 0,
    };
    GC init_gc = {
        .stats = init_stats,
        .from_bank_size = GC_BANK_SIZE,
        .from_bank_start = malloc(GC_BANK_SIZE),
        .to_bank_size = GC_BANK_SIZE,
        .to_bank_start = malloc(GC_BANK_SIZE),
    };
    init_gc.free_space_start = init_gc.from_bank_start;
    gc = init_gc;
}

int64_t *alloc_gc(uint32_t size, GCObjTag tag) {
    int64_t *ptr = gc.free_space_start;
    uint32_t taken_bytes = ((uint32_t) (ptr - gc.from_bank_start)) / 8;
    uint32_t free_space = gc.from_bank_size - taken_bytes;
    debugf("> alloc_gc(%u): had %u free space\n", size, free_space);

    if (free_space < size) {
        fprintf(stderr, "GC OOM\n");
        exit(1);
    }

    gc.free_space_start += size * 8;
    gc.stats.allocated_size += size;
    gc.stats.size += size;
    ((GCObjHeader *)ptr)->size = size;
    ((GCObjHeader *)ptr)->tag = tag;

    return ptr;
}

int64_t box_imm(int64_t n) {
    return (n << 1) + 1;
}

bool is_imm(int64_t n) {
    return n | 1;
}

int64_t unbox(int64_t n) {
    if (!is_imm(n)) {
        return n;
    }
    return n >> 1;
}

int64_t create_tuple(int64_t size, int64_t init) {
  GCTuple *tuple = (GCTuple *)alloc_gc(sizeof(GCTuple) + size * 8, Tuple);
  tuple->size = size;
  for (int64_t i = 0; i < size; i++) {
    tuple->fields[i] = ((int64_t*) init)[i];
  }

  return (int64_t) tuple;
}

int64_t tuple_nth(int64_t tuple, int64_t i) {
  int64_t unboxed_i = unbox(i);
  GCTuple *tuple_ptr = (GCTuple*) tuple;
  if (unboxed_i >= tuple_ptr->size) {
    fprintf(stderr, "tuple_nth: index is out of bounds\n");
    exit(1);
  }
  return tuple_ptr->fields[unboxed_i];
}

int64_t create_closure(int64_t callee, int64_t arity, int64_t argc, int64_t argv_) {
  assert(argc < arity);

  debugf("> create_closure(%ld, %ld, %ld, %ld)\n", callee, arity, argc, argv_);

  int64_t *argv = (int64_t*) argv_;

  GCClosure *closure = (GCClosure *)alloc_gc(sizeof(GCClosure) + arity * 8, Closure);
  closure->callee = callee;
  closure->arity = arity;
  closure->argc = argc;

  for (int64_t i = 0; i < argc; i++) {
    closure->args[i] = argv[i];
  }
  debugf("< create_closure() -> %ld\n", (int64_t) closure);

  return (int64_t) closure;
}

GCClosure *copy_closure(GCClosure *closure) {
  debugf("> copy_closure(%ld)\n", (int64_t) closure);

  GCClosure *closure2 = (GCClosure *)alloc_gc(sizeof(GCClosure) + closure->arity * 8, Closure);
  closure2->callee = closure->callee;
  closure2->arity = closure->arity;
  closure2->argc = closure->argc;
  for (int64_t i = 0; i < closure->argc; i++) {
    closure2->args[i] = closure->args[i];
  }

  debugf("< copy_closure() -> %ld\n", (int64_t) closure2);

  return closure2;
}

int64_t closure_apply(int64_t closure_, int64_t argc, int64_t argv_) {
  debugf("> closure_apply(%ld, %ld, %ld)\n", closure_, argc, argv_);

  int64_t *argv = (int64_t*) argv_;
  GCClosure *closure = copy_closure((GCClosure *)closure_);
  debugf("  closure_apply: closure stats %ld %ld %ld\n", closure->callee, closure->arity, closure->argc);
  int64_t current_argc = closure->argc;
  for (int64_t i = 0; i < argc; i++) {
    debugf("  closure_apply: arg %ld %ld is %ld in the orig\n", i, argv[i], i + current_argc);
    closure->args[i + current_argc] = argv[i];
    closure->argc++;
  }

  if (closure->argc >= closure->arity) {
    debugf("  closure_apply: calling %ld %ld %ld\n", closure->callee, closure->arity, (int64_t) closure->args);
    return call_function((void*) closure->callee, closure->arity, closure->args);
  } else {
    debugf("  closure_apply: returning a new closure %ld %ld %ld\n", closure->callee, closure->arity, (int64_t) closure->args);
    return (int64_t) closure;
  }
}

int64_t print_int(int64_t n) {
  return box_imm(printf("%ld\n", unbox(n)));
}

void exit2(void) {
  exit(0);
}
