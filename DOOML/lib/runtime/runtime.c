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
static int64_t *initial_sp; 

#define debugf printf

void gc_init() {
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

void sp_init(int64_t sp) {
    initial_sp = (int64_t *)sp;
}

void collect();

int64_t *gc_alloc(uint32_t size, GCObjTag tag) {
    int64_t *ptr = gc.free_space_start;
    uint32_t taken_bytes = ((uint32_t) (ptr - gc.from_bank_start)) / 8;
    uint32_t free_space = gc.from_bank_size - taken_bytes;
    debugf("> alloc_gc(%u): had %u free space\n", size, free_space);

    if (free_space < size) {
        collect();
        ptr = gc.free_space_start;
        taken_bytes = ((uint32_t) (ptr - gc.from_bank_start)) / 8;
        free_space = gc.from_bank_size - taken_bytes;
        if (free_space < size) {
            fprintf(stderr, "GC OOM\n");
            exit(1);
        }
    }

    gc.free_space_start += size * 8;
    gc.stats.allocated_size += size;
    gc.stats.size += size;
    ((GCObjHeader *)ptr)->size = size;
    ((GCObjHeader *)ptr)->tag = tag;

    return ptr;
}

GCClosure *gc_alloc_closure_base(int64_t callee, int64_t arity, int64_t argc) {
  GCClosure *closure = (GCClosure *)gc_alloc(sizeof(GCClosure) + arity * 8, Closure);
  closure->callee = callee;
  closure->arity = arity;
  closure->argc = argc;
  return closure;
}

GCTuple *gc_alloc_tuple_base(int64_t size) {
  GCTuple *tuple = (GCTuple *)gc_alloc(sizeof(GCTuple) + size * 8, Tuple);
  tuple->size = size;
  return tuple;
}

void gc_make_fwd(int64_t ptr, int64_t new_ptr) {
    ((GCObjHeader *)ptr)->tag = Forward;
    ((GCForward *)ptr)->ptr = new_ptr;
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

int64_t *get_sp() {
    return (int64_t *)__builtin_frame_address(0);
}

int64_t gc_scan_ptrs_on_stack(int64_t *range_start, int64_t *range_end, int64_t** ptrs) {
    int64_t *sp = get_sp(); 
    int64_t ptrs_idx = 0;
    // start from stack bottom to preserve order for minimizing forward ptr usages
    for (int64_t i = 0; initial_sp + i < sp; i++) {
        int64_t stack_val = initial_sp[i];
        if (!is_imm(stack_val) && stack_val > (int64_t)range_start && stack_val < (int64_t)range_end) {
            ptrs[ptrs_idx] = initial_sp + i;
            ptrs_idx += 1;
        }
    }

    return ptrs_idx;
}

int64_t gc_mark_and_copy(int64_t ptr) {
    GCObjTag tag = ((GCObjHeader *)ptr)->tag;
    if (tag == Forward) {
        return ((GCForward *)ptr)->ptr;
    }

    GCObjTag size = ((GCObjHeader *)ptr)->tag;

    if (tag == Closure) {
        GCClosure *closure = (GCClosure *)ptr;
        GCClosure *closure2 = gc_alloc_closure_base(closure->callee, closure->arity, closure->argc);

        for (int64_t i = 0; i < closure2->argc; i++) {
            int64_t arg = closure->args[i];
            if (is_imm(arg)) {
                closure2->args[i] = arg;
            } else {
                closure2->args[i] = gc_mark_and_copy(arg);
            }
        }

        gc_make_fwd(ptr, (int64_t)closure2);
        return (int64_t)closure2;
    }

    if (tag == Tuple) {
        GCTuple *tuple = (GCTuple *)ptr;
        GCTuple *tuple2 = gc_alloc_tuple_base(tuple->size);

        for (int64_t i = 0; i < tuple2->size; i++) {
            int64_t field = tuple->fields[i];
            if (is_imm(field)) {
                tuple2->fields[i] = field;
            } else {
                tuple2->fields[i] = gc_mark_and_copy(field);
            }
        }

        gc_make_fwd(ptr, (int64_t)tuple2);
        return (int64_t)tuple2;
    }

    fprintf(stderr, "unknown gc tag %u\n", tag);
    exit(1);
}

void collect() {
    int64_t *ptrs[UINT32_MAX];
    int64_t ptrs_size = gc_scan_ptrs_on_stack(gc.from_bank_start, gc.free_space_start, ptrs);

    int64_t *to_bank_start = gc.to_bank_start;
    int64_t to_bank_size = gc.to_bank_size;
    gc.to_bank_start = gc.from_bank_start;
    gc.from_bank_start = to_bank_start;
    gc.to_bank_size = gc.from_bank_size;
    gc.from_bank_size = to_bank_size;
    gc.free_space_start = gc.from_bank_start;
    gc.stats.bank_idx = 1 - gc.stats.bank_idx;
    gc.stats.size = 0;

    for (int64_t i = 0; i < ptrs_size; i++) {
        int64_t *old_ptr_on_stack = ptrs[i];
        *old_ptr_on_stack = gc_mark_and_copy(*old_ptr_on_stack);
    }

    gc.stats.runs += 1;
}

int64_t create_tuple(int64_t size, int64_t init) {
  GCTuple *tuple = gc_alloc_tuple_base(size);
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

  GCClosure *closure = gc_alloc_closure_base(callee, arity, argc);

  int64_t *argv = (int64_t*) argv_;
  for (int64_t i = 0; i < argc; i++) {
    closure->args[i] = argv[i];
  }
  debugf("< create_closure() -> %ld\n", (int64_t) closure);

  return (int64_t) closure;
}

GCClosure *copy_closure(GCClosure *closure) {
  debugf("> copy_closure(%ld)\n", (int64_t) closure);

  GCClosure *closure2 = gc_alloc_closure_base(closure->callee, closure->arity, closure->argc);
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
