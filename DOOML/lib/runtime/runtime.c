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

#ifdef DEBUG
#define debugf printf
#define debug_call(...) __VA_ARGS__
#else
#define debugf(...)
#define debug_call(...)
#endif

int64_t box_imm(int64_t n) {
    return (n << 1) + 1;
}

bool is_imm(int64_t n) {
    return n & 1;
}

int64_t unbox(int64_t n) {
    if (!is_imm(n)) {
        return n;
    }
    return n >> 1;
}

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
    debugf("> bank 0 ranges: %ld : %ld\n", (int64_t)init_gc.from_bank_start, (int64_t)(init_gc.from_bank_start + init_gc.from_bank_size));
    debugf("> bank 1 ranges: %ld : %ld\n", (int64_t)init_gc.to_bank_start, (int64_t)(init_gc.to_bank_start + init_gc.to_bank_size));
    gc = init_gc;
}

static inline int64_t *get_sp() {
    return (int64_t *)__builtin_frame_address(0);
}

void sp_init() {
    initial_sp = get_sp();
}

void print_obj_helper(int64_t ptr) {
    if (is_imm(ptr)) {
        printf("int %ld", unbox(ptr));
        return;
    }
    GCObjTag tag = ((GCObjHeader *)ptr)->tag;
    if (tag == Forward) {
        printf("Forward -> %ld: ", ((GCForward *)ptr)->ptr);
        print_obj_helper(((GCForward *)ptr)->ptr);
    }
    if (tag == Closure) {
        GCClosure *closure = (GCClosure *)ptr;
        printf("Closure %ld(", closure->callee);
        for (int i = 0; i < closure->arity; i++) {
            if (i < closure->argc) {
                print_obj_helper(closure->args[i]);
            } else {
                printf("...");
            }
            if (i != closure->arity - 1) {
                printf(", ");
            }
        }
        printf(")");
    }
    if (tag == Tuple) {
        GCTuple *tuple = (GCTuple *)ptr;
        printf("Tuple (");
        for (int i = 0; i < tuple->size; i++) {
            print_obj_helper(tuple->fields[i]);
            if (i != tuple->size - 1) {
                printf(", ");
            }
        }
        printf(")");
    }
}

void debug_print_value(int64_t ptr) {
    debug_call(printf("%ld: ", ptr));
    debug_call(print_obj_helper(ptr));
    debug_call(printf("\n"));
}

void gc_collect();

int64_t *gc_alloc(uint32_t size, GCObjTag tag) {
    int64_t *ptr = gc.free_space_start;
    uint32_t taken_bytes = ((uint32_t) (ptr - gc.from_bank_start)) / 8;
    uint32_t free_space = gc.from_bank_size - taken_bytes;

    if (free_space < size) {
        gc_collect();
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
  int64_t size = sizeof(GCClosure) + arity * 8;
  GCClosure *closure = (GCClosure *)gc_alloc(size, Closure);
  closure->callee = callee;
  closure->arity = arity;
  closure->argc = argc;
  debugf("< alloc(%ld): closure %ld with %ld(%ld out of %ld)\n", size, (int64_t)closure, callee, argc, arity);
  return closure;
}

GCTuple *gc_alloc_tuple_base(int64_t size) {
  int64_t obj_size = sizeof(GCClosure) + size * 8;
  GCTuple *tuple = (GCTuple *)gc_alloc(obj_size, Tuple);
  tuple->size = size;
  debugf("< alloc(%ld): tuple %ld with size %ld\n", obj_size, (int64_t)tuple, size);
  return tuple;
}

void gc_make_fwd(int64_t ptr, int64_t new_ptr) {
    ((GCObjHeader *)ptr)->tag = Forward;
    ((GCForward *)ptr)->ptr = new_ptr;
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

void gc_collect() {
    int64_t *sp = get_sp(); 
    int64_t gc_bank_range_start = (int64_t)gc.from_bank_start;
    int64_t gc_bank_range_end = (int64_t)gc.free_space_start;

    int64_t *to_bank_start = gc.to_bank_start;
    int64_t to_bank_size = gc.to_bank_size;
    gc.to_bank_start = gc.from_bank_start;
    gc.from_bank_start = to_bank_start;
    gc.to_bank_size = gc.from_bank_size;
    gc.from_bank_size = to_bank_size;
    gc.free_space_start = gc.from_bank_start;
    gc.stats.bank_idx = 1 - gc.stats.bank_idx;
    gc.stats.size = 0;

    debugf("> gc_collect\n");
    for (int64_t stack_cell = (int64_t)initial_sp; stack_cell >= (int64_t)sp; stack_cell--) {
        int64_t obj_ptr = *(int64_t*)stack_cell;
        if (!is_imm(obj_ptr) && obj_ptr >= gc_bank_range_start && obj_ptr < gc_bank_range_end) {
            debugf("  gc_root: %ld\n", obj_ptr);
            debug_print_value(obj_ptr);
            *(int64_t*)stack_cell = gc_mark_and_copy(obj_ptr);
            debugf("  new pointer on stack: %ld\n", *(int64_t*)stack_cell);
        }
    }

    gc.stats.runs += 1;
    debugf("< gc_collect\n");
}

void collect(int64_t unit) {
    gc_collect();
}

int64_t get_heap_start(int64_t unit) {
    return box_imm((int64_t)gc.from_bank_start);
}

int64_t get_heap_fin(int64_t unit) {
    return box_imm((int64_t)(gc.from_bank_start + gc.from_bank_size));
}

void print_gc_status(int64_t unit) {
    printf("GC status\n");
    printf("Bank index: %u\n", gc.stats.bank_idx);
    printf("Bank capacity: %u\n", gc.from_bank_size);
    printf("Allocated: %u\n", gc.stats.size);
    printf("Total allocated: %u\n", gc.stats.allocated_size);
    printf("GC runs: %ld\n", gc.stats.runs);
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

  GCClosure *closure = gc_alloc_closure_base(callee, arity, argc);

  int64_t *argv = (int64_t*) argv_;
  for (int64_t i = 0; i < argc; i++) {
    closure->args[i] = argv[i];
  }

  return (int64_t) closure;
}

GCClosure *copy_closure(GCClosure *closure) {
  GCClosure *closure2 = gc_alloc_closure_base(closure->callee, closure->arity, closure->argc);
  for (int64_t i = 0; i < closure->argc; i++) {
    closure2->args[i] = closure->args[i];
  }

  return closure2;
}

int64_t closure_apply(int64_t closure_, int64_t argc, int64_t argv_) {
  debugf("> closure_apply\n");
  debugf("  orig: ");
  debug_print_value(closure_);

  int64_t *argv = (int64_t*) argv_;
  GCClosure *closure = copy_closure((GCClosure *)closure_);
  int64_t current_argc = closure->argc;
  for (int64_t i = 0; i < argc; i++) {
    closure->args[i + current_argc] = argv[i];
    closure->argc++;
  }

  debugf("  applied: ");
  debug_print_value((int64_t)closure);

  if (closure->argc >= closure->arity) {
    debugf("  closure_apply: calling\n");
    return call_function((void*) closure->callee, closure->arity, closure->args);
  } else {
    debugf("  closure_apply: returning a new closure");
    return (int64_t) closure;
  }
}

int64_t print_int(int64_t n) {
  return box_imm(printf("%ld\n", unbox(n)));
}

void exit2(void) {
  exit(0);
}
