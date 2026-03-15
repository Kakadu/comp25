#include "call-runtime.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

#define MEM 64000
#define WORD_SIZE sizeof(GCWord)

typedef enum __attribute__((packed)) {
    Tuple,
    Closure,
    Forward
} GCObjTag;

typedef union {
    bool is_header : 1;

    struct {
        bool is_header : 1;
        int64_t value : 63;
    } value;

    struct {
        bool is_header : 1;
        GCObjTag tag;
        uint32_t bsize;
    } header;
} GCWord;

typedef struct {
    uint32_t bsize;
    uint32_t allocated_bsize;
    uint8_t bank_idx;
    uint64_t runs;
} GCStats;

typedef struct {
    GCStats stats;

    GCWord *from_bank_start;
    uint32_t from_bank_bsize;
    GCWord *to_bank_start;
    uint32_t to_bank_bsize;

    GCWord *free_space_start;
} GC;

typedef struct {
    GCWord header;
    GCWord callee;
    GCWord arity;
    GCWord argc;
    GCWord args[];
} GCClosure;

typedef struct {
    GCWord header;
    GCWord size;
    GCWord fields[];
} GCTuple;

typedef struct {
    GCWord header;
    GCWord ptr;
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
    assert(WORD_SIZE == 8);
    assert(GC_BANK_SIZE % WORD_SIZE == 0);
    GCStats init_stats = {
        .runs = 0,
        .bank_idx = 0,
        .bsize = 0,
        .allocated_bsize = 0,
    };
    GC init_gc = {
        .stats = init_stats,
        .from_bank_bsize = GC_BANK_SIZE,
        .from_bank_start = malloc(GC_BANK_SIZE),
        .to_bank_bsize = GC_BANK_SIZE,
        .to_bank_start = malloc(GC_BANK_SIZE),
    };
    init_gc.free_space_start = init_gc.from_bank_start;
    debugf("> bank 0 ranges: %ld : %ld\n", (int64_t)init_gc.from_bank_start, (int64_t)init_gc.from_bank_start + init_gc.from_bank_bsize);
    debugf("> bank 1 ranges: %ld : %ld\n", (int64_t)init_gc.to_bank_start, (int64_t)init_gc.to_bank_start + init_gc.to_bank_bsize);
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
    assert(((GCWord *)ptr)->is_header);
    GCObjTag tag = ((GCWord *)ptr)->header.tag;
    if (tag == Forward) {
        printf("Forward -> %ld: ", (int64_t)((GCForward *)ptr)->ptr.value.value);
        print_obj_helper(((GCForward *)ptr)->ptr.value.value);
    }
    if (tag == Closure) {
        GCClosure *closure = (GCClosure *)ptr;
        printf("Closure %ld(", (int64_t)closure->callee.value.value);
        for (int i = 0; i < closure->arity.value.value; i++) {
            if (i < closure->argc.value.value) {
                print_obj_helper(closure->args[i].value.value);
            } else {
                printf("...");
            }
            if (i != closure->arity.value.value - 1) {
                printf(", ");
            }
        }
        printf(")");
    }
    if (tag == Tuple) {
        GCTuple *tuple = (GCTuple *)ptr;
        printf("Tuple (");
        for (int i = 0; i < tuple->size.value.value; i++) {
            print_obj_helper(tuple->fields[i].value.value);
            if (i != tuple->size.value.value - 1) {
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

GCWord gc_header_to_word(uint32_t bsize, GCObjTag tag) {
    GCWord word = {
        .header = {
            .is_header = true,
            .tag = tag,
            .bsize = bsize,
        },
    };
    return word;
}

GCWord gc_value_to_word(int64_t value) {
    GCWord word = {
        .value = {
            .is_header = false,
            .value = value & 0x7FFFFFFFFFFFFFFF,
        },
    };
    return word;
}

GCWord *gc_alloc(uint32_t bsize, GCObjTag tag) {
    assert(bsize % WORD_SIZE == 0);
    debugf("> gc_alloc (%u, %u)\n", bsize, tag);
    GCWord *ptr = gc.free_space_start;
    uint32_t taken_bytes = ((uint32_t) (ptr - gc.from_bank_start)) * WORD_SIZE;
    uint32_t free_space = gc.from_bank_bsize - taken_bytes;
    debugf("  free_space: %u\n", free_space);

    if (free_space < bsize) {
        gc_collect();
        ptr = gc.free_space_start;
        taken_bytes = ((uint32_t) (ptr - gc.from_bank_start)) * WORD_SIZE;
        free_space = gc.from_bank_bsize - taken_bytes;
        if (free_space < bsize) {
            fprintf(stderr, "GC OOM\n");
            exit(1);
        }
    }

    gc.free_space_start += (bsize / WORD_SIZE);
    gc.stats.allocated_bsize += bsize;
    gc.stats.bsize += bsize;
    *ptr = gc_header_to_word(bsize, tag);

    return ptr;
}

GCClosure *gc_alloc_closure_base(int64_t callee, int64_t arity, int64_t argc) {
  int64_t bsize = sizeof(GCClosure) + arity * sizeof(GCWord);
  GCClosure *closure = (GCClosure *)gc_alloc(bsize, Closure);
  closure->callee = gc_value_to_word(callee);
  closure->arity = gc_value_to_word(arity);
  closure->argc = gc_value_to_word(argc);
  debugf("< alloc(%ld): closure %ld with %ld(%ld out of %ld)\n", bsize, (int64_t)closure, callee, argc, arity);
  return closure;
}

GCTuple *gc_alloc_tuple_base(int64_t size) {
  int64_t bsize = sizeof(GCTuple) + size * sizeof(GCWord);
  GCTuple *tuple = (GCTuple *)gc_alloc(bsize, Tuple);
  tuple->size = gc_value_to_word(size);
  debugf("< alloc(%ld): tuple %ld with size %ld\n", bsize, (int64_t)tuple, size);
  return tuple;
}

void gc_make_fwd(int64_t ptr, int64_t new_ptr) {
    assert(((GCWord *)ptr)->is_header);
    ((GCForward *)ptr)->header = gc_header_to_word(((GCForward *)ptr)->header.header.bsize, Forward);
    ((GCForward *)ptr)->ptr = gc_value_to_word(new_ptr);
}

int64_t gc_mark_and_copy(int64_t ptr, int64_t gc_bank_range_start, int64_t gc_bank_range_end) {
    debugf("> mark_and_copy: %ld\n", ptr);
    assert(((GCWord *)ptr)->is_header);
    GCObjTag tag = ((GCWord *)ptr)->header.tag;
    debugf("  tag: %d\n", tag);
    if (tag == Forward) {
        return ((GCForward *)ptr)->ptr.value.value;
    }

    if (tag == Closure) {
        GCClosure *closure = (GCClosure *)ptr;
        GCClosure *closure2 = gc_alloc_closure_base(closure->callee.value.value, closure->arity.value.value, closure->argc.value.value);

        // rewrites header and callee
        gc_make_fwd(ptr, (int64_t)closure2);

        int64_t callee = closure->callee.value.value;
        if (callee >= gc_bank_range_start && callee <= gc_bank_range_end) {
            closure2->callee = gc_value_to_word(gc_mark_and_copy(callee, gc_bank_range_start, gc_bank_range_end));
        }

        for (int64_t i = 0; i < closure2->argc.value.value; i++) {
            int64_t arg = closure->args[i].value.value;
            if (is_imm(arg)) {
                closure2->args[i] = gc_value_to_word(arg);
            } else {
                closure2->args[i] = gc_value_to_word(gc_mark_and_copy(arg, gc_bank_range_start, gc_bank_range_end));
            }
        }

        return (int64_t)closure2;
    }

    if (tag == Tuple) {
        GCTuple *tuple = (GCTuple *)ptr;
        GCTuple *tuple2 = gc_alloc_tuple_base(tuple->size.value.value);

        // rewrites header and size
        gc_make_fwd(ptr, (int64_t)tuple2);

        for (int64_t i = 0; i < tuple2->size.value.value; i++) {
            int64_t field = tuple->fields[i].value.value;
            if (is_imm(field)) {
                tuple2->fields[i] = gc_value_to_word(field);
            } else {
                tuple2->fields[i] = gc_value_to_word(gc_mark_and_copy(field, gc_bank_range_start, gc_bank_range_end));
            }
        }

        return (int64_t)tuple2;
    }

    fprintf(stderr, "unknown gc tag %u\n", tag);
    exit(1);
}

void gc_collect() {
    int64_t *sp = get_sp(); 
    int64_t gc_bank_range_start = (int64_t)gc.from_bank_start;
    int64_t gc_bank_range_end = (int64_t)gc.free_space_start;

    GCWord *to_bank_start = gc.to_bank_start;
    int64_t to_bank_bsize = gc.to_bank_bsize;
    gc.to_bank_start = gc.from_bank_start;
    gc.from_bank_start = to_bank_start;
    gc.to_bank_bsize = gc.from_bank_bsize;
    gc.from_bank_bsize = to_bank_bsize;
    gc.free_space_start = gc.from_bank_start;
    gc.stats.bank_idx = 1 - gc.stats.bank_idx;
    gc.stats.bsize = 0;

    debugf("> gc_collect\n");
    for (int64_t *stack_cell = initial_sp; stack_cell >= sp; stack_cell--) {
        int64_t obj_ptr = *stack_cell;
        if (!is_imm(obj_ptr) && obj_ptr >= gc_bank_range_start && obj_ptr < gc_bank_range_end && ((GCWord *)obj_ptr)->is_header) {
            debugf("  gc_root: %ld\n", obj_ptr);
            debug_print_value(obj_ptr);
            *stack_cell = gc_mark_and_copy(obj_ptr, gc_bank_range_start, gc_bank_range_end);
            debugf("  new pointer on stack: %ld\n", *stack_cell);
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
    return box_imm((int64_t)gc.from_bank_start + gc.from_bank_bsize);
}

void print_gc_status(int64_t unit) {
    printf("GC status\n");
    printf("Bank index: %u\n", gc.stats.bank_idx);
    printf("Bank capacity: %u\n", gc.from_bank_bsize);
    printf("Allocated: %u\n", gc.stats.bsize);
    printf("Total allocated: %u\n", gc.stats.allocated_bsize);
    printf("GC runs: %ld\n", gc.stats.runs);
}

int64_t create_tuple(int64_t size, int64_t init) {
  GCTuple *tuple = gc_alloc_tuple_base(size);
  for (int64_t i = 0; i < size; i++) {
    tuple->fields[i] = gc_value_to_word(((int64_t*) init)[i]);
  }
  debug_print_value((int64_t)tuple);

  return (int64_t) tuple;
}

int64_t tuple_nth(int64_t tuple, int64_t i) {
  int64_t unboxed_i = unbox(i);
  GCTuple *tuple_ptr = (GCTuple*) tuple;
  if (unboxed_i >= tuple_ptr->size.value.value) {
    fprintf(stderr, "tuple_nth: index is out of bounds\n");
    exit(1);
  }
  return tuple_ptr->fields[unboxed_i].value.value;
}

int64_t create_closure(int64_t callee, int64_t arity, int64_t argc, int64_t argv_) {
  assert(argc < arity);

  GCClosure *closure = gc_alloc_closure_base(callee, arity, argc);

  int64_t *argv = (int64_t*) argv_;
  for (int64_t i = 0; i < argc; i++) {
    closure->args[i] = gc_value_to_word(argv[i]);
  }
  debug_print_value((int64_t)closure);

  return (int64_t) closure;
}

GCClosure *copy_closure(GCClosure *closure) {
  GCClosure *closure2 = gc_alloc_closure_base(closure->callee.value.value, closure->arity.value.value, closure->argc.value.value);
  for (int64_t i = 0; i < closure->argc.value.value; i++) {
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
  int64_t current_argc = closure->argc.value.value;
  for (int64_t i = 0; i < argc; i++) {
    closure->args[i + current_argc] = gc_value_to_word(argv[i]);
    closure->argc.value.value++;
  }

  debugf("  applied: ");
  debug_print_value((int64_t)closure);

  if (closure->argc.value.value >= closure->arity.value.value) {
    debugf("  closure_apply: calling\n");

    int64_t args[closure->arity.value.value];
    for (int64_t i = 0; i < closure->arity.value.value; i++) {
        args[i] = closure->args[i].value.value;
    }

    int64_t callee = closure->callee.value.value;
    return call_function((void*) callee, closure->arity.value.value, args);
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
