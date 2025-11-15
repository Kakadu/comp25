/*Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*/

#include <alloca.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uintptr_t value;

#define TAG_INT ((value)1u)
#define IS_INT(v) (((v) & TAG_INT) != 0)
#define IS_PTR(v) (!IS_INT(v) && (v) != 0)
#define RV_GP_ARGS 8
#define WORD_SZ 8

static inline value mk_int(int64_t n) {
    return ((value)n << 1) | TAG_INT;
}
static inline int64_t un_int(value v) {
    return (int64_t)(v >> 1);
}

__attribute__((noreturn)) static void panic(const char* msg) {
    fputs(msg, stderr);
    fputc('\n', stderr);
    abort();
}

void print_int(int64_t tagged) {
    printf("%" PRId64 "\n", (int64_t)(((value)tagged) >> 1));
    fflush(stdout);
}

typedef struct GCType GCType;
typedef struct {
    GCType* type;
} GCHeader;

struct GCType {
    size_t (*size_bytes)(uint8_t* obj);
    void (*scan)(uint8_t* obj);
};

typedef struct Block {
    GCHeader h;
    int64_t len;
    value elems[];
} Block;

typedef struct Closure {
    GCHeader h;
    int64_t arity;
    int64_t received;
    void* code;
    value args[]; /*= arity */
} Closure;

static uint8_t *from_start, *from_end;
static uint8_t *to_start, *to_end;

static uint8_t* alloc_ptr;
static uint8_t* to_alloc;

static inline int in_from(const void* p) {
    const uint8_t* x = (const uint8_t*)p;
    return x >= from_start && x < from_end;
}
static inline int in_to(const void* p) {
    const uint8_t* x = (const uint8_t*)p;
    return x >= to_start && x < to_end;
}

static uint8_t* stack_top;

typedef struct {
    uint64_t runs;
    uint64_t allocations;
    uint64_t bytes_allocated_total;
    int current_bank; /* 0/1 */
} gc_stats_t;

static gc_stats_t GC_STATS = {0, 0, 0, 0};

static size_t block_size_of(uint8_t* p) {
    Block* b = (Block*)p;
    return sizeof(Block) + (size_t)b->len * sizeof(value);
}
static void block_scan(uint8_t* p);

static size_t clos_size_of(uint8_t* p) {
    Closure* c = (Closure*)p;
    return sizeof(Closure) + (size_t)c->arity * sizeof(value);
}
static void clos_scan(uint8_t* p);

static GCType VT_Block = {.size_bytes = block_size_of, .scan = block_scan};
static GCType VT_Closure = {.size_bytes = clos_size_of, .scan = clos_scan};

void rt_init(size_t heap_bytes) {
    uint8_t marker;
    stack_top = (uint8_t*)&marker;

    if (heap_bytes < (size_t)1 << 20) heap_bytes = (size_t)1 << 20;
    if (heap_bytes & 1u) heap_bytes++;

    uint8_t* heap = (uint8_t*)malloc(heap_bytes);
    if (!heap) panic("rt_init: OOM");

    size_t half = heap_bytes / 2u;
    from_start = heap;
    from_end = heap + half;
    to_start = heap + half;
    to_end = heap + heap_bytes;

    alloc_ptr = from_start;
    to_alloc = to_start;
    GC_STATS.current_bank = 0;
}

static inline int is_forwarded(GCHeader* h) {
    /*  вв старой копии после копирования header.type указывает на новую копию в to-space */
    return in_to(h->type);
}

static uint8_t* forward_obj(uint8_t* old);

static value forward_val(value v) {
    if (!IS_PTR(v)) return v;
    if (!in_from((void*)v)) return v;
    return (value)forward_obj((uint8_t*)v);
}

static uint8_t* forward_obj(uint8_t* old) {
    if (!in_from(old)) return old;

    GCHeader* h = (GCHeader*)old;

    if (is_forwarded(h)) return (uint8_t*)h->type;

    GCType* vt = h->type;
    size_t sz = vt->size_bytes(old);
    size_t rounded = (sz + 7u) & ~7u;

    if (to_alloc + rounded > to_end) panic("GC: to-space overflow");

    uint8_t* dst = to_alloc;
    to_alloc += rounded;

    memcpy(dst, old, sz);

    h->type = (GCType*)dst;

    return dst;
}

static void block_scan(uint8_t* p) {
    Block* b = (Block*)p;
    for (int64_t i = 0; i < b->len; ++i) {
        b->elems[i] = forward_val(b->elems[i]);
    }
}

static void clos_scan(uint8_t* p) {
    Closure* c = (Closure*)p;
    for (int64_t i = 0; i < c->received; ++i) {
        c->args[i] = forward_val(c->args[i]);
    }
}

static void gc_collect(void) {
    to_alloc = to_start;

    uint8_t sp_marker;
    uint8_t* sp = (uint8_t*)&sp_marker;
    for (value* slot = (value*)sp; (uint8_t*)slot < stack_top; ++slot) {
        value v = *slot;
        if (IS_PTR(v) && in_from((void*)v)) {
            *slot = (value)forward_obj((uint8_t*)v);
        }
    }

    for (uint8_t* scan = to_start; scan < to_alloc;) {
        GCHeader* h = (GCHeader*)scan;
        GCType* vt = h->type;
        vt->scan(scan);
        scan += (vt->size_bytes(scan) + 7u) & ~7u;
    }
    /*поменять местами*/
    uint8_t *s, *e;
    s = from_start;
    from_start = to_start;
    to_start = s;
    e = from_end;
    from_end = to_end;
    to_end = e;

    alloc_ptr = to_alloc;

    GC_STATS.runs++;
    GC_STATS.current_bank ^= 1;
}

static void* gc_alloc_bytes(size_t n, GCType* vt) {
    n = (n + 7u) & ~7u;

    if (alloc_ptr + n > from_end) {
        gc_collect();
        if (alloc_ptr + n > from_end) panic("GC: out of memory");
    }
    uint8_t* p = alloc_ptr;
    alloc_ptr += n;

    ((GCHeader*)p)->type = vt;

    GC_STATS.allocations++;
    GC_STATS.bytes_allocated_total += n;

    return p;
}

static inline size_t block_bytes_for_len(int64_t len) {
    return sizeof(Block) + (size_t)len * sizeof(value);
}
static inline size_t clos_bytes_for_arity(int64_t arity) {
    return sizeof(Closure) + (size_t)arity * sizeof(value);
}

void* alloc_block(int64_t len) {
    if (len < 0) panic("alloc_block: negative len");
    Block* b = (Block*)gc_alloc_bytes(block_bytes_for_len(len), &VT_Block);
    b->len = len;
    return (void*)b;
}

Closure* alloc_closure(void* code, int64_t arity) {
    if (arity < 0) panic("alloc_closure: negative arity");
    Closure* c = (Closure*)gc_alloc_bytes(clos_bytes_for_arity(arity), &VT_Closure);
    c->arity = arity;
    c->received = 0;
    c->code = code;
    if (arity) memset(c->args, 0, (size_t)arity * sizeof(value));
    return c;
}

Closure* copy_closure(const Closure* src) {
    Closure* dst = (Closure*)gc_alloc_bytes(clos_bytes_for_arity(src->arity), &VT_Closure);
    memcpy(dst, src, clos_bytes_for_arity(src->arity));
    return dst;
}

static value rv_call(void* fn, value* argv, int64_t n) {
    int64_t spill = (n > RV_GP_ARGS) ? (n - RV_GP_ARGS) : 0;
    size_t spill_bytes = (size_t)spill * WORD_SZ;
    value* spill_ptr = (spill > 0) ? argv + RV_GP_ARGS : NULL;

    value ret;
    asm volatile(
        "mv   t0, %[sz]\n"
        "sub  sp, sp, t0\n"

        "beqz %[cnt], 2f\n"
        "mv   t1, sp\n"
        "mv   t2, %[spill]\n"
        "mv   t3, %[cnt]\n"
        "li   t4, 0\n"
        "1:\n"
        "beq  t4, t3, 2f\n"
        "slli t5, t4, 3\n"
        "add  t6, t2, t5\n"
        "ld   t0, 0(t6)\n"
        "sd   t0, 0(t1)\n"
        "addi t1, t1, 8\n"
        "addi t4, t4, 1\n"
        "j    1b\n"
        "2:\n"

        "mv   a0, %[a0]\n"
        "mv   a1, %[a1]\n"
        "mv   a2, %[a2]\n"
        "mv   a3, %[a3]\n"
        "mv   a4, %[a4]\n"
        "mv   a5, %[a5]\n"
        "mv   a6, %[a6]\n"
        "mv   a7, %[a7]\n"

        "mv   t6, %[fn]\n"
        "jalr ra, t6, 0\n"

        "mv   t0, %[sz]\n"
        "add  sp, sp, t0\n"

        "mv   %[ret], a0\n"
        : [ret] "=r"(ret)
        : [fn] "r"(fn), [a0] "r"((n > 0) ? argv[0] : 0), [a1] "r"((n > 1) ? argv[1] : 0),
          [a2] "r"((n > 2) ? argv[2] : 0), [a3] "r"((n > 3) ? argv[3] : 0),
          [a4] "r"((n > 4) ? argv[4] : 0), [a5] "r"((n > 5) ? argv[5] : 0),
          [a6] "r"((n > 6) ? argv[6] : 0), [a7] "r"((n > 7) ? argv[7] : 0), [spill] "r"(spill_ptr),
          [cnt] "r"(spill), [sz] "r"(spill_bytes)
        : "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
          "memory");
    return ret;
}

value apply1(Closure* f, value arg) {
    if (!f) panic("apply1: null closure");

    const int64_t r = f->received;
    const int64_t n = f->arity;

    if (r + 1 < n) {
        Closure* g = copy_closure(f);
        g->args[g->received++] = arg;
        return (value)g;
    }

    if (r + 1 == n) {
        int64_t m = n ? n : 1;
        value* argv = (value*)alloca((size_t)m * sizeof(value));
        for (int64_t i = 0; i < r; ++i) argv[i] = f->args[i];
        argv[r] = arg;
        return rv_call(f->code, argv, n);
    }

    panic("apply1: too many arguments");
    __builtin_unreachable();
}

void print_gc_status(void) {
    size_t used_bytes = (size_t)(alloc_ptr - from_start);
    size_t free_bytes = (size_t)(from_end - alloc_ptr);
    size_t heap_bytes = (size_t)(from_end - from_start);

    printf("=== GC Status ===\n");
    printf("Current allocated: %zu\n", used_bytes);
    printf("Free        space: %zu\n", free_bytes);
    printf("Heap         size: %zu\n", heap_bytes);
    printf("Current      bank: %d\n", GC_STATS.current_bank);
    printf("Total   allocated: %" PRIu64 "\n", GC_STATS.bytes_allocated_total);
    printf("GC    collections: %" PRIu64 "\n", GC_STATS.runs);
    printf("GC    allocations: %" PRIu64 "\n", GC_STATS.allocations);
    printf("=================\n");
    fflush(stdout);
}

void collect(void) {
    gc_collect();
}
