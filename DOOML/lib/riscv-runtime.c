#include <assert.h>
#include <stdint.h>
#include <stdio.h>
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

int64_t call_function(void *func, int64_t nargs, int64_t *args) {
    int64_t result;

    if (nargs <= 8) {
        asm volatile (
            "li t0, 0\n"
            "beq %[nargs], t0, 1f\n"
            "ld a0, 0(%[args])\n"
            
            "li t0, 1\n"
            "beq %[nargs], t0, 1f\n"
            "ld a1, 8(%[args])\n"
            
            "li t0, 2\n"
            "beq %[nargs], t0, 1f\n"
            "ld a2, 16(%[args])\n"
            
            "li t0, 3\n"
            "beq %[nargs], t0, 1f\n"
            "ld a3, 24(%[args])\n"
            
            "li t0, 4\n"
            "beq %[nargs], t0, 1f\n"
            "ld a4, 32(%[args])\n"
            
            "li t0, 5\n"
            "beq %[nargs], t0, 1f\n"
            "ld a5, 40(%[args])\n"
            
            "li t0, 6\n"
            "beq %[nargs], t0, 1f\n"
            "ld a6, 48(%[args])\n"
            
            "li t0, 7\n"
            "beq %[nargs], t0, 1f\n"
            "ld a7, 56(%[args])\n"
            
            "1:\n"
            "jalr %[func]\n"

            "mv %[result], a0\n"

            : [result] "=r" (result)
            : [func] "r" (func), [nargs] "r" (nargs), [args] "r" (args)
            : "ra", "t0", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
              "memory"
        );
    } else {
        int64_t stack_size = (nargs - 8) * 8;
        
        asm volatile (
            "addi sp, sp, -32\n"
            "sd ra, 24(sp)\n"
            "sd s1, 8(sp)\n"
            
            "mv s1, %[nargs]\n"
            "mv s2, %[args]\n"
            
            "sub sp, sp, %[stack_size]\n"
            
            "ld a0, 0(s2)\n"
            "ld a1, 8(s2)\n"
            "ld a2, 16(s2)\n"
            "ld a3, 24(s2)\n"
            "ld a4, 32(s2)\n"
            "ld a5, 40(s2)\n"
            "ld a6, 48(s2)\n"
            "ld a7, 56(s2)\n"
            
            "li t0, 64\n"
            "add t1, s2, t0\n"
            "mv t2, sp\n"
            
            "li t3, 8\n"
            "sub t4, s1, t3\n"
            
            "copy_loop:\n"
            "beqz t4, copy_done\n"
            "ld t5, 0(t1)\n"
            "sd t5, 0(t2)\n"
            "addi t1, t1, 8\n"
            "addi t2, t2, 8\n"
            "addi t4, t4, -1\n"
            "j copy_loop\n"
            
            "copy_done:\n"
            "jalr %[func]\n"
            
            "mv %[result], a0\n"
            
            "add sp, sp, %[stack_size]\n"
            
            "ld s1, 8(sp)\n"
            "ld ra, 24(sp)\n"
            "addi sp, sp, 32\n"
            : [result] "=r" (result)
            : [func] "r" (func), [nargs] "r" (nargs), [args] "r" (args),
              [stack_size] "r" (stack_size)
            : "ra", "t0", "t1", "t2", "t3", "t4", "t5",
              "s1", "s2",
              "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
              "memory"
        );
    }

    return result;
}

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
    return call_function((void*) closure[0], closure[1], &(closure[3]));
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
