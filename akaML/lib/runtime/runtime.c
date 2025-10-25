#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_int(long n) { printf("%ld", n); }

typedef struct {
  int64_t arity;
  int64_t args_received;
  void *code;
  void *args[];
} closure;

closure *alloc_closure(void *func, int64_t arity) {
  closure *clos = malloc(sizeof(closure) + arity * sizeof(void *));
  if (!clos) {
    fprintf(stderr, "Closure allocation error\n");
    exit(1);
  }

  clos->arity = arity;
  clos->args_received = 0;
  clos->code = func;
  memset(clos->args, 0, arity * sizeof(void *));

  return clos;
}

closure *copy_closure(const closure *src) {
  size_t size = sizeof(closure) + src->arity * sizeof(void *);

  closure *dst = malloc(size);
  if (!dst) {
    fprintf(stderr, "Closure allocation error\n");
    exit(1);
  }

  memcpy(dst, src, size);
  return dst;
}

void *applyN(closure *f, int64_t argc, ...) {
  closure *f_closure = (closure *)f;
  assert(argc >= 0);
  assert(f_closure->args_received + argc <= f_closure->arity);

  va_list argp;
  va_start(argp, argc);

  int64_t n = f_closure->arity;
  void **args_all = malloc(n * sizeof(void *));

  for (int64_t i = 0; i < f_closure->args_received; i++)
    args_all[i] = f_closure->args[i];

  for (int64_t i = 0; i < argc; i++)
    args_all[f_closure->args_received + i] = va_arg(argp, void *);

  va_end(argp);

  if (f_closure->args_received + argc == n) {
    void *ret;

    int64_t stack_count = (n > 8) ? (n - 8) : 0;

    size_t stack_bytes = stack_count * 8;

    void **stack_args = (stack_count > 0) ? args_all + 8 : NULL;

    asm volatile(
        /* allocate space on the stack */
        "mv   t0, %[stack_bytes]\n"
        "sub  sp, sp, t0\n"

        /* push tail arguments onto the stack (if any) */
        "mv   t1, sp\n"
        "beqz %[stack_count], en1\n"
        "mv   t2, %[stack_args]\n"
        "mv   t3, %[stack_count]\n"
        "li   t4, 0\n"
        "el1:\n"
        "beq  t4, t3, en1\n"
        "slli t5, t4, 3\n"  /* offset = i * 8 */
        "add  t6, t2, t5\n" /* addr = &stack_args[i] */
        "ld   t0, 0(t6)\n"  /* t0 = stack_args[i] */
        "sd   t0, 0(t1)\n"  /* store on stack */
        "addi t1, t1, 8\n"
        "addi t4, t4, 1\n"
        "j el1\n"
        "en1:\n"

        /* loading the first 8 arguments into registers a0..a7 */
        "mv   a0, %[a0]\n"
        "mv   a1, %[a1]\n"
        "mv   a2, %[a2]\n"
        "mv   a3, %[a3]\n"
        "mv   a4, %[a4]\n"
        "mv   a5, %[a5]\n"
        "mv   a6, %[a6]\n"
        "mv   a7, %[a7]\n"

        /* load the function address into the register and call it via jalr */
        "mv   t6, %[fn]\n"
        "jalr ra, t6, 0\n"

        /* restore the stack */
        "mv   t0, %[stack_bytes]\n"
        "add  sp, sp, t0\n"

        /* return the result to a variable */
        "mv   %[ret], a0\n"

        : [ret] "=r"(ret)
        : [fn] "r"(f_closure->code), [a0] "r"(args_all[0]),
          [a1] "r"(args_all[1]), [a2] "r"(args_all[2]), [a3] "r"(args_all[3]),
          [a4] "r"(args_all[4]), [a5] "r"(args_all[5]), [a6] "r"(args_all[6]),
          [a7] "r"(args_all[7]), [stack_args] "r"(stack_args),
          [stack_count] "r"(stack_count), [stack_bytes] "r"(stack_bytes)
        : "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3",
          "a4", "a5", "a6", "a7", "memory");

    return ret;
  }

  closure *new_closure = copy_closure(f_closure);
  for (int64_t i = 0; i < argc; i++)
    new_closure->args[new_closure->args_received++] =
        args_all[f_closure->args_received + i];

  return new_closure;
}
