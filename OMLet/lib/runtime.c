#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  void *code;       // function pointer
  int64_t arity;    // total number of arguments
  int64_t received; // how many arguments are already applied
  void *args[];     // flexible array of applied arguments
} closure;

// allocate a new closure
closure *alloc_closure(void *code, int64_t arity) {
  closure *c = malloc(sizeof(closure) + sizeof(void *) * arity);
  c->code = code;
  c->arity = arity;
  c->received = 0;
  memset(c->args, 0, sizeof(void *) * arity);

  // printf("[alloc_closure] code=%p arity=%ld closure=%p\n",
  //        code, arity, (void *)c);

  return c;
}

// type for up to 8-argument functions
typedef void *(*fun8)(void *, void *, void *, void *, void *, void *, void *,
                      void *);

// apply arguments to a closure
void *apply(closure *f, int64_t arity, void **args, int64_t argc) {
  // printf("[apply] closure=%p arity=%ld received=%ld argc=%ld\n",
  //        (void *)f, f->arity, f->received, argc);

  int64_t total = f->received + argc;

  // full application
  if (total == f->arity) {
    void *call_args[8] = {0, 0, 0, 0, 0, 0, 0, 0};
    for (int i = 0; i < f->received; i++)
      call_args[i] = f->args[i];
    for (int i = 0; i < argc; i++)
      call_args[f->received + i] = args[i];

    // printf("[apply] full application, calling function %p\n", f->code);

    switch (f->arity) {
    case 1:
      return ((fun8)f->code)(call_args[0], NULL, NULL, NULL, NULL, NULL, NULL,
                             NULL);
    case 2:
      return ((fun8)f->code)(call_args[0], call_args[1], NULL, NULL, NULL, NULL,
                             NULL, NULL);
    case 3:
      return ((fun8)f->code)(call_args[0], call_args[1], call_args[2], NULL,
                             NULL, NULL, NULL, NULL);
    case 4:
      return ((fun8)f->code)(call_args[0], call_args[1], call_args[2],
                             call_args[3], NULL, NULL, NULL, NULL);
    case 5:
      return ((fun8)f->code)(call_args[0], call_args[1], call_args[2],
                             call_args[3], call_args[4], NULL, NULL, NULL);
    case 6:
      return ((fun8)f->code)(call_args[0], call_args[1], call_args[2],
                             call_args[3], call_args[4], call_args[5], NULL,
                             NULL);
    case 7:
      return ((fun8)f->code)(call_args[0], call_args[1], call_args[2],
                             call_args[3], call_args[4], call_args[5],
                             call_args[6], NULL);
    case 8:
      return ((fun8)f->code)(call_args[0], call_args[1], call_args[2],
                             call_args[3], call_args[4], call_args[5],
                             call_args[6], call_args[7]);
    default:
      fprintf(stderr, "[apply] unsupported arity: %ld\n", f->arity);
      exit(1);
    }
  }

  // partial application : create new closure
  closure *partial = malloc(sizeof(closure) + sizeof(void *) * f->arity);
  partial->code = f->code;
  partial->arity = f->arity;
  partial->received = total;
  for (int i = 0; i < f->received; i++)
    partial->args[i] = f->args[i];
  for (int i = 0; i < argc; i++)
    partial->args[f->received + i] = args[i];

  // printf("[apply] partial application: new closure=%p total_received=%ld\n",
  //        (void *)partial, total);

  return partial;
}

void print_int(int a) {
  // printf("[print_int] %ld\n", n);
  printf("%d", a);
  fflush(stdout);
}
