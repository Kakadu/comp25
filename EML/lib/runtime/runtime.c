#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define TO_ML_INTEGER(n) ((uint64_t)((uint64_t)(n) >> 1))

void print_int(long n) { printf("%ld", TO_ML_INTEGER(n)); }

#define TAG_CLOSURE 247

/*
 * По RISC-V ABI первые 8 аргументов передаются через регистры a0..a7,
 * остальные кладутся на стек перед инструкцией call.
*/
#define RISCV_REG_ARGS 8



static void *eml_alloc(size_t size_in_bytes, uint64_t tag) {
#ifdef ENABLE_GC
  uint64_t size_in_words =
      ((uint64_t)size_in_bytes + sizeof(uint64_t) - 1) / sizeof(uint64_t);
  return gc_alloc(size_in_words, tag);
#else
  return malloc(size_in_bytes);
#endif
}

/* ========== Closure ========== */

/*
 * Замыкание — это объект из четырёх частей:
 *
 *   code     — указатель на машинный код функции
 *   arity    — сколько аргументов функция ожидает всего
 *   received — сколько аргументов уже накоплено (частичное применение)
 *   args[]   — flexible array: аргументы, сохранённые при частичных вызовах
 *
 * Пример: `let add x y = x + y` порождает
 *   { code=add, arity=2, received=0, args=[] }
 *
 * После `add 1`:
 *   { code=add, arity=2, received=1, args=[1] }
 *
 * После `(add 1) 2` замыкание насыщено — происходит реальный вызов add(1, 2).
 */
typedef struct {
  void *code;
  int64_t arity;
  int64_t received;
  void *args[];
} closure;

/*
 * Создаёт новое замыкание для функции с кодом code и арностью arity.
 * Выделяет память под заголовок + arity слотов для аргументов.
 * Все поля аргументов обнуляются — замыкание пока не получило ни одного.
 */
closure *alloc_closure(void *code, int64_t arity) {
  size_t size_in_bytes = sizeof(closure) + arity * sizeof(void *);

  closure *c = (closure *)eml_alloc(size_in_bytes, TAG_CLOSURE);

  c->code = code;
  c->arity = arity;
  c->received = 0;

  memset(c->args, 0, sizeof(void *) * arity);
  return c;
}

/*
 * Создаёт полную побайтовую копию замыкания src.
 * Используется при частичном применении: оригинал остаётся нетронутым,
 * а в копию дописываются новые аргументы.
 */
static closure *copy_closure(const closure *src) {
  size_t total_size = sizeof(closure) + src->arity * sizeof(void *);

  closure *dst = (closure *)eml_alloc(total_size, TAG_CLOSURE);

  memcpy(dst, src, total_size);
  return dst;
}



/*
 * Вызывает полностью насыщенное замыкание, соблюдая RISC-V calling convention.
 *
 * Нельзя использовать обычный вызов C: компилятор не знает арность функции
 * в момент компиляции, поэтому размещение аргументов делается вручную
 * через inline assembly.
 *
 * Схема работы:
 *   1. Сдвигаем sp вниз на args_in_stack * 8, резервируя место для стековых аргументов.
 *   2. Если args_in_stack > 0 — копируем args[8..arity-1] на стек.
 *   3. Загружаем args[0..7] в регистры a0..a7.
 *   4. Прыгаем в cl->code через jalr (сохраняет адрес возврата в ra).
 *   5. После возврата восстанавливаем sp и забираем результат из a0.
 */
static void *call_closure_full(closure *c, void **args) {
  int64_t arity = c->arity;
  int64_t args_in_stack = (arity > RISCV_REG_ARGS) ? (arity - RISCV_REG_ARGS) : 0; //Сколько нужно аргументов положить на стек
  size_t storage_for_stack_args = (size_t)args_in_stack * sizeof(void *); // Сколько нужно места для этих аргументов
  void **stack_args = (args_in_stack > 0) ? args + RISCV_REG_ARGS : NULL; //Указатель на первый аргумент стека
  void *result;

  asm volatile(
      /* 1. Резервируем место для стековых аргументов (9-й, 10-й, ...) */
      "mv   t0, %[storage_for_stack_args]\n"
      "sub  sp, sp, t0\n"

      /* 2. Копируем стековые аргументы: цикл по args[8..arity-1].
       *    t1 — куда пишем (текущий слот на стеке),
       *    t2 — откуда читаем (&stack_args[i]),
       *    t3 — счётчик итераций (args_in_stack),
       *    t4 — индекс i. */
      "beqz %[args_in_stack], .Lend_stack_push\n"
      "mv   t1, sp\n"           // t1 — текущий адрес записи на стеке 
      "mv   t2, %[stack_args]\n" // t2 — начало массива стековых аргументов 
      "mv   t3, %[args_in_stack]\n"  // t3 — сколько итераций 
      "li   t4, 0\n"            // t4 = 0 — индекс i 
      ".Lloop_stack_push:\n"
      "beq  t4, t3, .Lend_stack_push\n"   // если i == on_stack, выходим
      "slli t5, t4, 3\n"                  // t5 = i * 8 (смещение в байтах)
      "add  t6, t2, t5\n"                 // t6 = &stack_args[i]
      "ld   t0, 0(t6)\n"                  // t0 = stack_args[i]
      "sd   t0, 0(t1)\n"                  // *t1 = t0 — записали на стек
      "addi t1, t1, 8\n"                  // t1 += 8 — следующий слот
      "addi t4, t4, 1\n"                  // i++ */
      "j .Lloop_stack_push\n"             // повторить
      ".Lend_stack_push:\n"

      /* 3. Загружаем первые 8 аргументов в регистры a0..a7. */
      "mv   a0, %[a0]\n"
      "mv   a1, %[a1]\n"
      "mv   a2, %[a2]\n"
      "mv   a3, %[a3]\n"
      "mv   a4, %[a4]\n"
      "mv   a5, %[a5]\n"
      "mv   a6, %[a6]\n"
      "mv   a7, %[a7]\n"

      /* 4. Непрямой вызов через указатель cl->code. */
      "mv   t6, %[fn]\n"      /* t6 = адрес функции (c->code) */
      "jalr ra, t6, 0\n"      /* переход по адресу в t6, адрес возврата в ra */

      /* 5. Восстанавливаем стек и сохраняем возвращаемое значение из a0. */
      "mv   t0, %[storage_for_stack_args]\n"
      "add  sp, sp, t0\n"     /* sp += stack_bytes — освободить место на стеке */
      "mv   %[result], a0\n"  /* result = возвращаемое значение из a0 */

      : [result] "=r"(result)
      : [fn] "r"(c->code),
        [a0] "r"(args[0]), [a1] "r"(args[1]),
        [a2] "r"(args[2]), [a3] "r"(args[3]),
        [a4] "r"(args[4]), [a5] "r"(args[5]),
        [a6] "r"(args[6]), [a7] "r"(args[7]),
        [stack_args] "r"(stack_args), [args_in_stack] "r"(args_in_stack),
        [storage_for_stack_args] "r"(storage_for_stack_args)
      /* Clobber list: GCC не должен хранить важные данные в этих регистрах,
       * так как asm-блок их перезаписывает. */
      : "t0", "t1", "t2", "t3", "t4", "t5", "t6",
        "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "memory");

  return result;
}

/*
 * Применяет argc аргументов к замыканию cl. Реализует каррирование:
 *
 *   Сценарий 1 — полное применение (received + argc == arity):
 *     Собирает ВСЕ аргументы (ранее накопленные + новые) в единый массив
 *     all_args и вызывает call_closure_full — происходит реальный вызов функции.
 *
 *   Сценарий 2 — частичное применение (аргументов всё ещё не хватает):
 *     Копирует замыкание через copy_closure, дописывает новые аргументы
 *     в args[] копии и возвращает её как новое замыкание.
 *     Позже, когда придут оставшиеся аргументы, произойдёт полный вызов.
 *
 * Пример:
 *   let add x y = x + y      →  alloc_closure(add, 2)
 *   let g = add 1             →  eml_applyN: received(0)+1 < 2 → частичное,
 *                                  g = { code=add, arity=2, received=1, args=[1] }
 *   let h = g 2               →  eml_applyN: received(1)+1 == 2 → полное,
 *                                  all_args=[1,2] → call_closure_full → add(1,2)
 *
 * argv — массив из argc указателей, собранный вызывающей стороной на стеке.
 */
void *eml_applyN(closure *c, int64_t argc, void **argv) {

  int64_t all_receive_args = c->received + argc;

  if (all_receive_args == c->arity) {
    /* Полное применение: объединяем накопленные и новые аргументы. */
    int64_t total_count_args = c->arity; //требуемое число аргументов 
    void **args = (void **)eml_alloc(total_count_args * sizeof(void *), TAG_CLOSURE);

    /*Старые аргументы. Копируются аргументы, уже накопленные в замыкании.
    Пример: (add 1) 2 → в замыкании args = [1], в args получается [1, ?]. */
    for (int64_t i = 0; i < c->received; i++) {
      args[i] = c->args[i];
    }
    /* Новые аргументы. Дописываются аргументы из текущего вызова (argv).
    Пример: argv = [2] → args = [1, 2].*/
    for (int64_t i = 0; i < argc; i++) {
      args[c->received + i] = argv[i];
    }
    /*Собранный массив передаётся в call_closure_full, 
    которая раскладывает аргументы по регистрам и стеку и делает переход по c->code. 
    Результат возвращается наверх.*/
    return call_closure_full(c, args);
  }

  /* Частичное применение: сохраняем новые аргументы в копии замыкания. 
  Создаётся копия текущего замыкания, чтобы не портить оригинал.
  Пример: после add 1 замыкание add не меняется, создаётся новое partial для g.
  */
  closure *partial = copy_closure(c);

  /*Для каждого нового аргумента:
  записываем его в partial->args[partial->received];
  увеличиваем partial->received.
  received++ делает и запись, и инкремент.
  Пример: add 1 → partial->received идёт 0→1, partial->args[0] = 1.*/
  for (int64_t i = 0; i < argc; i++) {
    partial->args[partial->received++] = argv[i];
  }

  return partial;

  /* Примеры трассировки:
    let add x y = x + y;
    1. let g = add 1
  c = {code=add, arity=2, received=0, args=[]};
  argc = 1, argv = [1];
  all_receive_args = 1, 1 ≠ 2 → частичное применение;
  partial копия c, partial->args[0]=1, partial->received=1;
  возвращается {code=add, arity=2, received=1, args=[1]}.
  2. let r = g 2
  c = {code=add, arity=2, received=1, args=[1]};
  argc = 1, argv = [2];
  all_receive_args = 2, 2 == 2 → полное применение;
  args = [1, 2];
  call_closure_full(c, args) вызывает add(1, 2) → возвращает 3.
  3. Прямой вызов add 1 2
  c = {code=add, arity=2, received=0, args=[]};
  argc = 2, argv = [1, 2];
  all_receive_args = 2 → полное применение;
  args = [1, 2];
  выполняется add(1, 2) → 3.
*/
}

