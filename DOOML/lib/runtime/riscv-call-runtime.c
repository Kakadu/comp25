#include "call-runtime.h"

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

