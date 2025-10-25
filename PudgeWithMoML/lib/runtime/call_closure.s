    .text
    .globl call_closure
    .type  call_closure, @function
# a0 = code, a1 = argc, a2 = argv
call_closure:
    addi    sp, sp, -64
    sd      ra, 56(sp)
    sd      s0, 48(sp)
    sd      s1, 40(sp)
    sd      s2, 32(sp)
    sd      s3, 24(sp)

    mv      s0, a0
    mv      s1, a1
    mv      s2, a2
    mv      s3, zero

    andi    t0, s1, 1
    mv      t1, s1
    beq     t0, zero, 1f
    addi    t1, t1, 1
1:
    slli    t2, t1, 3
    mv      s3, t2
    sub     sp, sp, t2

    mv      t3, zero
2:
    beq     t3, s1, 3f
    slli    t4, t3, 3
    add     t5, s2, t4
    ld      t6, 0(t5)
    add     t0, sp, t4
    sd      t6, 0(t0)
    addi    t3, t3, 1
    j       2b

3:
    mv      t0, s0
    jalr    t0

    add     sp, sp, s3

    ld      ra, 56(sp)
    ld      s0, 48(sp)
    ld      s1, 40(sp)
    ld      s2, 32(sp)
    ld      s3, 24(sp)
    addi    sp, sp, 64
    ret
