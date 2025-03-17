.globl _start

.section .data
buffer: 
    .word 1
    .word 2
    .word 3
    .word 4
    .word 5

.section .text

_start:
    li t0, 4
    vsetvli t1, t0, e32
    la t2, buffer
    vle32.v v0, 0(t2)
    addi t3, t2, 4
    vle32.v v1, 0(t3)
    vadd.vv v2, v0, v1
    la t4, buffer
    vse32.v v2, 0(t4)
    vredsum.vs v3, v0, v0
    la t5, buffer
    vse32.v v3, 0(t5)
    lw a0, 0(t5)
    la t0, buffer
    addi t2, zero, 10
    mv t3, zero
convert_loop:
    rem t4, a0, t2
    div a0, a0, t2
    addi t4, t4, 48
    sb t4, 0(t0)
    addi t0, t0, 1
    addi t3, t3, 1
    bnez a0, convert_loop
    la t0, buffer
    mv t4, t0
    add t5, t0, t3
    addi t5, t5, -1
reverse_loop:
    bge t4, t5, end_reverse
    lb t6, 0(t4)
    lb t1, 0(t5)
    sb t1, 0(t4)
    sb t6, 0(t5)
    addi t4, t4, 1
    addi t5, t5, -1
    j reverse_loop
end_reverse:
    li a7, 64
    li a0, 1
    la a1, buffer
    mv a2, t3
    ecall
    li a7, 93
    li a0, 0
    ecall
