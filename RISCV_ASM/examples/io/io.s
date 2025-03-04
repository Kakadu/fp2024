.globl _start

.section .data
buffer:     .space 32

.section .text

_start:
    li a7, 63
    li a0, 0
    la a1, buffer
    li a2, 32
    ecall

    li a7, 64
    li a0, 1
    la a1, buffer
    mv a2, a2
    ecall

    li a7, 93
    li a0, 0
    ecall
