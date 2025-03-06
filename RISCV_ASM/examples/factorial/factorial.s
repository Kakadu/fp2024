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

    la t0, buffer
    mv a0, zero
    li t2, 10
convert_string_to_number:
    lb t1, 0(t0)
    beqz t1, end_convert
    addi t1, t1, -48
    bltz t1, end_convert
    bge t1, t2, end_convert
    mul a0, a0, t2
    add a0, a0, t1
    addi t0, t0, 1
    j convert_string_to_number
end_convert:
    addi t1, zero, 1
loop:
    beqz a0, exit
    mul t1, t1, a0
    addi a0, a0, -1
    j loop
exit:
    la t0, buffer
    addi t2, zero, 10
    mv a0, t1
    addi t3, zero, 0
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
    add t0, t0, t3
    addi t1, zero, 10
    sb t1, 0(t0)
    addi t3, t3, 1
    li a7, 64
    li a0, 1
    la a1, buffer
    mv a2, t3
    ecall
    li a7, 93
    li a0, 0
    ecall
