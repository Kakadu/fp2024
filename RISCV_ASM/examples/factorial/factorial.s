	.file	"factorial.c"
	.option pic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	1
	.globl	factorial
	.type	factorial, @function
factorial:
.LFB23:
	.cfi_startproc
	mv	a5,a0
	li	a0,1
	beq	a5,zero,.L5
.L2:
	mv	a4,a5
	addiw	a5,a5,-1
	mulw	a0,a4,a0
	bne	a5,zero,.L2
.L5:
	ret
	.cfi_endproc
.LFE23:
	.size	factorial, .-factorial
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align	3
.LC0:
	.string	"%d"
	.align	3
.LC1:
	.string	"%d\n"
	.section	.text.startup,"ax",@progbits
	.align	1
	.globl	main
	.type	main, @function
main:
.LFB24:
	.cfi_startproc
	addi	sp,sp,-32
	.cfi_def_cfa_offset 32
	sd	s0,16(sp)
	.cfi_offset 8, -16
	la	s0,__stack_chk_guard
	ld	a5, 0(s0)
	sd	a5, 8(sp)
	li	a5, 0
	addi	a1,sp,4
	lla	a0,.LC0
	sd	ra,24(sp)
	.cfi_offset 1, -8
	call	__isoc99_scanf@plt
	li	a5,1
	bne	a0,a5,.L16
	lw	a5,4(sp)
	mv	a2,a0
	beq	a5,zero,.L14
.L13:
	mv	a4,a5
	addiw	a5,a5,-1
	mulw	a2,a4,a2
	bne	a5,zero,.L13
.L14:
	lla	a1,.LC1
	li	a0,2
	call	__printf_chk@plt
	li	a0,0
.L12:
	ld	a4, 8(sp)
	ld	a5, 0(s0)
	xor	a5, a4, a5
	li	a4, 0
	bne	a5,zero,.L22
	ld	ra,24(sp)
	.cfi_remember_state
	.cfi_restore 1
	ld	s0,16(sp)
	.cfi_restore 8
	addi	sp,sp,32
	.cfi_def_cfa_offset 0
	jr	ra
.L16:
	.cfi_restore_state
	li	a0,-1
	j	.L12
.L22:
	call	__stack_chk_fail@plt
	.cfi_endproc
.LFE24:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 13.2.0-23ubuntu4) 13.2.0"
	.section	.note.GNU-stack,"",@progbits
