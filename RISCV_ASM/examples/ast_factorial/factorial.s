	.file	"factorial.c"
	.option nopic
	.attribute arch, "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	1
	.globl	factorial
	.type	factorial, @function
factorial:
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
	.size	factorial, .-factorial
	.ident	"GCC: (gc891d8dc23e) 13.2.0"
