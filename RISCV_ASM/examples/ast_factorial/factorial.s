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
