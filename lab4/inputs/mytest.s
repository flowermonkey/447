        .text
__start:	addiu $v0, $zero, 10
		addiu $t0, $zero, 5
		addiu $t1, $v0, 300
		addiu $t2, $zero, 500
		addiu $t3, $t0, 34
		addiu $t3, $t3, 45
		syscall
