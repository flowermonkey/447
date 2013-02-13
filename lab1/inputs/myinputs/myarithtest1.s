main: 
		addiu $3, $zero,0xffff
		or $4, $3, $3
		beq $3,$4, move_div
		ori $zero, $zero, 0

move_div:
		sll $7, $4, 16
		mfhi $8
		mflo $9
		div $7,$3 
		mfhi $10
		mflo $11
		ori $zero, $zero, 0
		ori $zero, $zero, 0
		divu $7, $3
		mfhi $12
		mflo $13
		bne $zero, $15, done
		ori $zero, $zero, 0

done:
		addiu $2, $zero, 0xa
		syscall
