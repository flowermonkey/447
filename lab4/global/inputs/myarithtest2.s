main: 
		addiu $3, $zero,0xffff
		or $4, $3, $3
		jal move_mult
		ori $zero, $zero, 0
go_on:
		lui $14, 0x1000
		lw $7, 0($14)
		lw $8, 4($14)
		lw $9, 8($14)
		lw $10, 12($14)
		lw $11, 16($14)
		lw $12, 20($14)
		lw $13, 24($14)
		lw $16, 28($14)
		lw $17, 32($14)
		mthi $3
		mtlo $4
		j done
		ori $zero, $zero, 0

move_mult:
		sll $7, $4, 16
		mfhi $8
		mflo $9
		mult $7, $3 
		mfhi $10
		mflo $11
		ori $zero, $zero, 0
		ori $zero, $zero, 0
		multu $7, $3
		mfhi $12
		mflo $13
		ori $zero, $zero, 0
		srl $15, $4, 8 
		mult $15, $15
		mfhi $16
		mflo $17

		
		lui $14, 0x1000
		sw $7, 0($14)
		sw $8, 4($14)
		sw $9, 8($14)
		sw $10, 12($14)
		sw $11, 16($14)
		sw $12, 20($14)
		sw $13, 24($14)
		sw $16, 28($14)
		sw $17, 32($14)

		jr $31
		ori $zero, $zero, 0

done:
		addiu $2, $zero, 0xa
		syscall
