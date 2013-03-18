	# My Test
	.text
main:
		addiu $3, $zero, 0xffff
		addi $4, $zero, 0xffff
		add $5, $3, $4
		addu $6, $3, $4
		sub $7, $5, $6
		subu $8, $4, $5
		andi $9, $4, 0x00aa
		ori $10, $9, 0xaa55
		xori $11, $10, 0xff00
		xor $12, $11, $4
		or $13, $11, $5
		nor $14, $11, $10
		and $15, $11, $10
		j done 
		#;; Comment
done:
		addiu $2, $zero, 0xa
		syscall
