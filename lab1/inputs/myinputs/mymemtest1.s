	.text
main:
        #;;  Set a base address
        lui    	$3, 0x1000

        addiu  	$4, $zero, 0xff
        addiu  	$5, $zero, 0xab
        addiu  	$6, $zero, 0xcd
        addiu  	$7, $zero, 0xef
       	lui 	$8, 0xff	
		addiu 	$9, $zero, 0x2
		sllv	$9, $9, $9

        #;; Place a test pattern in memory
        sh		$4, 6($3)
        sh     	$7, 4($3)
        sh     	$6, 2($3)
        sh     	$5, 0($3)

        lw     	$9,  4($3)
		
		lhu		$10, 6($3)
		sll		$10, $10, 16
		
		lhu		$11, 4($3)
		or		$10, $10, $11
		
		beq		$10, $9, next_word
		ori		$zero,$zero,0
		
next_word:
        lw     	$11, 0($3)
		
		lhu		$12, 2($3)
		sll		$12, $12, 16
		
		lhu		$13, 0($3)
		or		$12, $12, $13
		
		bne		$11, $12, done
		ori		$zero,$zero,0

		sb		$4, 8($3)
		sb		$4, 9($3)
		lhu 	$13, 8($3)
		lh 		$14, 8($3)

done:
		addiu $2, $zero, 0xa
		syscall
		
