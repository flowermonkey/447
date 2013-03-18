	.text
main:
        #;;  Set a base address
        lui    	$3, 0x1000

        addiu  	$4, $zero, 0xff
        addiu  	$5, $zero, 0xab
        addiu  	$6, $zero, 0xcd
       	lui 	$7, 0xff	
		addiu 	$8, $zero, 0x2
		sllv	$8, $8, $8

        #;; Place a test pattern in memory
        sb     	$7, 0($3)
        sb     	$6, 1($3)
        sb     	$5, 2($3)
        sb     	$7, 3($3)
        sb     	$6, 4($3)
        sb     	$5, 5($3)
        sb     	$7, 6($3)
        sb     	$6, 7($3)

        lw     	$9,  4($3)
		srav   	$10, $9, $8
		
		lbu		$11, 7($3)
		sll		$11, $11, 24
		
		lbu		$12, 6($3)
		sll		$12, $12, 16
		or		$11, $12, $11
		
		lbu		$12, 5($3)
		sllv	$12, $12, $8
		or		$11, $12, $11
		
		lbu		$12, 4($3)
		or		$11, $12, $11
	
		bne		$11, $9, done
		ori		$zero,$zero,0

		sb		$4, 8($3)
		lbu 	$12, 8($3)
		lb 		$13, 8($3)

done:
		addiu $2, $zero, 0xa
		syscall
