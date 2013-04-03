.text
main:
    lui $t0, 1 
    addiu $3, $0, 0
    sb $3, 0($t0)

    addiu  $t0, $t0, 1
    addiu $3, $0, 1
    sb $3, 0($t0)

    addiu  $t0, $t0, 1
    addiu $3, $0, 2 
    sb $3, 0($t0)
    
primeloop:
    lbu $t2, 0($t0)
    beq $t2, $0, done # not prime? continue
    addiu $t0, $t0, -1 # prime: decrement counter
    j primeloop # keep looking if not at 5000th prime

    addiu $v0, $0, 10
    syscall

