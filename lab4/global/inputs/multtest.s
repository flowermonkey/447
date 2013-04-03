.text
main:
ori $t9, $0, 0xfff1

multu $t0, $t9
mfhi $t2
mflo $t3

mult $t0, $t9
mfhi $t4
mflo $t5
syscall
