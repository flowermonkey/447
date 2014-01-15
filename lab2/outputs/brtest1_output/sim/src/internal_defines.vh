////
//// Internal signal constants
////

// ALU
`define ALU_ADD      4'b0000
`define ALU_SUB      4'b0001
`define ALU_AND      4'b0010
`define ALU_OR       4'b0011
`define ALU_NOR      4'b0100
`define ALU_XOR      4'b0101
`define ALU_SLT      4'b0110
`define ALU_SLTU     4'b0111
`define ALU_BEQ      4'b1000
`define ALU_BNE      4'b1001
`define ALU_BLE      4'b1010
`define ALU_BGE      4'b1011
`define ALU_BL       4'b1100
`define ALU_BG       4'b1101

`define MEM_BYTE     3'b000
`define MEM_U_BYTE   3'b100
`define MEM_HALF     3'b001
`define MEM_U_HALF   3'b101
`define MEM_WORD     3'b010

`define	RD			 2'b00
`define	RT			 2'b01
`define	R31			 2'b10
