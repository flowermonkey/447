#include <stdio.h>
#include "shell.h"

//Defines for easier readability of registers
#define OPCODE ((curr_instr >> 26) & 0x3F) 
#define OPCODE_SPECIAL (curr_instr & 0x3F) 
#define OPCODE_REGIMM ((curr_instr >> 16) & 0x3F) 

#define SIGNEXD(val) ((val & 0x8000) ? ((val) | 0xFFFF0000 ) \
												: ((val) & 0x0000FFFF))
#define TARGET (SIGNEXD(curr_instr)<<2)
#define	RS ((curr_instr>>21) & 0x1F)
#define	RT ((curr_instr>>16) & 0x1F)
#define	RD ((curr_instr>>11) & 0x1F)

//array of functions used to index into asm instructions
void (*asm_func [0x2C]) ();
void (*asm_func_special [0x2C]) ();
void (*asm_func_regimm [0x12]) ();
uint32_t curr_instr = 0;  //decoded pc counter instruction

//ASSEMBLY INSTRUCTION FUNCTIONS/*{{{*/
void nop() 
{
	NEXT_STATE.PC |= NEXT_STATE.PC;
}

void special()
{
	if(OPCODE_SPECIAL < 0x2C)
		asm_func_special[OPCODE_SPECIAL] ();
	else
		nop();
}

void regimm()
{
	if(OPCODE_REGIMM < 0x22)
		asm_func_regimm[OPCODE_REGIMM] ();
	else
		nop();
}

void j()
{
	NEXT_STATE.PC = (NEXT_STATE.PC & 0xF0000000) | 
						((curr_instr & 0x03FFFFFF)<<2);
}

void jal()
{
	NEXT_STATE.REGS[31] = NEXT_STATE.PC;
	NEXT_STATE.PC =  (NEXT_STATE.PC & 0xF0000000) | 
						((curr_instr & 0x03FFFFFF)<<2);
}

void beq()
{
	if(CURRENT_STATE.REGS[RS]==CURRENT_STATE.REGS[RT]) 
		NEXT_STATE.PC =	NEXT_STATE.PC + TARGET; 
}

void bne()
{
	if(CURRENT_STATE.REGS[RS]!=CURRENT_STATE.REGS[RT])
		NEXT_STATE.PC = NEXT_STATE.PC + TARGET;
}

void blez()
{
	if( !!(CURRENT_STATE.REGS[RS] & 0x80000000) || (CURRENT_STATE.REGS[RS]==0))
		NEXT_STATE.PC = NEXT_STATE.PC + TARGET;
}

void bgtz()
{
	if(!(CURRENT_STATE.REGS[RS] & 0x80000000) || (CURRENT_STATE.REGS[RS]!=0))
		NEXT_STATE.PC = NEXT_STATE.PC + TARGET;
}

void addi()
{
	NEXT_STATE.REGS[RT] = (int32_t)(CURRENT_STATE.REGS[RS]) +
							(int32_t)(SIGNEXD(curr_instr));
	
	//NOTE: OVERFLOW Exception is ignored
}

void addiu()
{
	NEXT_STATE.REGS[RT] = (uint32_t)(CURRENT_STATE.REGS[RS]) +
							(uint32_t)(SIGNEXD(curr_instr));
}

void slti()
{
	NEXT_STATE.REGS[RT] = ((int32_t)(CURRENT_STATE.REGS[RS]) < 
							(int32_t)(SIGNEXD(curr_instr))) ? 0x1 : 0x0;
}

void sltiu()
{
	NEXT_STATE.REGS[RT] = ((uint32_t)(CURRENT_STATE.REGS[RS]) < 
							(uint32_t) (SIGNEXD(curr_instr))) ? 0x1 : 0x0;
}

void andi()
{
	NEXT_STATE.REGS[RT] = 0x0000FFFF & CURRENT_STATE.REGS[RS] & curr_instr;
}

void ori()
{
	NEXT_STATE.REGS[RT] = 0x0000FFFF & (CURRENT_STATE.REGS[RS] | curr_instr);
}

void xori()
{
	NEXT_STATE.REGS[RT] = 0x0000FFFF & (CURRENT_STATE.REGS[RS] ^ curr_instr);
}

void lui()
{
	NEXT_STATE.REGS[RT] = curr_instr << 16;
}

void lb()
{
	uint32_t addr = CURRENT_STATE.REGS[RS] + SIGNEXD(curr_instr);
	uint32_t mem = mem_read_32(addr - (addr&0x3));
	
	mem = (mem >> ((addr & 0x3)*8));
	NEXT_STATE.REGS[RT] = (mem & 0x80) ? (mem | 0xFFFFFF00) : (mem & 0xFF);
	//NOTE: Exceptions ignored
}

void lh()
{
	uint32_t addr = CURRENT_STATE.REGS[RS] + SIGNEXD(curr_instr);
	uint32_t mem = mem_read_32(addr - (addr&0x3));

	NEXT_STATE.REGS[RT] = ((addr&0x3)>1) ? (mem>>16)|0xFFFF0000 : SIGNEXD(mem&0xFFFF);
	//NOTE: Exceptions ignored
}

void lw()
{
	uint32_t addr = CURRENT_STATE.REGS[RS] + SIGNEXD(curr_instr);

	NEXT_STATE.REGS[RT] =  mem_read_32(addr - (addr&0x3));
	//NOTE: Exceptions ignored
}

void lbu()
{
	uint32_t addr = CURRENT_STATE.REGS[RS] + SIGNEXD(curr_instr);
	uint32_t mem = mem_read_32(addr - (addr&0x3));

	mem = (mem >> ((addr & 0x3)*8));
	NEXT_STATE.REGS[RT] = (mem & 0xFF);
	//NOTE: Exceptions ignored
}

void lhu()
{
	uint32_t addr = CURRENT_STATE.REGS[RS] + SIGNEXD(curr_instr);
	uint32_t mem = mem_read_32(addr - (addr&0x3));

	NEXT_STATE.REGS[RT] = (mem & 0xFFFF);
	NEXT_STATE.REGS[RT] = ((addr&0x3)>1) ? (mem>>16) : mem;
	NEXT_STATE.REGS[RT] &= 0xFFFF;

	//NOTE: Exceptions ignored
}

void sb()
{
	uint32_t addr = CURRENT_STATE.REGS[RS] + SIGNEXD(curr_instr);
	uint32_t mem = mem_read_32(addr - (addr&0x3));
	uint32_t byte =  CURRENT_STATE.REGS[RT] & 0xFF;
	uint32_t byte_loc = addr & 0x3;
	uint32_t byte_mask = (byte_loc==3) ? (0x00FFFFFF) :
							((int32_t)(0x80000000)>>(((3-(byte_loc))*8)-1));
	
	byte_mask |= (~byte_mask>>8);
	mem &= byte_mask;
	mem |= (byte << ((addr & 0x3)*8));
	mem_write_32((addr - (addr&3)), mem);
	//NOTE: Exceptions ignored
}

void sh()
{
	uint32_t addr = CURRENT_STATE.REGS[RS] + SIGNEXD(curr_instr);
	uint32_t mem = mem_read_32(addr - (addr&0x3));
	uint32_t half = ((addr&0x3) < 2) ? (CURRENT_STATE.REGS[RT] & 0xFFFF)
						: CURRENT_STATE.REGS[RT] << 16;
	
	mem = ((addr&3) < 2) ? (mem&0xFFFF0000) : (mem&0xFFFF);
	mem |= half;
	mem_write_32((addr - (addr&0x3)), mem);
	//NOTE: Exceptions ignored
}

void sw()
{
	uint32_t addr = CURRENT_STATE.REGS[RS] + SIGNEXD(curr_instr);
	uint32_t word =  CURRENT_STATE.REGS[RT];

	mem_write_32((addr - (addr&0x3)), word);
	//NOTE: Exceptions ignored
}

void sll()
{
	uint32_t sa = ((curr_instr>>6) & 0x1F);
	
	NEXT_STATE.REGS[RD] = CURRENT_STATE.REGS[RT] << sa; 
}

void srl() 
{
	uint32_t sa = ((curr_instr>>6) & 0x1F);
	
	NEXT_STATE.REGS[RD] = (uint32_t)(CURRENT_STATE.REGS[RT]) >> sa; 
}

void sra() 
{
	uint32_t sa = ((curr_instr>>6) & 0x1F);
	
	NEXT_STATE.REGS[RD] = (int32_t)(CURRENT_STATE.REGS[RT]) >> sa; 
}

void sllv() 
{
	uint32_t shift = CURRENT_STATE.REGS[RS] & 0x1F;

	NEXT_STATE.REGS[RD] = CURRENT_STATE.REGS[RT] << shift; 
}
void srlv() 
{
	uint32_t shift = CURRENT_STATE.REGS[RS] & 0x1F;

	NEXT_STATE.REGS[RD] = (uint32_t) CURRENT_STATE.REGS[RT] >> shift; 
}

void srav() 
{
	uint32_t shift = CURRENT_STATE.REGS[RS] & 0x1F;

	NEXT_STATE.REGS[RD] = (int32_t)(CURRENT_STATE.REGS[RT]) >> shift; 
}

void jr() 
{
	NEXT_STATE.PC = CURRENT_STATE.REGS[RS];
}

void jalr() 
{
	NEXT_STATE.PC = CURRENT_STATE.REGS[RS];
	if(RD==0)
		NEXT_STATE.REGS[31] = CURRENT_STATE.PC +4;
	else
		NEXT_STATE.REGS[RD] = CURRENT_STATE.PC +4;
}

void syscall() 
{
	if(CURRENT_STATE.REGS[2] == 0x0A)
		RUN_BIT = 0;
}

void mfhi() 
{
	uint32_t next_instr1 = mem_read_32(CURRENT_STATE.PC+4) & 0x3F;
	uint32_t next_instr2 = mem_read_32(CURRENT_STATE.PC+8) & 0x3F;
	
	if((next_instr1 == 0x1A) || (next_instr1 == 0x1B) ||
		(next_instr1 == 0x18) || (next_instr1 == 0x19) ||
		(next_instr2 == 0x1A) || (next_instr2== 0x1B) ||
		(next_instr1 == 0x18) || (next_instr2 == 0x19))
		return;

	NEXT_STATE.REGS[RD] = CURRENT_STATE.HI;
}

void mthi() 
{
	NEXT_STATE.HI = CURRENT_STATE.REGS[RS];
}

void mflo() 
{
	uint32_t next_instr1 = mem_read_32(CURRENT_STATE.PC+4) & 0x3F;
	uint32_t next_instr2 = mem_read_32(CURRENT_STATE.PC+8) & 0x3F;
	
	if((next_instr1 == 0x1A) || (next_instr1 == 0x1B) ||
		(next_instr1 == 0x18) || (next_instr1 == 0x19) ||
		(next_instr2 == 0x1A) || (next_instr2== 0x1B) ||
		(next_instr1 == 0x18) || (next_instr2 == 0x19))
		return;
	
	NEXT_STATE.REGS[RD] = CURRENT_STATE.LO;
}

void mtlo() 
{
	NEXT_STATE.LO = CURRENT_STATE.REGS[RS];
}

void mult() 
{
	int64_t result = 0;
	
	result = (int64_t)((int32_t)CURRENT_STATE.REGS[RS]) * 
						(int64_t)((int32_t)CURRENT_STATE.REGS[RT]);
	NEXT_STATE.HI = (uint32_t)((result>>31)>>1);
	NEXT_STATE.LO = (uint32_t)(result);
}

void multu() 
{
	uint64_t result = 0;
	
	result = (uint64_t)((uint32_t)CURRENT_STATE.REGS[RS]) * 
					(uint64_t)((uint32_t)CURRENT_STATE.REGS[RT]);
	NEXT_STATE.HI = (uint32_t)((result>>31)>>1);
	NEXT_STATE.LO = (uint32_t)(result);
}

void div() 
{
	NEXT_STATE.LO = (int32_t)(CURRENT_STATE.REGS[RS]) / (int32_t)(CURRENT_STATE.REGS[RT]);
	NEXT_STATE.HI =  (int32_t)(CURRENT_STATE.REGS[RS]) % (int32_t)(CURRENT_STATE.REGS[RT]);
}

void divu() 
{
	NEXT_STATE.LO = (uint32_t)(CURRENT_STATE.REGS[RS]) / (uint32_t)(CURRENT_STATE.REGS[RT]);
	NEXT_STATE.HI =  (uint32_t)(CURRENT_STATE.REGS[RS]) % (uint32_t)(CURRENT_STATE.REGS[RT]);
}

void add() 
{
	NEXT_STATE.REGS[RD] = (int32_t)(CURRENT_STATE.REGS[RS]) +
							(int32_t)(CURRENT_STATE.REGS[RT]);
	//NOTE: OVERFLOW Exception is ignored
}
void addu() 
{
	NEXT_STATE.REGS[RD] = (uint32_t)(CURRENT_STATE.REGS[RS]) +
							(uint32_t)(CURRENT_STATE.REGS[RT]);
}

void sub() 
{
	NEXT_STATE.REGS[RD] = (int32_t)(CURRENT_STATE.REGS[RS]) -
							(int32_t)(CURRENT_STATE.REGS[RT]);
	//NOTE: OVERFLOW Exception is ignored
}

void subu() 
{
	NEXT_STATE.REGS[RD] = (uint32_t)(CURRENT_STATE.REGS[RS]) -
							(uint32_t)(CURRENT_STATE.REGS[RT]);
}

void and() 
{
	NEXT_STATE.REGS[RD] = CURRENT_STATE.REGS[RS] & CURRENT_STATE.REGS[RT];
}

void or() 
{
	NEXT_STATE.REGS[RD] = CURRENT_STATE.REGS[RS] | CURRENT_STATE.REGS[RT];
}

void xor() 
{
	NEXT_STATE.REGS[RD] = CURRENT_STATE.REGS[RS] ^ CURRENT_STATE.REGS[RT];
}

void nor() 
{
	NEXT_STATE.REGS[RD] = ~(CURRENT_STATE.REGS[RS] | CURRENT_STATE.REGS[RT]);
}

void slt() 
{
	NEXT_STATE.REGS[RD] = ((int32_t)(CURRENT_STATE.REGS[RS]) <
							(int32_t)(CURRENT_STATE.REGS[RT])) ? 0x1 : 0x0;
}

void sltu() 
{
	NEXT_STATE.REGS[RD] = ((uint32_t)(CURRENT_STATE.REGS[RS]) <
							(uint32_t)(CURRENT_STATE.REGS[RT])) ? 0x1 : 0x0;
}

void bltz() 
{
	if(CURRENT_STATE.REGS[RS] & 0x80000000) 
		NEXT_STATE.PC =  NEXT_STATE.PC + TARGET;
}

void bgez() 
{
	if(!(CURRENT_STATE.REGS[RS] & 0x80000000)) 
		NEXT_STATE.PC =  NEXT_STATE.PC + TARGET;
}

void bltzal() 
{
	if(RS==31)
		return;

	NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
	
	if((CURRENT_STATE.REGS[RS] & 0x80000000)) 
		NEXT_STATE.PC =  NEXT_STATE.PC + TARGET;

}

void bgezal() 
{
	if(RS==31)
		return;
	
	NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
	
	if(!(CURRENT_STATE.REGS[RS] & 0x80000000)) 
		NEXT_STATE.PC =  NEXT_STATE.PC + TARGET;
}
/*}}}*/

void process_instruction()
{
	uint32_t i;
	
	//decode current pc to get instruction information
	curr_instr = mem_read_32(CURRENT_STATE.PC);

	//advance next state pc by one instruction [4 bytes]
	NEXT_STATE.PC = CURRENT_STATE.PC + 4;

	//The following are instruction assignments to globally defined function 
	//arrays. Any array index without a defined instruction gets replaced 
	//with a NOP.
	//NOP is defined as an OR of the currentPC with itself

	//asm_func instruction assignments{{{
	asm_func[0x00] = special;
	asm_func[0x01] = regimm;
	asm_func[0x02] = j;
	asm_func[0x03] = jal;
	asm_func[0x04] = beq;
	asm_func[0x05] = bne;
	asm_func[0x06] = blez;
	asm_func[0x07] = bgtz;
	asm_func[0x08] = addi;
	asm_func[0x09] = addiu;
	asm_func[0x0A] = slti;
	asm_func[0x0B] = sltiu;
	asm_func[0x0C] = andi;
	asm_func[0x0D] = ori;
	asm_func[0x0E] = xori;
	asm_func[0x0F] = lui;
	asm_func[0x20] = lb;
	asm_func[0x21] = lh;
	asm_func[0x22] = nop;
	asm_func[0x23] = lw;
	asm_func[0x24] = lbu;
	asm_func[0x25] = lhu;
	asm_func[0x26] = nop;
	asm_func[0x27] = nop;
	asm_func[0x28] = sb;
	asm_func[0x29] = sh;
	asm_func[0x2A] = nop;
	asm_func[0x2B] = sw;
	
	for(i=0x10; i<0x20; i++)
		asm_func[i] = nop; //}}}

	//asm_func_special instruction assignments {{{
	asm_func_special[0x00] = sll;
	asm_func_special[0x01] = nop;
	asm_func_special[0x02] = srl;
	asm_func_special[0x03] = sra;
	asm_func_special[0x04] = sllv;
	asm_func_special[0x05] = nop;
	asm_func_special[0x06] = srlv;
	asm_func_special[0x07] = srav;
	asm_func_special[0x08] = jr;
	asm_func_special[0x09] = jalr;
	asm_func_special[0x0A] = nop;
	asm_func_special[0x0B] = nop;
	asm_func_special[0x0C] = syscall;
	asm_func_special[0x0D] = nop;
	asm_func_special[0x0E] = nop;
	asm_func_special[0x0F] = nop;
	asm_func_special[0x10] = mfhi;
	asm_func_special[0x11] = mthi;
	asm_func_special[0x12] = mflo;
	asm_func_special[0x13] = mtlo;
	asm_func_special[0x14] = nop;
	asm_func_special[0x15] = nop;
	asm_func_special[0x16] = nop;
	asm_func_special[0x17] = nop;
	asm_func_special[0x18] = mult;
	asm_func_special[0x19] = multu;
	asm_func_special[0x1A] = div;
	asm_func_special[0x1B] = divu;
	asm_func_special[0x1C] = nop;
	asm_func_special[0x1D] = nop;
	asm_func_special[0x1E] = nop;
	asm_func_special[0x1F] = nop;
	asm_func_special[0x20] = add;
	asm_func_special[0x21] = addu;
	asm_func_special[0x22] = sub;
	asm_func_special[0x23] = subu;
	asm_func_special[0x24] = and;
	asm_func_special[0x25] = or;
	asm_func_special[0x26] = xor;
	asm_func_special[0x27] = nor;
	asm_func_special[0x28] = nop;
	asm_func_special[0x29] = nop;
	asm_func_special[0x2A] = slt;
	asm_func_special[0x2B] = sltu; //}}}
	
	//asm_func_regimm instruction assignments /{{{
	asm_func_regimm[0x00] = bltz;
	asm_func_regimm[0x01] = bgez;
	asm_func_regimm[0x10] = bltzal;
	asm_func_regimm[0x11] = bgezal;

	for(i=0x02; i<0x10; i++)
		asm_func_regimm[i] = nop; //}}}
	
	//running decoded instruction by using opcode to index into array
	if(OPCODE < 0x2C)
		asm_func[OPCODE] ();
	else
		nop();
	
	//this is done in order to disregard any writes to $zero
	NEXT_STATE.REGS[0x00] = 0x0;
}
