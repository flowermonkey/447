/*
 *
 * Redistributions of any form whatsoever must retain and/or include the
 * following acknowledgment, notices and disclaimer:
 *
 * This product includes software developed by Carnegie Mellon University. 
 *
 * Copyright (c) 2004 by Babak Falsafi and James Hoe,
 * Computer Architecture Lab at Carnegie Mellon (CALCM), 
 * Carnegie Mellon University.
 *
 * This source file was written and maintained by Jared Smolens 
 * as part of the Two-Way In-Order Superscalar project for Carnegie Mellon's 
 * Introduction to Computer Architecture course, 18-447. The source file
 * is in part derived from code originally written by Herman Schmit and 
 * Diana Marculescu.
 *
 * You may not use the name "Carnegie Mellon University" or derivations 
 * thereof to endorse or promote products derived from this software.
 *
 * If you modify the software you must place a notice on or within any 
 * modified version provided or made available to any third party stating 
 * that you have modified the software.  The notice shall include at least 
 * your name, address, phone number, email address and the date and purpose 
 * of the modification.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY WARRANTY OF ANY KIND, EITHER 
 * EXPRESS, IMPLIED OR STATUTORY, INCLUDING BUT NOT LIMITED TO ANYWARRANTY 
 * THAT THE SOFTWARE WILL CONFORM TO SPECIFICATIONS OR BE ERROR-FREE AND ANY 
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, 
 * TITLE, OR NON-INFRINGEMENT.  IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY 
 * BE LIABLE FOR ANY DAMAGES, INCLUDING BUT NOT LIMITED TO DIRECT, INDIRECT, 
 * SPECIAL OR CONSEQUENTIAL DAMAGES, ARISING OUT OF, RESULTING FROM, OR IN 
 * ANY WAY CONNECTED WITH THIS SOFTWARE (WHETHER OR NOT BASED UPON WARRANTY, 
 * CONTRACT, TORT OR OTHERWISE).
 *
 */

// Include the MIPS constants
`include "mips_defines.vh"
`include "internal_defines.vh"

////
//// mips_decode: Decode MIPS instructions
////
//// op      (input)  - Instruction opcode
//// funct2  (input)  - Instruction minor opcode
//// dcd_rt  (input)  - Instruction minor opcode
//// alu_sel (output) - Selects the ALU function
//// ctrl_we (output) - Write to the register file
//// Sys     (output) - System call exception
//// RI      (output) - Reserved instruction exception
//// regDest (output) - Controls which reg is RD/control logic for PC
//// isLui   (output) - Flag for LUI instr
//// isShift (output) - Flag for shift instrs
//// leftShift 	(output) - Flag for left shift instrs
//// arithShift (Output) - Flag for arithmetic shift instrs
//// en_memLd 	(Output) - Enables load decoder for memory
//// memToReg 	(Output) - Enables load decoder for memory
//// isSe 		(Output) - flag for sign extended immd
//// ldType 	(Output) - flag for loading BYTE, HALF, or WORD

////
module mips_decode(/*AUTOARG*/
   // Outputs
   ctrl_we, ctrl_Sys, ctrl_RI, regDest, isImm, isShift, leftShift,
   arithShift, en_memLd, memToReg, isLui, isSe, ldType, alu__sel,
   isJal, isBranch, isALU, mult_act,fwd_src,mult_op,
   jump,link,
   // Inputs
   dcd_op, dcd_funct2,dcd_rd, dcd_rt
   );

   input       [5:0] dcd_op, dcd_funct2;
   input       [4:0] dcd_rd, dcd_rt;
   output reg        ctrl_we, ctrl_Sys, ctrl_RI, isImm, isShift,mult_act,
   					 leftShift, arithShift, en_memLd, memToReg,isLui, isSe,
                     isJal, isBranch, isALU, jump,link;
   output reg  [1:0] regDest;
   output reg  [2:0] ldType,mult_op,fwd_src;
   output reg  [3:0] alu__sel;

	always @(*) begin
		alu__sel = 4'hx;
		ctrl_we = 1'b0;
		ctrl_Sys = 1'b0;
		ctrl_RI = 1'b0;
		regDest = 2'bxx;
		isImm = 1'bx;
		isShift = 1'b0;
		leftShift = 1'bx;
		arithShift = 1'bx;
		en_memLd = 1'bx;
		memToReg = 1'b0;
		isLui = 1'b0;
		isSe = 1'bx;
		ldType = 3'hx;
		mult_act = 1'b0;
        mult_op  = 3'hx;
        fwd_src = 3'hx;
        isJal = 1'b0;
        isALU = 1'b0;
        isBranch = 1'b0;
	    jump = 1'b0;
        link = 1'b0;
		case(dcd_op)
			`OP_OTHER0:
				case(dcd_funct2)
					`OP0_SYSCALL: begin
						ctrl_Sys = 1'b1;
                    end
					`OP0_ADD:
					begin
						alu__sel = `ALU_ADD;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;
                        fwd_src = 3'd0;
					end
					`OP0_ADDU:
					begin
						alu__sel = `ALU_ADD;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;
                        fwd_src = 3'd0;
					end
					`OP0_AND:
					begin
						alu__sel = `ALU_AND;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;
                        fwd_src = 3'd0;
					end
					`OP0_NOR:
					begin
						alu__sel = `ALU_NOR;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;	
                        fwd_src = 3'd0;
					end
					`OP0_OR:
					begin
						alu__sel = `ALU_OR;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;	
                        fwd_src = 3'd0;
					end
					`OP0_SUB:
					begin
						alu__sel = `ALU_SUB;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;	
                        fwd_src = 3'd0;
					end
					`OP0_SUBU:
					begin
						alu__sel = `ALU_SUB;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;	
                        fwd_src = 3'd0;
					end
					`OP0_XOR:
					begin
						alu__sel = `ALU_XOR;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;	
                        fwd_src = 3'd0;
					end
					`OP0_SLL:
					begin
						isShift = 1'b1;
						leftShift = 1'b1;
						arithShift = 1'b0;
						regDest = `RD;
						isImm = 1'b1;
						ctrl_we = 1'b1;
                        fwd_src = 3'd3;
					end
					`OP0_SLLV:
					begin
						isShift = 1'b1;
						leftShift = 1'b1;
						arithShift = 1'b0;
						regDest = `RD;
						isImm = 1'b0;
						ctrl_we = 1'b1;
                        fwd_src = 3'd3;
					end
					`OP0_SRA:
					begin
						isShift = 1'b1;
						leftShift = 1'b0;
						arithShift = 1'b1;
						regDest = `RD;
						isImm = 1'b1;
						ctrl_we = 1'b1;
                        fwd_src = 3'd3;
					end
					`OP0_SRAV:
					begin
						isShift = 1'b1;
						leftShift = 1'b0;
						arithShift = 1'b1;
						regDest = `RD;
						isImm = 1'b0;
						ctrl_we = 1'b1;
                        fwd_src = 3'd3;
					end
					`OP0_SRL:
					begin
						isShift = 1'b1;
						leftShift = 1'b0;
						arithShift = 1'b0;
						regDest = `RD;
						isImm = 1'b1;
						ctrl_we = 1'b1;
                        fwd_src = 3'd3;
					end
					`OP0_SRLV:
					begin
						isShift = 1'b1;
						leftShift = 1'b0;
						arithShift = 1'b0;
						regDest = `RD;
						isImm = 1'b0;
						ctrl_we = 1'b1;
                        fwd_src = 3'd3;
					end
					`OP0_SLT:
					begin
						alu__sel = `ALU_SLT;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;	
                        fwd_src = 3'd0;
					end
					`OP0_SLTU:
					begin
						alu__sel = `ALU_SLTU;
						ctrl_we = 1'b1;
						regDest = `RD;
						isImm = 1'b0;	
                        fwd_src = 3'd0;
					end
                    `OP0_MFHI:    
					begin
		                mult_act = 1'b1;
                        mult_op  = `MUL_MFHI;
						ctrl_we = 1'b1;
						regDest = `RD;
                        isShift = 1'b1;
                        memToReg = 1'b1;
                        fwd_src = 3'd1;
					end
                    `OP0_MTHI:    
					begin
		                mult_act = 1'b1;
                        mult_op  = `MUL_MTHI;
						regDest = `RD;
                        isShift = 1'b1;
                        memToReg = 1'b1;
                        fwd_src = 3'd1;
					end
                    `OP0_MFLO:    
					begin
		                mult_act = 1'b1;
                        mult_op  = `MUL_MFLO;
						ctrl_we = 1'b1;
						regDest = `RD;
                        isShift = 1'b1;
                        memToReg = 1'b1;
                        fwd_src = 3'd1;
					end
                    `OP0_MTLO:    
					begin
		                mult_act = 1'b1;
                        mult_op  = `MUL_MTLO;
						regDest = `RD;
                        isShift = 1'b1;
                        memToReg = 1'b1;
                        fwd_src = 3'd1;
					end
                    `OP0_MULT:    
					begin
		                mult_act = 1'b1;
                        mult_op  = `MUL_MULT;
						ctrl_we = 1'b1;
						regDest = `RD;
                        isShift = 1'b1;
                        memToReg = 1'b1;
                        fwd_src = 3'd1;
					end
                    `OP0_MULTU:
					begin
		                mult_act = 1'b1;
                        mult_op  = `MUL_MULTU;
						ctrl_we = 1'b1;
						regDest = `RD;
                        isShift = 1'b1;
                        memToReg = 1'b1;
                        fwd_src = 3'd1;
					end
                    `OP0_JALR:
					begin
						alu__sel = `ALU_ADD;
						isImm = 1'b0;
						isALU = 1'b1;
						isJal = 1'b0;
						ctrl_we = 1'b1;
						regDest = (dcd_rd==5'd0) ? `R31 : `RD;
                        link = 1'b1;
						jump = 1'b1;
                        fwd_src = 3'd4;
					end
					`OP0_JR:
					begin
						alu__sel = `ALU_ADD;
						isImm = 1'b0;
						isALU = 1'b1;
						isJal = 1'b0;
						jump = 1'b1;
					end
					default:
						ctrl_RI = 1'b1;
				endcase
            `OP_OTHER1:
				case (dcd_rt)
					`OP1_BGEZ:
					begin
						//ALU
						alu__sel = `ALU_BGE;
						isImm = 1'b0;
						//PC Data Select
						isALU = 1'b1;
						isJal = 1'b1;
						isBranch = 1'b1;
						jump = 1'b0;
					end
					`OP1_BGEZAL:
					begin
						//ALU
						alu__sel = `ALU_BGE;
						isImm = 1'b0;
						//Write Data Select
						ctrl_we = 1'b1;
						regDest = `R31;
                        link = 1'b1;
                        fwd_src = 3'd4;
						//PC Data Select
						isALU = 1'b0;
						isJal = 1'b0;
						isBranch = 1'b1;
						jump = 1'b0;
					end
					`OP1_BLTZ:
					begin
						//ALU select
						alu__sel = `ALU_BL;
						isImm = 1'b0;
						//PC signal select
						isALU = 1'b0;
						isJal = 1'b0;
						isBranch = 1'b1;
						jump = 1'b0;
					end
					`OP1_BLTZAL:
					begin
						//ALU select
						alu__sel = `ALU_BL;
						isImm = 1'b0;
						//Write Data Select
						ctrl_we = 1'b1;
						regDest = `R31;
                        link = 1'b1;
                        fwd_src = 3'd4;
						//PC signal select
						isALU = 1'b0;
						isJal = 1'b0;
						isBranch = 1'b1;
						jump = 1'b0;
					end
					default:
						ctrl_RI = 1'b1;
				endcase
			`OP_ADDI:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b1;
				regDest = `RT;
				isImm = 1'b1;
				isSe = 1'b1;
                fwd_src = 3'd0;
			end
			`OP_ADDIU:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b1;
				regDest = `RT;
				isImm = 1'b1;
				isSe = 1'b1;
                fwd_src = 3'd0;
			end
			`OP_ANDI:
			begin
				alu__sel = `ALU_AND;
				ctrl_we = 1'b1;
				regDest = `RT;
				isImm = 1'b1;
				isSe = 1'b0;
                fwd_src = 3'd0;
			end
			`OP_ORI:
			begin
				alu__sel = `ALU_OR;
				ctrl_we = 1'b1;
				regDest = `RT;
				isImm = 1'b1;	
				isSe = 1'b0;
                fwd_src = 3'd0;
			end
			`OP_XORI:
			begin
				alu__sel = `ALU_XOR;
				ctrl_we = 1'b1;
				regDest = `RT;
				isImm = 1'b1;	
				isSe = 1'b0;
                fwd_src = 3'd0;
			end
			`OP_SLTI:
			begin
				alu__sel = `ALU_SLT;
				ctrl_we = 1'b1;
				regDest = `RT;
				isImm = 1'b1;	
				isSe = 1'b1;
                fwd_src = 3'd0;
			end
			`OP_SLTIU:
			begin
				alu__sel = `ALU_SLTU;
				ctrl_we = 1'b1;
				regDest = `RT;
				isImm = 1'b1;	
				isSe = 1'b1;
                fwd_src = 3'd0;
			end
			`OP_LUI:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b1;
				regDest = `RT;
				memToReg = 1'b0;
				isShift = 1'b1; //bc comes from shiftReg
				isImm = 1'b1;
				isLui = 1'b1;
				leftShift = 1'b1;
				arithShift = 1'b0;
				isSe = 1'b0;
                fwd_src = 3'd3;
			end
			`OP_LB:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b1;
				regDest = `RT;
				memToReg = 1'b1;
				isImm = 1'b1;
				isSe = 1'b1;
				en_memLd = 1'b0;
				ldType = `MEM_BYTE;
                fwd_src = 3'd2;
			end
			`OP_LBU:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b1; 
				regDest = `RT;
				memToReg = 1'b1;
				isImm = 1'b1;
				isSe = 1'b1;
				en_memLd = 1'b0;
				ldType = `MEM_U_BYTE;
                fwd_src = 3'd2;
			end
			`OP_LH:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b1;
				regDest = `RT;
				memToReg = 1'b1;
				isImm = 1'b1;
				isSe = 1'b1;
				en_memLd = 1'b0;
				ldType = `MEM_HALF;
                fwd_src = 3'd2;
			end
			`OP_LHU:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b1; 
				regDest = `RT;
				memToReg = 1'b1;
				isImm = 1'b1;
				isSe = 1'b1;
				en_memLd = 1'b0;
				ldType = `MEM_U_HALF;
                fwd_src = 3'd2;
			end
			`OP_LW:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b1;
				regDest = `RT;
				memToReg = 1'b1;
				isImm = 1'b1;
				isSe = 1'b1;
				en_memLd = 1'b0;
				ldType = `MEM_WORD;
                fwd_src = 3'd2;
			end
			`OP_SB:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b0;
				isImm = 1'b1;
				isSe = 1'b1;
				en_memLd = 1'b1;
				ldType = `MEM_BYTE;
			end
			`OP_SH:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b0;
				isImm = 1'b1;
				isSe = 1'b1;
				en_memLd = 1'b1;
				ldType = `MEM_HALF;
			end
			`OP_SW:
			begin
				alu__sel = `ALU_ADD;
				ctrl_we = 1'b0;
				isImm = 1'b1;
				isSe = 1'b1;
				en_memLd = 1'b1;
				ldType = `MEM_WORD;
			end
            `OP_NOP:
            begin
                ctrl_RI = 1'b0;                 
            end
            `OP_J:
			begin
				isJal = 1'b1;
				jump = 1'b1;
			end
			`OP_JAL:
			begin
				isJal = 1'b1;
				ctrl_we = 1'b1;
				regDest = `R31;
				jump = 1'b1;
                link = 1'b1;
                fwd_src = 3'd4;
			end
			`OP_BEQ:
			begin
				//ALU select
				alu__sel = `ALU_BEQ;
				isImm = 1'b0;
				//PC signal select
				isALU = 1'b0;
				isJal = 1'b0;
				isBranch = 1'b1;
				jump = 1'b0;
			end
			`OP_BGTZ:
			begin
				//ALU select
				alu__sel = `ALU_BG;
				isImm = 1'b0;
				//PC signal select
				isALU = 1'b0;
				isJal = 1'b0;
				isBranch = 1'b1;
				jump = 1'b0;
			end
			`OP_BLEZ:
			begin
				//ALU select
				alu__sel = `ALU_BLE;
				isImm = 1'b0;
				//PC signal select
				isALU = 1'b0;
				isJal = 1'b0;
				isBranch = 1'b1;
				jump = 1'b0;
			end
			`OP_BNE:
			begin
				//ALU select
				alu__sel = `ALU_BNE;
				isImm = 1'b0;
				//PC signal select
				isALU = 1'b0;
				isJal = 1'b0;
				isBranch = 1'b1;
				jump = 1'b0;
			end	
			default:
			begin
				ctrl_RI = 1'b1;
			end
		endcase // case(op)
	end

endmodule
