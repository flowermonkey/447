/*
 * ECE 18-447, Spring 2013
 *
 * MIPS pipeline timing simulator
 *
 * Chris Fallin, 2012
 */

#include "pipe.h"
#include "shell.h"
#include "mips.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define DEBUG 
#define GSHARE(X,Y) (uint8_t) (((0xFF)&(X)) ^ (((Y)>>0x2)&(0xFF)))
#define BTB_IDX(X) (uint32_t) (((X)>>2) & (0x3FF))

/* debug */
void print_op(Pipe_Op *op)
{
    if (op)
        printf("OP (PC=%08x inst=%08x) src1=R%d (%08x) src2=R%d (%08x) dst=R%d valid %d (%08x) br=%d taken=%d dest=%08x mem=%d addr=%08x predictedPC as=%08x\n",
                op->pc, op->instruction, op->reg_src1, op->reg_src1_value, op->reg_src2, op->reg_src2_value, op->reg_dst, op->reg_dst_value_ready,
                op->reg_dst_value, op->is_branch, op->branch_taken,
                op->branch_dest, op->is_mem, op->mem_addr, op->predicted_dir);
    else
        printf("(null)\n");
}

/* global pipeline state */
Pipe_State pipe;

/*Current state of Cache*/
line_t instCache [64][4];
line_t dataCache [256][8];
line_t L2[512][16];   

/*MSHR*/
mshr_t MSHR [16];
uint8_t empty_mshr=0;
queue_t memQ[16]; //suppose to be infinite, but for our pipe line this should be enough
int8_t row_buf[8];

/*Count for stalls*/
int stall_time = 0;
int instr_stall_count = 0;
int data_stall_count = 0;

/*Global Branch Prediction Vaiables*/
uint8_t ghr = 0;
uint8_t pht[256];
buffer_t btb[1024];

void pipe_init()
{
    int i; 

    memset(&pipe, 0, sizeof(Pipe_State));
    pipe.PC = 0x00400000;
    cache_init();
    
    for (i=0; i<256; i++)
        pht[i] = 0;
    
    for (i=0; i<1024; i++){
        btb[i].valid=0;
        btb[i].tag=0;
        btb[i].U=0;
        btb[i].target=0;
    }
}

void pipe_cycle()
{
    uint8_t pending_miss;
    uint8_t set;
    uint32_t tag;
    int i;

#ifdef DEBUG
    printf("\n\n----\n\nPIPELINE: (Cycle: %d)\n", stat_cycles);
    printf("DCODE: "); print_op(pipe.decode_op);
    printf("EXEC : "); print_op(pipe.execute_op);
    printf("MEM  : "); print_op(pipe.mem_op);
    printf("WB   : "); print_op(pipe.wb_op);
    printf("\n");
#endif

    pipe_stage_wb();
    pipe_stage_mem();
    pipe_stage_execute();
    pipe_stage_decode();
    pipe_stage_fetch();

    /* handle branch recoveries */
    if (pipe.branch_recover) {
#ifdef DEBUG
        printf("branch recovery: new dest %08x flush %d stages\n", pipe.branch_dest, pipe.branch_flush);
#endif

        pipe.PC = pipe.branch_dest;
        
        if (pipe.branch_flush >= 2) {
            if (pipe.decode_op) free(pipe.decode_op);
            pipe.decode_op = NULL;
        }

        if (pipe.branch_flush >= 3) {
            if (pipe.execute_op) free(pipe.execute_op);
            pipe.execute_op = NULL;
        }

        if (pipe.branch_flush >= 4) {
            if (pipe.mem_op) free(pipe.mem_op);
            pipe.mem_op = NULL;
        }

        if (pipe.branch_flush >= 5) {
            if (pipe.wb_op) free(pipe.wb_op);
            pipe.wb_op = NULL;
        }

        pipe.branch_recover = 0;
        pipe.branch_dest = 0;
        pipe.branch_flush = 0;
        
        //if there is a missed instruct while recovery
        if(pipe.stall_inst == 1){
            pending_miss = 1;
            set = ((pipe.PC>>5)  & 0x3F);
            tag = ((pipe.PC>>11) & 0x1FFFFF);
            for(i=0; i<4; i++){
                if ((instCache[set][i].valid==1) && (instCache[set][i].tag == tag)){
                    pending_miss = 0;
                    break;
                }
            }
            if(pending_miss == 0){
                pipe.stall_inst = 0;
                if(pipe.mem_request == 0)
                    instr_stall_count = 0;
                else{
                    instr_stall_count++;
                }
            }
        }
        stat_squash++;
    }
}

void pipe_recover(int flush, uint32_t dest)
{
    /* if there is already a recovery scheduled, it must have come from a later
     * stage (which executes older instructions), hence that recovery overrides
     * our recovery. Simply return in this case. */
    if (pipe.branch_recover) return;

    /* schedule the recovery. This will be done once all pipeline stages simulate the current cycle. */
    pipe.branch_recover = 1;
    pipe.branch_flush = flush;
    pipe.branch_dest = dest;
}

void pipe_stage_wb()
{
    /* if there is no instruction in this pipeline stage, we are done */
    if (!pipe.wb_op)
        return;

    /* grab the op out of our input slot */
    Pipe_Op *op = pipe.wb_op;
    pipe.wb_op = NULL;

    /* if this instruction writes a register, do so now */
    if (op->reg_dst != -1 && op->reg_dst != 0) {
        pipe.REGS[op->reg_dst] = op->reg_dst_value;
#ifdef DEBUG
        printf("R%d = %08x\n", op->reg_dst, op->reg_dst_value);
#endif
    }

    /* if this was a syscall, perform action */
    if (op->opcode == OP_SPECIAL && op->subop == SUBOP_SYSCALL) {
        if (op->reg_src1_value == 0xA) {
            pipe.PC = op->pc; /* fetch will do pc += 4, then we stop with correct PC */
            RUN_BIT = 0;
        }
    }

    /* free the op */
    free(op);

    stat_inst_retire++;
}

void pipe_stage_mem()
{
    /* if there is no instruction in this pipeline stage, we are done */
    if (!pipe.mem_op)
        return;

    /* grab the op out of our input slot */
    Pipe_Op *op = pipe.mem_op;

    uint32_t val = 0;
    if (op->is_mem)
        val = cache_read(op->mem_addr & ~3, 0);
    if(val == -1)
        return;

    switch (op->opcode) {
        case OP_LW:
        case OP_LH:
        case OP_LHU:
        case OP_LB:
        case OP_LBU:
            {
                /* extract needed value */
                op->reg_dst_value_ready = 1;
                if (op->opcode == OP_LW) {
                    op->reg_dst_value = val;
                }
                else if (op->opcode == OP_LH || op->opcode == OP_LHU) {
                    if (op->mem_addr & 2)
                        val = (val >> 16) & 0xFFFF;
                    else
                        val = val & 0xFFFF;

                    if (op->opcode == OP_LH)
                        val |= (val & 0x8000) ? 0xFFFF8000 : 0;

                    op->reg_dst_value = val;
                }
                else if (op->opcode == OP_LB || op->opcode == OP_LBU) {
                    switch (op->mem_addr & 3) {
                        case 0:
                            val = val & 0xFF;
                            break;
                        case 1:
                            val = (val >> 8) & 0xFF;
                            break;
                        case 2:
                            val = (val >> 16) & 0xFF;
                            break;
                        case 3:
                            val = (val >> 24) & 0xFF;
                            break;
                    }

                    if (op->opcode == OP_LB)
                        val |= (val & 0x80) ? 0xFFFFFF80 : 0;

                    op->reg_dst_value = val;
                }
            }
            break;

        case OP_SB:
            switch (op->mem_addr & 3) {
                case 0: val = (val & 0xFFFFFF00) | ((op->mem_value & 0xFF) << 0); break;
                case 1: val = (val & 0xFFFF00FF) | ((op->mem_value & 0xFF) << 8); break;
                case 2: val = (val & 0xFF00FFFF) | ((op->mem_value & 0xFF) << 16); break;
                case 3: val = (val & 0x00FFFFFF) | ((op->mem_value & 0xFF) << 24); break;
            }

            cache_write(op->mem_addr & ~3, val);
            if((pipe.stall_data == 1) || (pipe.Dstayout ==1))
                return;
            break;

        case OP_SH:
#ifdef DEBUG
            printf("SH: addr %08x val %04x old word %08x\n", op->mem_addr, op->mem_value & 0xFFFF, val);
#endif
            if (op->mem_addr & 2)
                val = (val & 0x0000FFFF) | (op->mem_value) << 16;
            else
                val = (val & 0xFFFF0000) | (op->mem_value & 0xFFFF);
#ifdef DEBUG
            printf("new word %08x\n", val);
#endif

            cache_write(op->mem_addr & ~3, val);
            if((pipe.stall_data == 1) || (pipe.Dstayout ==1))
                return;
            break;

        case OP_SW:
            val = op->mem_value;
            cache_write(op->mem_addr & ~3, val);
            if((pipe.stall_data == 1) || (pipe.Dstayout ==1))
                return;
            break;
    }

    /* clear stage input and transfer to next stage */
    pipe.mem_op = NULL;
    pipe.wb_op = op;
}

void pipe_stage_execute()
{
    /* if a multiply/divide is in progress, decrement cycles until value is ready */
    if (pipe.multiplier_stall > 0)
        pipe.multiplier_stall--;

    /* if downstream stall, return (and leave any input we had) */
    if (pipe.mem_op != NULL)
        return;

    /* if no op to execute, return */
    if (pipe.execute_op == NULL)
        return;

    /* grab op and read sources */
    Pipe_Op *op = pipe.execute_op;

    /* read register values, and check for bypass; stall if necessary */
    int stall = 0;
    if (op->reg_src1 != -1) {
        if (op->reg_src1 == 0)
            op->reg_src1_value = 0;
        else if (pipe.mem_op && pipe.mem_op->reg_dst == op->reg_src1) {
            if (!pipe.mem_op->reg_dst_value_ready)
                stall = 1;
            else
                op->reg_src1_value = pipe.mem_op->reg_dst_value;
        }
        else if (pipe.wb_op && pipe.wb_op->reg_dst == op->reg_src1) {
            op->reg_src1_value = pipe.wb_op->reg_dst_value;
        }
        else
            op->reg_src1_value = pipe.REGS[op->reg_src1];
    }
    if (op->reg_src2 != -1) {
        if (op->reg_src2 == 0)
            op->reg_src2_value = 0;
        else if (pipe.mem_op && pipe.mem_op->reg_dst == op->reg_src2) {
            if (!pipe.mem_op->reg_dst_value_ready)
                stall = 1;
            else
                op->reg_src2_value = pipe.mem_op->reg_dst_value;
        }
        else if (pipe.wb_op && pipe.wb_op->reg_dst == op->reg_src2) {
            op->reg_src2_value = pipe.wb_op->reg_dst_value;
        }
        else
            op->reg_src2_value = pipe.REGS[op->reg_src2];
    }

    /* if bypassing requires a stall (e.g. use immediately after load),
     * return without clearing stage input */
    if (stall) 
        return;

    /* execute the op */
    switch (op->opcode) {
        case OP_SPECIAL:
            op->reg_dst_value_ready = 1;
            switch (op->subop) {
                case SUBOP_SLL:
                    op->reg_dst_value = op->reg_src2_value << op->shamt;
                    break;
                case SUBOP_SLLV:
                    op->reg_dst_value = op->reg_src2_value << op->reg_src1_value;
                    break;
                case SUBOP_SRL:
                    op->reg_dst_value = op->reg_src2_value >> op->shamt;
                    break;
                case SUBOP_SRLV:
                    op->reg_dst_value = op->reg_src2_value >> op->reg_src1_value;
                    break;
                case SUBOP_SRA:
                    op->reg_dst_value = (int32_t)op->reg_src2_value >> op->shamt;
                    break;
                case SUBOP_SRAV:
                    op->reg_dst_value = (int32_t)op->reg_src2_value >> op->reg_src1_value;
                    break;
                case SUBOP_JR:
                case SUBOP_JALR:
                    op->reg_dst_value = op->pc + 4;
                    op->branch_dest = op->reg_src1_value;
                    op->branch_taken = 1;
                    break;

                case SUBOP_MULT:
                    {
                        /* we set a result value right away; however, we will
                         * model a stall if the program tries to read the value
                         * before it's ready (or overwrite HI/LO). Also, if
                         * another multiply comes down the pipe later, it will
                         * update the values and re-set the stall cycle count
                         * for a new operation.
                         */
                        int64_t val = (int64_t)((int32_t)op->reg_src1_value) * (int64_t)((int32_t)op->reg_src2_value);
                        uint64_t uval = (uint64_t)val;
                        pipe.HI = (uval >> 32) & 0xFFFFFFFF;
                        pipe.LO = (uval >>  0) & 0xFFFFFFFF;

                        /* four-cycle multiplier latency */
                        pipe.multiplier_stall = 4;
                    }
                    break;
                case SUBOP_MULTU:
                    {
                        uint64_t val = (uint64_t)op->reg_src1_value * (uint64_t)op->reg_src2_value;
                        pipe.HI = (val >> 32) & 0xFFFFFFFF;
                        pipe.LO = (val >>  0) & 0xFFFFFFFF;

                        /* four-cycle multiplier latency */
                        pipe.multiplier_stall = 4;
                    }
                    break;

                case SUBOP_DIV:
                    if (op->reg_src2_value != 0) {

                        int32_t val1 = (int32_t)op->reg_src1_value;
                        int32_t val2 = (int32_t)op->reg_src2_value;
                        int32_t div, mod;

                        div = val1 / val2;
                        mod = val1 % val2;

                        pipe.LO = div;
                        pipe.HI = mod;
                    } else {
                        // really this would be a div-by-0 exception
                        pipe.HI = pipe.LO = 0;
                    }

                    /* 32-cycle divider latency */
                    pipe.multiplier_stall = 32;
                    break;

                case SUBOP_DIVU:
                    if (op->reg_src2_value != 0) {
                        pipe.HI = (uint32_t)op->reg_src1_value % (uint32_t)op->reg_src2_value;
                        pipe.LO = (uint32_t)op->reg_src1_value / (uint32_t)op->reg_src2_value;
                    } else {
                        /* really this would be a div-by-0 exception */
                        pipe.HI = pipe.LO = 0;
                    }

                    /* 32-cycle divider latency */
                    pipe.multiplier_stall = 32;
                    break;

                case SUBOP_MFHI:
                    /* stall until value is ready */
                    if (pipe.multiplier_stall > 0)
                        return;

                    op->reg_dst_value = pipe.HI;
                    break;
                case SUBOP_MTHI:
                    /* stall to respect WAW dependence */
                    if (pipe.multiplier_stall > 0)
                        return;

                    pipe.HI = op->reg_src1_value;
                    break;

                case SUBOP_MFLO:
                    /* stall until value is ready */
                    if (pipe.multiplier_stall > 0)
                        return;

                    op->reg_dst_value = pipe.LO;
                    break;
                case SUBOP_MTLO:
                    /* stall to respect WAW dependence */
                    if (pipe.multiplier_stall > 0)
                        return;

                    pipe.LO = op->reg_src1_value;
                    break;

                case SUBOP_ADD:
                case SUBOP_ADDU:
                    op->reg_dst_value = op->reg_src1_value + op->reg_src2_value;
                    break;
                case SUBOP_SUB:
                case SUBOP_SUBU:
                    op->reg_dst_value = op->reg_src1_value - op->reg_src2_value;
                    break;
                case SUBOP_AND:
                    op->reg_dst_value = op->reg_src1_value & op->reg_src2_value;
                    break;
                case SUBOP_OR:
                    op->reg_dst_value = op->reg_src1_value | op->reg_src2_value;
                    break;
                case SUBOP_NOR:
                    op->reg_dst_value = ~(op->reg_src1_value | op->reg_src2_value);
                    break;
                case SUBOP_XOR:
                    op->reg_dst_value = op->reg_src1_value ^ op->reg_src2_value;
                    break;
                case SUBOP_SLT:
                    op->reg_dst_value = ((int32_t)op->reg_src1_value <
                            (int32_t)op->reg_src2_value) ? 1 : 0;
                    break;
                case SUBOP_SLTU:
                    op->reg_dst_value = (op->reg_src1_value < op->reg_src2_value) ? 1 : 0;
                    break;
            }
            break;

        case OP_BRSPEC:
            switch (op->subop) {
                case BROP_BLTZ:
                case BROP_BLTZAL:
                    if ((int32_t)op->reg_src1_value < 0) op->branch_taken = 1;
                    break;

                case BROP_BGEZ:
                case BROP_BGEZAL:
                    if ((int32_t)op->reg_src1_value >= 0) op->branch_taken = 1;
                    break;
            }
            break;

        case OP_BEQ:
            if (op->reg_src1_value == op->reg_src2_value) op->branch_taken = 1;
            break;

        case OP_BNE:
            if (op->reg_src1_value != op->reg_src2_value) op->branch_taken = 1;
            break;

        case OP_BLEZ:
            if ((int32_t)op->reg_src1_value <= 0) op->branch_taken = 1;
            break;

        case OP_BGTZ:
            if ((int32_t)op->reg_src1_value > 0) op->branch_taken = 1;
            break;

        case OP_ADDI:
        case OP_ADDIU:
            op->reg_dst_value_ready = 1;
            op->reg_dst_value = op->reg_src1_value + op->se_imm16;
            break;
        case OP_SLTI:
            op->reg_dst_value_ready = 1;
            op->reg_dst_value = (int32_t)op->reg_src1_value < (int32_t)op->se_imm16 ? 1 : 0;
            break;
        case OP_SLTIU:
            op->reg_dst_value_ready = 1;
            op->reg_dst_value = (uint32_t)op->reg_src1_value < (uint32_t)op->se_imm16 ? 1 : 0;
            break;
        case OP_ANDI:
            op->reg_dst_value_ready = 1;
            op->reg_dst_value = op->reg_src1_value & op->imm16;
            break;
        case OP_ORI:
            op->reg_dst_value_ready = 1;
            op->reg_dst_value = op->reg_src1_value | op->imm16;
            break;
        case OP_XORI:
            op->reg_dst_value_ready = 1;
            op->reg_dst_value = op->reg_src1_value ^ op->imm16;
            break;
        case OP_LUI:
            op->reg_dst_value_ready = 1;
            op->reg_dst_value = op->imm16 << 16;
            break;

        case OP_LW:
        case OP_LH:
        case OP_LHU:
        case OP_LB:
        case OP_LBU:
            op->mem_addr = op->reg_src1_value + op->se_imm16;
            break;

        case OP_SW:
        case OP_SH:
        case OP_SB:
            op->mem_addr = op->reg_src1_value + op->se_imm16;
            op->mem_value = op->reg_src2_value;
            break;
    }

    /* handle branch recoveries at this point */
    if ((op->is_branch == 1) && 
            ((op->predicted_dir != op->branch_taken) ||
         //    (op->predicted_dest != op->branch_dest) if direction is diff.
         //    dest are diff....my branch_dest is never a value of "not taken"
             ((btb[BTB_IDX(op->pc)].valid == 0) || 
              (btb[BTB_IDX(op->pc)].tag != op->pc)))){
        if (op->branch_taken == 1)
            pipe_recover(3, op->branch_dest);
        else 
            pipe_recover(3, op->pc + 4); 
    }
    if (op->is_branch == 1)
        update_branch_predictions(op->pc, op->branch_taken, op->branch_cond, op->branch_dest);
    /* remove from upstream stage and place in downstream stage */
    pipe.execute_op = NULL;
    pipe.mem_op = op;
}

void pipe_stage_decode()
{
    /* if downstream stall, return (and leave any input we had) */
    if (pipe.execute_op != NULL)
        return;

    /* if no op to decode, return */
    if (pipe.decode_op == NULL)
        return;
    
    /* grab op and remove from stage input */
    Pipe_Op *op = pipe.decode_op;
    pipe.decode_op = NULL;

    /* set up info fields (source/dest regs, immediate, jump dest) as necessary */
    uint32_t opcode = (op->instruction >> 26) & 0x3F;
    uint32_t rs = (op->instruction >> 21) & 0x1F;
    uint32_t rt = (op->instruction >> 16) & 0x1F;
    uint32_t rd = (op->instruction >> 11) & 0x1F;
    uint32_t shamt = (op->instruction >> 6) & 0x1F;
    uint32_t funct1 = (op->instruction >> 0) & 0x1F;
    uint32_t funct2 = (op->instruction >> 0) & 0x3F;
    uint32_t imm16 = (op->instruction >> 0) & 0xFFFF;
    uint32_t se_imm16 = imm16 | ((imm16 & 0x8000) ? 0xFFFF8000 : 0);
    uint32_t targ = (op->instruction & ((1UL << 26) - 1)) << 2;

    op->opcode = opcode;
    op->imm16 = imm16;
    op->se_imm16 = se_imm16;
    op->shamt = shamt;

    switch (opcode) {
        case OP_SPECIAL:
            /* all "SPECIAL" insts are R-types that use the ALU and both source
             * regs. Set up source regs and immediate value. */
            op->reg_src1 = rs;
            op->reg_src2 = rt;
            op->reg_dst = rd;
            op->subop = funct2;
            if (funct2 == SUBOP_SYSCALL) {
                op->reg_src1 = 2; // v0
                op->reg_src2 = 3; // v1
            }
            if (funct2 == SUBOP_JR || funct2 == SUBOP_JALR) {
                op->is_branch = 1;
                op->branch_cond = 0;
            }

            break;

        case OP_BRSPEC:
            /* branches that have -and-link variants come here */
            op->is_branch = 1;
            op->reg_src1 = rs;
            op->is_branch = 1;
            op->branch_cond = 1; /* conditional branch */
            op->branch_dest = op->pc + 4 + (se_imm16 << 2);
            op->subop = rt;
            if (rt == BROP_BLTZAL || rt == BROP_BGEZAL) {
                /* link reg */
                op->reg_dst = 31;
                op->reg_dst_value = op->pc + 4;
                op->reg_dst_value_ready = 1;
            }
            break;

        case OP_JAL:
            op->reg_dst = 31;
            op->reg_dst_value = op->pc + 4;
            op->reg_dst_value_ready = 1;
            op->branch_taken = 1;
            /* fallthrough */
        case OP_J:
            op->is_branch = 1;
            op->branch_cond = 0;
            op->branch_taken = 1;
            op->branch_dest = (op->pc & 0xF0000000) | targ;
            break;

        case OP_BEQ:
        case OP_BNE:
        case OP_BLEZ:
        case OP_BGTZ:
            /* ordinary conditional branches (resolved after execute) */
            op->is_branch = 1;
            op->branch_cond = 1;
            op->branch_dest = op->pc + 4 + (se_imm16 << 2);
            op->reg_src1 = rs;
            op->reg_src2 = rt;
            break;

        case OP_ADDI:
        case OP_ADDIU:
        case OP_SLTI:
        case OP_SLTIU:
            /* I-type ALU ops with sign-extended immediates */
            op->reg_src1 = rs;
            op->reg_dst = rt;
            break;

        case OP_ANDI:
        case OP_ORI:
        case OP_XORI:
        case OP_LUI:
            /* I-type ALU ops with non-sign-extended immediates */
            op->reg_src1 = rs;
            op->reg_dst = rt;
            break;

        case OP_LW:
        case OP_LH:
        case OP_LHU:
        case OP_LB:
        case OP_LBU:
        case OP_SW:
        case OP_SH:
        case OP_SB:
            /* memory ops */
            op->is_mem = 1;
            op->reg_src1 = rs;
            if (opcode == OP_LW || opcode == OP_LH || opcode == OP_LHU || opcode == OP_LB || opcode == OP_LBU) {
                /* load */
                op->mem_write = 0;
                op->reg_dst = rt;
            }
            else {
                /* store */
                op->mem_write = 1;
                op->reg_src2 = rt;
            }
            break;
    }
    /* we will handle reg-read together with bypass in the execute stage */

    /* place op in downstream slot */
    pipe.execute_op = op;
}

void pipe_stage_fetch()
{   
    uint32_t branch,opcode;
    /* if pipeline is stalled (our output slot is not empty), return */
    if (pipe.decode_op != NULL)
        return;
    else if (RUN_BIT == 0){
        pipe.PC += 4;
        return;
    }

    /* Allocate an op and send it down the pipeline. */
    Pipe_Op *op = malloc(sizeof(Pipe_Op));
    memset(op, 0, sizeof(Pipe_Op));
    op->reg_src1 = op->reg_src2 = op->reg_dst = -1;

    op->instruction = cache_read(pipe.PC, 1);
    if(op->instruction == -1)
        return;
    op->pc = pipe.PC;
    
    //predict PC
    opcode = (op->instruction >> 26) & 0x3F;
    branch = (opcode == OP_BRSPEC || opcode == OP_JAL || opcode == OP_J ||
              opcode == OP_BEQ || opcode == OP_BNE || opcode == OP_BLEZ || 
              opcode == OP_BGTZ);

    if(branch == 1){
        op->predicted_dir = prediction(pipe.PC);
        op->predicted_dest = pipe.predicted_dest;
       
        pipe.PC = op->predicted_dest;
        pipe.decode_op = op;
        stat_inst_fetch++;
        return;
    }

    /* update PC if not predicted*/
    pipe.PC += 4;
    pipe.decode_op = op;
    stat_inst_fetch++;
}

void cache_init()
{
    int i,j;
    for(i = 0; i<64; i++)
        for(j = 0; j<4; j++){
            instCache[i][j].valid = 0;
            instCache[i][j].tag = 0;
            instCache[i][j].R = 0;
        }

    for(i = 0; i<256; i++)
        for(j = 0; j<8; j++){
            dataCache[i][j].valid = 0;
            dataCache[i][j].tag = 0;
            dataCache[i][j].R = 0;
        }

    for(i = 0; i<512; i++)
        for(j = 0; j<16; j++){
            L2[i][j].valid = 0; 
            L2[i][j].tag = 0; 
            L2[i][j].R = 0; 
        }
    
    for(j = 0; j<16; j++){
        MSHR[j].valid = 0;
        MSHR[j].cacheAddr = 0;
        MSHR[j].done = 0;
        memQ[j].valid = 0;
        memQ[j].done = 0;
        memQ[j].stallTime = 0;
    }

    for(j = 0; j<8; j++){
        row_buf[j] = -1;
        pipe.DRAMbusy[j] = 0;
    }

    pipe.comLineBusy = 0;
    pipe.addrLineBusy = 0;
    pipe.dataLineBusy = 0;
    pipe.curr_mshr = 0;
    pipe.Istayout = 0;
    pipe.Dstayout = 0;
    pipe.mem_request = 0;
    pipe.cDone1 = 0;
    pipe.cDone2 = 0;
    pipe.aDone = 0;
    pipe.dDone1 =0;
    pipe.dDone2 =0;
    pipe.dDone3 =0;
    pipe.datDone = 0;
}

uint32_t cache_read(uint32_t addr, uint8_t isInst)
{    
    int i;
    uint32_t memory [8];
    uint8_t set = (isInst == 1) ? ((addr>>5)  & 0x3F) : ((addr>>5) & 0xFF);
    uint32_t tag = (isInst == 1) ? ((addr>>11) & 0x1FFFFF) : ((addr>>13) & 0x7FFFF);
    
    uint8_t lru_entry = 0;
    int evict = 0;

    //Check to see if in cache
    if(isInst == 1){
        for(i=0; i<4; i++){
            if ((instCache[set][i].valid==1) && (instCache[set][i].tag == tag)){
                cache_updateLRU(set,i,isInst);
                return  mem_read_32(addr);
            }
        }
    }
    else{
         for(i=0; i<8; i++){
            if ((dataCache[set][i].valid==1) && (dataCache[set][i].tag == tag)){
                cache_updateLRU(set,i,isInst);
                return   mem_read_32(addr);
            }
       }
    }
        
    //If NOT FOUND, request it from L2
    L2_request(addr, isInst);
    if ((pipe.stall_inst == 1) || (pipe.stall_data == 1)
        || (pipe.Dstayout == 1) || (pipe.Istayout == 1))
        return -1;

    //Then, stall
    //Load new memory into cache
    if(isInst == 1){
        for(i=0;i<4;i++){
            if(instCache[set][i].valid == 0){
                cache_updateLRU(set,i,isInst);
                instCache[set][i].valid = 1;
                instCache[set][i].tag = tag;
                return mem_read_32(addr); 
            }
            else{
                if (instCache[set][i].R > lru_entry){
                    lru_entry = instCache[set][i].R;
                    evict = i;
                }
            }
        }
        //If it does not fit, EVICT least-recetly-used entry
        cache_updateLRU(set,evict,isInst);
        instCache[set][evict].valid = 1;
        instCache[set][evict].tag = tag;
        return mem_read_32(addr); 
    }
    else{
        for(i=0;i<8;i++){
            if(dataCache[set][i].valid == 0){
                cache_updateLRU(set,i,isInst);
                dataCache[set][i].valid = 1;
                dataCache[set][i].tag = tag;
                return mem_read_32(addr); 
            }
            else{
                if (dataCache[set][i].R > lru_entry){
                    lru_entry = dataCache[set][i].R;
                    evict = i;
                }
            }
        }
        //If it does not fit, EVICT least-recetly-used entry
        cache_updateLRU(set,evict,isInst);
        dataCache[set][evict].valid = 1;
        dataCache[set][evict].tag = tag;
        return mem_read_32(addr); 
    }
    return -1;
}

void L2_request(uint32_t addr, uint8_t isInst){
    uint8_t L2_set = (addr >> 5) & 0x1FF;
    uint32_t L2_tag = (addr >> 14) & 0x3FFFF;
    uint8_t lru_entry = 0;
    int i, evict = 0;
    
   // if(empty_mshr == 15){
     //   return;
    //}

#ifdef DEBUG
            printf("L2 requested");
#endif
    //Search for hit
    for(i=0; i<16; i++){
        if ((L2[L2_set][i].valid == 1) && (L2[L2_set][i].tag == L2_tag)){
#ifdef DEBUG
            printf("L2 Hit! @ set=%d, tag=0x%08x,\n",L2_set, L2_tag);
#endif
            stall_pipe(15, isInst);
            if((pipe.stall_inst == 1) || (pipe.stall_data == 1))
                return;
            cache_updateLRU(L2_set,i,2);
            return;
        }
    }
    
#ifdef DEBUG
            printf("L2 Missed!\n");
#endif
    if(((pipe.mem_ibusy==0) && (isInst==1) &&
        (pipe.insertingI==0)) ||
        ((pipe.mem_dbusy==0) && (isInst==0) && 
        (pipe.insertingD==0))){
        
#ifdef DEBUG
            printf("No other stalls due to DRAM for same cache type...so create\
inst=%d,MSHR\n", isInst);
#endif

        //Stall for Memory Controller Sim.
        pipe.mem_request = 1;
        stall_pipe(5, isInst);
        if((pipe.stall_inst == 1) || (pipe.stall_data == 1))
            return;
        pipe.mem_request = 0;
        //Send Memory Request
        for(i=0; i<16; i++){
            if(memQ[i].valid == 0){
                memQ[i].addr = addr;
                memQ[i].rank = i;
                memQ[i].row = (addr>>16) & 0xFFFF;
                memQ[i].bank = (addr>>5) & 0x7;
                memQ[i].isInst = isInst;
                memQ[i].valid = 1;
                memQ[i].done = 0;
                break;
            }
        }

        //If a Miss, allocate MSHR
        MSHR[empty_mshr].valid = 1;
        MSHR[empty_mshr].cacheAddr = addr;
        MSHR[empty_mshr].done = 0;
        empty_mshr = (empty_mshr == 15) ? 0 : (empty_mshr + 1);
        
    }
    
    while((MSHR[pipe.curr_mshr].valid == 1) &&
           (MSHR[pipe.curr_mshr].done == 0)){
        
        //prevent from another miss to disrupt count of other miss
       /* if((pipe.stall_inst==1) && (isInst==0)){
            pipe.Dstayout=1;
            return;
        }else if((pipe.stall_data==1) && (isInst==1)){
            pipe.Istayout=1;
            return;
        }
        */

        //set values to stall
#ifdef DEBUG
            printf("Waiting for MSHR to clear in DRAM\n");
#endif

        if (isInst == 0){ 
#ifdef DEBUG
            printf("It is a data cache MSHR\n");
#endif
            if (pipe.stall_inst == 0){
                pipe.mem_dbusy = checkDRAM(isInst);
                pipe.stall_data = pipe.mem_dbusy;
            }
            else{
#ifdef DEBUG
            printf("Told to STAYOUT!\n");
#endif
                pipe.Dstayout=1;
                return;
            }
        }
        else if (isInst == 1){ 
#ifdef DEBUG
            printf("It is a instr cache MSHR\n");
#endif
            if (pipe.stall_data == 0){
                pipe.mem_ibusy = checkDRAM(isInst);
                pipe.stall_inst = pipe.mem_ibusy;
            }
            else{
#ifdef DEBUG
            printf("Told to STAYOUT!\n");
#endif
                pipe.Istayout=1 ;
                return;
            }
        }

        if((pipe.stall_inst == 1) || (pipe.stall_data == 1))
            return;
          
#ifdef DEBUG
            printf("DRAM Request forfilled!\n");
#endif
        //clean up possibily set values
        if (pipe.mem_dbusy == 0) pipe.Istayout = 0;
        if (pipe.mem_ibusy == 0) pipe.Dstayout = 0;
    }

    if((MSHR[pipe.curr_mshr].valid == 1) &&
            (MSHR[pipe.curr_mshr].done == 1)){
        
#ifdef DEBUG
            printf("Removing completed MSHR\n");
       // printf("make it here\n");
#endif
        if(isInst == 0)
            pipe.insertingD = 1;
        else    
            pipe.insertingI = 1;

        stall_pipe(5, isInst);
        if((pipe.stall_inst == 1) || (pipe.stall_data == 1))
            return;
    //    pipe.mem_request = 0;
        
        MSHR[pipe.curr_mshr].valid = 0;
        MSHR[pipe.curr_mshr].done = 0;
        pipe.curr_mshr = (pipe.curr_mshr == 15) ? 0 : (pipe.curr_mshr + 1);

        if(isInst == 0)
            pipe.insertingD = 0;
        else    
            pipe.insertingI = 0;
        
#ifdef DEBUG
            printf("L2 INSERT! @ set=%d, tag=0x%08x,\n",L2_set, L2_tag);
#endif

        for(i=0;i<16;i++){
            if(L2[L2_set][i].valid == 0){
                cache_updateLRU(L2_set,i,2);
                L2[L2_set][i].valid = 1;
                L2[L2_set][i].tag = L2_tag;
                return; 
            }
            else{
                if (L2[L2_set][i].R > lru_entry){
                    lru_entry = L2[L2_set][i].R;
                    evict = i;
                }
            }
        }
        //If it does not fit, EVICT least-recetly-used entry
        cache_updateLRU(L2_set,evict,2);
        L2[L2_set][evict].valid = 1;
        L2[L2_set][evict].tag = L2_tag;
        return; 
    }   
}

int checkDRAM(uint8_t isInst){
    uint32_t remove = -1;
    int i,j = 0;
#ifdef DEBUG
/*    for(j=0; j<5;j++)
         printf("memQ[%d].valid,done,bank,row = %d,%d,%d,%d\n",j, memQ[j].valid,
         memQ[j].done,memQ[j].bank, memQ[j].row);      
  */  //printf("mem[0].valid.done = %d,%d \n", memQ[0].valid, memQ[0].done);
#endif

    if(((memQ[0].valid == 1) && (memQ[0].done == 1))
        || memQ[0].valid == 0)
        return 0;
    //check row
    //queue ComLine
    //queue DRAM
    //queue DataLine
    //check next on queue for queue-ability

    //case 1
    if (row_buf[memQ[0].bank] == -1){
        qComLine(0,pipe.cDone1,pipe.cDone2);
        //if((pipe.comLineBusy != 0) && (cDone1 != 0))
        //   return 1;
        if(pipe.comLineBusy == 0)
            pipe.cDone1 = 1;

        qDRAM(memQ[0].bank,0,pipe.dDone1,pipe.dDone2,pipe.dDone3);
        if((pipe.DRAMbusy[memQ[0].bank] != 0) && (pipe.dDone1 == 0))
            return 1;
        pipe.dDone1 = 1;

        qAddrLine(pipe.aDone);
        //if((pipe.addrLineBusy != 0))
        //   return 1;
        if(pipe.addrLineBusy == 0)
            pipe.aDone = 1;

        qDRAM(memQ[0].bank,1,pipe.dDone1,pipe.dDone2,pipe.dDone3);
        if((pipe.DRAMbusy[memQ[0].bank] != 0) && (pipe.dDone2 == 0))
            return 1;
        pipe.dDone2 = 1;

        qDataLine(pipe.datDone); 
        if((pipe.dataLineBusy != 0))
            return 1;
        pipe.datDone = 1;

        //update row buffer
        row_buf[memQ[0].bank] = memQ[0].row;
        //update memQ
        memQ[0].done = 1;
        //update MSHR
        for(j=0;j<16;j++){
            if((MSHR[j].valid == 1) && (MSHR[j].cacheAddr == memQ[0].addr)){
                MSHR[j].done=1;
            }
        }
        remove = 0;

        pipe.cDone1 = 0;
        pipe.dDone1 = 0;
        pipe.dDone2 = 0;
        pipe.aDone = 0;
        pipe.datDone = 0;
    }
    else{
        //case 2
        //search the memQ for values equal to row buf
        for (i=0; i<16; i++){
            if((row_buf[memQ[i].bank] == memQ[i].row) && (memQ[i].valid == 1)){

                if(schedulable(memQ[i].bank) == 0){
                    remove = -2;
                    continue;
                }

                qAddrLine(pipe.aDone);
                //        if((pipe.addrLineBusy != 0))
                //          return 1;
                if(pipe.addrLineBusy == 0)
                    pipe.aDone = 1;

                qDRAM(memQ[i].bank,0,pipe.dDone1,pipe.dDone2,pipe.dDone3);
                if((pipe.DRAMbusy[memQ[i].bank] != 0) && (pipe.dDone1 == 0))
                    return 1;
                pipe.dDone1 = 1;

                qDataLine(pipe.datDone); 
                if((pipe.dataLineBusy != 0))
                    return 1;
                pipe.datDone = 1;

                memQ[i].done = 1;
                for(j=0;j<16;j++)
                    if(MSHR[j].cacheAddr == memQ[i].addr)
                        MSHR[j].done=1;
                remove = i;
                
                pipe.aDone = 0;
                pipe.dDone1 = 0;
                pipe.datDone = 0;
                // break;
            }
        }
    }
    //case3
    //misses for all memQ requests
    if(remove == -1){
        qComLine(0,pipe.cDone1,pipe.cDone2);
        //if((pipe.comLineBusy != 0) && (cDone1 != 0))
        //    return 1;
        if(pipe.comLineBusy == 0)
            pipe.cDone1 = 1;

        qDRAM(memQ[0].bank,0,pipe.dDone1,pipe.dDone2,pipe.dDone3);
        if((pipe.DRAMbusy[0] != 0) && (pipe.dDone1 == 0))
            return 1;
        pipe.dDone1 = 1;

        qComLine(1,pipe.cDone1,pipe.cDone2);
        //if((pipe.comLineBusy != 0) && (cDone2 !=0))
        //    return 1;
        if(pipe.comLineBusy == 0)
            pipe.cDone2 = 1;

        qDRAM(memQ[0].bank,1,pipe.dDone1,pipe.dDone2,pipe.dDone3);
        if((pipe.DRAMbusy[0] != 0) && (pipe.dDone2 == 0))
            return 1;
        pipe.dDone2 = 1;

        qAddrLine(pipe.aDone);
        //if((pipe.addrLineBusy != 0))
        //    return 1;
        if(pipe.addrLineBusy == 0)
            pipe.aDone = 1;

        qDRAM(memQ[0].bank,2,pipe.dDone1,pipe.dDone2,pipe.dDone3);
        if((pipe.DRAMbusy[0] != 0) && (pipe.dDone3 == 0))
            return 1;
        pipe.dDone3 = 1;

        qDataLine(pipe.datDone); 
        if((pipe.dataLineBusy != 0))
            return 1;
        pipe.datDone = 1;

        row_buf[memQ[0].bank] = memQ[0].row;
        memQ[0].done = 1;
        for(j=0;j<16;j++)
            if(MSHR[j].cacheAddr == memQ[0].addr)
                MSHR[j].done=1;
        remove = 0;
        
        pipe.cDone1 = 0;
        pipe.cDone2 = 0;
        pipe.dDone1 = 0;
        pipe.dDone2 = 0;
        pipe.dDone3 = 0;
        pipe.aDone = 0;
        pipe.datDone = 0;
    }

    //reorganize memQ
    for (i = remove;i<14;i++)
        memQ[i] = memQ[i+1];
    memQ[15].valid = 0;
    return 0;
}

void qComLine(uint8_t type, uint8_t done1, uint8_t done2){
   if(((type==0)&&(done1==1))||
      ((type==1)&&(done2==1)))
       return;

   pipe.comLineBusy+=1;
   if (pipe.comLineBusy == 5)
       pipe.comLineBusy = 0;
}

void qAddrLine(uint8_t done){
   if (done == 1)
       return;

   pipe.addrLineBusy+=1;
   if (pipe.addrLineBusy == 5)
       pipe.addrLineBusy = 0;
}

void qDRAM(uint8_t bank, uint8_t type, uint8_t done1, uint8_t done2,
                         uint8_t done3){
    
   if(((type==0)&&(done1==1))||
      ((type==1)&&(done2==1))||
      ((type==2)&&(done3==1)))
       return;
   
   pipe.DRAMbusy[bank]+=1;
   if (pipe.DRAMbusy[bank] == 101)
       pipe.DRAMbusy[bank] = 0;
}

void qDataLine(uint8_t done){ 
        pipe.cDone1 = 0;
   if (done == 1)
       return;

   pipe.dataLineBusy+=1;
   if (pipe.dataLineBusy == 51)
       pipe.dataLineBusy = 0;
}

uint8_t schedulable(uint8_t bank){

    if((pipe.comLineBusy == 0) && 
        ((pipe.DRAMbusy[bank] == 0) || (pipe.DRAMbusy[bank] >= 96)) &&
        ((pipe.addrLineBusy == 0) &&
        ((pipe.dataLineBusy == 0) || (pipe.dataLineBusy >= 46))))
        return 1;

    return 0;
}

void stall_pipe(int stall_time, int8_t isInst){
     if(isInst == 1){
        if(instr_stall_count < stall_time){     
            pipe.stall_inst = 1;
            instr_stall_count++; 
            return;
        }
        else{
            pipe.stall_inst = 0;
            instr_stall_count = 0;
        }
    }
    else { 
        if(data_stall_count < stall_time){     
            pipe.stall_data = 1;
            data_stall_count++; 
            return;
        }
        else{
            pipe.stall_data = 0;
            data_stall_count = 0;
        }
    }
}

void cache_updateLRU(uint8_t set, int newHit, uint8_t isInst)
{
    int i; 
    if(isInst == 1){
        for(i=0; i<4; i++){
            if((instCache[set][i].R != 0) && (i != newHit)
                 && (instCache[set][i].R <= instCache[set][newHit].R))
                instCache[set][i].R++;
        }     
        instCache[set][newHit].R = 1;
    }
    else if (isInst == 0){ 
         for(i=0; i<8; i++){
            if((dataCache[set][i].R != 0) && (i != newHit)
                 && (dataCache[set][i].R <= dataCache[set][newHit].R))
                dataCache[set][i].R++;
        }
        dataCache[set][newHit].R = 1;
    }
    else if (isInst == 2){ 
         for(i=0; i<16; i++){
            if((L2[set][i].R != 0) && (i != newHit)
                 && (L2[set][i].R <= L2[set][newHit].R))
                L2[set][i].R++;
        }
        L2[set][newHit].R = 1;
    }
}

void cache_write(uint32_t addr, uint32_t data)
{
    int i;
    uint32_t memory [8];
    uint8_t set = (addr>>5) & 0xFF;
    uint32_t tag = (addr>>13) & 0x7FFFF;
    uint8_t lru_entry = 0;
    int evict = 0;
    
    //If data tag is already in cache
    for(i=0;i<8;i++){
        if ((dataCache[set][i].valid == 1) && (dataCache[set][i].tag == tag)){
                cache_updateLRU(set,i,0);
                mem_write_32(addr, data);
                return;
            }       
    }
    
    //If not in Cache
    //If NOT FOUND, stall
    L2_request(addr, 0);
    if ((pipe.stall_inst == 1) || (pipe.stall_data == 1)
        || (pipe.Dstayout == 1) || (pipe.Istayout == 1))
        return;
    
    //search for free location to store memory in 
    //and then adjust memory at this location
    for(i=0;i<8;i++){
        if(dataCache[set][i].valid == 0){
            cache_updateLRU(set,i,0); 
            dataCache[set][i].valid = 1;
            dataCache[set][i].tag = tag;
            mem_write_32(addr, data);
            return;
        }
        else{
            if (dataCache[set][i].R > lru_entry){
                lru_entry = dataCache[set][i].R;
                evict = i;
            }
        }
    }
    
    //If it does not fit, EVICT least-recetly-used entry
    cache_updateLRU(set,evict,0);
    dataCache[set][evict].valid = 1;
    dataCache[set][evict].tag = tag;
    mem_write_32(addr, data);
}

uint32_t prediction(uint32_t branch_pc)
{

    if((btb[BTB_IDX(branch_pc)].tag != branch_pc) || 
            (btb[BTB_IDX(branch_pc)].valid == 0)){
        pipe.predicted_dest = branch_pc + 4;
        return 0;

    }
    else if((btb[BTB_IDX(branch_pc)].tag == branch_pc) &&
            (btb[BTB_IDX(branch_pc)].valid == 1)){
        if ((btb[BTB_IDX(branch_pc)].U == 1) ||
                (pht[GSHARE(ghr,branch_pc)] > 1)){
            pipe.predicted_dest = btb[BTB_IDX(branch_pc)].target;
            return 1;
        }
    }

    pipe.predicted_dest = branch_pc + 4;
    return 0;
}

void update_branch_predictions(uint32_t branch_pc, uint8_t taken, 
                                uint8_t cond, uint32_t target)
{
    if(cond==1){

        //update pht
        if((taken==1) && (pht[GSHARE(ghr,branch_pc)] != 3))
            pht[GSHARE(ghr,branch_pc)] += 1;
        else if ((taken==0) && (pht[GSHARE(ghr,branch_pc)] != 0))
            pht[GSHARE(ghr,branch_pc)] -= 1;
        
        //update ghr
        ghr = (ghr<<1)|taken;
    }

    //update btb
    btb[BTB_IDX(branch_pc)].valid = 1;
    btb[BTB_IDX(branch_pc)].tag = branch_pc;
    btb[BTB_IDX(branch_pc)].U = (cond==1) ? 0:1;
    btb[BTB_IDX(branch_pc)].target = target;
}
