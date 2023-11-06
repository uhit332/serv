`default_nettype none
module serv_decode_compressed
  #(parameter [0:0] PRE_REGISTER = 1,
    parameter [0:0] MDU = 0,
    parameter       CHECK_VALIDITY = 1
  )
  (
   input wire        clk,
   //Input
   input wire [15:0] i_wb_rdt,
   input wire        i_wb_en,
   //To state
   output reg       o_sh_right,
   output reg       o_bne_or_bge,
   output reg       o_cond_branch,
   output reg       o_e_op,
   output reg       o_ebreak,
   output reg       o_branch_op,
   output reg       o_shift_op,
   output reg       o_slt_or_branch,
   output reg       o_rd_op,
   output reg       o_two_stage_op,
   output reg       o_dbus_en,
   //MDU
   output reg       o_mdu_op,
   //To bufreg
   output reg       o_bufreg_rs1_en,
   output reg       o_bufreg_imm_en,
   output reg       o_bufreg_clr_lsb,
   output reg       o_bufreg_sh_signed,
   //To ctrl
   output reg       o_ctrl_jal_or_jalr,
   output reg       o_ctrl_utype,
   output reg       o_ctrl_pc_rel,
   output reg       o_ctrl_mret,
   //To alu
   output reg       o_alu_sub,
   output reg [1:0] o_alu_bool_op,
   output reg       o_alu_cmp_eq,
   output reg       o_alu_cmp_sig,
   output reg [2:0] o_alu_rd_sel,
   //To mem IF
   output reg       o_mem_signed,
   output reg       o_mem_word,
   output reg       o_mem_half,
   output reg       o_mem_cmd,
   //To CSR
   output reg       o_csr_en,
   output reg [1:0] o_csr_addr,
   output reg       o_csr_mstatus_en,
   output reg       o_csr_mie_en,
   output reg       o_csr_mcause_en,
   output reg [1:0] o_csr_source,
   output reg       o_csr_d_sel,
   output reg       o_csr_imm_en,
   output reg       o_mtval_pc,
   //To top
   output reg       o_op_b_source,
   //To RF IF
   output reg       o_rd_mem_en,
   output reg       o_rd_csr_en,
   output reg       o_rd_alu_en
   //output wire      o_valid_instruction
   );

   reg [15:0] whole;
   reg [1:0] quadrant;
   reg [2:0] funct3;
   reg [2:0] funct3l;
   reg        op20;
   reg        op21;
   reg        op22;
   reg        op26;

   reg       imm25;
   reg       imm30;

   

   // coded from this table https://github.com/riscv/riscv-opcodes
   wire co_valid_instruction = 
     !CHECK_VALIDITY ||
       whole != 0 && (
         quadrant == 2'b00 && (funct3 == 3'b010 || funct3 == 3'b110) ||
	 quadrant == 2'b01 && !(funct3 == 3'b100 && funct3l == 3'b111) ||
	 quadrant == 2'b10 && (funct3 != 3'b001 && funct3 != 3'b011 && funct3 != 3'b101 && funct3 != 3'b111)
       )
    ;
 
   wire co_mdu_op     = 0;

   wire co_two_stage_op =
        quadrant == 2'b10 && funct3 != 3'b100 ||
	quadrant == 2'b01 && (funct3 == 3'b111 || funct3 == 3'b110 || funct3 == 3'b100 && !funct3l[1]) ||
	quadrant == 2'b00
   ;

   wire co_shift_op = quadrant == 2'b10 && funct3 == 3'b100 && funct3l[1] || quadrant == 2'b10 && funct3 == 3'b000;
   wire co_slt_or_branch = quadrant == 2'b01 && funct3[2:1] == 2'b11;
   wire co_branch_op = co_slt_or_branch;


   wire co_dbus_en    = (quadrant == 2'b00 || quadrant == 2'b10) && (funct3 == 3'b010 || funct3 == 3'b110);
   wire co_mtval_pc   = 0;    
   wire co_mem_word   = 1;
   wire co_rd_alu_en  = !co_dbus_en && !co_branch_op; 
   wire co_rd_mem_en  = 0; // todo: what is this 

   //jal,branch = imm
   //jalr,jr    = rs1
   //mem        = rs1+imm
   //shift      = rs1
   wire co_bufreg_rs1_en = quadrant == 2'b00 || (quadrant == 2'b01 || quadrant == 2'b11) && 
	   (funct3 != 3'b000 && funct3 != 3'b001 && funct3 != 3'b101);
   wire co_bufreg_imm_en = quadrant == 2'b00 || (quadrant == 2'b01 || quadrant == 2'b11) && (funct3 != 3'b100 && funct3 != 3'b000);

   // it is not enough to clear just one lsb for the compressed instructions,
   // cause all loads and stores are aligned, so it must be done in the immdec
   wire co_bufreg_clr_lsb = 0;

   //Conditional branch
   //True for BRANCH
   //False for JAL/JALR
   wire co_cond_branch = funct3[2:1] == 2'b11;

   wire co_ctrl_utype       = quadrant == 2'b01 && funct3 == 3'b011; // lui only
   wire co_ctrl_jal_or_jalr = !co_ctrl_utype; 

   //PC-relative operations
   //True for jal, j, b* 
   //False for jr, jalr, lui, ebreak
   wire co_ctrl_pc_rel = funct3 != 3'b100 && funct3 != 3'b011; 
   
   //Write to RD
   //True for OP-IMM, AUIPC, OP, LUI, SYSTEM, JALR, JAL, LOAD
   //False for STORE, BRANCH, J, JR
   wire co_rd_op = !(quadrant == 2'b00 && funct3 == 3'b110 || 
	   quadrant == 2'b10 && (funct3 == 3'b110 || funct3 == 3'b000 && funct3l[2] == 0) || 
	   quadrant == 2'b01 && funct3 == 3'b101
	   ;

   //
   //funct3
   //

   wire co_sh_right   = quadrant == 2'b10;
   wire co_bne_or_bge = funct3[0];

   // ebreak is the only compressed e_op
   wire co_ebreak = 1;


   //opcode & funct3 & op21

   wire co_ctrl_mret = 0;
   //Matches system opcodes except CSR accesses = compressed ebreak
   wire co_e_op = i_wb_rdt == 16'h9002;

`ifdef RISCV_FORMAL
   /*
    SERV makes no guarantees to handle illegal instructions
    Instead of adding more complexity to the decoder, we add
    these assumptions ensure that no illegal instructions
    generated by riscv-formal
    are identified as mret or e_op instructions.
    */
   always @(posedge clk) begin
      assume(co_valid_instruction == 1'b1);
   end
`endif

   //opcode & funct3 & imm30

   wire co_bufreg_sh_signed = quadrant == 2'b01 && funct3 == 3'b100 && funct3l[1:0] == 2'b01;

   /*
    True for sub, b*, slt*
    False for add*
    op    opcode f3  i30
    b*    11000  xxx x   t
    addi  00100  000 x   f
    slt*  0x100  01x x   t
    add   01100  000 0   f
    sub   01100  000 1   t
    */
   wire co_alu_sub = quadrant == 2'b01 && funct3[2] && (funct3 != 3'b100 || funct3l != 3'b111 || funct2 != 2'b01);


   wire co_alu_cmp_eq = 1;

   wire co_alu_cmp_sig = 0; // only used for result_lt, which is not used for compressed

   // actually the dbus_we = separates stores from loads 
   wire co_mem_cmd  = funct3[2];
   wire co_mem_signed = 0;
   wire co_mem_half   = 0;

   wire co_3reg_alu =  quadrant == 2'b01 && funct3 == 3'b100 && funct3l[1:0] == 2'b11; 
   wire [1:0] co_alu_bool_op = (!co_3reg_alu) ? 2'b01 : (funct2 == 2'b01) ? : 2'b00 : funct2;

   wire [2:0] co_alu_rd_sel;
   assign co_alu_rd_sel[0] = !co_shift_op; // Add/sub, but really anything not shift
   assign co_alu_rd_sel[1] = 0; //SLT*
   assign co_alu_rd_sel[2] = co_3reg_alu && func3l == 3'b011 && funct2 != 0; //Bool

   //0 (OP_B_SOURCE_IMM) when OPIMM
   //1 (OP_B_SOURCE_RS2) when OP
   // TODO: must be a third option to load 0 for a compressed branch
   wire co_op_b_source = co_3reg_alu;

   generate
      if (PRE_REGISTER) begin

         always @(posedge clk) begin
            if (i_wb_en) begin
	       whole  <= i_wb_rdt;
	       funct7 <= i_wb_rdt[31:25];
               funct3 <= i_wb_rdt[14:12];
               imm30  <= i_wb_rdt[30];
               imm25  <= i_wb_rdt[25];
               opcode <= i_wb_rdt[6:2];
               op20   <= i_wb_rdt[20];
               op21   <= i_wb_rdt[21];
               op22   <= i_wb_rdt[22];
               op26   <= i_wb_rdt[26];
            end
         end

         always @(*) begin
            o_sh_right         = co_sh_right;
            o_bne_or_bge       = co_bne_or_bge;
            o_cond_branch      = co_cond_branch;
            o_dbus_en          = co_dbus_en;
            o_mtval_pc         = co_mtval_pc;
	    o_two_stage_op     = co_two_stage_op;
            o_e_op             = co_e_op;
            o_ebreak           = co_ebreak;
            o_branch_op        = co_branch_op;
            o_shift_op         = co_shift_op;
            o_slt_or_branch    = co_slt_or_branch;
            o_rd_op            = co_rd_op;
            o_mdu_op           = co_mdu_op;
            o_ext_funct3       = co_ext_funct3;
            o_bufreg_rs1_en    = co_bufreg_rs1_en;
            o_bufreg_imm_en    = co_bufreg_imm_en;
            o_bufreg_clr_lsb   = co_bufreg_clr_lsb;
            o_bufreg_sh_signed = co_bufreg_sh_signed;
            o_ctrl_jal_or_jalr = co_ctrl_jal_or_jalr;
            o_ctrl_utype       = co_ctrl_utype;
            o_ctrl_pc_rel      = co_ctrl_pc_rel;
            o_ctrl_mret        = co_ctrl_mret;
            o_alu_sub          = co_alu_sub;
            o_alu_bool_op      = co_alu_bool_op;
            o_alu_cmp_eq       = co_alu_cmp_eq;
            o_alu_cmp_sig      = co_alu_cmp_sig;
            o_alu_rd_sel       = co_alu_rd_sel;
            o_mem_signed       = co_mem_signed;
            o_mem_word         = co_mem_word;
            o_mem_half         = co_mem_half;
            o_mem_cmd          = co_mem_cmd;
            o_csr_en           = co_csr_en;
            o_csr_addr         = co_csr_addr;
            o_csr_mstatus_en   = co_csr_mstatus_en;
            o_csr_mie_en       = co_csr_mie_en;
            o_csr_mcause_en    = co_csr_mcause_en;
            o_csr_source       = co_csr_source;
            o_csr_d_sel        = co_csr_d_sel;
            o_csr_imm_en       = co_csr_imm_en;
            o_immdec_ctrl      = co_immdec_ctrl;
            o_immdec_en        = co_immdec_en;
            o_op_b_source      = co_op_b_source;
            o_rd_csr_en        = co_rd_csr_en;
            o_rd_alu_en        = co_rd_alu_en;
            o_rd_mem_en        = co_rd_mem_en;
	    //o_valid_instruction= co_valid_instruction;
         end

      end else begin

         always @(*) begin
	    whole   = i_wb_rdt;
	    funct7  = i_wb_rdt[31:25];
            funct3  = i_wb_rdt[14:12];
            imm30   = i_wb_rdt[30];
            imm25   = i_wb_rdt[25];
            opcode  = i_wb_rdt[6:2];
            op20    = i_wb_rdt[20];
            op21    = i_wb_rdt[21];
            op22    = i_wb_rdt[22];
            op26    = i_wb_rdt[26];
         end

         always @(posedge clk) begin
            if (i_wb_en) begin
               o_sh_right         <= co_sh_right;
               o_bne_or_bge       <= co_bne_or_bge;
               o_cond_branch      <= co_cond_branch;
               o_e_op             <= co_e_op;
               o_ebreak           <= co_ebreak;
               o_two_stage_op     <= co_two_stage_op;
               o_dbus_en          <= co_dbus_en;
               o_mtval_pc         <= co_mtval_pc;
               o_branch_op        <= co_branch_op;
               o_shift_op         <= co_shift_op;
               o_slt_or_branch    <= co_slt_or_branch;
               o_rd_op            <= co_rd_op;
               o_mdu_op           <= co_mdu_op;
               o_ext_funct3       <= co_ext_funct3;
               o_bufreg_rs1_en    <= co_bufreg_rs1_en;
               o_bufreg_imm_en    <= co_bufreg_imm_en;
               o_bufreg_clr_lsb   <= co_bufreg_clr_lsb;
               o_bufreg_sh_signed <= co_bufreg_sh_signed;
               o_ctrl_jal_or_jalr <= co_ctrl_jal_or_jalr;
               o_ctrl_utype       <= co_ctrl_utype;
               o_ctrl_pc_rel      <= co_ctrl_pc_rel;
               o_ctrl_mret        <= co_ctrl_mret;
               o_alu_sub          <= co_alu_sub;
               o_alu_bool_op      <= co_alu_bool_op;
               o_alu_cmp_eq       <= co_alu_cmp_eq;
               o_alu_cmp_sig      <= co_alu_cmp_sig;
               o_alu_rd_sel       <= co_alu_rd_sel;
               o_mem_signed       <= co_mem_signed;
               o_mem_word         <= co_mem_word;
               o_mem_half         <= co_mem_half;
               o_mem_cmd          <= co_mem_cmd;
               o_csr_en           <= co_csr_en;
               o_csr_addr         <= co_csr_addr;
               o_csr_mstatus_en   <= co_csr_mstatus_en;
               o_csr_mie_en       <= co_csr_mie_en;
               o_csr_mcause_en    <= co_csr_mcause_en;
               o_csr_source       <= co_csr_source;
               o_csr_d_sel        <= co_csr_d_sel;
               o_csr_imm_en       <= co_csr_imm_en;
               o_immdec_ctrl      <= co_immdec_ctrl;
               o_immdec_en        <= co_immdec_en;
               o_op_b_source      <= co_op_b_source;
               o_rd_csr_en        <= co_rd_csr_en;
               o_rd_alu_en        <= co_rd_alu_en;
               o_rd_mem_en        <= co_rd_mem_en;
	       //o_valid_instruction<= co_valid_instruction;
            end
         end

      end
   endgenerate

endmodule
