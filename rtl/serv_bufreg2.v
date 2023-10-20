module serv_bufreg2
  #(
   parameter BITS_PER_CYCLE = 4,
   parameter LB = $clog2(BITS_PER_CYCLE)
  )
  (
   input wire 	      i_clk,
   //State
   input wire 	      i_en,
   input wire 	      i_init,
   input wire 	      i_cnt_done,
   input wire [1:0]   i_lsb,
   input wire 	      i_byte_valid,
   output wire 	      o_sh_done,
   output wire 	      o_sh_done_r,
   //Control
   input wire 	      i_op_b_sel,
   input wire 	      i_shift_op,
   input wire         i_right_shift_op,
   input wire [LB-1:0]  i_shift_counter_lsb,
   //Data
   input wire [BITS_PER_CYCLE-1:0] i_rs2,
   input wire [BITS_PER_CYCLE-1:0] i_imm,
   output wire [BITS_PER_CYCLE-1:0] o_op_b,
   output wire [BITS_PER_CYCLE-1:0] o_q,
   output wire [LB-1:0] o_shift_counter_lsb,
   //External
   output wire [31:0] o_dat,
   input wire 	      i_load,
   input wire [31:0]  i_dat);

   reg [31:0] 	 dat;

   assign o_op_b = i_op_b_sel ? i_rs2 : i_imm;

   wire 	 dat_en = i_shift_op | (i_en & i_byte_valid);

   /* The dat register has three different use cases for store, load and
    shift operations.
    store : Data to be written is shifted to the correct position in dat during
            init by dat_en and is presented on the data bus as o_wb_dat
    load  : Data from the bus gets latched into dat during i_wb_ack and is then
            shifted out at the appropriate time to end up in the correct
            position in rd
    shift : Data is shifted in during init. After that, the six LSB are used as
            a downcounter (with bit 5 initially set to 0) that triggers
            o_sh_done and o_sh_done_r when they wrap around to indicate that
            the requested number of shifts have been performed
    */
   wire decrement = i_shift_op & !i_init;
   reg decrement_ff = 0;
   wire [5:0] dat_shamt = (decrement) ?
	      //Down counter mode
	      (   (i_right_shift_op && !decrement_ff && i_shift_counter_lsb != 0) ? 
                  // this is just to make a shift for amount not divisible by BITS_PER_CYCLE
                  dat[5:0] : 
                  (dat[5:0]-BITS_PER_CYCLE)
              ) :
	      //Shift reg mode with optional clearing of bit 5
	      {dat[5+BITS_PER_CYCLE] & !(i_shift_op & i_cnt_done),dat[4+BITS_PER_CYCLE:BITS_PER_CYCLE]};

   assign o_sh_done = dat_shamt[5];
   assign o_sh_done_r = dat[5];
   assign o_shift_counter_lsb = dat[LB-1:0];

   assign o_q =
	       ((i_lsb == 2'd3) ? dat[23+BITS_PER_CYCLE:24] :
	       ((i_lsb == 2'd2) ? dat[15+BITS_PER_CYCLE:16] :
	       ((i_lsb == 2'd1) ? dat[7+BITS_PER_CYCLE:8] :
	                          dat[-1+BITS_PER_CYCLE:0])));

   assign o_dat = dat;

   always @(posedge i_clk) begin
      decrement_ff <= decrement;
      if (dat_en | i_load)
	dat <= i_load ? i_dat : {o_op_b, dat[31:6+BITS_PER_CYCLE], dat_shamt}; 
   end

endmodule
