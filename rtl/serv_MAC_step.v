
module serv_MAC_step#(
	parameter W = 1,
	parameter B = W -1
)
(
	input wire clk,

	input wire i_MAC_step1,
	input wire i_MAC_step2,
	input wire i_alu_cmp,
	input wire i_cnt0,
	input wire i_cnt_done,
	input wire i_cnt_en,
	input wire i_init,
	input wire i_rs1,

	output reg o_pc_plus8,
	output wire o_pc_minus4,
        output wire o_pc_plus0
);

reg flagb;

always @(posedge clk) begin
	if (i_cnt0 & i_init & i_MAC_step1) flagb <= i_rs1;
	if (i_cnt_done & !i_init) flagb <= 0;
        if (i_cnt_en & i_init)  o_pc_plus8 <= i_MAC_step1 & i_alu_cmp;
end

assign o_pc_minus4 = i_MAC_step2;
assign o_pc_plus0  = i_MAC_step1 & !flagb & !o_pc_plus8;

endmodule

