module RF(
	output reg [31:0] readOut1, readOut2,
	input reset_l, clk, read_b
	input reg [4:0] scr1, scr2, dest,
	input reg [31:0] writeIn);

	reg [31:0] [31:0] registers
	
	reg i = 0;

	always * begin
		if (~read_l)
			readOut1 = registers[scr1];
			readOut2 = registers[scr2];
	end
	
	always @(posedge ck or negedge reset_l) begin
		if (~reset_l) begin
			for (i=0; i<32; i++)
				registers[i] <= 0;
		end
		else if (read_l)
			registers[dest] <= writeIn;
		else
			registers <= registers;

		registers[0] <= 0;
	end 

endmodule : RF
