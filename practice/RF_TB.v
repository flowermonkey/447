module RF_TB();

	reg [31:0] readOut1, readOut2, writeIn;
	reg reset_l, clk, read_l;
	reg [4:0] scr1, scr2, dest;
	


	initial begin
		clk = 1; reset_l = 0;
		#10 reset_l = 1; 
		forever #5 clk = ~clk;
	end

	initial begin
		scr1 <= 0;
		scr2 <= 4;
		read_l <= 0;
		dest <= 0;
		writeIn <= 1;
		@(posedge clk);
		$display("writeIn: %d", writeIn);
		read_l <= 1; 
		//writing to reg0 <- 1
		@(posedge clk);
		dest <= 4;
		writeIn <= 0xFFFFFFFF;
		//writing to reg4 <- 4294967295
		@(posedge clk);
		read_l <= 0;
		//updating outputs after writes 
		@(posedge clk);
		
		reset_l <= 0;

		@(posedge clk);	
		reset_l <= 0;
		scr1 <= 14;
		scr2 <= 31;
		dest <= 14;
		writeIn <= 0xdeadbeef
		read_l <= 0;

		@(posedge clk);	
		read_l <= 1;
		//writing to reg14 <- 0xdeadbeef
		@(posedge clk);	
		read_l = 0;
		@(posedge clk);	

		$finish
	end
	
	RF rf(
	//Outputs
	.readOut1	(readOut1), 
	.readOut2	(readOut2),
	//Inputs
	.reset_l 	(reset_l),
	.clk		(clk),
	.read_l		(read_l),
	.scr1		(scr1),
	.scr2		(scr2),
	.dest		(dest),
	.writeIn	(writeIn));

endmodule : RF_TB
