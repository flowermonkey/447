module FSM_tb();
	reg clock;
	reg In;
	reg reset_b;
	
	initial begin
	
		reset_b = 1;
		clock = 0;
		
		#50;
		reset_b = 0;
		In = 0;
		
		#50;
		reset_b = 1;
		clock = 0;
		
		#50 clock = 1;
		#50 clock = 0;
		In = 1;
		
		#50 clock = 1;
		#50 clock = 0;
		
		#50 clock = 1;
		#50 clock = 0;
		In = 0;
		
		#50 clock = 1;
		#50 clock = 0;
		
		#50 clock = 1;
		#50 clock = 0;
		
		$finish;
	end
	
	FSM fsm(/*AUTOINST*/
		// Outputs
		.Out			(Out),
		// Inputs
		.reset_b		(reset_b),
		.clock			(clock),
		.In			(In));
endmodule
