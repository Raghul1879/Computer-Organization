/**************************************************************
    Name : Raghul S
	Roll No: EE20B103
	Assignment 2 : Single cycle CPU (ALU + Load/Store + Branch)
**************************************************************/

`timescale 1ns / 1ps
module cpu (
    input clk, 
    input reset,
    output [31:0] iaddr,
    input [31:0] idata,
    output [31:0] daddr,
    input [31:0] drdata,
    output [31:0] dwdata,
    output [3:0] dwe
);
    reg [31:0] iaddr;
    reg [31:0] daddr;
    reg [31:0] dwdata;
    reg [3:0]  dwe;

    reg [31:0] imm;
    reg [5:0] x;
    reg [4:0] rs1,rs2,rd;
    reg [31:0] rv1;
    reg [31:0] rv2;
    reg [31:0] rf_in;
    reg [31:0] ctrl;


    reg signed [31:0] rfmem[31:0];//declaring the required memory
    integer i;

    initial begin for (i=0;i<32;i=i+1)
    begin rfmem[i] =0; end end //forcing the first register to zero
 
    always @(posedge clk) begin

        if (rd != 0)
		begin 
			rfmem[rd] = rf_in;
		end       	
 
        if (reset) begin
            iaddr<=0;
            daddr <= 0;
            dwdata <= 0;
            dwe <= 0;
		end 
        else begin 

            case (x)
		       6'b011101 : iaddr = iaddr + imm;                          //JAL
			   6'b011111 :  iaddr = (rv1+ imm) & 32'hfffffffe;           //JALR
		       6'b100000 :  if (ctrl==32'b1)begin iaddr= iaddr + imm;end //BEQ
						else begin iaddr= iaddr +4;end
		       6'b100001 : if (ctrl==32'b1)begin iaddr= iaddr + imm;end  //BNE
						else begin iaddr= iaddr +4;end
		       6'b100010 : if (ctrl==32'b1)begin iaddr= iaddr + imm;end  //BLT
						else begin iaddr= iaddr +4;end
		       6'b100011 : if (ctrl==32'b1)begin iaddr= iaddr + imm;end  //BGE
						else begin iaddr= iaddr +4;end
		       6'b100100 : if (ctrl==32'b1)begin iaddr= iaddr + imm;end  //BLTU
						else begin iaddr= iaddr +4;end
		       6'b100101 : if (ctrl==32'b1)begin iaddr= iaddr + imm;end  //BGEU
						else begin iaddr= iaddr +4;end
		       default : iaddr = iaddr + 4;
            endcase

        end

    end

  always @(*) begin

   //Decoding part 

    if (idata[6:0]== 7'b0110111 ) //LUI
    begin 
	  rd<=idata[11:7];rs1<=5'b0;rs2<=5'b0;
	  imm <= idata & 32'hfffff000;
	  x<= 6'b000000;
    end
    else if (idata[6:0]== 7'b0010111 ) //AUIPC
    begin 
	  rd<=idata[11:7];rs1<=5'b0;rs2<=5'b0;
	  imm <= idata & 32'hfffff000;
	  x <= 6'b000001;
    end
    else if (idata[6:0]== 7'b0000011 && idata[14:12]==3'b000 ) //LB 
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:20]};
	  x<= 6'b000010;
    end
    else if (idata[6:0]== 7'b0000011 && idata[14:12]==3'b001 ) //LH
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:20]};
	  x<= 6'b000011; 
    end
    else if (idata[6:0]== 7'b0000011 && idata[14:12]==3'b010 ) //LW
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:20]};
	  x<= 6'b000100;
    end
    else if (idata[6:0]== 7'b0000011 && idata[14:12]==3'b100 ) //LBU
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:20]};
	  x<= 6'b000101;
    end
    else if (idata[6:0]== 7'b0000011 && idata[14:12]==3'b101 ) //LHU
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:20]};
	  x<= 6'b000110;
    end
    else if (idata[6:0]== 7'b0100011 && idata[14:12]==3'b000 ) //SB
    begin 
	  rs2<=idata[24:20];rs1<=idata[19:15];rd<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:25],idata[11:7]};
	  x<= 6'b000111;
    end
    else if (idata[6:0]== 7'b0100011 && idata[14:12]==3'b001 ) //SH
    begin 
	  rs2<=idata[24:20];rs1<=idata[19:15];rd<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:25],idata[11:7]};
	  x<= 6'b001000;
    end
    else if (idata[6:0]== 7'b0100011 && idata[14:12]==3'b010 ) //SW(17)
    begin 
	  rs2<=idata[24:20];rs1<=idata[19:15];rd<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:25],idata[11:7]};
	  x<= 6'b001001;
    end
    else if (idata[6:0]== 7'b0010011 && idata[14:12]==3'b000 ) //ADDI
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:20]};
	  x<= 6'b001010;
    end
    else if (idata[6:0]== 7'b0010011 && idata[14:12]==3'b010 ) //SLTI
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20{idata[31]}},idata[31:20]};
	  x<= 6'b001011;
	end
    else if (idata[6:0]== 7'b0010011 && idata[14:12]==3'b011 ) //SLTIU
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20'b0},idata[31:20]};
	  x<= 6'b001100;
    end
    else if (idata[6:0]== 7'b0010011 && idata[14:12]==3'b100 ) //XORI 
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20'b0},idata[31:20]};
	  x<= 6'b001101;
    end
    else if (idata[6:0]== 7'b0010011 && idata[14:12]==3'b110 ) //ORI
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20'b0},idata[31:20]};
	  x<= 6'b001110;
    end
    else if (idata[6:0]== 7'b0010011 && idata[14:12]==3'b111 ) //ANDI(23)
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {{20'b0},idata[31:20]};
	  x<= 6'b001111;
    end
    else if (idata[6:0]== 7'b0010011 && idata[14:12]==3'b001 ) //SLLI
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {27'b0,idata[24:20]};
	  x<= 6'b010000;
    end
    else if (idata[6:0]== 7'b0010011 && idata[14:12]==3'b101 && idata[31:25]==7'b0000000 ) //SRLI
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {27'b0,idata[24:20]};
	  x<= 6'b010001;
    end
    else if (idata[6:0]== 7'b0010011 && idata[14:12]==3'b101 && idata[31:25]==7'b0100000 ) //SRAI
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2<=5'b0;
	  imm <= {27'b0,idata[24:20]};
	  x<= 6'b010010;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b000 && idata[31:25]==7'b0000000 ) //ADD
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
	  x<= 6'b010011;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b000 && idata[31:25]==7'b0100000 ) //SUB
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
	  x<= 6'b010100;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b001 && idata[31:25]==7'b0000000 ) //SLL
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
	  x<= 6'b010101;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b010 && idata[31:25]==7'b0000000 ) //SLT
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
	  x<= 6'b010110;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b011 && idata[31:25]==7'b0000000 ) //SLTU(31)
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
	  x<= 6'b010111;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b100 && idata[31:25]==7'b0000000 ) //XOR
    begin 
	  rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
	  x<= 6'b011000;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b101 && idata[31:25]==7'b0000000 ) //SRL
    begin 
	   rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
	   x<= 6'b011001;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b101 && idata[31:25]==7'b0100000 ) //SRA
    begin 
	   rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
	   x<= 6'b011010;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b110 && idata[31:25]==7'b0000000 ) //OR
    begin 
	   rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
       x<= 6'b011011;
    end
    else if (idata[6:0]== 7'b0110011 && idata[14:12]==3'b111 && idata[31:25]==7'b0000000 ) //AND
    begin 
	   rd<=idata[11:7];rs1<=idata[19:15];rs2 <= idata[24:20];imm<=32'b0;
       x<= 6'b011100;
    end
    else if (idata[6:0]== 7'b1101111 ) //JAL
    begin 
	   rd<=idata[11:7];rs1<=5'b0;rs2<=5'b0;
	   imm <= {{12{idata[31]}},idata[19:12],idata[20],idata[30:21],1'b0};
	   x<= 6'b011101;
    end
    else if (idata[6:0]== 7'b1100111 && idata[14:12]==3'b000) //JALR 
    begin 
	    rd<=idata[11:7]; rs1<=idata[19:15];rs2<=5'b0;
	    imm <= {{20{idata[31]}},idata[31:20]};
	    x<= 6'b011111;
    end
    else if (idata[6:0]== 7'b1100011 && idata[14:12]==3'b000 ) //BEQ
    begin 
	    rs2<=idata[24:20];rs1<=idata[19:15];rd<=5'b0;
	    imm <= {{19{idata[31]}},idata[31],idata[7],idata[30:25],idata[11:8],1'b0};
        x<= 6'b100000;
    end
    else if (idata[6:0]== 7'b1100011 && idata[14:12]==3'b001 ) //BNE
    begin 
	    rs2<=idata[24:20];rs1<=idata[19:15];rd<=5'b0;
	    imm <= {{20{idata[31]}},idata[7],idata[30:25],idata[11:8],1'b0};
	    x<= 6'b100001;
    end
    else if (idata[6:0]== 7'b1100011 && idata[14:12]==3'b100 ) //BLT
    begin 
	    rs2<=idata[24:20];rs1<=idata[19:15];rd<=5'b0;
	    imm <= {{20{idata[31]}},idata[7],idata[30:25],idata[11:8],1'b0};
        x<= 6'b100010;
    end
    else if (idata[6:0]== 7'b1100011 && idata[14:12]==3'b101 ) //BGE
    begin 
	    rs2<=idata[24:20];rs1<=idata[19:15];rd<=5'b0;
	    imm <= {{20{idata[31]}},idata[7],idata[30:25],idata[11:8],1'b0};
	    x<= 6'b100011;
    end
    else if (idata[6:0]== 7'b1100011 && idata[14:12]==3'b110 ) //BLTU
    begin 
	    rs2<=idata[24:20];rs1<=idata[19:15];rd<=5'b0;
	    imm <= {{20{idata[31]}},idata[7],idata[30:25],idata[11:8],1'b0};
	    x<= 6'b100100;
    end
    else if (idata[6:0]== 7'b1100011 && idata[14:12]==3'b111 ) //BGEU
       begin 
        rs2<=idata[24:20];rs1<=idata[19:15];rd<=5'b0;
        imm <= {{20{idata[31]}},idata[7],idata[30:25],idata[11:8],1'b0};
	    x<= 6'b100101;
    end
    else begin rd=5'b0;rs1=5'b0;rs2=5'b0;x=6'b111111;imm=32'b0;end
  
    rv1 = rfmem[rs1];
    rv2 = rfmem[rs2];

        // ALU computations
    case (x)
           6'b000000:  begin    //LUI
            rf_in = imm;
           end

           6'b000001: begin    //AUIPC
             rf_in = imm + iaddr;
           end

           6'b000010:  begin  //LB 
            daddr = rv1 + imm;
            rf_in = {{24{drdata[7]}}, drdata[7:0]};
           end

           6'b000011:  begin  //LH
            daddr = rv1 + imm;
            rf_in = {{16{drdata[15]}}, drdata[15:0]};
            
           end

           6'b000100: begin   //LW
             daddr = rv1 + imm;  
             rf_in = drdata;
            end
         
           6'b000101:  begin    //LBU
             daddr = rv1 + imm;  
             rf_in = drdata[7:0];
            end

           6'b000110:  begin    //LHU
             daddr = rv1 + imm;  
             rf_in = drdata[15:0];
            end

           6'b000111: begin     //SB
             dwdata = rv2;
             daddr = rv1 + imm;  
             if (daddr[1:0] ==2'b00 ) dwe=4'b0001;
			       else if (daddr[1:0] ==2'b01 ) dwe=4'b0010;
		       	 else if (daddr[1:0] ==2'b10 ) dwe=4'b0100;
			       else if (daddr[1:0] ==2'b11 ) dwe=4'b1000;
             end
           
           6'b001000:  begin    //SH
             dwdata = rv2;
             daddr = rv1 + imm;  
             if (daddr[1:0] ==2'b00 ) dwe=4'b0011;
			       else if (daddr[1:0] ==2'b10 ) dwe=4'b1100;
           end
           6'b001001:  begin    //SW
             dwdata = rv2;
             daddr = rv1 + imm;  
             dwe = 4'b1111;

           end

           6'b001010: begin    //ADDI
             rf_in = rv1 + imm;  
			end

           6'b001011: begin    //SLTI
             rf_in = $signed(rv1)<$signed(imm);
             
           end
		   6'b001100: begin    //SLTIU  
             rf_in = rv1<imm;
             
           end
		   6'b001101: begin    //XORI
             rf_in = rv1^imm;
             
           end
		   6'b001110: begin    //ORI
             rf_in = rv1|imm;
             
           end
		   6'b001111: begin    //ANDI
             rf_in = rv1&imm;
             
           end
		   6'b010000: begin    //SLLI
             rf_in = rv1<<imm;
             
           end	
		   6'b010001: begin    //SRLI
             rf_in= rv1>>imm;
             
           end
           6'b010010: begin    //SRAI
             rf_in = $signed(rv1) >>> imm;
             
           end
		   6'b010011: begin    //ADD
             rf_in = rv1+rv2;
             
           end
		   6'b010100: begin    //SUB
             rf_in = rv1-rv2;
             
           end
		   6'b010101: begin    //SLL
             rf_in = rv1<<rv2;
             
           end
	       6'b010110: begin    //SLT
             rf_in = $signed(rv1)<$signed(rv2);
             
           end
		   6'b010111: begin    //SLTU
             rf_in= rv1<rv2;
              
           end
		   6'b011000: begin    //XOR
             rf_in = rv1^rv2;
             
           end
	       6'b011001: begin    //SRL
             rf_in = rv1>>rv2;
             
           end
		   6'b011010: begin    //SRA
             rf_in = $signed(rv1) >>> rv2;
             
           end
		   6'b011011: begin    //OR
             rf_in = rv1|rv2;
             
           end
		   6'b011100: begin    //AND
             rf_in = rv1&rv2;
             
           end 
           6'b011101: begin  //JAL
		     rf_in = iaddr + 4;
		
		   end
		   6'b011111: begin //JALR
		     rf_in = iaddr +4;
			 
		   end

           6'b100000: begin rf_in = 0; if (rv1 == rv2)begin ctrl <= 32'b1;end else begin ctrl<= 32'b0; end end //BEQ
		   6'b100001: begin rf_in = 0; if ($signed(rv1) != $signed(rv2))begin ctrl <= 32'b1;end else begin ctrl<= 32'b0; end end //BNE
		   6'b100010: begin rf_in = 0; if ($signed(rv1) < $signed(rv2))begin ctrl <= 32'b1;end else begin ctrl<= 32'b0; end end //BLT
		   6'b100011: begin rf_in = 0; if ($signed(rv1) >= $signed(rv2))begin ctrl <= 32'b1;end else begin ctrl<= 32'b0; end end //BGE
		   6'b100100: begin rf_in = 0; if (rv1 < rv2)begin ctrl <= 32'b1;end else begin ctrl<= 32'b0; end end  //BLTU
		   6'b100101: begin rf_in = 0; if (rv1 >= rv2)begin ctrl <= 32'b1;end else begin ctrl<= 32'b0; end end //BGEU
           default: rf_in = 0;
    endcase
        
  end
    
endmodule