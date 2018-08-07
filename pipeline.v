module pipeline(m);
	input m;
	
	// some important buses 
	wire clk, reset;
	
	//fetch wires
	wire [15:0] two;
	wire [15:0] zeros;
	wire ifpcwriteEn, ifmemorywriteEn, ifmemoryreadEn; //initilize to constants
	wire [15:0] ifwritedata; //initilize to constants

	wire [15:0] ifpcbranchmuxbus;
	wire [15:0] ifsamepcmuxbus;
	wire [15:0] ifpcbus;
	wire ifmemoryreset;
	wire [15:0] ifpcincrementbus;
	wire ifinpcarrybus, ifcbus;
	wire [15:0] ifmemoryoutbus;
	wire [15:0]	ifnewinstructionhzbus;
	wire [15:0] ifinstructionmuxbus;
	wire ifnewinstructionflag;
	wire [47:0] iftolatch;
	wire iflatchwriteEn;
	wire dectolatchwriteEn;
	wire registerexecutelatchwriteEn;
	wire executememorylatchtwriteEn;
	wire memorywritebacklatchwriteEn;
	//
	clockgenerator cg (clk);	
	
	// decode wires
	wire [47:0] fetchdecodelatchbus;
	wire [21:0] decontrolbus;
	wire [21:0] newdecontrolbus;
	wire deczeroflagbus;
	wire [2:0] declmsmregbus;
	wire [7:0] decnewbitsbus;
	wire  decvalidflagbus;
	wire decnop;
	wire [72:0] dectolatch;
	wire decpresentstatebus;
    wire [21:0] decnewcontrolbus;
	
	//Register stage wires
	wire [72:0] decoderregisterlatchbus;
	wire [2:0] regrbmuxbus;
	wire [15:0] regreadDatabus1;
	wire [15:0] regreadDatabus2;
	wire [15:0] regextendzerobus;
	wire regeqfalgbus1;
	wire regeqfalgbus2;
	wire [15:0] regreadDatafinalbus1;
	wire [15:0] regreadDatafinalbus2;
	wire [126:0] registerexecutelatchbus;
	wire [126:0] registertolatch;
	wire [21:0] regnewcontrols;
	
	//execute wires 
	wire [15:0] exfwdmuxbus1;
	wire [15:0] exfwdmuxbus2;
	wire [15:0] exalubus1;
	wire [15:0] exalubus2;
	wire [15:0] exlsbus;
	wire [15:0] exaluoutputbus;
	wire exequalitybus;
	wire exz, exc, exz1, exc1, exz2, exc2, exnewz, exnewc;
	wire exzcwriteEn;
	wire [21:0] exnewcontrols;
	wire executetolatch;
	wire beqbranchflag;
	wire [15:0] branchaddressbus;
	wire finalbranchflag;
	wire [15:0] exsebus1;
	wire [15:0] exsebus2;
	//functions
	initial begin 
		exzcwriteEn <=1;
		end
	//memory wires 
	wire [123:0] executememorylatchbus;
	wire [15:0] memdatamemorymuxbus;
	wire [15:0] memdatamemoryoutbus;
	wire [139:0] memtolatch;
	wire [21:0] memnewcontrols;
	//writeback wire
	wire [136:0] memorywritebacklatchbus;
	wire wbpcflag;
	wire [15:0] wbdatamuxbus;
	wire [2:0] wbrcmuxbus;
	wire wbpcjalflag;
	wire wbpcjlrflag;
	
    //forwarding wires
	wire [15:0] fwddata_a, fwddata_b;
	wire fwdstore;
	wire fwdselecta, fwdselectb;

	//fetch functions
	initial begin
		two <= 2;
		zeros <=0;
		ifpcwriteEn <=1;
		ifinpcarry <=0;
		ifmemorywriteEn <=0;
		ifmemoryreadEn <=1;
		ifwritedata <=0;
		ifmemoryreset <=0;
		iflatchwriteEn <=1;
		dectolatchwriteEn <=1;
		registerexecutelatchwriteEn <=1;
		executememorylatchtwriteEn <=1;
		memorywritebacklatchwriteEn <=1;
		end

		
	// Fetch	
	mux2_16to16 pcbranchmux (ifsamepcmuxbus,ifpcincrementbus, ifpcbus,ifnewinstructionflag); // controls previous instruction enabling for lmsm type of instructions
	mux2_16to16 samepcmux (ifpcbranchmuxbus,ifsamepcmuxbus, branchaddressbus,finalbranchflag);	// controls  branch type of instructions
	register r7 (ifpcwriteEn, ifpcbus , ifpcbranchmuxbus, clk, reset); //programme counter
	fullAdder_16 fa (two,  ifpcbus,	ifinpcarry,	ifpcincrementbus,	ifcbus);
	memory16bitaddressable insmem1(ifmemoryreadEn , ifmemorywriteEn, clk, ifmemoryreset, ifwritedata, ifmemoryoutbus, ifpcbus);
	hazard_unit hz1(fetchdecodelatchbus[15:0],decoderregisterlatchbus[15:0],decnewbitsbus,registerexecutelatchbus[70],finalbranchflag,declmsmregbus, decvalidflagbus, decnop,ifnewinstructionhzbus,ifnewinstructionflag);
	mux2_16to16 newinstructionmux (ifinstructionmuxbus,ifmemoryoutbus, ifnewinstructionhzbus,ifnewinstructionflag); // controls previous instruction enabling for lmsm type of instructions
	
	// Fetch_Decode register
	always @(ifmemoryoutbus or ifpcbus or ifpcincrementbus)
		begin
		iftolatch[15:0] <= ifinstructionmuxbus;
		iftolatch[31:0] <= ifpcincrementbus;
		iftolatch[47:32] <= ifpcbus;
		end
	fetch_decode fd (iflatchwriteEn, fetchdecodelatchbus, iftolatch, clk, reset);
	
	
	
	// Decode	
	currentstate cs(clk, decontrolbus[21], decpresentstatebus, reset);
	outputcontroller oc(decontrolbus, deczeroflagbus, decnop, decpresentstatebus, fetchdecodelatchbus[15:12]);	
	lmsmunit loadmltiple(declmsmregbus,	deczeroflagbus,	decnewbitsbus,	decvalidflagbus,	fetchdecodelatchbus[7:0]);	
	flush f0( decontrolbus, decnewcontrolbus, finalbranchflag);
	always @(fetchdecodelatchbus or declmsmregbus or decontrolbus)
		begin
		dectolatch[47:0] <= fetchdecodelatchbus;
		dectolatch[50:48] <= declmsmregbus;
		dectolatch[72:51] <= decnewcontrolbus;
		end
	
	
	//	Decode_Register register
	decode_register dr (dectolatchwriteEn, decoderregisterlatchbus, dectolatch, clk, reset);
	reg [2:0] seven;
	
	initial begin 
		seven <= 7;
		end
	// Register stage	
	mux2_3to3 muxrb (regrbmuxbus, dectolatch[8:6], dectolatch[50:48], dectolatch[19]);
	regfile8_16 rf(dectolatch[11:9],regrbmuxbus,wbrcmuxbus,regreadDatabus1,regreadDatabus2,wbdatamuxbus,memorywritebacklatchbus[61], clk, reset);
	extendzeros ez( regextendzerobus,dectolatch[8:0]);
	equality3 regeq1(regeqfalgbus1, seven, dectolatch[11:9]);
	equality3 regeq2(regeqfalgbus2, seven, regrbmuxbus);
	mux2_16to16 regramux1(regreadDatafinalbus1, regreadDatabus1, ifpcbus, regeqfalgbus1);// Ra ==7
	mux2_16to16 regrbmux1(regreadDatafinalbus2, regreadDatabus2, ifpcbus, regeqfalgbus2);// Rb ==7
	flush f1(decoderregisterlatchbus[72:51], regnewcontrols, finalbranchflag);
	always @(decoderregisterlatchbus or regextendzerobus or regreadDatafinalbus1 or regreadDatafinalbus2  )
		begin
		registertolatch [50:0] <= decoderregisterlatchbus[50:0];
		registertolatch [72:51] <= regnewcontrols;
		registertolatch[75:73] <= dectolatch[11:9];
		registertolatch[78:76] <= regrbmuxbus;
		registertolatch[94:79] <= extendzeros;
		registertolatch[110:95] <= regreadDatafinalbus1;
		registertolatch[126:111] <= regreadDatafinalbus2;
		end
	
	
	// Register_execute register
	register_execute re (registerexecutelatchwriteEn, registerexecutelatchbus, registertolatch, clk, reset);

	// Execute 
	
	mux2_16to16 exfwdmuxa (exfwdmuxbus1, registerexecutelatchbus[110:95], fwddata_a,fwdselecta); //forwarding mux a
	mux2_16to16 exfwdmuxb (exfwdmuxbus2, registerexecutelatchbus[110:95], fwddata_b,fwdselectb); //forwarding mux b
	mux4_16to16 exalumuxa(exalubus1, exfwdmuxbus1, registerexecutelatchbus[93:78], exsebus1, two, registerexecutelatchbus[9:8]);
	mux8_16to16 exalumuxb(registerexecutelatchbus[56:54], registerexecutelatchbus[126:111], registerexecutelatchbus[31:16], exsebus1, exsebus2, two, zeros, zeros, zeros, exalubus2);
	signextend15_6 se1(exsebus1,registerexecutelatchbus[5:0]);
	signextend15_9 se2(exsebus2, registerexecutelatchbus[8:0]);
	leftShift ls1( exlsbus,exsebus1);  
	aluiitb alu1(exalubus1,exalubus2,registerexecutelatchbus[11],exaluoutputbus,exz,exc);
	equality exeq1(exequalitybus,exalubus1,exalubus2);
	reg1bit r1(exzcwriteEn , exz1, exnewz, clk, reset); //z1
	reg1bit r2(exzcwriteEn , exc1, exnewc, clk, reset);	//c1
	reg1bit r3(exzcwriteEn , exz2, exz1, clk, reset); //z2
	reg1bit r4(exzcwriteEn , exc2, exc1, clk, reset); //c2
	flush f2(registerexecutelatchbus[72:51], exeflushnewcontrols, finalbranchflag);
	conditional_controls cc1(registerexecutelatchbus[15:12],registerexecutelatchbus[1:0],exeflushnewcontrols , exz, exc, exz1,exc1, exz2, exc2, writebackbranchflag, exnewcontrols, exnewz, exnewc);

	and a1 (beqbranchflag, exequalitybus, registerexecutelatchbus[65]);
	or exo1(finalbranchflag ,beqbranchflag, flushflag);
	mux2_16to16 branchmuxexecute1 (branchaddressbus, exaluoutputbus,wbjlrbus,flushflag); //forwarding mux a
	
	
	always @(registerexecutelatchbus or regextendzerobus or regreadDatafinalbus1 or regreadDatafinalbus2  )
		begin
		executetolatch [50:0] <= registerexecutelatchbus[50:0];
		executetolatch[72:51] <= exnewcontrols;
		executetolatch[88:73] <= registerexecutelatchbus[94:79];
		executetolatch[104:89] <= exaluoutputbus;
		executetolatch[120:105] <= registerexecutelatchbus[126:111];
		executetolatch[123:121] <= registerexecutelatchbus[75:73];
		end
	// Execute_memory register
	execute_memory em(executememorylatchtwriteEn, executememorylatchbus, executetolatch, clk, reset);
	// Memory
	flush f3(executememorylatchbus[72:51],memnewcontrols,flushflag);
	mux2_16to16 datamemorymux (memdatamemorymuxbus,executememorylatchbus[104:89], memorywritebacklatchbus[120:105],fwdstore);
	memory16bitaddressable datamemory (executememorylatchbus[66] , executememorylatchbus[67], clk, reset, executememorylatchbus[120:105], memdatamemoryoutbus, memdatamemorymuxbus);
	always @(executememorylatchbus or memdatamemoryoutbus)
		begin
		memtolatch [50:0] <= executememorylatchbus[50:0];
		memtolatch [72:51] <= memnewcontrols;		
		memtolatch [104:73] <= executememorylatchbus[104:73];
		memtolatch [120:105] <= memdatamemoryoutbus;		
		memtolatch [136:121] <= executememorylatchbus[120:105]; // we did not entered Rb yet which will be high impedence if not connected to any of the inputs or outputs
		end
	
	// Memory_writeback register
	memory_writeback memwb1 (memorywritebacklatchwriteEn, memorywritebacklatchbus, memtolatch, clk, reset);
	// Writeback 
	
	mux8_16to16 wbdatabusmux1(wbdatamuxbus, memorywritebacklatchbus[88:73], memorywritebacklatchbus[104:89], memorywritebacklatchbus[120:105], memorywritebacklatchbus[31:16], memorywritebacklatchbus[47:32], zeros, zeros, zeros, memorywritebacklatchbus[53:51]);
	mux4_3to3 wbrcmux1(wbrcmuxbus,memorywritebacklatchbus[11:9] , memorywritebacklatchbus[8:6], memorywritebacklatchbus[5:3], memorywritebacklatchbus[48:50],memorywritebacklatchbus[58:57]);
	mux2_16to16 wbjal1(wbjalbus,wbdatamuxbus, memorywritebacklatchbus[104:89],wbpcjalflag); //jal
	mux2_16to16 wbjlr1(wbjlrbus,wbjalbus, memorywritebacklatchbus[136:121],wbpcjlrflag);
	equality3 wbeq1(wbpcflag, seven,wbrcmuxbus);
	or branchtrigger1( wbbranchflag, isjal, isjlr, wbpcflag);
	and branchvalid1 ( flushflag, wbbranchflag, memorywritebacklatchbus[71]);
	
//	and wbvalidpcjalfalg(wbpcjalflag, memorywritebacklatchbus[71], memorywritebacklatchbus[64]);
//	and wbvalidpcjlrflag(wbpcjlrflag, memorywritebacklatchbus[71], memorywritebacklatchbus[63]);
	//Forwarding unit
	
	forwarding_unit fu(registerexecutelatchbus[75:73], registerexecutelatchbus[78:76], executememorylatchbus, memorywritebacklatchbus, fwddata_a,fwddata_b, fwdstore, fwdselecta, fwdselectb);
	endmodule

module flush(oldcontrols, newcontrols, flushflag);
	input [21:0] oldcontrols;
	input flushflag;
	output [21:0] newcontrols;
	reg [21:0] newcontrols;
	always @(oldcontrols or flushflag)
		begin
		if(flushflag ==1)
			begin
			newcontrols <=0;
			end
		else 
			begin
			newcontrols <= oldcontrols;
			end
		end
	endmodule
	
module conditional_controls(opcode,funct,controls, z, c, z1,c1, z2, c2, writebackbranchflag, newcontrols, newz, newc);
	input [3:0] opcode;
	input [1:0] funct;
	input [21:0] controls;
	input z, c;
	input z1,c1;
	input z2, c2;
	input writebackbranchflag;
	output newcontrols;
	output newz, newc;
	reg newcontrols;
	reg newz, newc;
//	if branchfrom writeback then load from state2 unconditionally
//	else 
//		if instruction is invalid do nothing
//		else if instruction is valid and flag is down then make instruction invalid
//			and if setflag is true then ste the new flag
    always @( opcode )
	case(opcode)
		4'b0000:
			begin
			if (controls[20] ==1)
				begin
				if(funct ==0)
					begin
					newz <=z;
					newc <=c;
					end
				else if (funct == 1)
					begin
					newz <=z;
					newc <=c;					
					if(z1 ==0)
						begin
						newcontrols <=0;
						end
					else 
						begin
						newcontrols <= controls;
						end
					end
				else if(funct ==2)
					begin
					newz <=z;
					newc <=c;					
					if(c1==0)
						begin
						newcontrols <=0;
						end
					else 
						begin
						newcontrols <= controls;
						end
					end
				end
			else
				begin
				newcontrols <= controls;
				newz <=z1;
				newc <=c1;
				end
			end
		4'b0010:
			begin
			if (controls[20] ==1)
				begin
				if(funct ==0)
					begin
					newz <=z;
					newc <=c1;
					end
				else if (funct == 1)
					begin
					newz <=z;
					newc <=c1;
					if(z1 ==0)
						begin
						newcontrols <=0;
						end
					else 
						begin
						newcontrols <= controls;
						end
					end
				else if(funct ==2)
					begin
					newz <=z;
					newc <=c1;
					if(c1==0)
						begin
						newcontrols <=0;
						end
					else 
						begin
						newcontrols <= controls;
						end
					end
				end
			else
				begin
				newcontrols <= controls;
				end
			end
		4'b0001:
			begin
			if (controls[20] ==1)
				begin
				newcontrols <= controls;
				newz <=z;
				newc <=c;
				end
			else
				begin
				newcontrols <= controls;
				newz <=z1;
				newc <=c1;
				end
			end
		4'b0100:
			begin
			if (controls[20] ==1)
				begin
				newcontrols <= controls;
				newz <=z;
				newc <=c1;
				end
			else 
				begin
				newz <=z1;
				newc <=c1;
				newcontrols <= controls;
				end;
			end
		default:
			begin
			newcontrols <= controls;
			newz <=z1;
			newc <=c1;
			end
		endcase
		always @ ( newz or newc or writebackbranchflag or c2 or z2)
			begin
			if(writebackbranchflag ==1)
				begin
				newz <=z2;
				newc <=c2;
				end
			end
	endmodule
	
module get_destination_register (instruction, lmsmbits, instruction_validity, destination_registerbits,hazardvalidity);
	input [15:0] instruction;
	input [2:0] lmsmbits;
	input instruction_validity;
	output [2:0] destination_registerbits;	
	output hazardvalidity;
	reg [2:0] destination_registerbits;
	wire hazardvalidity;
	reg nofwd;
	always @(instruction or lmsmbits)
		begin
		case(instruction[15:12])
		4'b0100: //lw
			begin
			destination_registerbits <= instruction[11:9];
			nofwd <=0;
			end
		4'b0110: //lm
			begin
			destination_registerbits <= lmsmbits;
			nofwd <=0;
			end		
    	default:
			begin
			destination_registerbits <= 0;
			nofwd <=1;
			end
		endcase
		end
	and a1 (hazardvalidity, instruction_validity, ~nofwd);	
		
	endmodule

	
module hazard_unit(decodeinstruction,registerinstruction,newbits,register_validity,flushsignal,lmsmregbits, validflag, nop,newinstruction,newinstructionflag);
	input [15:0]decodeinstruction;
	input [15:0]registerinstruction;
	input [7:0] newbits;
	input register_validity;	
	input flushsignal;
	input [2:0] lmsmregbits;
	input validflag;
	
	output nop;
	output [15:0] newinstruction;
	output newinstructionflag;

	
	reg nop;	
	reg [15:0] newinstruction;
	reg newinstructionflag;
	wire [2:0] destination_registerbits;
	
    wire hazardvalidity;
	
// 	get_destiniation_register gdr (destination_registerbits, registerinstruction, lmsmregbits);
	get_destination_register gdr (registerinstruction, lmsmregbits, register_validity, destination_registerbits, hazardvalidity);
	always @(registerinstruction or register_validity or decodeinstruction or destination_registerbits)
	begin
		if(registerinstruction[15:12] == 4'b0100 && hazardvalidity==1) // load type instance
			if (decodeinstruction[15:12] == 4'b0000 ) // add, adc, adz
				begin
				if( decodeinstruction[11:9]== destination_registerbits)
					begin 
					nop <=1;
					end
				else if (decodeinstruction[8:6]== destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end
			if (decodeinstruction[15:12] == 4'b0010 ) // ndu, ndc, ndz
				begin
				if( decodeinstruction[11:9]== destination_registerbits)
					begin 
					nop <=1;
					end
				else if (decodeinstruction[8:6]== destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end
			if (decodeinstruction[15:12] == 4'b0001 ) // adi
				begin
				if( decodeinstruction[11:9]== destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end		
			if (decodeinstruction[15:12] == 4'b0100  ) //lw
				begin
				if (decodeinstruction[8:6] == destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end
			if (decodeinstruction[15:12] == 4'b0101  ) //sw
				begin
				if (decodeinstruction[8:6] == destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end
			if (decodeinstruction[15:12] == 4'b1100  ) //beq
				begin
				if( decodeinstruction[11:9]== destination_registerbits)
					begin 
					nop <=1;
					end
				else if (decodeinstruction[8:6]== destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end
			if (decodeinstruction[15:12] == 4'b1000  ) //jal
				begin
				if( decodeinstruction[11:9]== destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end
			if (decodeinstruction[15:12] == 4'b1001  ) //jlr
				begin
				if( decodeinstruction[8:6]== destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end
			if (decodeinstruction[15:12] == 4'b0110 ) //lm
				begin
				if( lmsmregbits== destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end
			if (decodeinstruction[15:12] == 4'b0111 ) //sm
				begin
				if( lmsmregbits== destination_registerbits)
					begin 
					nop <=1;
					end
				else 
					begin
					nop <=0;
					end
				end	
				
				
		if ( decodeinstruction[15:12] == 4'b0110 && validflag==1 && hazardvalidity ==1 && flushsignal ==0) //lm
			begin
			newinstruction[15:8] <= decodeinstruction[15:8];
			newinstruction[7:0] <= newbits;
			newinstructionflag <=1;
			end
		if (decodeinstruction[15:12] == 4'b0111 && validflag ==1 && hazardvalidity ==1 &&flushsignal ==0) //sm
			begin
			newinstruction[15:8] <= decodeinstruction[15:8];
			newinstruction[7:0] <= newbits;
			newinstructionflag <=1;
			end
		else	
			begin
			newinstructionflag <=0;
			newinstruction <=0;
			end
			
		end
	
	endmodule
	
	
module lmsmunit(regbits,	zeroflag,	newbits,	validflag,	initialbits);
	input [7:0] initialbits;
	
	output [2:0] regbits;
	output zeroflag;
	output [7:0]newbits;
	output validflag;
	
	reg [7:0] notchangebits;
	wire [7:0]newbits;
	wire validflag;
	
	wire [7:0] topewire;
	wire [7:0] inwire;
	wire [7:0] changebits;
// 	wire [7:0] newbits;
	
	priorityencoder8_3 pe (regbits, topewire);
	or or1(o1, topewire[7],topewire[6],topewire[5],topewire[4]);
	or or2(o2, topewire[3],topewire[2],topewire[1],topewire[0]);
	or or3(zeroflag, o1, o2);
	dec3to8 dc (changebits, regbits);
	always @(changebits)
		begin
		    notchangebits <= ~changebits;
		end
	and n7 (newbits[7], notchangebits[0], topewire[7]);
	and n6 (newbits[6], notchangebits[1], topewire[6]);
	and n5 (newbits[5], notchangebits[2], topewire[5]);
	and n4 (newbits[4], notchangebits[3], topewire[4]);
	and n3 (newbits[3], notchangebits[4], topewire[3]);
	and n2 (newbits[2], notchangebits[5], topewire[2]);
	and n1 (newbits[1], notchangebits[6], topewire[1]);
	and n0 (newbits[0], notchangebits[7], topewire[0]);
	or or4(o3, newbits[7],newbits[6],newbits[5],newbits[4]);
	or or5(o4, newbits[3],newbits[2],newbits[1],newbits[0]);
	or or6(validflag, o3, o4);	
	endmodule

module forwarding_unit(Ra, Rb, executememorylatch, memorywritebacklatch, fwddata_a,fwddata_b, fwdstore, fwdselecta, fwdselectb);
// we need to forward only when the instruction is valid and hence we takevalidity bits also into accunt
// higher priority should be given to earlier executed instruction
	input [2:0] Ra;
	input [2:0] Rb;
	input [123:0] executememorylatch;
	input [138:0] memorywritebacklatch;

	output [15:0] fwddata_a;
	output [15:0] fwddata_b;
	output fwdselecta;
	output fwdselectb;
	output fwdstore;

	reg fwdselecta;
	reg fwdselectb;
	reg [15:0] fwddata_a;
	reg [15:0] fwddata_b;
	reg fwdstore;

	wire [2:0] dst1;
	wire [2:0] dst2;
	wire [15:0] memorydestination_data;
	wire [15:0] writeback_data;
	wire nofwdmem;
	wire nofwdwb;


//	get_destination_register_memory(instruction, aluoutput, extendzeros, destination_registerbits, destinationdata, nofwd);
	get_destination_register_memory gdm (executememorylatch[15:0], executememorylatch[104:81], executememorylatch[88:73], dst1, memorydestination_data, nofwdmem);
//	get_destination_register_wb(instruction,lmsmregbits, aluoutput, extendzeros,memorydata, destination_registerbits, destinationdata, nofwd);
	get_destination_register_wb gdw(memorywritebacklatch[15:0],memorywritebacklatch[50:48], memorywritebacklatch[103:89], memorywritebacklatch[88:73],memorywritebacklatch[119:104], dst2, writeback_data, nofwdwb);
	
	
	always @(Ra or Rb or executememorylatch or memorywritebacklatch )
		begin
		if(Ra==dst1  &&  executememorylatch[71]==1)
			begin
			fwdselecta <= ~nofwdmem;
			fwddata_a <= memorydestination_data;
			end
		else if(Ra ==dst2 &&  memorywritebacklatch[71]==1)
			begin
			fwdselecta <= ~nofwdwb;
			fwddata_a <= writeback_data;
			end
		else 
			begin	
			fwdselecta <=0;
			fwddata_a <=0;
			end
		if(Rb==dst1 &&  executememorylatch[71]==1)
			begin
			fwdselectb <=~nofwdmem;
			fwddata_b <= memorydestination_data;
			end
		else if(Rb ==dst2 &&  memorywritebacklatch[71]==1)
			begin
			fwdselectb <=~nofwdwb;
			fwddata_b <= writeback_data;
			end
		else 
			begin	
			fwdselectb <=0;
			fwddata_b <=0;
			end
		if(memorywritebacklatch[15:12] ==4'b0100 && executememorylatch[15:12]== 4'b0101 && memorywritebacklatch[71]==1) // load followed by store
			begin
			if(dst2 == executememorylatch[8:6])
				begin
				fwdstore =1;
				end
			else 
				begin
				fwdstore =0;
				end
			end
		if(memorywritebacklatch[15:12] ==4'b0100 && executememorylatch[15:12]== 4'b0111 && memorywritebacklatch[71] ==1 ) // load followed by storemultiple
			begin
			if(dst2 == executememorylatch[50:48])
				begin
				fwdstore =1;
				end
			else 
				begin
				fwdstore =0;
				end
			end
		if(memorywritebacklatch[15:12] ==4'b0110 && executememorylatch[15:12]== 4'b0101 && memorywritebacklatch[71] ==1 ) // load multiple followed by store
			begin
			if(memorywritebacklatch[50:48]== executememorylatch[8:6])
				begin
				fwdstore =1;
				end
			else 
				begin
				fwdstore =0;
				end
			end
		end
	endmodule
	
module get_destination_register_memory(instruction, aluoutput, extendzeros, destination_registerbits, destinationdata, nofwd);
	input [15:0] instruction;
	input [15:0] aluoutput;
	input [15:0] extendzeros;
	
	output [2:0] destination_registerbits;	
	output [15:0] destinationdata;
	output nofwd;
	
	reg [15:0] destinationdata;
	reg [2:0] destination_registerbits;
	reg nofwd;
	
	always @(instruction)
		begin
		case(instruction[15:12])
		4'b0000: // add, adc, adz
			begin
			destination_registerbits <= instruction[5:3];
			nofwd <=0;
			destinationdata<=aluoutput;
			end
		4'b0001: //adi
			begin
			destination_registerbits <= instruction[8:6];
			nofwd <=0;
			destinationdata <= aluoutput;
			end		
		4'b0010: //ndu, ndc, ndz
			begin
			destination_registerbits <= instruction[5:3];
			nofwd <=0;
			destinationdata <= aluoutput;
			end
		4'b0011: //lxi
			begin
			destination_registerbits <= instruction[5:3];
			nofwd <=0;
			destinationdata <= extendzeros;
			end
		default:
			begin
			destination_registerbits <= 0;
			nofwd <=1;
			destinationdata <=0;
			end
		endcase
		end
	endmodule

module get_destination_register_wb(instruction,lmsmregbits, aluoutput, extendzeros,memorydata, destination_registerbits, destinationdata, nofwd);
	input [15:0] instruction;
	input [2:0] lmsmregbits;
	input [15:0] aluoutput;
	input [15:0] extendzeros;
	input [15:0] memorydata;
	
	output [2:0] destination_registerbits;	
	output [15:0] destinationdata;
	output nofwd;
	
	reg nofwd;
	reg [2:0] destination_registerbits;
	reg [15:0] destinationdata;
	
	always @(instruction)
		begin
		case(instruction[15:12])
		4'b0000: // add, adc, adz
			begin
			destination_registerbits <= instruction[5:3];
			nofwd <=0;
			destinationdata<=aluoutput;
			end
		4'b0001: //adi
			begin
			destination_registerbits <= instruction[8:6];
			nofwd <=0;
			destinationdata <= aluoutput;
			end		
		4'b0010: //ndu, ndc, ndz
			begin
			destination_registerbits <= instruction[5:3];
			nofwd <=0;
			destinationdata <= aluoutput;
			end
		4'b0011: //lxi
			begin
			destination_registerbits <= instruction[5:3];
			nofwd <=0;
			destinationdata <= extendzeros;
			end
		4'b0100: //lw
			begin // 
			destination_registerbits <= instruction[11:9];
			nofwd <=0;
			destinationdata <= memorydata ;
			end
		4'b0110: //lm
			begin
			destination_registerbits <= lmsmregbits;
			nofwd <=0;
			destinationdata <= memorydata;
			end
		default:
			begin
			destination_registerbits <= 0;
			nofwd <=1;
			destinationdata <=0;
			end
		endcase
		end
	endmodule
	


	
module fetch_decode(writeEn, out, in, clk, reset);
	input writeEn, clk, reset;
	input [47:0] in;
	output [47:0] out;
	
	reg1bit reg1(.writeEn(writeEn),.outbit(out[0]), .inbit(in[0]), .clk(clk), .reset(reset));
	reg1bit reg2(.writeEn(writeEn),.outbit(out[1]), .inbit(in[1]), .clk(clk), .reset(reset));
	reg1bit reg3(.writeEn(writeEn),.outbit(out[2]), .inbit(in[2]), .clk(clk), .reset(reset));
	reg1bit reg4(.writeEn(writeEn),.outbit(out[3]), .inbit(in[3]), .clk(clk), .reset(reset));
	reg1bit reg5(.writeEn(writeEn),.outbit(out[4]), .inbit(in[4]), .clk(clk), .reset(reset));
	reg1bit reg6(.writeEn(writeEn),.outbit(out[5]), .inbit(in[5]), .clk(clk), .reset(reset));
	reg1bit reg7(.writeEn(writeEn),.outbit(out[6]), .inbit(in[6]), .clk(clk), .reset(reset));
	reg1bit reg8(.writeEn(writeEn),.outbit(out[7]), .inbit(in[7]), .clk(clk), .reset(reset));
	reg1bit reg9(.writeEn(writeEn),.outbit(out[8]), .inbit(in[8]), .clk(clk), .reset(reset));
	reg1bit reg10(.writeEn(writeEn),.outbit(out[9]), .inbit(in[9]), .clk(clk), .reset(reset));
	reg1bit reg11(.writeEn(writeEn),.outbit(out[10]), .inbit(in[10]), .clk(clk), .reset(reset));
	reg1bit reg12(.writeEn(writeEn),.outbit(out[11]), .inbit(in[11]), .clk(clk), .reset(reset));
	reg1bit reg13(.writeEn(writeEn),.outbit(out[12]), .inbit(in[12]), .clk(clk), .reset(reset));
	reg1bit reg14(.writeEn(writeEn),.outbit(out[13]), .inbit(in[13]), .clk(clk), .reset(reset));
	reg1bit reg15(.writeEn(writeEn),.outbit(out[14]), .inbit(in[14]), .clk(clk), .reset(reset));
	reg1bit reg16(.writeEn(writeEn),.outbit(out[15]), .inbit(in[15]), .clk(clk), .reset(reset));
	reg1bit reg17(.writeEn(writeEn),.outbit(out[16]), .inbit(in[16]), .clk(clk), .reset(reset));
	reg1bit reg18(.writeEn(writeEn),.outbit(out[17]), .inbit(in[17]), .clk(clk), .reset(reset));
	reg1bit reg19(.writeEn(writeEn),.outbit(out[18]), .inbit(in[18]), .clk(clk), .reset(reset));
	reg1bit reg20(.writeEn(writeEn),.outbit(out[19]), .inbit(in[19]), .clk(clk), .reset(reset));
	reg1bit reg21(.writeEn(writeEn),.outbit(out[20]), .inbit(in[20]), .clk(clk), .reset(reset));
	reg1bit reg22(.writeEn(writeEn),.outbit(out[21]), .inbit(in[21]), .clk(clk), .reset(reset));
	reg1bit reg23(.writeEn(writeEn),.outbit(out[22]), .inbit(in[22]), .clk(clk), .reset(reset));
	reg1bit reg24(.writeEn(writeEn),.outbit(out[23]), .inbit(in[23]), .clk(clk), .reset(reset));
	reg1bit reg25(.writeEn(writeEn),.outbit(out[24]), .inbit(in[24]), .clk(clk), .reset(reset));
	reg1bit reg26(.writeEn(writeEn),.outbit(out[25]), .inbit(in[25]), .clk(clk), .reset(reset));
	reg1bit reg27(.writeEn(writeEn),.outbit(out[26]), .inbit(in[26]), .clk(clk), .reset(reset));
	reg1bit reg28(.writeEn(writeEn),.outbit(out[27]), .inbit(in[27]), .clk(clk), .reset(reset));
	reg1bit reg29(.writeEn(writeEn),.outbit(out[28]), .inbit(in[28]), .clk(clk), .reset(reset));
	reg1bit reg30(.writeEn(writeEn),.outbit(out[29]), .inbit(in[29]), .clk(clk), .reset(reset));
	reg1bit reg31(.writeEn(writeEn),.outbit(out[30]), .inbit(in[30]), .clk(clk), .reset(reset));
	reg1bit reg32(.writeEn(writeEn),.outbit(out[31]), .inbit(in[31]), .clk(clk), .reset(reset));
	reg1bit reg33(.writeEn(writeEn),.outbit(out[32]), .inbit(in[32]), .clk(clk), .reset(reset));
	reg1bit reg34(.writeEn(writeEn),.outbit(out[33]), .inbit(in[33]), .clk(clk), .reset(reset));
	reg1bit reg35(.writeEn(writeEn),.outbit(out[34]), .inbit(in[34]), .clk(clk), .reset(reset));
	reg1bit reg36(.writeEn(writeEn),.outbit(out[35]), .inbit(in[35]), .clk(clk), .reset(reset));
	reg1bit reg37(.writeEn(writeEn),.outbit(out[36]), .inbit(in[36]), .clk(clk), .reset(reset));
	reg1bit reg38(.writeEn(writeEn),.outbit(out[37]), .inbit(in[37]), .clk(clk), .reset(reset));
	reg1bit reg39(.writeEn(writeEn),.outbit(out[38]), .inbit(in[38]), .clk(clk), .reset(reset));
	reg1bit reg40(.writeEn(writeEn),.outbit(out[39]), .inbit(in[39]), .clk(clk), .reset(reset));
	reg1bit reg41(.writeEn(writeEn),.outbit(out[40]), .inbit(in[40]), .clk(clk), .reset(reset));
	reg1bit reg42(.writeEn(writeEn),.outbit(out[41]), .inbit(in[41]), .clk(clk), .reset(reset));
	reg1bit reg43(.writeEn(writeEn),.outbit(out[42]), .inbit(in[42]), .clk(clk), .reset(reset));
	reg1bit reg44(.writeEn(writeEn),.outbit(out[43]), .inbit(in[43]), .clk(clk), .reset(reset));
	reg1bit reg45(.writeEn(writeEn),.outbit(out[44]), .inbit(in[44]), .clk(clk), .reset(reset));
	reg1bit reg46(.writeEn(writeEn),.outbit(out[45]), .inbit(in[45]), .clk(clk), .reset(reset));
	reg1bit reg47(.writeEn(writeEn),.outbit(out[46]), .inbit(in[46]), .clk(clk), .reset(reset));
	reg1bit reg48(.writeEn(writeEn),.outbit(out[47]), .inbit(in[47]), .clk(clk), .reset(reset));
	endmodule

module decode_register(writeEn, out, in, clk, reset);
	input writeEn, clk, reset;
	input [72:0] in;
	output [72:0] out;	
	reg1bit reg1(.writeEn(writeEn),.outbit(out[0]), .inbit(in[0]), .clk(clk), .reset(reset)); //instruction
	reg1bit reg2(.writeEn(writeEn),.outbit(out[1]), .inbit(in[1]), .clk(clk), .reset(reset));
	reg1bit reg3(.writeEn(writeEn),.outbit(out[2]), .inbit(in[2]), .clk(clk), .reset(reset));
	reg1bit reg4(.writeEn(writeEn),.outbit(out[3]), .inbit(in[3]), .clk(clk), .reset(reset));
	reg1bit reg5(.writeEn(writeEn),.outbit(out[4]), .inbit(in[4]), .clk(clk), .reset(reset));
	reg1bit reg6(.writeEn(writeEn),.outbit(out[5]), .inbit(in[5]), .clk(clk), .reset(reset));
	reg1bit reg7(.writeEn(writeEn),.outbit(out[6]), .inbit(in[6]), .clk(clk), .reset(reset));
	reg1bit reg8(.writeEn(writeEn),.outbit(out[7]), .inbit(in[7]), .clk(clk), .reset(reset));
	reg1bit reg9(.writeEn(writeEn),.outbit(out[8]), .inbit(in[8]), .clk(clk), .reset(reset));
	reg1bit reg10(.writeEn(writeEn),.outbit(out[9]), .inbit(in[9]), .clk(clk), .reset(reset));
	reg1bit reg11(.writeEn(writeEn),.outbit(out[10]), .inbit(in[10]), .clk(clk), .reset(reset));
	reg1bit reg12(.writeEn(writeEn),.outbit(out[11]), .inbit(in[11]), .clk(clk), .reset(reset));
	reg1bit reg13(.writeEn(writeEn),.outbit(out[12]), .inbit(in[12]), .clk(clk), .reset(reset));
	reg1bit reg14(.writeEn(writeEn),.outbit(out[13]), .inbit(in[13]), .clk(clk), .reset(reset));
	reg1bit reg15(.writeEn(writeEn),.outbit(out[14]), .inbit(in[14]), .clk(clk), .reset(reset));
	reg1bit reg16(.writeEn(writeEn),.outbit(out[15]), .inbit(in[15]), .clk(clk), .reset(reset));
	reg1bit reg17(.writeEn(writeEn),.outbit(out[16]), .inbit(in[16]), .clk(clk), .reset(reset));//pc
	reg1bit reg18(.writeEn(writeEn),.outbit(out[17]), .inbit(in[17]), .clk(clk), .reset(reset));
	reg1bit reg19(.writeEn(writeEn),.outbit(out[18]), .inbit(in[18]), .clk(clk), .reset(reset));
	reg1bit reg20(.writeEn(writeEn),.outbit(out[19]), .inbit(in[19]), .clk(clk), .reset(reset));
	reg1bit reg21(.writeEn(writeEn),.outbit(out[20]), .inbit(in[20]), .clk(clk), .reset(reset));
	reg1bit reg22(.writeEn(writeEn),.outbit(out[21]), .inbit(in[21]), .clk(clk), .reset(reset));
	reg1bit reg23(.writeEn(writeEn),.outbit(out[22]), .inbit(in[22]), .clk(clk), .reset(reset));
	reg1bit reg24(.writeEn(writeEn),.outbit(out[23]), .inbit(in[23]), .clk(clk), .reset(reset));
	reg1bit reg25(.writeEn(writeEn),.outbit(out[24]), .inbit(in[24]), .clk(clk), .reset(reset));
	reg1bit reg26(.writeEn(writeEn),.outbit(out[25]), .inbit(in[25]), .clk(clk), .reset(reset));
	reg1bit reg27(.writeEn(writeEn),.outbit(out[26]), .inbit(in[26]), .clk(clk), .reset(reset));
	reg1bit reg28(.writeEn(writeEn),.outbit(out[27]), .inbit(in[27]), .clk(clk), .reset(reset));
	reg1bit reg29(.writeEn(writeEn),.outbit(out[28]), .inbit(in[28]), .clk(clk), .reset(reset));
	reg1bit reg30(.writeEn(writeEn),.outbit(out[29]), .inbit(in[29]), .clk(clk), .reset(reset));
	reg1bit reg31(.writeEn(writeEn),.outbit(out[30]), .inbit(in[30]), .clk(clk), .reset(reset));
	reg1bit reg32(.writeEn(writeEn),.outbit(out[31]), .inbit(in[31]), .clk(clk), .reset(reset));
	reg1bit reg33(.writeEn(writeEn),.outbit(out[32]), .inbit(in[32]), .clk(clk), .reset(reset));// pc+1
	reg1bit reg34(.writeEn(writeEn),.outbit(out[33]), .inbit(in[33]), .clk(clk), .reset(reset));
	reg1bit reg35(.writeEn(writeEn),.outbit(out[34]), .inbit(in[34]), .clk(clk), .reset(reset));
	reg1bit reg36(.writeEn(writeEn),.outbit(out[35]), .inbit(in[35]), .clk(clk), .reset(reset));
	reg1bit reg37(.writeEn(writeEn),.outbit(out[36]), .inbit(in[36]), .clk(clk), .reset(reset));
	reg1bit reg38(.writeEn(writeEn),.outbit(out[37]), .inbit(in[37]), .clk(clk), .reset(reset));
	reg1bit reg39(.writeEn(writeEn),.outbit(out[38]), .inbit(in[38]), .clk(clk), .reset(reset));
	reg1bit reg40(.writeEn(writeEn),.outbit(out[39]), .inbit(in[39]), .clk(clk), .reset(reset));
	reg1bit reg41(.writeEn(writeEn),.outbit(out[40]), .inbit(in[40]), .clk(clk), .reset(reset));
	reg1bit reg42(.writeEn(writeEn),.outbit(out[41]), .inbit(in[41]), .clk(clk), .reset(reset));
	reg1bit reg43(.writeEn(writeEn),.outbit(out[42]), .inbit(in[42]), .clk(clk), .reset(reset));
	reg1bit reg44(.writeEn(writeEn),.outbit(out[43]), .inbit(in[43]), .clk(clk), .reset(reset));
	reg1bit reg45(.writeEn(writeEn),.outbit(out[44]), .inbit(in[44]), .clk(clk), .reset(reset));
	reg1bit reg46(.writeEn(writeEn),.outbit(out[45]), .inbit(in[45]), .clk(clk), .reset(reset));
	reg1bit reg47(.writeEn(writeEn),.outbit(out[46]), .inbit(in[46]), .clk(clk), .reset(reset));
	reg1bit reg48(.writeEn(writeEn),.outbit(out[47]), .inbit(in[47]), .clk(clk), .reset(reset));
	reg1bit reg49(.writeEn(writeEn),.outbit(out[48]), .inbit(in[48]), .clk(clk), .reset(reset));//lmsm regbits
	reg1bit reg50(.writeEn(writeEn),.outbit(out[49]), .inbit(in[49]), .clk(clk), .reset(reset));
	reg1bit reg51(.writeEn(writeEn),.outbit(out[50]), .inbit(in[50]), .clk(clk), .reset(reset));
	reg1bit reg52(.writeEn(writeEn),.outbit(out[51]), .inbit(in[51]), .clk(clk), .reset(reset));// control signals
	reg1bit reg53(.writeEn(writeEn),.outbit(out[52]), .inbit(in[52]), .clk(clk), .reset(reset));
	reg1bit reg54(.writeEn(writeEn),.outbit(out[53]), .inbit(in[53]), .clk(clk), .reset(reset));
	reg1bit reg55(.writeEn(writeEn),.outbit(out[54]), .inbit(in[54]), .clk(clk), .reset(reset));
	reg1bit reg56(.writeEn(writeEn),.outbit(out[55]), .inbit(in[55]), .clk(clk), .reset(reset));
	reg1bit reg57(.writeEn(writeEn),.outbit(out[56]), .inbit(in[56]), .clk(clk), .reset(reset));
	reg1bit reg58(.writeEn(writeEn),.outbit(out[57]), .inbit(in[57]), .clk(clk), .reset(reset));
	reg1bit reg59(.writeEn(writeEn),.outbit(out[58]), .inbit(in[58]), .clk(clk), .reset(reset));
	reg1bit reg60(.writeEn(writeEn),.outbit(out[59]), .inbit(in[59]), .clk(clk), .reset(reset));
	reg1bit reg61(.writeEn(writeEn),.outbit(out[60]), .inbit(in[60]), .clk(clk), .reset(reset));
	reg1bit reg62(.writeEn(writeEn),.outbit(out[61]), .inbit(in[61]), .clk(clk), .reset(reset));
	reg1bit reg63(.writeEn(writeEn),.outbit(out[62]), .inbit(in[62]), .clk(clk), .reset(reset));
	reg1bit reg64(.writeEn(writeEn),.outbit(out[63]), .inbit(in[63]), .clk(clk), .reset(reset));
	reg1bit reg65(.writeEn(writeEn),.outbit(out[64]), .inbit(in[64]), .clk(clk), .reset(reset));
	reg1bit reg66(.writeEn(writeEn),.outbit(out[65]), .inbit(in[65]), .clk(clk), .reset(reset));
	reg1bit reg67(.writeEn(writeEn),.outbit(out[66]), .inbit(in[66]), .clk(clk), .reset(reset));
	reg1bit reg68(.writeEn(writeEn),.outbit(out[67]), .inbit(in[67]), .clk(clk), .reset(reset));
	reg1bit reg69(.writeEn(writeEn),.outbit(out[68]), .inbit(in[68]), .clk(clk), .reset(reset));
	reg1bit reg70(.writeEn(writeEn),.outbit(out[69]), .inbit(in[69]), .clk(clk), .reset(reset));
	reg1bit reg71(.writeEn(writeEn),.outbit(out[70]), .inbit(in[70]), .clk(clk), .reset(reset));
	reg1bit reg72(.writeEn(writeEn),.outbit(out[71]), .inbit(in[71]), .clk(clk), .reset(reset));	
	reg1bit reg73(.writeEn(writeEn),.outbit(out[72]), .inbit(in[72]), .clk(clk), .reset(reset));	
	endmodule

	
module register_execute(writeEn, out, in, clk, reset);
	input writeEn, clk, reset;
	input [126:0] in;
	output [126:0] out;	
	reg1bit reg1(.writeEn(writeEn),.outbit(out[0]), .inbit(in[0]), .clk(clk), .reset(reset)); //instruction
	reg1bit reg2(.writeEn(writeEn),.outbit(out[1]), .inbit(in[1]), .clk(clk), .reset(reset));
	reg1bit reg3(.writeEn(writeEn),.outbit(out[2]), .inbit(in[2]), .clk(clk), .reset(reset));
	reg1bit reg4(.writeEn(writeEn),.outbit(out[3]), .inbit(in[3]), .clk(clk), .reset(reset));
	reg1bit reg5(.writeEn(writeEn),.outbit(out[4]), .inbit(in[4]), .clk(clk), .reset(reset));
	reg1bit reg6(.writeEn(writeEn),.outbit(out[5]), .inbit(in[5]), .clk(clk), .reset(reset));
	reg1bit reg7(.writeEn(writeEn),.outbit(out[6]), .inbit(in[6]), .clk(clk), .reset(reset));
	reg1bit reg8(.writeEn(writeEn),.outbit(out[7]), .inbit(in[7]), .clk(clk), .reset(reset));
	reg1bit reg9(.writeEn(writeEn),.outbit(out[8]), .inbit(in[8]), .clk(clk), .reset(reset));
	reg1bit reg10(.writeEn(writeEn),.outbit(out[9]), .inbit(in[9]), .clk(clk), .reset(reset));
	reg1bit reg11(.writeEn(writeEn),.outbit(out[10]), .inbit(in[10]), .clk(clk), .reset(reset));
	reg1bit reg12(.writeEn(writeEn),.outbit(out[11]), .inbit(in[11]), .clk(clk), .reset(reset));
	reg1bit reg13(.writeEn(writeEn),.outbit(out[12]), .inbit(in[12]), .clk(clk), .reset(reset));
	reg1bit reg14(.writeEn(writeEn),.outbit(out[13]), .inbit(in[13]), .clk(clk), .reset(reset));
	reg1bit reg15(.writeEn(writeEn),.outbit(out[14]), .inbit(in[14]), .clk(clk), .reset(reset));
	reg1bit reg16(.writeEn(writeEn),.outbit(out[15]), .inbit(in[15]), .clk(clk), .reset(reset));
	reg1bit reg17(.writeEn(writeEn),.outbit(out[16]), .inbit(in[16]), .clk(clk), .reset(reset));//pc
	reg1bit reg18(.writeEn(writeEn),.outbit(out[17]), .inbit(in[17]), .clk(clk), .reset(reset));
	reg1bit reg19(.writeEn(writeEn),.outbit(out[18]), .inbit(in[18]), .clk(clk), .reset(reset));
	reg1bit reg20(.writeEn(writeEn),.outbit(out[19]), .inbit(in[19]), .clk(clk), .reset(reset));
	reg1bit reg21(.writeEn(writeEn),.outbit(out[20]), .inbit(in[20]), .clk(clk), .reset(reset));
	reg1bit reg22(.writeEn(writeEn),.outbit(out[21]), .inbit(in[21]), .clk(clk), .reset(reset));
	reg1bit reg23(.writeEn(writeEn),.outbit(out[22]), .inbit(in[22]), .clk(clk), .reset(reset));
	reg1bit reg24(.writeEn(writeEn),.outbit(out[23]), .inbit(in[23]), .clk(clk), .reset(reset));
	reg1bit reg25(.writeEn(writeEn),.outbit(out[24]), .inbit(in[24]), .clk(clk), .reset(reset));
	reg1bit reg26(.writeEn(writeEn),.outbit(out[25]), .inbit(in[25]), .clk(clk), .reset(reset));
	reg1bit reg27(.writeEn(writeEn),.outbit(out[26]), .inbit(in[26]), .clk(clk), .reset(reset));
	reg1bit reg28(.writeEn(writeEn),.outbit(out[27]), .inbit(in[27]), .clk(clk), .reset(reset));
	reg1bit reg29(.writeEn(writeEn),.outbit(out[28]), .inbit(in[28]), .clk(clk), .reset(reset));
	reg1bit reg30(.writeEn(writeEn),.outbit(out[29]), .inbit(in[29]), .clk(clk), .reset(reset));
	reg1bit reg31(.writeEn(writeEn),.outbit(out[30]), .inbit(in[30]), .clk(clk), .reset(reset));
	reg1bit reg32(.writeEn(writeEn),.outbit(out[31]), .inbit(in[31]), .clk(clk), .reset(reset));
	reg1bit reg33(.writeEn(writeEn),.outbit(out[32]), .inbit(in[32]), .clk(clk), .reset(reset));// pc+1
	reg1bit reg34(.writeEn(writeEn),.outbit(out[33]), .inbit(in[33]), .clk(clk), .reset(reset));
	reg1bit reg35(.writeEn(writeEn),.outbit(out[34]), .inbit(in[34]), .clk(clk), .reset(reset));
	reg1bit reg36(.writeEn(writeEn),.outbit(out[35]), .inbit(in[35]), .clk(clk), .reset(reset));
	reg1bit reg37(.writeEn(writeEn),.outbit(out[36]), .inbit(in[36]), .clk(clk), .reset(reset));
	reg1bit reg38(.writeEn(writeEn),.outbit(out[37]), .inbit(in[37]), .clk(clk), .reset(reset));
	reg1bit reg39(.writeEn(writeEn),.outbit(out[38]), .inbit(in[38]), .clk(clk), .reset(reset));
	reg1bit reg40(.writeEn(writeEn),.outbit(out[39]), .inbit(in[39]), .clk(clk), .reset(reset));
	reg1bit reg41(.writeEn(writeEn),.outbit(out[40]), .inbit(in[40]), .clk(clk), .reset(reset));
	reg1bit reg42(.writeEn(writeEn),.outbit(out[41]), .inbit(in[41]), .clk(clk), .reset(reset));
	reg1bit reg43(.writeEn(writeEn),.outbit(out[42]), .inbit(in[42]), .clk(clk), .reset(reset));
	reg1bit reg44(.writeEn(writeEn),.outbit(out[43]), .inbit(in[43]), .clk(clk), .reset(reset));
	reg1bit reg45(.writeEn(writeEn),.outbit(out[44]), .inbit(in[44]), .clk(clk), .reset(reset));
	reg1bit reg46(.writeEn(writeEn),.outbit(out[45]), .inbit(in[45]), .clk(clk), .reset(reset));
	reg1bit reg47(.writeEn(writeEn),.outbit(out[46]), .inbit(in[46]), .clk(clk), .reset(reset));
	reg1bit reg48(.writeEn(writeEn),.outbit(out[47]), .inbit(in[47]), .clk(clk), .reset(reset));
	reg1bit reg49(.writeEn(writeEn),.outbit(out[48]), .inbit(in[48]), .clk(clk), .reset(reset));//lmsm regbits
	reg1bit reg50(.writeEn(writeEn),.outbit(out[49]), .inbit(in[49]), .clk(clk), .reset(reset));
	reg1bit reg51(.writeEn(writeEn),.outbit(out[50]), .inbit(in[50]), .clk(clk), .reset(reset));
	reg1bit reg52(.writeEn(writeEn),.outbit(out[51]), .inbit(in[51]), .clk(clk), .reset(reset));// control signals
	reg1bit reg53(.writeEn(writeEn),.outbit(out[52]), .inbit(in[52]), .clk(clk), .reset(reset));
	reg1bit reg54(.writeEn(writeEn),.outbit(out[53]), .inbit(in[53]), .clk(clk), .reset(reset));
	reg1bit reg55(.writeEn(writeEn),.outbit(out[54]), .inbit(in[54]), .clk(clk), .reset(reset));
	reg1bit reg56(.writeEn(writeEn),.outbit(out[55]), .inbit(in[55]), .clk(clk), .reset(reset));
	reg1bit reg57(.writeEn(writeEn),.outbit(out[56]), .inbit(in[56]), .clk(clk), .reset(reset));
	reg1bit reg58(.writeEn(writeEn),.outbit(out[57]), .inbit(in[57]), .clk(clk), .reset(reset));
	reg1bit reg59(.writeEn(writeEn),.outbit(out[58]), .inbit(in[58]), .clk(clk), .reset(reset));
	reg1bit reg60(.writeEn(writeEn),.outbit(out[59]), .inbit(in[59]), .clk(clk), .reset(reset));
	reg1bit reg61(.writeEn(writeEn),.outbit(out[60]), .inbit(in[60]), .clk(clk), .reset(reset));
	reg1bit reg62(.writeEn(writeEn),.outbit(out[61]), .inbit(in[61]), .clk(clk), .reset(reset));
	reg1bit reg63(.writeEn(writeEn),.outbit(out[62]), .inbit(in[62]), .clk(clk), .reset(reset));
	reg1bit reg64(.writeEn(writeEn),.outbit(out[63]), .inbit(in[63]), .clk(clk), .reset(reset));
	reg1bit reg65(.writeEn(writeEn),.outbit(out[64]), .inbit(in[64]), .clk(clk), .reset(reset));
	reg1bit reg66(.writeEn(writeEn),.outbit(out[65]), .inbit(in[65]), .clk(clk), .reset(reset));
	reg1bit reg67(.writeEn(writeEn),.outbit(out[66]), .inbit(in[66]), .clk(clk), .reset(reset));
	reg1bit reg68(.writeEn(writeEn),.outbit(out[67]), .inbit(in[67]), .clk(clk), .reset(reset));
	reg1bit reg69(.writeEn(writeEn),.outbit(out[68]), .inbit(in[68]), .clk(clk), .reset(reset));
	reg1bit reg70(.writeEn(writeEn),.outbit(out[69]), .inbit(in[69]), .clk(clk), .reset(reset));
	reg1bit reg71(.writeEn(writeEn),.outbit(out[70]), .inbit(in[70]), .clk(clk), .reset(reset));	
	reg1bit reg72(.writeEn(writeEn),.outbit(out[71]), .inbit(in[71]), .clk(clk), .reset(reset));
	reg1bit reg73(.writeEn(writeEn),.outbit(out[72]), .inbit(in[72]), .clk(clk), .reset(reset));
	reg1bit reg74(.writeEn(writeEn),.outbit(out[73]), .inbit(in[73]), .clk(clk), .reset(reset));// ra forwarding bits
	reg1bit reg75(.writeEn(writeEn),.outbit(out[74]), .inbit(in[74]), .clk(clk), .reset(reset));
	reg1bit reg76(.writeEn(writeEn),.outbit(out[75]), .inbit(in[75]), .clk(clk), .reset(reset));
	reg1bit reg77(.writeEn(writeEn),.outbit(out[76]), .inbit(in[76]), .clk(clk), .reset(reset));// rb forwarding bits	
	reg1bit reg78(.writeEn(writeEn),.outbit(out[77]), .inbit(in[77]), .clk(clk), .reset(reset));
	reg1bit reg79(.writeEn(writeEn),.outbit(out[78]), .inbit(in[78]), .clk(clk), .reset(reset));
	reg1bit reg80(.writeEn(writeEn),.outbit(out[79]), .inbit(in[79]), .clk(clk), .reset(reset));// extend zeros
	reg1bit reg81(.writeEn(writeEn),.outbit(out[80]), .inbit(in[80]), .clk(clk), .reset(reset));
	reg1bit reg82(.writeEn(writeEn),.outbit(out[81]), .inbit(in[81]), .clk(clk), .reset(reset));
	reg1bit reg83(.writeEn(writeEn),.outbit(out[82]), .inbit(in[82]), .clk(clk), .reset(reset));
	reg1bit reg84(.writeEn(writeEn),.outbit(out[83]), .inbit(in[83]), .clk(clk), .reset(reset));
	reg1bit reg85(.writeEn(writeEn),.outbit(out[84]), .inbit(in[84]), .clk(clk), .reset(reset));
	reg1bit reg86(.writeEn(writeEn),.outbit(out[85]), .inbit(in[85]), .clk(clk), .reset(reset));
	reg1bit reg87(.writeEn(writeEn),.outbit(out[86]), .inbit(in[86]), .clk(clk), .reset(reset));
	reg1bit reg88(.writeEn(writeEn),.outbit(out[87]), .inbit(in[87]), .clk(clk), .reset(reset));
	reg1bit reg89(.writeEn(writeEn),.outbit(out[88]), .inbit(in[88]), .clk(clk), .reset(reset));
	reg1bit reg90(.writeEn(writeEn),.outbit(out[89]), .inbit(in[89]), .clk(clk), .reset(reset));
	reg1bit reg91(.writeEn(writeEn),.outbit(out[90]), .inbit(in[90]), .clk(clk), .reset(reset));	
	reg1bit reg92(.writeEn(writeEn),.outbit(out[91]), .inbit(in[91]), .clk(clk), .reset(reset));
	reg1bit reg93(.writeEn(writeEn),.outbit(out[92]), .inbit(in[92]), .clk(clk), .reset(reset));
	reg1bit reg94(.writeEn(writeEn),.outbit(out[93]), .inbit(in[93]), .clk(clk), .reset(reset));
	reg1bit reg95(.writeEn(writeEn),.outbit(out[94]), .inbit(in[94]), .clk(clk), .reset(reset));
	reg1bit reg96(.writeEn(writeEn),.outbit(out[95]), .inbit(in[95]), .clk(clk), .reset(reset));// Ra
	reg1bit reg97(.writeEn(writeEn),.outbit(out[96]), .inbit(in[96]), .clk(clk), .reset(reset));	
	reg1bit reg98(.writeEn(writeEn),.outbit(out[97]), .inbit(in[97]), .clk(clk), .reset(reset));
	reg1bit reg99(.writeEn(writeEn),.outbit(out[98]), .inbit(in[98]), .clk(clk), .reset(reset));
	reg1bit reg100(.writeEn(writeEn),.outbit(out[99]), .inbit(in[99]), .clk(clk), .reset(reset));
	reg1bit reg101(.writeEn(writeEn),.outbit(out[100]), .inbit(in[100]), .clk(clk), .reset(reset));
	reg1bit reg102(.writeEn(writeEn),.outbit(out[101]), .inbit(in[101]), .clk(clk), .reset(reset));
	reg1bit reg103(.writeEn(writeEn),.outbit(out[102]), .inbit(in[102]), .clk(clk), .reset(reset));
	reg1bit reg104(.writeEn(writeEn),.outbit(out[103]), .inbit(in[103]), .clk(clk), .reset(reset));
	reg1bit reg105(.writeEn(writeEn),.outbit(out[104]), .inbit(in[104]), .clk(clk), .reset(reset));
	reg1bit reg106(.writeEn(writeEn),.outbit(out[105]), .inbit(in[105]), .clk(clk), .reset(reset));
	reg1bit reg107(.writeEn(writeEn),.outbit(out[106]), .inbit(in[106]), .clk(clk), .reset(reset));
	reg1bit reg108(.writeEn(writeEn),.outbit(out[107]), .inbit(in[107]), .clk(clk), .reset(reset));
	reg1bit reg109(.writeEn(writeEn),.outbit(out[108]), .inbit(in[108]), .clk(clk), .reset(reset));
	reg1bit reg110(.writeEn(writeEn),.outbit(out[109]), .inbit(in[109]), .clk(clk), .reset(reset));
	reg1bit reg111(.writeEn(writeEn),.outbit(out[110]), .inbit(in[110]), .clk(clk), .reset(reset));	
	reg1bit reg112(.writeEn(writeEn),.outbit(out[111]), .inbit(in[111]), .clk(clk), .reset(reset));// Rb
	reg1bit reg113(.writeEn(writeEn),.outbit(out[112]), .inbit(in[112]), .clk(clk), .reset(reset));
	reg1bit reg114(.writeEn(writeEn),.outbit(out[113]), .inbit(in[113]), .clk(clk), .reset(reset));
	reg1bit reg115(.writeEn(writeEn),.outbit(out[114]), .inbit(in[114]), .clk(clk), .reset(reset));
	reg1bit reg116(.writeEn(writeEn),.outbit(out[115]), .inbit(in[115]), .clk(clk), .reset(reset));
	reg1bit reg117(.writeEn(writeEn),.outbit(out[116]), .inbit(in[116]), .clk(clk), .reset(reset));	
	reg1bit reg118(.writeEn(writeEn),.outbit(out[117]), .inbit(in[117]), .clk(clk), .reset(reset));
	reg1bit reg119(.writeEn(writeEn),.outbit(out[118]), .inbit(in[118]), .clk(clk), .reset(reset));
	reg1bit reg120(.writeEn(writeEn),.outbit(out[119]), .inbit(in[119]), .clk(clk), .reset(reset));
	reg1bit reg121(.writeEn(writeEn),.outbit(out[120]), .inbit(in[120]), .clk(clk), .reset(reset));
	reg1bit reg122(.writeEn(writeEn),.outbit(out[121]), .inbit(in[121]), .clk(clk), .reset(reset));
	reg1bit reg123(.writeEn(writeEn),.outbit(out[122]), .inbit(in[122]), .clk(clk), .reset(reset));
	reg1bit reg124(.writeEn(writeEn),.outbit(out[123]), .inbit(in[123]), .clk(clk), .reset(reset));
	reg1bit reg125(.writeEn(writeEn),.outbit(out[124]), .inbit(in[124]), .clk(clk), .reset(reset));
	reg1bit reg126(.writeEn(writeEn),.outbit(out[125]), .inbit(in[125]), .clk(clk), .reset(reset));
	reg1bit reg127(.writeEn(writeEn),.outbit(out[126]), .inbit(in[126]), .clk(clk), .reset(reset));

	endmodule

module execute_memory (writeEn, out, in, clk, reset);
	input writeEn, clk, reset;
	input [128:0] in;
	output [128:0] out;	
	reg1bit reg1(.writeEn(writeEn),.outbit(out[0]), .inbit(in[0]), .clk(clk), .reset(reset)); //instruction
	reg1bit reg2(.writeEn(writeEn),.outbit(out[1]), .inbit(in[1]), .clk(clk), .reset(reset));
	reg1bit reg3(.writeEn(writeEn),.outbit(out[2]), .inbit(in[2]), .clk(clk), .reset(reset));
	reg1bit reg4(.writeEn(writeEn),.outbit(out[3]), .inbit(in[3]), .clk(clk), .reset(reset));
	reg1bit reg5(.writeEn(writeEn),.outbit(out[4]), .inbit(in[4]), .clk(clk), .reset(reset));
	reg1bit reg6(.writeEn(writeEn),.outbit(out[5]), .inbit(in[5]), .clk(clk), .reset(reset));
	reg1bit reg7(.writeEn(writeEn),.outbit(out[6]), .inbit(in[6]), .clk(clk), .reset(reset));
	reg1bit reg8(.writeEn(writeEn),.outbit(out[7]), .inbit(in[7]), .clk(clk), .reset(reset));
	reg1bit reg9(.writeEn(writeEn),.outbit(out[8]), .inbit(in[8]), .clk(clk), .reset(reset));
	reg1bit reg10(.writeEn(writeEn),.outbit(out[9]), .inbit(in[9]), .clk(clk), .reset(reset));
	reg1bit reg11(.writeEn(writeEn),.outbit(out[10]), .inbit(in[10]), .clk(clk), .reset(reset));
	reg1bit reg12(.writeEn(writeEn),.outbit(out[11]), .inbit(in[11]), .clk(clk), .reset(reset));
	reg1bit reg13(.writeEn(writeEn),.outbit(out[12]), .inbit(in[12]), .clk(clk), .reset(reset));
	reg1bit reg14(.writeEn(writeEn),.outbit(out[13]), .inbit(in[13]), .clk(clk), .reset(reset));
	reg1bit reg15(.writeEn(writeEn),.outbit(out[14]), .inbit(in[14]), .clk(clk), .reset(reset));
	reg1bit reg16(.writeEn(writeEn),.outbit(out[15]), .inbit(in[15]), .clk(clk), .reset(reset));
	reg1bit reg17(.writeEn(writeEn),.outbit(out[16]), .inbit(in[16]), .clk(clk), .reset(reset));//pc
	reg1bit reg18(.writeEn(writeEn),.outbit(out[17]), .inbit(in[17]), .clk(clk), .reset(reset));
	reg1bit reg19(.writeEn(writeEn),.outbit(out[18]), .inbit(in[18]), .clk(clk), .reset(reset));
	reg1bit reg20(.writeEn(writeEn),.outbit(out[19]), .inbit(in[19]), .clk(clk), .reset(reset));
	reg1bit reg21(.writeEn(writeEn),.outbit(out[20]), .inbit(in[20]), .clk(clk), .reset(reset));
	reg1bit reg22(.writeEn(writeEn),.outbit(out[21]), .inbit(in[21]), .clk(clk), .reset(reset));
	reg1bit reg23(.writeEn(writeEn),.outbit(out[22]), .inbit(in[22]), .clk(clk), .reset(reset));
	reg1bit reg24(.writeEn(writeEn),.outbit(out[23]), .inbit(in[23]), .clk(clk), .reset(reset));
	reg1bit reg25(.writeEn(writeEn),.outbit(out[24]), .inbit(in[24]), .clk(clk), .reset(reset));
	reg1bit reg26(.writeEn(writeEn),.outbit(out[25]), .inbit(in[25]), .clk(clk), .reset(reset));
	reg1bit reg27(.writeEn(writeEn),.outbit(out[26]), .inbit(in[26]), .clk(clk), .reset(reset));
	reg1bit reg28(.writeEn(writeEn),.outbit(out[27]), .inbit(in[27]), .clk(clk), .reset(reset));
	reg1bit reg29(.writeEn(writeEn),.outbit(out[28]), .inbit(in[28]), .clk(clk), .reset(reset));
	reg1bit reg30(.writeEn(writeEn),.outbit(out[29]), .inbit(in[29]), .clk(clk), .reset(reset));
	reg1bit reg31(.writeEn(writeEn),.outbit(out[30]), .inbit(in[30]), .clk(clk), .reset(reset));
	reg1bit reg32(.writeEn(writeEn),.outbit(out[31]), .inbit(in[31]), .clk(clk), .reset(reset));
	reg1bit reg33(.writeEn(writeEn),.outbit(out[32]), .inbit(in[32]), .clk(clk), .reset(reset));// pc+1
	reg1bit reg34(.writeEn(writeEn),.outbit(out[33]), .inbit(in[33]), .clk(clk), .reset(reset));
	reg1bit reg35(.writeEn(writeEn),.outbit(out[34]), .inbit(in[34]), .clk(clk), .reset(reset));
	reg1bit reg36(.writeEn(writeEn),.outbit(out[35]), .inbit(in[35]), .clk(clk), .reset(reset));
	reg1bit reg37(.writeEn(writeEn),.outbit(out[36]), .inbit(in[36]), .clk(clk), .reset(reset));
	reg1bit reg38(.writeEn(writeEn),.outbit(out[37]), .inbit(in[37]), .clk(clk), .reset(reset));
	reg1bit reg39(.writeEn(writeEn),.outbit(out[38]), .inbit(in[38]), .clk(clk), .reset(reset));
	reg1bit reg40(.writeEn(writeEn),.outbit(out[39]), .inbit(in[39]), .clk(clk), .reset(reset));
	reg1bit reg41(.writeEn(writeEn),.outbit(out[40]), .inbit(in[40]), .clk(clk), .reset(reset));
	reg1bit reg42(.writeEn(writeEn),.outbit(out[41]), .inbit(in[41]), .clk(clk), .reset(reset));
	reg1bit reg43(.writeEn(writeEn),.outbit(out[42]), .inbit(in[42]), .clk(clk), .reset(reset));
	reg1bit reg44(.writeEn(writeEn),.outbit(out[43]), .inbit(in[43]), .clk(clk), .reset(reset));
	reg1bit reg45(.writeEn(writeEn),.outbit(out[44]), .inbit(in[44]), .clk(clk), .reset(reset));
	reg1bit reg46(.writeEn(writeEn),.outbit(out[45]), .inbit(in[45]), .clk(clk), .reset(reset));
	reg1bit reg47(.writeEn(writeEn),.outbit(out[46]), .inbit(in[46]), .clk(clk), .reset(reset));
	reg1bit reg48(.writeEn(writeEn),.outbit(out[47]), .inbit(in[47]), .clk(clk), .reset(reset));
	reg1bit reg49(.writeEn(writeEn),.outbit(out[48]), .inbit(in[48]), .clk(clk), .reset(reset));//lmsm regbits
	reg1bit reg50(.writeEn(writeEn),.outbit(out[49]), .inbit(in[49]), .clk(clk), .reset(reset));
	reg1bit reg51(.writeEn(writeEn),.outbit(out[50]), .inbit(in[50]), .clk(clk), .reset(reset));
	reg1bit reg52(.writeEn(writeEn),.outbit(out[51]), .inbit(in[51]), .clk(clk), .reset(reset));// control signals
	reg1bit reg53(.writeEn(writeEn),.outbit(out[52]), .inbit(in[52]), .clk(clk), .reset(reset));
	reg1bit reg54(.writeEn(writeEn),.outbit(out[53]), .inbit(in[53]), .clk(clk), .reset(reset));
	reg1bit reg55(.writeEn(writeEn),.outbit(out[54]), .inbit(in[54]), .clk(clk), .reset(reset));
	reg1bit reg56(.writeEn(writeEn),.outbit(out[55]), .inbit(in[55]), .clk(clk), .reset(reset));
	reg1bit reg57(.writeEn(writeEn),.outbit(out[56]), .inbit(in[56]), .clk(clk), .reset(reset));
	reg1bit reg58(.writeEn(writeEn),.outbit(out[57]), .inbit(in[57]), .clk(clk), .reset(reset));
	reg1bit reg59(.writeEn(writeEn),.outbit(out[58]), .inbit(in[58]), .clk(clk), .reset(reset));
	reg1bit reg60(.writeEn(writeEn),.outbit(out[59]), .inbit(in[59]), .clk(clk), .reset(reset));
	reg1bit reg61(.writeEn(writeEn),.outbit(out[60]), .inbit(in[60]), .clk(clk), .reset(reset));
	reg1bit reg62(.writeEn(writeEn),.outbit(out[61]), .inbit(in[61]), .clk(clk), .reset(reset));
	reg1bit reg63(.writeEn(writeEn),.outbit(out[62]), .inbit(in[62]), .clk(clk), .reset(reset));
	reg1bit reg64(.writeEn(writeEn),.outbit(out[63]), .inbit(in[63]), .clk(clk), .reset(reset));
	reg1bit reg65(.writeEn(writeEn),.outbit(out[64]), .inbit(in[64]), .clk(clk), .reset(reset));
	reg1bit reg66(.writeEn(writeEn),.outbit(out[65]), .inbit(in[65]), .clk(clk), .reset(reset));
	reg1bit reg67(.writeEn(writeEn),.outbit(out[66]), .inbit(in[66]), .clk(clk), .reset(reset));
	reg1bit reg68(.writeEn(writeEn),.outbit(out[67]), .inbit(in[67]), .clk(clk), .reset(reset));
	reg1bit reg69(.writeEn(writeEn),.outbit(out[68]), .inbit(in[68]), .clk(clk), .reset(reset));
	reg1bit reg70(.writeEn(writeEn),.outbit(out[69]), .inbit(in[69]), .clk(clk), .reset(reset));
	reg1bit reg71(.writeEn(writeEn),.outbit(out[70]), .inbit(in[70]), .clk(clk), .reset(reset));	
	reg1bit reg72(.writeEn(writeEn),.outbit(out[71]), .inbit(in[71]), .clk(clk), .reset(reset));
	reg1bit reg73(.writeEn(writeEn),.outbit(out[72]), .inbit(in[72]), .clk(clk), .reset(reset));
	reg1bit reg74(.writeEn(writeEn),.outbit(out[73]), .inbit(in[73]), .clk(clk), .reset(reset));// extend zeros
	reg1bit reg75(.writeEn(writeEn),.outbit(out[74]), .inbit(in[74]), .clk(clk), .reset(reset));
	reg1bit reg76(.writeEn(writeEn),.outbit(out[75]), .inbit(in[75]), .clk(clk), .reset(reset));
	reg1bit reg77(.writeEn(writeEn),.outbit(out[76]), .inbit(in[76]), .clk(clk), .reset(reset));	
	reg1bit reg78(.writeEn(writeEn),.outbit(out[77]), .inbit(in[77]), .clk(clk), .reset(reset));
	reg1bit reg79(.writeEn(writeEn),.outbit(out[78]), .inbit(in[78]), .clk(clk), .reset(reset));
	reg1bit reg80(.writeEn(writeEn),.outbit(out[79]), .inbit(in[79]), .clk(clk), .reset(reset));
	reg1bit reg81(.writeEn(writeEn),.outbit(out[80]), .inbit(in[80]), .clk(clk), .reset(reset));
	reg1bit reg82(.writeEn(writeEn),.outbit(out[81]), .inbit(in[81]), .clk(clk), .reset(reset));
	reg1bit reg83(.writeEn(writeEn),.outbit(out[82]), .inbit(in[82]), .clk(clk), .reset(reset));
	reg1bit reg84(.writeEn(writeEn),.outbit(out[83]), .inbit(in[83]), .clk(clk), .reset(reset));
	reg1bit reg85(.writeEn(writeEn),.outbit(out[84]), .inbit(in[84]), .clk(clk), .reset(reset));
	reg1bit reg86(.writeEn(writeEn),.outbit(out[85]), .inbit(in[85]), .clk(clk), .reset(reset));
	reg1bit reg87(.writeEn(writeEn),.outbit(out[86]), .inbit(in[86]), .clk(clk), .reset(reset));
	reg1bit reg88(.writeEn(writeEn),.outbit(out[87]), .inbit(in[87]), .clk(clk), .reset(reset));
	reg1bit reg89(.writeEn(writeEn),.outbit(out[88]), .inbit(in[88]), .clk(clk), .reset(reset));
	reg1bit reg90(.writeEn(writeEn),.outbit(out[89]), .inbit(in[89]), .clk(clk), .reset(reset));// alu_out
	reg1bit reg91(.writeEn(writeEn),.outbit(out[90]), .inbit(in[90]), .clk(clk), .reset(reset));	
	reg1bit reg92(.writeEn(writeEn),.outbit(out[91]), .inbit(in[91]), .clk(clk), .reset(reset));
	reg1bit reg93(.writeEn(writeEn),.outbit(out[92]), .inbit(in[92]), .clk(clk), .reset(reset));
	reg1bit reg94(.writeEn(writeEn),.outbit(out[93]), .inbit(in[93]), .clk(clk), .reset(reset));
	reg1bit reg95(.writeEn(writeEn),.outbit(out[94]), .inbit(in[94]), .clk(clk), .reset(reset));
	reg1bit reg96(.writeEn(writeEn),.outbit(out[95]), .inbit(in[95]), .clk(clk), .reset(reset));
	reg1bit reg97(.writeEn(writeEn),.outbit(out[96]), .inbit(in[96]), .clk(clk), .reset(reset));	
	reg1bit reg98(.writeEn(writeEn),.outbit(out[97]), .inbit(in[97]), .clk(clk), .reset(reset));
	reg1bit reg99(.writeEn(writeEn),.outbit(out[98]), .inbit(in[98]), .clk(clk), .reset(reset));
	reg1bit reg100(.writeEn(writeEn),.outbit(out[99]), .inbit(in[99]), .clk(clk), .reset(reset));
	reg1bit reg101(.writeEn(writeEn),.outbit(out[100]), .inbit(in[100]), .clk(clk), .reset(reset));
	reg1bit reg102(.writeEn(writeEn),.outbit(out[101]), .inbit(in[101]), .clk(clk), .reset(reset));
	reg1bit reg103(.writeEn(writeEn),.outbit(out[102]), .inbit(in[102]), .clk(clk), .reset(reset));
	reg1bit reg104(.writeEn(writeEn),.outbit(out[103]), .inbit(in[103]), .clk(clk), .reset(reset));
	reg1bit reg105(.writeEn(writeEn),.outbit(out[104]), .inbit(in[104]), .clk(clk), .reset(reset));
	reg1bit reg106(.writeEn(writeEn),.outbit(out[105]), .inbit(in[105]), .clk(clk), .reset(reset));// Rb
	reg1bit reg107(.writeEn(writeEn),.outbit(out[106]), .inbit(in[106]), .clk(clk), .reset(reset));
	reg1bit reg108(.writeEn(writeEn),.outbit(out[107]), .inbit(in[107]), .clk(clk), .reset(reset));
	reg1bit reg109(.writeEn(writeEn),.outbit(out[108]), .inbit(in[108]), .clk(clk), .reset(reset));
	reg1bit reg110(.writeEn(writeEn),.outbit(out[109]), .inbit(in[109]), .clk(clk), .reset(reset));
	reg1bit reg111(.writeEn(writeEn),.outbit(out[110]), .inbit(in[110]), .clk(clk), .reset(reset));	
	reg1bit reg112(.writeEn(writeEn),.outbit(out[111]), .inbit(in[111]), .clk(clk), .reset(reset));
	reg1bit reg113(.writeEn(writeEn),.outbit(out[112]), .inbit(in[112]), .clk(clk), .reset(reset));
	reg1bit reg114(.writeEn(writeEn),.outbit(out[113]), .inbit(in[113]), .clk(clk), .reset(reset)); 
	reg1bit reg115(.writeEn(writeEn),.outbit(out[114]), .inbit(in[114]), .clk(clk), .reset(reset));
	reg1bit reg116(.writeEn(writeEn),.outbit(out[115]), .inbit(in[115]), .clk(clk), .reset(reset));
	reg1bit reg117(.writeEn(writeEn),.outbit(out[116]), .inbit(in[116]), .clk(clk), .reset(reset));	
	reg1bit reg118(.writeEn(writeEn),.outbit(out[117]), .inbit(in[117]), .clk(clk), .reset(reset));
	reg1bit reg119(.writeEn(writeEn),.outbit(out[118]), .inbit(in[118]), .clk(clk), .reset(reset));
	reg1bit reg120(.writeEn(writeEn),.outbit(out[119]), .inbit(in[119]), .clk(clk), .reset(reset));
	reg1bit reg121(.writeEn(writeEn),.outbit(out[120]), .inbit(in[120]), .clk(clk), .reset(reset));
//	reg1bit reg122(.writeEn(writeEn),.outbit(out[121]), .inbit(in[121]), .clk(clk), .reset(reset));
//	reg1bit reg123(.writeEn(writeEn),.outbit(out[122]), .inbit(in[122]), .clk(clk), .reset(reset));
//	reg1bit reg124(.writeEn(writeEn),.outbit(out[123]), .inbit(in[123]), .clk(clk), .reset(reset));
//	reg1bit reg125(.writeEn(writeEn),.outbit(out[124]), .inbit(in[124]), .clk(clk), .reset(reset));
//	reg1bit reg126(.writeEn(writeEn),.outbit(out[125]), .inbit(in[125]), .clk(clk), .reset(reset));
//	reg1bit reg127(.writeEn(writeEn),.outbit(out[126]), .inbit(in[126]), .clk(clk), .reset(reset));
//	reg1bit reg128(.writeEn(writeEn),.outbit(out[127]), .inbit(in[127]), .clk(clk), .reset(reset));
//	reg1bit reg129(.writeEn(writeEn),.outbit(out[128]), .inbit(in[128]), .clk(clk), .reset(reset));

	endmodule
	
module memory_writeback (writeEn, out, in, clk, reset);
	input writeEn, clk, reset;
	input [136:0] in;
	output [136:0] out;	
	reg1bit reg1(.writeEn(writeEn),.outbit(out[0]), .inbit(in[0]), .clk(clk), .reset(reset)); //instruction
	reg1bit reg2(.writeEn(writeEn),.outbit(out[1]), .inbit(in[1]), .clk(clk), .reset(reset));
	reg1bit reg3(.writeEn(writeEn),.outbit(out[2]), .inbit(in[2]), .clk(clk), .reset(reset));
	reg1bit reg4(.writeEn(writeEn),.outbit(out[3]), .inbit(in[3]), .clk(clk), .reset(reset));
	reg1bit reg5(.writeEn(writeEn),.outbit(out[4]), .inbit(in[4]), .clk(clk), .reset(reset));
	reg1bit reg6(.writeEn(writeEn),.outbit(out[5]), .inbit(in[5]), .clk(clk), .reset(reset));
	reg1bit reg7(.writeEn(writeEn),.outbit(out[6]), .inbit(in[6]), .clk(clk), .reset(reset));
	reg1bit reg8(.writeEn(writeEn),.outbit(out[7]), .inbit(in[7]), .clk(clk), .reset(reset));
	reg1bit reg9(.writeEn(writeEn),.outbit(out[8]), .inbit(in[8]), .clk(clk), .reset(reset));
	reg1bit reg10(.writeEn(writeEn),.outbit(out[9]), .inbit(in[9]), .clk(clk), .reset(reset));
	reg1bit reg11(.writeEn(writeEn),.outbit(out[10]), .inbit(in[10]), .clk(clk), .reset(reset));
	reg1bit reg12(.writeEn(writeEn),.outbit(out[11]), .inbit(in[11]), .clk(clk), .reset(reset));
	reg1bit reg13(.writeEn(writeEn),.outbit(out[12]), .inbit(in[12]), .clk(clk), .reset(reset));
	reg1bit reg14(.writeEn(writeEn),.outbit(out[13]), .inbit(in[13]), .clk(clk), .reset(reset));
	reg1bit reg15(.writeEn(writeEn),.outbit(out[14]), .inbit(in[14]), .clk(clk), .reset(reset));
	reg1bit reg16(.writeEn(writeEn),.outbit(out[15]), .inbit(in[15]), .clk(clk), .reset(reset));
	reg1bit reg17(.writeEn(writeEn),.outbit(out[16]), .inbit(in[16]), .clk(clk), .reset(reset));//pc
	reg1bit reg18(.writeEn(writeEn),.outbit(out[17]), .inbit(in[17]), .clk(clk), .reset(reset));
	reg1bit reg19(.writeEn(writeEn),.outbit(out[18]), .inbit(in[18]), .clk(clk), .reset(reset));
	reg1bit reg20(.writeEn(writeEn),.outbit(out[19]), .inbit(in[19]), .clk(clk), .reset(reset));
	reg1bit reg21(.writeEn(writeEn),.outbit(out[20]), .inbit(in[20]), .clk(clk), .reset(reset));
	reg1bit reg22(.writeEn(writeEn),.outbit(out[21]), .inbit(in[21]), .clk(clk), .reset(reset));
	reg1bit reg23(.writeEn(writeEn),.outbit(out[22]), .inbit(in[22]), .clk(clk), .reset(reset));
	reg1bit reg24(.writeEn(writeEn),.outbit(out[23]), .inbit(in[23]), .clk(clk), .reset(reset));
	reg1bit reg25(.writeEn(writeEn),.outbit(out[24]), .inbit(in[24]), .clk(clk), .reset(reset));
	reg1bit reg26(.writeEn(writeEn),.outbit(out[25]), .inbit(in[25]), .clk(clk), .reset(reset));
	reg1bit reg27(.writeEn(writeEn),.outbit(out[26]), .inbit(in[26]), .clk(clk), .reset(reset));
	reg1bit reg28(.writeEn(writeEn),.outbit(out[27]), .inbit(in[27]), .clk(clk), .reset(reset));
	reg1bit reg29(.writeEn(writeEn),.outbit(out[28]), .inbit(in[28]), .clk(clk), .reset(reset));
	reg1bit reg30(.writeEn(writeEn),.outbit(out[29]), .inbit(in[29]), .clk(clk), .reset(reset));
	reg1bit reg31(.writeEn(writeEn),.outbit(out[30]), .inbit(in[30]), .clk(clk), .reset(reset));
	reg1bit reg32(.writeEn(writeEn),.outbit(out[31]), .inbit(in[31]), .clk(clk), .reset(reset));
	reg1bit reg33(.writeEn(writeEn),.outbit(out[32]), .inbit(in[32]), .clk(clk), .reset(reset));// pc+1
	reg1bit reg34(.writeEn(writeEn),.outbit(out[33]), .inbit(in[33]), .clk(clk), .reset(reset));
	reg1bit reg35(.writeEn(writeEn),.outbit(out[34]), .inbit(in[34]), .clk(clk), .reset(reset));
	reg1bit reg36(.writeEn(writeEn),.outbit(out[35]), .inbit(in[35]), .clk(clk), .reset(reset));
	reg1bit reg37(.writeEn(writeEn),.outbit(out[36]), .inbit(in[36]), .clk(clk), .reset(reset));
	reg1bit reg38(.writeEn(writeEn),.outbit(out[37]), .inbit(in[37]), .clk(clk), .reset(reset));
	reg1bit reg39(.writeEn(writeEn),.outbit(out[38]), .inbit(in[38]), .clk(clk), .reset(reset));
	reg1bit reg40(.writeEn(writeEn),.outbit(out[39]), .inbit(in[39]), .clk(clk), .reset(reset));
	reg1bit reg41(.writeEn(writeEn),.outbit(out[40]), .inbit(in[40]), .clk(clk), .reset(reset));
	reg1bit reg42(.writeEn(writeEn),.outbit(out[41]), .inbit(in[41]), .clk(clk), .reset(reset));
	reg1bit reg43(.writeEn(writeEn),.outbit(out[42]), .inbit(in[42]), .clk(clk), .reset(reset));
	reg1bit reg44(.writeEn(writeEn),.outbit(out[43]), .inbit(in[43]), .clk(clk), .reset(reset));
	reg1bit reg45(.writeEn(writeEn),.outbit(out[44]), .inbit(in[44]), .clk(clk), .reset(reset));
	reg1bit reg46(.writeEn(writeEn),.outbit(out[45]), .inbit(in[45]), .clk(clk), .reset(reset));
	reg1bit reg47(.writeEn(writeEn),.outbit(out[46]), .inbit(in[46]), .clk(clk), .reset(reset));
	reg1bit reg48(.writeEn(writeEn),.outbit(out[47]), .inbit(in[47]), .clk(clk), .reset(reset));
	reg1bit reg49(.writeEn(writeEn),.outbit(out[48]), .inbit(in[48]), .clk(clk), .reset(reset));//lmsm regbits
	reg1bit reg50(.writeEn(writeEn),.outbit(out[49]), .inbit(in[49]), .clk(clk), .reset(reset));
	reg1bit reg51(.writeEn(writeEn),.outbit(out[50]), .inbit(in[50]), .clk(clk), .reset(reset));
	reg1bit reg52(.writeEn(writeEn),.outbit(out[51]), .inbit(in[51]), .clk(clk), .reset(reset));// control signals
	reg1bit reg53(.writeEn(writeEn),.outbit(out[52]), .inbit(in[52]), .clk(clk), .reset(reset));
	reg1bit reg54(.writeEn(writeEn),.outbit(out[53]), .inbit(in[53]), .clk(clk), .reset(reset));
	reg1bit reg55(.writeEn(writeEn),.outbit(out[54]), .inbit(in[54]), .clk(clk), .reset(reset));
	reg1bit reg56(.writeEn(writeEn),.outbit(out[55]), .inbit(in[55]), .clk(clk), .reset(reset));
	reg1bit reg57(.writeEn(writeEn),.outbit(out[56]), .inbit(in[56]), .clk(clk), .reset(reset));
	reg1bit reg58(.writeEn(writeEn),.outbit(out[57]), .inbit(in[57]), .clk(clk), .reset(reset));
	reg1bit reg59(.writeEn(writeEn),.outbit(out[58]), .inbit(in[58]), .clk(clk), .reset(reset));
	reg1bit reg60(.writeEn(writeEn),.outbit(out[59]), .inbit(in[59]), .clk(clk), .reset(reset));
	reg1bit reg61(.writeEn(writeEn),.outbit(out[60]), .inbit(in[60]), .clk(clk), .reset(reset));
	reg1bit reg62(.writeEn(writeEn),.outbit(out[61]), .inbit(in[61]), .clk(clk), .reset(reset));
	reg1bit reg63(.writeEn(writeEn),.outbit(out[62]), .inbit(in[62]), .clk(clk), .reset(reset));
	reg1bit reg64(.writeEn(writeEn),.outbit(out[63]), .inbit(in[63]), .clk(clk), .reset(reset));
	reg1bit reg65(.writeEn(writeEn),.outbit(out[64]), .inbit(in[64]), .clk(clk), .reset(reset));
	reg1bit reg66(.writeEn(writeEn),.outbit(out[65]), .inbit(in[65]), .clk(clk), .reset(reset));
	reg1bit reg67(.writeEn(writeEn),.outbit(out[66]), .inbit(in[66]), .clk(clk), .reset(reset));
	reg1bit reg68(.writeEn(writeEn),.outbit(out[67]), .inbit(in[67]), .clk(clk), .reset(reset));
	reg1bit reg69(.writeEn(writeEn),.outbit(out[68]), .inbit(in[68]), .clk(clk), .reset(reset));
	reg1bit reg70(.writeEn(writeEn),.outbit(out[69]), .inbit(in[69]), .clk(clk), .reset(reset));
	reg1bit reg71(.writeEn(writeEn),.outbit(out[70]), .inbit(in[70]), .clk(clk), .reset(reset));	
	reg1bit reg72(.writeEn(writeEn),.outbit(out[71]), .inbit(in[71]), .clk(clk), .reset(reset));
	reg1bit reg73(.writeEn(writeEn),.outbit(out[72]), .inbit(in[72]), .clk(clk), .reset(reset));
	reg1bit reg74(.writeEn(writeEn),.outbit(out[73]), .inbit(in[73]), .clk(clk), .reset(reset));// extend zeros
	reg1bit reg75(.writeEn(writeEn),.outbit(out[74]), .inbit(in[74]), .clk(clk), .reset(reset));
	reg1bit reg76(.writeEn(writeEn),.outbit(out[75]), .inbit(in[75]), .clk(clk), .reset(reset));
	reg1bit reg77(.writeEn(writeEn),.outbit(out[76]), .inbit(in[76]), .clk(clk), .reset(reset));	
	reg1bit reg78(.writeEn(writeEn),.outbit(out[77]), .inbit(in[77]), .clk(clk), .reset(reset));
	reg1bit reg79(.writeEn(writeEn),.outbit(out[78]), .inbit(in[78]), .clk(clk), .reset(reset));
	reg1bit reg80(.writeEn(writeEn),.outbit(out[79]), .inbit(in[79]), .clk(clk), .reset(reset));
	reg1bit reg81(.writeEn(writeEn),.outbit(out[80]), .inbit(in[80]), .clk(clk), .reset(reset));
	reg1bit reg82(.writeEn(writeEn),.outbit(out[81]), .inbit(in[81]), .clk(clk), .reset(reset));
	reg1bit reg83(.writeEn(writeEn),.outbit(out[82]), .inbit(in[82]), .clk(clk), .reset(reset));
	reg1bit reg84(.writeEn(writeEn),.outbit(out[83]), .inbit(in[83]), .clk(clk), .reset(reset));
	reg1bit reg85(.writeEn(writeEn),.outbit(out[84]), .inbit(in[84]), .clk(clk), .reset(reset));
	reg1bit reg86(.writeEn(writeEn),.outbit(out[85]), .inbit(in[85]), .clk(clk), .reset(reset));
	reg1bit reg87(.writeEn(writeEn),.outbit(out[86]), .inbit(in[86]), .clk(clk), .reset(reset));
	reg1bit reg88(.writeEn(writeEn),.outbit(out[87]), .inbit(in[87]), .clk(clk), .reset(reset));
	reg1bit reg89(.writeEn(writeEn),.outbit(out[88]), .inbit(in[88]), .clk(clk), .reset(reset));
	reg1bit reg90(.writeEn(writeEn),.outbit(out[89]), .inbit(in[89]), .clk(clk), .reset(reset));// alu_out
	reg1bit reg91(.writeEn(writeEn),.outbit(out[90]), .inbit(in[90]), .clk(clk), .reset(reset));	
	reg1bit reg92(.writeEn(writeEn),.outbit(out[91]), .inbit(in[91]), .clk(clk), .reset(reset));
	reg1bit reg93(.writeEn(writeEn),.outbit(out[92]), .inbit(in[92]), .clk(clk), .reset(reset));
	reg1bit reg94(.writeEn(writeEn),.outbit(out[93]), .inbit(in[93]), .clk(clk), .reset(reset));
	reg1bit reg95(.writeEn(writeEn),.outbit(out[94]), .inbit(in[94]), .clk(clk), .reset(reset));
	reg1bit reg96(.writeEn(writeEn),.outbit(out[95]), .inbit(in[95]), .clk(clk), .reset(reset));
	reg1bit reg97(.writeEn(writeEn),.outbit(out[96]), .inbit(in[96]), .clk(clk), .reset(reset));	
	reg1bit reg98(.writeEn(writeEn),.outbit(out[97]), .inbit(in[97]), .clk(clk), .reset(reset));
	reg1bit reg99(.writeEn(writeEn),.outbit(out[98]), .inbit(in[98]), .clk(clk), .reset(reset));
	reg1bit reg100(.writeEn(writeEn),.outbit(out[99]), .inbit(in[99]), .clk(clk), .reset(reset));
	reg1bit reg101(.writeEn(writeEn),.outbit(out[100]), .inbit(in[100]), .clk(clk), .reset(reset));
	reg1bit reg102(.writeEn(writeEn),.outbit(out[101]), .inbit(in[101]), .clk(clk), .reset(reset));
	reg1bit reg103(.writeEn(writeEn),.outbit(out[102]), .inbit(in[102]), .clk(clk), .reset(reset));
	reg1bit reg104(.writeEn(writeEn),.outbit(out[103]), .inbit(in[103]), .clk(clk), .reset(reset));
	reg1bit reg105(.writeEn(writeEn),.outbit(out[104]), .inbit(in[104]), .clk(clk), .reset(reset));
	reg1bit reg106(.writeEn(writeEn),.outbit(out[105]), .inbit(in[105]), .clk(clk), .reset(reset));//memory read
	reg1bit reg107(.writeEn(writeEn),.outbit(out[106]), .inbit(in[106]), .clk(clk), .reset(reset));
	reg1bit reg108(.writeEn(writeEn),.outbit(out[107]), .inbit(in[107]), .clk(clk), .reset(reset));
	reg1bit reg109(.writeEn(writeEn),.outbit(out[108]), .inbit(in[108]), .clk(clk), .reset(reset));
	reg1bit reg110(.writeEn(writeEn),.outbit(out[109]), .inbit(in[109]), .clk(clk), .reset(reset));
	reg1bit reg111(.writeEn(writeEn),.outbit(out[110]), .inbit(in[110]), .clk(clk), .reset(reset));	
	reg1bit reg112(.writeEn(writeEn),.outbit(out[111]), .inbit(in[111]), .clk(clk), .reset(reset));
	reg1bit reg113(.writeEn(writeEn),.outbit(out[112]), .inbit(in[112]), .clk(clk), .reset(reset));
	reg1bit reg114(.writeEn(writeEn),.outbit(out[113]), .inbit(in[113]), .clk(clk), .reset(reset));
	reg1bit reg115(.writeEn(writeEn),.outbit(out[114]), .inbit(in[114]), .clk(clk), .reset(reset));
	reg1bit reg116(.writeEn(writeEn),.outbit(out[115]), .inbit(in[115]), .clk(clk), .reset(reset));
	reg1bit reg117(.writeEn(writeEn),.outbit(out[116]), .inbit(in[116]), .clk(clk), .reset(reset));	
	reg1bit reg118(.writeEn(writeEn),.outbit(out[117]), .inbit(in[117]), .clk(clk), .reset(reset));
	reg1bit reg119(.writeEn(writeEn),.outbit(out[118]), .inbit(in[118]), .clk(clk), .reset(reset));
	reg1bit reg120(.writeEn(writeEn),.outbit(out[119]), .inbit(in[119]), .clk(clk), .reset(reset));
	reg1bit reg121(.writeEn(writeEn),.outbit(out[120]), .inbit(in[120]), .clk(clk), .reset(reset));
	reg1bit reg122(.writeEn(writeEn),.outbit(out[121]), .inbit(in[121]), .clk(clk), .reset(reset));// Rb
	reg1bit reg123(.writeEn(writeEn),.outbit(out[122]), .inbit(in[122]), .clk(clk), .reset(reset));
	reg1bit reg124(.writeEn(writeEn),.outbit(out[123]), .inbit(in[123]), .clk(clk), .reset(reset));
	reg1bit reg125(.writeEn(writeEn),.outbit(out[124]), .inbit(in[124]), .clk(clk), .reset(reset));
	reg1bit reg126(.writeEn(writeEn),.outbit(out[125]), .inbit(in[125]), .clk(clk), .reset(reset));
	reg1bit reg127(.writeEn(writeEn),.outbit(out[126]), .inbit(in[126]), .clk(clk), .reset(reset));
	reg1bit reg128(.writeEn(writeEn),.outbit(out[127]), .inbit(in[127]), .clk(clk), .reset(reset));
	reg1bit reg129(.writeEn(writeEn),.outbit(out[128]), .inbit(in[128]), .clk(clk), .reset(reset));
	reg1bit reg130(.writeEn(writeEn),.outbit(out[129]), .inbit(in[129]), .clk(clk), .reset(reset));
	reg1bit reg131(.writeEn(writeEn),.outbit(out[130]), .inbit(in[130]), .clk(clk), .reset(reset));
	reg1bit reg132(.writeEn(writeEn),.outbit(out[131]), .inbit(in[131]), .clk(clk), .reset(reset));
	reg1bit reg133(.writeEn(writeEn),.outbit(out[132]), .inbit(in[132]), .clk(clk), .reset(reset));
	reg1bit reg134(.writeEn(writeEn),.outbit(out[133]), .inbit(in[133]), .clk(clk), .reset(reset));
	reg1bit reg135(.writeEn(writeEn),.outbit(out[134]), .inbit(in[134]), .clk(clk), .reset(reset));
	reg1bit reg136(.writeEn(writeEn),.outbit(out[135]), .inbit(in[135]), .clk(clk), .reset(reset));
	reg1bit reg137(.writeEn(writeEn),.outbit(out[136]), .inbit(in[136]), .clk(clk), .reset(reset));
//	reg1bit reg138(.writeEn(writeEn),.outbit(out[137]), .inbit(in[137]), .clk(clk), .reset(reset));// Forwarded Ra in 
//	reg1bit reg139(.writeEn(writeEn),.outbit(out[138]), .inbit(in[138]), .clk(clk), .reset(reset));
//	reg1bit reg140(.writeEn(writeEn),.outbit(out[139]), .inbit(in[139]), .clk(clk), .reset(reset));


	endmodule
	
	
	
module clockgenerator(clk);
	output clk;
	reg clk;
	initial
		begin
		clk = 0;
		end
	always begin
		#20 clk = !clk;
		end
	endmodule
module mux8to1(select,inp,oup);
	input [2:0] select;
	input [7:0] inp;
	output oup;
	wire [7:0] decoded;
	dec3to8 dec1 (.select(select), .decoded(decoded));
	and  a1 (o1, inp[0], decoded[0]);
	and  a2 (o2, inp[1], decoded[1]);
	and  a3 (o3, inp[2], decoded[2]);
	and  a4 (o4, inp[3], decoded[3]);
	and  a5 (o5, inp[4], decoded[4]);
	and  a6 (o6, inp[5], decoded[5]);
	and  a7 (o7, inp[6], decoded[6]);
	and  a8 (o8, inp[7], decoded[7]);
	or  or1 (oup1, o1, o2, o3, o4);
	or  or2 (oup2, o5, o6, o7, o8);
	or  or3 (oup, oup1, oup2);
	endmodule
module mux4to1(select,inp,oup);
	input [1:0] select;
	input [3:0] inp;
	output oup;
	wire [3:0] decoded;
	dec2to4 dec1 (.select(select), .decoded(decoded));
	and  a1 (o1, inp[0], decoded[0]);
	and  a2 (o2, inp[1], decoded[1]);
	and  a3 (o3, inp[2], decoded[2]);
	and  a4 (o4, inp[3], decoded[3]);
	or  or1 (oup, o1, o2, o3, o4);
	endmodule
module mux2to1(select,inp,oup);
	input  select;
	input [1:0] inp;
	output oup;
	wire [1:0] decoded;
	dec1to2 dec1 (.select(select), .decoded(decoded));
	and  a1 (o1, inp[0], decoded[0]);
	and  a2 (o2, inp[1], decoded[1]);
	or  or1 (oup, o1, o2 );
	endmodule
	
module mux2_16to16(oup,inp1, inp2,select);
	input [15:0] inp1;
	input[15:0] inp2;
	input select;
	output [15:0] oup;
	mux2to1 m16  (select,{inp1[15],inp2[15]},oup[15]);
	mux2to1 m15  (select,{inp1[14],inp2[14]},oup[14]);
	mux2to1 m14  (select,{inp1[13],inp2[13]},oup[13]);
	mux2to1 m13  (select,{inp1[12],inp2[12]},oup[12]);
	mux2to1 m12  (select,{inp1[11],inp2[11]},oup[11]);
	mux2to1 m11  (select,{inp1[10],inp2[10]},oup[10]);
	mux2to1 m10  (select,{inp1[9],inp2[9]},oup[9]);
	mux2to1 m9  (select,{inp1[8],inp2[8]},oup[8]);
	mux2to1 m8  (select,{inp1[7],inp2[7]},oup[7]);
	mux2to1 m7  (select,{inp1[6],inp2[6]},oup[6]);
	mux2to1 m6  (select,{inp1[5],inp2[5]},oup[5]);
	mux2to1 m5  (select,{inp1[4],inp2[4]},oup[4]);
	mux2to1 m4  (select,{inp1[3],inp2[3]},oup[3]);
	mux2to1 m3  (select,{inp1[2],inp2[2]},oup[2]);
	mux2to1 m2  (select,{inp1[1],inp2[1]},oup[1]);
	mux2to1 m1  (select,{inp1[0],inp2[0]},oup[0]);
	endmodule
	
module mux4_16to16(oup, inp1, inp2, inp3, inp4,select);
	input [15:0] inp1;
	input[15:0] inp2;
	input [15:0] inp3;
	input [15:0] inp4;
	input [1:0] select;
	output [15:0] oup;
	mux4to1 m16  (select,{inp1[15],inp2[15],inp3[15],inp4[15]},oup[15]);
	mux4to1 m15  (select,{inp1[14],inp2[14],inp3[14],inp4[14]},oup[14]);
	mux4to1 m14  (select,{inp1[13],inp2[13],inp3[13],inp4[13]},oup[13]);
	mux4to1 m13  (select,{inp1[12],inp2[12],inp3[12],inp4[12]},oup[12]);
	mux4to1 m12  (select,{inp1[11],inp2[11],inp3[11],inp4[11]},oup[11]);
	mux4to1 m11  (select,{inp1[10],inp2[10],inp3[10],inp4[10]},oup[10]);
	mux4to1 m10  (select,{inp1[9],inp2[9],inp3[9],inp4[9]},oup[9]);
	mux4to1 m9  (select,{inp1[8],inp2[8],inp3[8],inp4[8]},oup[8]);
	mux4to1 m8  (select,{inp1[7],inp2[7],inp3[7],inp4[7]},oup[7]);
	mux4to1 m7  (select,{inp1[6],inp2[6],inp3[6],inp4[6]},oup[6]);
	mux4to1 m6  (select,{inp1[5],inp2[5],inp3[5],inp4[5]},oup[5]);
	mux4to1 m5  (select,{inp1[4],inp2[4],inp3[4],inp4[4]},oup[4]);
	mux4to1 m4  (select,{inp1[3],inp2[3],inp3[3],inp4[3]},oup[3]);
	mux4to1 m3  (select,{inp1[2],inp2[2],inp3[2],inp4[2]},oup[2]);
	mux4to1 m2  (select,{inp1[1],inp2[1],inp3[1],inp4[1]},oup[1]);
	mux4to1 m1  (select,{inp1[0],inp2[0],inp3[0],inp4[0]},oup[0]);
	endmodule
	
module mux2_3to3(oup, inp1, inp2, select);
	input select;
	input [2:0] inp1;
	input [2:0] inp2;
	output [2:0] oup ;
	
	mux2to1 m3  (select,{inp1[2],inp2[2]},oup[2]);
	mux2to1 m2  (select,{inp1[1],inp2[1]},oup[1]);
	mux2to1 m1  (select,{inp1[0],inp2[0]},oup[0]);
	endmodule

module mux4_3to3(oup,inp1, inp2, inp3, inp4,select);
	input [2:0] inp1, inp2, inp3, inp4;
	output [2:0] oup;
	input [1:0] select;
	mux4to1 m3  (select,{inp1[2],inp2[2],inp3[2],inp4[2]},oup[2]);
	mux4to1 m2  (select,{inp1[1],inp2[1],inp3[1],inp4[1]},oup[1]);
	mux4to1 m1  (select,{inp1[0],inp2[0],inp3[0],inp4[0]},oup[0]);
	endmodule
	
module mux8_16to16_2 (select, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, readreg);
	input [2:0] select;
	input [15:0] reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8;
	output [15:0] readreg;
	mux8to1 m1 (select,{ reg1[0], reg2[0], reg3[0], reg4[0], reg5[0], reg6[0], reg7[0], reg8[0]},readreg[0]);
	mux8to1 m2 (select,{ reg1[1], reg2[1], reg3[1], reg4[1], reg5[1], reg6[1], reg7[1], reg8[1]},readreg[1]);
	mux8to1 m3 (select,{ reg1[2], reg2[2], reg3[2], reg4[2], reg5[2], reg6[2], reg7[2], reg8[2]},readreg[2]);
	mux8to1 m4 (select,{ reg1[3], reg2[3], reg3[3], reg4[3], reg5[3], reg6[3], reg7[3], reg8[3]},readreg[3]);
	mux8to1 m5 (select,{ reg1[4], reg2[4], reg3[4], reg4[4], reg5[4], reg6[4], reg7[4], reg8[4]},readreg[4]);
	mux8to1 m6 (select,{ reg1[5], reg2[5], reg3[5], reg4[5], reg5[5], reg6[5], reg7[5], reg8[5]},readreg[5]);
	mux8to1 m7 (select,{ reg1[6], reg2[6], reg3[6], reg4[6], reg5[6], reg6[6], reg7[6], reg8[6]},readreg[6]);
	mux8to1 m8 (select,{ reg1[7], reg2[7], reg3[7], reg4[7], reg5[7], reg6[7], reg7[7], reg8[7]},readreg[7]);
	mux8to1 m9 (select,{ reg1[8], reg2[8], reg3[8], reg4[8], reg5[8], reg6[8], reg7[8], reg8[8]},readreg[8]);
	mux8to1 m10 (select,{ reg1[9], reg2[9], reg3[9], reg4[9], reg5[9], reg6[9], reg7[9], reg8[9]},readreg[9]);
	mux8to1 m11 (select,{ reg1[10], reg2[10], reg3[10], reg4[10], reg5[10], reg6[10], reg7[10], reg8[10]},readreg[10]);
	mux8to1 m12 (select,{ reg1[11], reg2[11], reg3[11], reg4[11], reg5[11], reg6[11], reg7[11], reg8[11]},readreg[11]);
	mux8to1 m13 (select,{ reg1[12], reg2[12], reg3[12], reg4[12], reg5[12], reg6[12], reg7[12], reg8[12]},readreg[12]);
	mux8to1 m14 (select,{ reg1[13], reg2[13], reg3[13], reg4[13], reg5[13], reg6[13], reg7[13], reg8[13]},readreg[13]);
	mux8to1 m15 (select,{ reg1[14], reg2[14], reg3[14], reg4[14], reg5[14], reg6[14], reg7[14], reg8[14]},readreg[14]);
	mux8to1 m16 (select,{ reg1[15], reg2[15], reg3[15], reg4[15], reg5[15], reg6[15], reg7[15], reg8[15]},readreg[15]);	
	endmodule

module mux2_8to8(inwire, newbits, initialbits, sel);
	input [7:0] newbits, initialbits;
	input sel;
	output [7:0] inwire;
	mux2to1 m7(sel, {newbits[7], initialbits[7]}, inwire[7]);
	mux2to1 m6(sel, {newbits[6], initialbits[6]}, inwire[6]);
	mux2to1 m5(sel, {newbits[5], initialbits[5]}, inwire[5]);
	mux2to1 m4(sel, {newbits[4], initialbits[4]}, inwire[4]);
	mux2to1 m3(sel, {newbits[3], initialbits[3]}, inwire[3]);
	mux2to1 m2(sel, {newbits[2], initialbits[2]}, inwire[2]);
	mux2to1 m1(sel, {newbits[1], initialbits[1]}, inwire[1]);
	mux2to1 m0(sel, {newbits[0], initialbits[0]}, inwire[0]);
	endmodule
	
	
module memory16bitaddressable(readEn , writeEn, clk, reset, inData, outData, address);
	input readEn , writeEn, clk, reset;
	input [15:0] inData;
	output [15:0] outData;
	input [15:0] address;
	reg [7:0] dataram [0:511];
	reg [15:0] outData;
	integer i;
	initial begin
		for (i = 0; i < 512; i = i + 1) begin
			dataram[i] <= 0;
		end
			dataram[0] <= 8'b01000000;	//lw r0 r1 32
			dataram[1] <= 8'b01100000;	
			dataram[2] <= 8'b00110100; //lhi r2 1
			dataram[3] <= 1;				
			dataram[4] <= 8'b01010000;	//sw r0 r1 34
			dataram[5] <= 8'b01100010;	
			dataram[6] <= 8'b00000000; //add r0 r0 r1
			dataram[7] <= 8'b00010000;	
			dataram[8] <= 8'b00001001;//add r4 r5 r3
			dataram[9] <= 8'b01011000;
			dataram[10] <= 8'b00001000;//adz r4 r0 r3
			dataram[11] <= 8'b00011001;
			dataram[12] <= 8'b00001000;//adz r4 r0 r5
			dataram[13] <= 8'b00101001;
			dataram[14] <= 8'b00010000;//adi r0 r0 6
			dataram[15] <= 8'b00000110;
			dataram[16] <= 8'b01000000;//lw r0 r1 
			dataram[17] <= 8'b01100100;
			dataram[18] <= 8'b00010000;//adi r0 r0 6
			dataram[19] <= 8'b00000110;
			dataram[20] <= 8'b00100000;//ndu r0 r0r2
			dataram[21] <= 8'b01010000;
			dataram[22] <= 8'b01000010;//lw r1 r0 32
			dataram[23] <= 8'b00100000;
			dataram[24] <= 8'b01000100;//lw r2 r0 32
			dataram[25] <= 8'b00100000;
			dataram[26] <= 8'b11000010;//beq r1 r2 20
			dataram[27] <= 8'b10010100;
			//data memory
			dataram[33] <= 10;
			dataram[36] <=8'b11111111;
			dataram[37] <=8'b11111111;
			//jump
			dataram[66] <=8'b10001010;//jal r5 r0 6
			dataram[67] <=8'b00001010;
			dataram[68] <=8'b01100000;//lm r0 00000000
			dataram[69] <=8'b00000000;
			dataram[70] <=8'b01100000;//lm r0 01111111
			dataram[71] <=8'b01111111;
			dataram[72] <=8'b01110000;//sm r0 00000000
			dataram[73] <=8'b00000000;
			dataram[74] <=8'b01110000;//lm r0 01001010
			dataram[75] <=8'b10001010;

			
			//another jump
			dataram[86] <=8'b10011101;//jlr r6 r5
			dataram[87] <=8'b01000000;
			
			
	end
//	always @(negedge reset)
//		begin
//			dataram[0] <= 16'b0100000001011111;
//			dataram[65] <= 10;
//		end
//	
	always @(posedge clk ) begin
		if(writeEn==1'b1)
			begin
			dataram[address] <= inData[15:8];
			dataram[address + 1] <= inData[7:0];
			end
		end
	
	always @(address or readEn) begin
		if (readEn==1'b1)
		    begin
		    outData[15:8] <= dataram[address];
			 outData[7:0]  <= dataram[address +1];
		    end
		else
		    begin
		    outData <= 16'b0;
		    end
		end
//	assign outData =out;
	endmodule
module register_nc(out, in, clk, reset);
	input clk,reset;
	input [15:0] in;
	output [15:0] out;
	dflipflop d1 (.d(in[0]), .clk(clk), .reset(reset), .q(out[0]));
	dflipflop d2 (.d(in[1]), .clk(clk), .reset(reset), .q(out[1]));
	dflipflop d3 (.d(in[2]), .clk(clk), .reset(reset), .q(out[2]));
	dflipflop d4 (.d(in[3]), .clk(clk), .reset(reset), .q(out[3]));
	dflipflop d5 (.d(in[4]), .clk(clk), .reset(reset), .q(out[4]));
	dflipflop d6 (.d(in[5]), .clk(clk), .reset(reset), .q(out[5]));
	dflipflop d7 (.d(in[6]), .clk(clk), .reset(reset), .q(out[6]));
	dflipflop d8 (.d(in[7]), .clk(clk), .reset(reset), .q(out[7]));
	dflipflop d9 (.d(in[8]), .clk(clk), .reset(reset), .q(out[8]));
	dflipflop d10 (.d(in[9]), .clk(clk), .reset(reset), .q(out[9]));
	dflipflop d11 (.d(in[10]), .clk(clk), .reset(reset), .q(out[10]));
	dflipflop d12 (.d(in[11]), .clk(clk), .reset(reset), .q(out[11]));
	dflipflop d13 (.d(in[12]), .clk(clk), .reset(reset), .q(out[12]));
	dflipflop d14 (.d(in[13]), .clk(clk), .reset(reset), .q(out[13]));
	dflipflop d15 (.d(in[14]), .clk(clk), .reset(reset), .q(out[14]));
	dflipflop d16 (.d(in[15]), .clk(clk), .reset(reset), .q(out[15]));
	endmodule

module currentstate(clk, nextstate, presentstate, reset);
	input clk, reset;
	input  nextstate;
	output presentstate;
	reg we;

	initial begin
	we =1;
	end
	reg1bit one(we, presentstate, nextstate, clk, reset);
	endmodule
	
module outputcontroller(controls, zeroflag, nop, presentstate, opcode);
	input [3:0] opcode;
	input zeroflag;
	input nop;
	input presentstate;
	output [21:0] controls;
	output nextstate;
	
	reg nextstate;
	reg [21:0] controls;
	
	reg  instructionvalid ,	rb,	setz,	setc,	is_str,	is_load,	alu_control,	Reg_write,	is_beq,	is_jal,	is_jlr;	
	reg [1:0] Ra_mux,Rc_wb;
	reg [2:0] Rb_mux,Rc_data;
	
	always @( opcode or zeroflag or nop or presentstate)
		begin
		case(opcode )
			4'b0000: 
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=1	;
				 setc	<=1	;
				 is_str	<=0	;
				 is_load	<=0	;
				 is_beq	<=0	;
				 is_jal	<=0	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=1	;
				 Ra_mux	<=0	;
				 Rc_wb	<=2	;
				 Rb_mux	<=0	;
				 Rc_data <=1;

            end
			4'b0001: 
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=1		;
				 setc	<=1	;
				 is_str	<=0	;
				 is_load	<=0	;
				 is_beq	<=0	;
				 is_jal	<=0	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=1	;
				 Ra_mux	<=0	;
				 Rc_wb	<=1	;
				 Rb_mux	<=2	;
				 Rc_data <=1;

			end
			4'b0010: 
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=0	;
				 setc	<=1	;
				 is_str	<=0	;
				 is_load	<=0	;
				 is_beq	<=0	;
				 is_jal	<=0	;
				 is_jlr	<=0	;
				 alu_control	<=1	;
				 Reg_write	<=1	;
				 Ra_mux	<=0	;
				 Rc_wb	<=2	;
				 Rb_mux	<=0	;
				 Rc_data <=1;
			end
			4'b0011: 
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=0	;
				 setc	<=0	;
				 is_str	<=0	;
				 is_load	<=0	;
				 is_beq	<=0	;
				 is_jal	<=0	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=1	;
				 Ra_mux	<=0	;
				 Rc_wb	<=1	;
				 Rb_mux	<=0	;
				 Rc_data <=0;
			end
			4'b0100: 
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=0	;
				 setc	<=0	;
				 is_str	<=0	;
				 is_load	<=1	;
				 is_beq	<=0	;
				 is_jal	<=0	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=1	;
				 Ra_mux	<=2	;
				 Rc_wb	<=2	;
				 Rb_mux	<=0	;
				 Rc_data <=2;
            end
			4'b0101: 
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=0	;
				 setc	<=0	;
				 is_str	<=1	;
				 is_load	<=0	;
				 is_beq	<=0	;
				 is_jal	<=0	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=0	;
				 Ra_mux	<=2	;
				 Rc_wb	<=2	;
				 Rb_mux	<=0	;
				 Rc_data <=2;

            end
			4'b0110: 
			begin
				instructionvalid <=1;
				rb <=1	;
				setz	<=0	;
				setc	<=0	;
				is_str	<=0	;
				is_load	<=1	;
				is_beq	<=0	;
				is_jal	<=0	;
				is_jlr	<=0	;
				alu_control	<=0	;
				Reg_write	<=1	;
				Ra_mux	<=0	;
				Rc_wb	<=3	;
				Rb_mux	<=4	;
				Rc_data <=2;
				if(presentstate ==1)
					begin
					nextstate <=1;
					end
				else if(presentstate ==0)
					begin
					nextstate <=0;
					end
				end
			4'b0111:
			begin
				instructionvalid <=1;
				 rb <=1	;
				 setz	<=0	;
				 setc	<=0	;
				 is_str	<=1	;
				 is_load	<=0	;
				 is_beq	<=0	;
				 is_jal	<=0	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=0	;
				 Ra_mux	<=0	;
				 Rc_wb	<=0	;
				 Rb_mux	<=4	;
				 Rc_data <=1;
					 if(presentstate==1)
						begin
						nextstate <=1;
						end
					else if(presentstate ==0)
						begin
						nextstate <=0;
						end
				 end
			
			4'b0000: 
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=1	;
				 setc	<=1	;
				 is_str	<=0	;
				 is_load	<=0	;
				 is_beq	<=0	;
				 is_jal	<=0	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=1	;
				 Ra_mux	<=0	;
				 Rc_wb	<=2	;
				 Rb_mux	<=0	;
				 Rc_data <=1;
			    end
			4'b1100:
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=0	;
				 setc	<=0	;
				 is_str	<=0	;
				 is_load	<=0	;
				 is_beq	<=1	;
				 is_jal	<=0	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=1	;
				 Ra_mux	<=2	;
				 Rc_wb	<=0	;
				 Rb_mux	<=1	;
				 Rc_data <=0;
				 end
			4'b1000:
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=0	;
				 setc	<=0	;
				 is_str	<=0	;
				 is_load	<=0	;
				 is_beq	<=0	;
				 is_jal	<=1	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=1	;
				 Ra_mux	<=2	;
				 Rc_wb	<=0	;
				 Rb_mux	<=1	;
				 Rc_data <=0;

                end
			4'b1001:
			begin
				nextstate <=0;
				instructionvalid <=1;
				 rb <=0	;
				 setz	<=0	;
				 setc	<=0	;
				 is_str	<=0	;
				 is_load	<=0	;
				 is_beq	<=0	;
				 is_jal	<=1	;
				 is_jlr	<=0	;
				 alu_control	<=0	;
				 Reg_write	<=1	;
				 Ra_mux	<=2	;
				 Rc_wb	<=0	;
				 Rb_mux	<=1	;
				 Rc_data <=0;
                end
			endcase
			end
			always @ (zeroflag or presentstate or instructionvalid or rb or setz	or setc	or is_str	or is_load	or is_beq	or is_jal	or is_jlr	or alu_control	or Reg_write	or Ra_mux	or Rc_wb	or Rb_mux	or Rc_data or nop )
				begin
				if(zeroflag ==0)
					begin
					if(nop ==1)
						begin
						controls <=0;
						end
					else
						begin
						controls[21] <= presentstate;
						controls[20] <= instructionvalid;
						controls[19]<=rb ;
						controls[18]<=setz;
						controls[17]<=setc	;
						controls[16]<=is_str	;
						controls[15]<=is_load	;
						controls[14]<=is_beq	;
						controls[13]<=is_jal;
						controls[12]<=is_jlr	;
						controls[11]	<=alu_control	;
						controls[10]<=Reg_write	;
						controls[9:8]<=Ra_mux	;
						controls[7:6]<=Rc_wb	;
						controls[5:3]<=Rb_mux	;
						controls[2:0]<=Rc_data ;
						end
					end
				else if(zeroflag ==1)
					begin
					controls <=0;
					end
				end
	endmodule
	
module extendzeros( out,in );
	output [15:0] out;
	input [8:0] in;
	reg [15:0] out;
	always @ (in)
		begin
		out <= {in, 7'b0};
		end
	endmodule
	
module regfile8_16(readReg1,readReg2,writeReg,readData1,readData2,writeData,writeEnable, clk, reset);
	input [15:0] writeData;
	input [2:0] readReg1, readReg2, writeReg;
	input writeEnable, clk, reset;
	output [15:0] readData1, readData2;
	wire [15:0] regwire [0:7];
	wire [0:7] decodedWriteRegister;
	wire [0:7] decodedWriteRegisterEnabled;
	dec3to8 dec (decodedWriteRegister, writeReg);
	writeEnabled wrEn (writeEnable, decodedWriteRegister, decodedWriteRegisterEnabled); 
	register reg1 (.writeEn(decodedWriteRegisterEnabled[0]), .out(regwire[0]), .in(writeData), .clk(clk), .reset(reset));
	register reg2 (.writeEn(decodedWriteRegisterEnabled[1]), .out(regwire[1]), .in(writeData), .clk(clk), .reset(reset));
	register reg3 (.writeEn(decodedWriteRegisterEnabled[2]), .out(regwire[2]), .in(writeData), .clk(clk), .reset(reset));
	register reg4 (.writeEn(decodedWriteRegisterEnabled[3]), .out(regwire[3]), .in(writeData), .clk(clk), .reset(reset));
	register reg5 (.writeEn(decodedWriteRegisterEnabled[4]), .out(regwire[4]), .in(writeData), .clk(clk), .reset(reset));
	register reg6 (.writeEn(decodedWriteRegisterEnabled[5]), .out(regwire[5]), .in(writeData), .clk(clk), .reset(reset));
	register reg7 (.writeEn(decodedWriteRegisterEnabled[6]), .out(regwire[6]), .in(writeData), .clk(clk), .reset(reset));
	register reg8 (.writeEn(decodedWriteRegisterEnabled[7]), .out(regwire[7]), .in(writeData), .clk(clk), .reset(reset));
	mux8_16to16 readmux1 (readReg1, regwire[7], regwire[6], regwire[5], regwire[4], regwire[3], regwire[2], regwire[1], regwire[0], readData1);
	mux8_16to16 readmux2 (readReg2, regwire[7], regwire[6], regwire[5], regwire[4], regwire[3], regwire[2], regwire[1], regwire[0], readData2);	
	endmodule	
module writeEnabled( writeEn, decodedRegister, writeEnabledControl);
	input writeEn;
	input [7:0] decodedRegister;
	output [7:0] writeEnabledControl;
	and  a1 (writeEnabledControl[0] , writeEn, decodedRegister[0]);
	and  a2 (writeEnabledControl[1] , writeEn, decodedRegister[1]);
	and  a3 (writeEnabledControl[2] , writeEn, decodedRegister[2]);
	and  a4 (writeEnabledControl[3] , writeEn, decodedRegister[3]);
	and  a5 (writeEnabledControl[4] , writeEn, decodedRegister[4]);
	and  a6 (writeEnabledControl[5] , writeEn, decodedRegister[5]);
	and  a7 (writeEnabledControl[6] , writeEn, decodedRegister[6]);
	and  a8 (writeEnabledControl[7] , writeEn, decodedRegister[7]);	
	endmodule
module register(writeEn, out, in, clk, reset);
	input writeEn, clk,reset;
	input [15:0] in;
	output [15:0] out;
	reg1bit reg1(.writeEn(writeEn),.outbit(out[0]), .inbit(in[0]), .clk(clk), .reset(reset));
	reg1bit reg2(.writeEn(writeEn),.outbit(out[1]), .inbit(in[1]), .clk(clk), .reset(reset));
	reg1bit reg3(.writeEn(writeEn),.outbit(out[2]), .inbit(in[2]), .clk(clk), .reset(reset));
	reg1bit reg4(.writeEn(writeEn),.outbit(out[3]), .inbit(in[3]), .clk(clk), .reset(reset));
	reg1bit reg5(.writeEn(writeEn),.outbit(out[4]), .inbit(in[4]), .clk(clk), .reset(reset));
	reg1bit reg6(.writeEn(writeEn),.outbit(out[5]), .inbit(in[5]), .clk(clk), .reset(reset));
	reg1bit reg7(.writeEn(writeEn),.outbit(out[6]), .inbit(in[6]), .clk(clk), .reset(reset));
	reg1bit reg8(.writeEn(writeEn),.outbit(out[7]), .inbit(in[7]), .clk(clk), .reset(reset));
	reg1bit reg9(.writeEn(writeEn),.outbit(out[8]), .inbit(in[8]), .clk(clk), .reset(reset));
	reg1bit reg10(.writeEn(writeEn),.outbit(out[9]), .inbit(in[9]), .clk(clk), .reset(reset));
	reg1bit reg11(.writeEn(writeEn),.outbit(out[10]), .inbit(in[10]), .clk(clk), .reset(reset));
	reg1bit reg12(.writeEn(writeEn),.outbit(out[11]), .inbit(in[11]), .clk(clk), .reset(reset));
	reg1bit reg13(.writeEn(writeEn),.outbit(out[12]), .inbit(in[12]), .clk(clk), .reset(reset));
	reg1bit reg14(.writeEn(writeEn),.outbit(out[13]), .inbit(in[13]), .clk(clk), .reset(reset));
	reg1bit reg15(.writeEn(writeEn),.outbit(out[14]), .inbit(in[14]), .clk(clk), .reset(reset));
	reg1bit reg16(.writeEn(writeEn),.outbit(out[15]), .inbit(in[15]), .clk(clk), .reset(reset));
	endmodule
module mux8_16to16 (select, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, readreg);
	input [2:0] select;
	input [15:0] reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8;
	output [15:0] readreg;
	mux8to1 m1 (select,{ reg8[0], reg7[0], reg6[0], reg5[0], reg4[0], reg3[0], reg2[0], reg1[0]},readreg[0]);
	mux8to1 m2 (select,{ reg8[1], reg7[1], reg6[1], reg5[1], reg4[1], reg3[1], reg2[1], reg1[1]},readreg[1]);
	mux8to1 m3 (select,{ reg8[2], reg7[2], reg6[2], reg5[2], reg4[2], reg3[2], reg2[2], reg1[2]},readreg[2]);
	mux8to1 m4 (select,{ reg8[3], reg7[3], reg6[3], reg5[3], reg4[3], reg3[3], reg2[3], reg1[3]},readreg[3]);
	mux8to1 m5 (select,{ reg8[4], reg7[4], reg6[4], reg5[4], reg4[4], reg3[4], reg2[4], reg1[4]},readreg[4]);
	mux8to1 m6 (select,{ reg8[5], reg7[5], reg6[5], reg5[5], reg4[5], reg3[5], reg2[5], reg1[5]},readreg[5]);
	mux8to1 m7 (select,{ reg8[6], reg7[6], reg6[6], reg5[6], reg4[6], reg3[6], reg2[6], reg1[6]},readreg[6]);
	mux8to1 m8 (select,{ reg8[7], reg7[7], reg6[7], reg5[7], reg4[7], reg3[7], reg2[7], reg1[7]},readreg[7]);
	mux8to1 m9 (select,{ reg8[8], reg7[8], reg6[8], reg5[8], reg4[8], reg3[8], reg2[8], reg1[8]},readreg[8]);
	mux8to1 m10 (select,{ reg8[9], reg7[9], reg6[9], reg5[9], reg4[9], reg3[9], reg2[9], reg1[9]},readreg[9]);
	mux8to1 m11 (select,{ reg8[10], reg7[10], reg6[10], reg5[10], reg4[10], reg3[10], reg2[10], reg1[10]},readreg[10]);
	mux8to1 m12 (select,{ reg8[11], reg7[11], reg6[11], reg5[11], reg4[11], reg3[11], reg2[11], reg1[11]},readreg[11]);
	mux8to1 m13 (select,{ reg8[12], reg7[12], reg6[12], reg5[12], reg4[12], reg3[12], reg2[12], reg1[12]},readreg[12]);
	mux8to1 m14 (select,{ reg8[13], reg7[13], reg6[13], reg5[13], reg4[13], reg3[13], reg2[13], reg1[13]},readreg[13]);
	mux8to1 m15 (select,{ reg8[14], reg7[14], reg6[14], reg5[14], reg4[14], reg3[14], reg2[14], reg1[14]},readreg[14]);
	mux8to1 m16 (select,{ reg8[15], reg7[15], reg6[15], reg5[15], reg4[15], reg3[15], reg2[15], reg1[15]},readreg[15]);	
	endmodule
	
	
module signextend15_6(out,in);
	output [15:0] out;
	input [5:0] in;
	reg [15:0] out;
	always @(in)
		begin
		out = {10'b0,in};
		end
	endmodule
	
module signextend15_9(out, in);
	output [15:0] out;
	input [8:0] in;
	reg [15:0] out;
	always @(in)
		begin
		out = {7'b0,in};
		end
	endmodule

module equality3(eqflag,first_operand,second_operand);
	input [2:0] first_operand;
	input [2:0] second_operand;
	output eqflag;
	reg eqflag;
	reg [2:0] xo;
	always @(first_operand or first_operand)
		begin
		xo = first_operand^ first_operand;
		if(xo==0)
			begin
			    eqflag <= 1;
			end
		else
			begin 
			    eqflag <= 0;
			end
		end
	endmodule
module equality(eqflag,first_operand,second_operand);
	input [15:0] first_operand;
	input [15:0] second_operand;
	output eqflag;
	reg eqflag;
	reg [15:0] xo;
	always @(first_operand or first_operand)
		begin
		xo = first_operand^ first_operand;
		if(xo==0)
			begin
			    eqflag <= 1;
			end
		else
			begin 
			    eqflag <= 0;
			end
		end
	endmodule

	
	
module aluiitb(i_add_term1,i_add_term2,sel,o_1,z,c);
   input [15:0]  i_add_term1;
   input [15:0]  i_add_term2;
	input  sel;
   
	output [15:0] o_1;
	output z;
	output c;
	
	reg [15:0] outreg;
	reg cin;
	
	wire [15:0] o_result;
	wire [15:0] nandout;

//	reg c;
	reg z;
	always @(i_add_term1 or i_add_term2)
		begin
		    cin <=0;
		end
	fullAdder_16 fa1 (.i_add_term1(i_add_term1), .i_add_term2(i_add_term2), .inpcarry(cin), .o_result(o_result),.c(c));
	nandout_16 no (i_add_term1, i_add_term2, nandout);
	always @(sel or o_result or nandout)
	  begin
			case (sel)
			0: outreg = o_result;
			1: outreg = nandout;
			endcase
		end	
	always @(outreg)
		begin
		if(outreg == 0)
		begin
			    z <= 1;
		end
		else
		begin
			 z<= 0;
		end
		end
	assign o_1 = outreg;
	endmodule
	
module nandout_16 (inp1, inp2, oup);
	input [15:0] inp1;
	input [15:0] inp2;
	output [15:0] oup;
	nand n1 (oup[0], inp1[0], inp2[0]);
	nand n2 (oup[1], inp1[1], inp2[1]);
	nand n3 (oup[2], inp1[2], inp2[2]);
	nand n4 (oup[3], inp1[3], inp2[3]);
	nand n5 (oup[4], inp1[4], inp2[4]);
	nand n6 (oup[5], inp1[5], inp2[5]);
	nand n7 (oup[6], inp1[6], inp2[6]);
	nand n8 (oup[7], inp1[7], inp2[7]);
	nand n9 (oup[8], inp1[8], inp2[8]);
	nand n10 (oup[9], inp1[9], inp2[9]);
	nand n11 (oup[10], inp1[10], inp2[10]);
	nand n12 (oup[11], inp1[11], inp2[11]);
	nand n13 (oup[12], inp1[12], inp2[12]);
	nand n14 (oup[13], inp1[13], inp2[13]);
	nand n15 (oup[14], inp1[14], inp2[14]);
	nand n16 (oup[15], inp1[15], inp2[15]);
	endmodule
module fullAdder_16
	(
   input [15:0]  i_add_term1,
   input [15:0]  i_add_term2,
    input inpcarry,
   output [15:0] o_result,
   output c
	);
	wire [16:0]    w_CARRY;
	wire [15:0]    w_SUM;
	assign w_CARRY[0] = inpcarry;
	fullAdder full_adder_1
    ( 
      .a(i_add_term1[0]),
      .b(i_add_term2[0]),
      .ci(w_CARRY[0]),
      .s(w_SUM[0]),
      .co(w_CARRY[1])
      );
 
	fullAdder full_adder_2
    ( 
      .a(i_add_term1[1]),
      .b(i_add_term2[1]),
      .ci(w_CARRY[1]),
      .s(w_SUM[1]),
      .co(w_CARRY[2])
      );
	fullAdder full_adder_3
    ( 
      .a(i_add_term1[2]),
      .b(i_add_term2[2]),
      .ci(w_CARRY[2]),
      .s(w_SUM[2]),
      .co(w_CARRY[3])
      );
	fullAdder full_adder_4
    ( 
      .a(i_add_term1[3]),
      .b(i_add_term2[3]),
      .ci(w_CARRY[3]),
      .s(w_SUM[3]),
      .co(w_CARRY[4])
      );
	fullAdder full_adder_5
    ( 
      .a(i_add_term1[4]),
      .b(i_add_term2[4]),
      .ci(w_CARRY[4]),
      .s(w_SUM[4]),
      .co(w_CARRY[5])
      );
	fullAdder full_adder_6
    ( 
      .a(i_add_term1[5]),
      .b(i_add_term2[5]),
      .ci(w_CARRY[5]),
      .s(w_SUM[5]),
      .co(w_CARRY[6])
      );
	fullAdder full_adder_7
    ( 
      .a(i_add_term1[6]),
      .b(i_add_term2[6]),
      .ci(w_CARRY[6]),
      .s(w_SUM[6]),
      .co(w_CARRY[7])
      );
	fullAdder full_adder_8
    ( 
      .a(i_add_term1[7]),
      .b(i_add_term2[7]),
      .ci(w_CARRY[7]),
      .s(w_SUM[7]),
      .co(w_CARRY[8])
      );
	fullAdder full_adder_9
    ( 
      .a(i_add_term1[8]),
      .b(i_add_term2[8]),
      .ci(w_CARRY[8]),
      .s(w_SUM[8]),
      .co(w_CARRY[9])
      );
	fullAdder full_adder_10
    ( 
      .a(i_add_term1[9]),
      .b(i_add_term2[9]),
      .ci(w_CARRY[9]),
      .s(w_SUM[9]),
      .co(w_CARRY[10])
      );
	fullAdder full_adder_11
    ( 
      .a(i_add_term1[10]),
      .b(i_add_term2[10]),
      .ci(w_CARRY[10]),
      .s(w_SUM[10]),
      .co(w_CARRY[11])
      );
	fullAdder full_adder_12
    ( 
      .a(i_add_term1[11]),
      .b(i_add_term2[11]),
      .ci(w_CARRY[11]),
      .s(w_SUM[11]),
      .co(w_CARRY[12])
      );
	fullAdder full_adder_13
    ( 
      .a(i_add_term1[12]),
      .b(i_add_term2[12]),
      .ci(w_CARRY[12]),
      .s(w_SUM[12]),
      .co(w_CARRY[13])
      );
	fullAdder full_adder_14
    ( 
      .a(i_add_term1[13]),
      .b(i_add_term2[13]),
      .ci(w_CARRY[13]),
      .s(w_SUM[13]),
      .co(w_CARRY[14])
      );
	fullAdder full_adder_15
    ( 
      .a(i_add_term1[14]),
      .b(i_add_term2[14]),
      .ci(w_CARRY[14]),
      .s(w_SUM[14]),
      .co(w_CARRY[15])
      );
fullAdder full_adder_16
    ( 
      .a(i_add_term1[15]),
      .b(i_add_term2[15]),
      .ci(w_CARRY[15]),
      .s(w_SUM[15]),
      .co(w_CARRY[16])
      );
	assign o_result = w_SUM;   // Verilog Concatenation
	assign c = w_CARRY[16];
	endmodule // ripple_carry_adder_2_FA
module fullAdder(s,co,a,b,ci);
	output s,co;
	input a,b,ci;
	xor u1(s,a,b,ci);
	and u2(n1,a,b);
	and u3(n2,b,ci);
	and u4(n3,a,ci);
	or u5(co,n1,n2,n3);
	endmodule	
	
module reg1bit(writeEn, outbit, inbit, clk, reset);
	input writeEn, inbit, clk, reset;
	output outbit;
	wire in1, in2;
	and  and1(in1, writeEn, inbit);
	and and2(in2, ~writeEn, outbit);
	or  or1(d, in1, in2);
	dflipflop dff1(.d(d),.clk(clk),.reset(reset),.q(outbit));
	endmodule
	
module priorityencoder8_3 (binary_out ,  encoder_in );
	output [2:0] binary_out ;
	input [7:0] encoder_in ; 
	reg [2:0] binary_out ;     
	always @ ( encoder_in)
	begin
		if (encoder_in[7] == 1) begin
			binary_out <= 7; 
		end else if (encoder_in[6] == 1) begin
			binary_out <= 6; 
		end else if (encoder_in[5] == 1) begin
			binary_out <= 5; 
		end else if (encoder_in[4] == 1) begin
			binary_out <= 4; 
		end else if (encoder_in[3] == 1) begin
			binary_out <= 3; 
		end else if (encoder_in[2] == 1) begin
			binary_out <= 2; 
		end else if (encoder_in[1] == 1) begin
			binary_out <= 1; 
		end else if (encoder_in[0] == 1) begin
			binary_out <= 0; 
		end
		else begin
			binary_out <=0;
		end
		
	end
	endmodule  
module dec3to8(decoded,select);
	input [2:0] select;
	output [7:0] decoded;
	wire [2:0] opp; 
	not  nota(opp[0],select[0]);
	not  notb(opp[1],select[1]);
	not  notc(opp[2],select[2]);
	and am1(decoded[7],opp[2],opp[1],opp[0]);
	and am2(decoded[6],opp[2],opp[1],select[0]);
	and am3(decoded[5],opp[2],select[1],opp[0]);
	and am4(decoded[4],opp[2],select[1],select[0]);
	and am5(decoded[3],select[2],opp[1],opp[0]);
	and am6(decoded[2],select[2],opp[1],select[0]);
	and am7(decoded[1],select[2],select[1],opp[0]);
	and am8(decoded[0],select[2],select[1],select[0]);
	endmodule

module dec1to2(select,decoded);
	input  select;
	output [1:0] decoded;
	wire  opp; 
	not  nota(opp,select);
	and am1(decoded[1],opp);
	and am2(decoded[0],select);
	endmodule
	
module dec2to4(select,decoded);
	input [1:0] select;
	output [3:0] decoded;
	wire [1:0] opp; 
	not  nota(opp[0],select[0]);
	not  notb(opp[1],select[1]);
	and am1(decoded[3],opp[1],opp[0]);
	and am2(decoded[2],opp[1],select[0]);
	and am3(decoded[1],select[1],opp[0]);
	and am4(decoded[0],select[1],select[0]);
	endmodule
	
module dflipflop(d, clk, reset, q );
	input clk, reset,d ;
	output q;
	reg q;
	always @(posedge clk or posedge reset)begin
		if(reset ==1'b1)
			q <= 0;
		else
			q = d;
	end
	endmodule


module leftShift ( SO,DI);  
	input  [15:0] DI;  
	output [15:0] SO;  
	reg[15:0] SO;  
	always @(DI )  
	begin  
      SO <= DI << 1;  
	end  
	endmodule
