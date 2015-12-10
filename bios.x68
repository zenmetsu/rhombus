*****************************************************************************************************
*****************************************************************************************************
* Equates section										*****
												*****
****************************************							*****
*   Addresses											*****
												*****
ROMBAS		EQU	0		ROM BASE ADDRESS					*****
RAMBAS		EQU	$F00000		RAM BASE ADDRESS					*****
STACK		EQU	$F003FF		INITIAL STACK POINTER					*****
MFPBAS		EQU	$EF0000		MFP BASE ADDRESS					*****
MFPVCT		EQU	$40		VECTOR FOR MFP SOURCED INTERRUPT			*****
NOP		EQU	$4E71		STANDARD 68000 NOP INSTRUCTION				*****
												*****
****************************************							*****
*   MC68901 MFP Registers									*****
												*****
MFPGPIP		EQU	MFPBAS+$1	GPIP DATA						*****
MFPAER		EQU	MFPBAS+$3	ACTIVE EDGE						*****
MFPDDR		EQU     MFPBAS+$5								*****
MFPIERA		EQU     MFPBAS+$7								*****
MFPIERB 	EQU     MFPBAS+$9								*****
MFPIPRA		EQU     MFPBAS+$B								*****
MFPIPRB		EQU     MFPBAS+$D								*****
MFPISRA		EQU     MFPBAS+$F								*****
MFPISRB		EQU     MFPBAS+$11								*****
MFPIMRA		EQU     MFPBAS+$13								*****
MFPIMRB		EQU     MFPBAS+$15								*****
MFPVR		EQU     MFPBAS+$17								*****
MFPTACR		EQU     MFPBAS+$19								*****
MFPTBCR		EQU     MFPBAS+$1B								*****
MFPTCDCR	EQU     MFPBAS+$1D								*****
MFPTADR		EQU     MFPBAS+$1F								*****
MFPTBDR		EQU     MFPBAS+$21								*****
MFPTCDR		EQU     MFPBAS+$23								*****
MFPTDDR		EQU     MFPBAS+$25								*****
MFPSCR		EQU     MFPBAS+$27								*****
MFPUCR		EQU     MFPBAS+$29								*****
MFPRSR		EQU     MFPBAS+$2B								*****
MFPTSR		EQU     MFPBAS+$2D								*****
MFPUDR		EQU     MFPBAS+$2F								*****
												*****	
****************************************							*****
*   ASCII Equates										*****
												*****
CTRLC		EQU	$03									*****
BEL		EQU	$07									*****
TAB		EQU	$09									*****
LF		EQU	$0A								     ***********
CR		EQU	$0D								      *********
CTRLX		EQU	$18								       *******
ESC		EQU	$1B									*****
												 ***
												  *
*****************************************************************************************************
*****************************************************************************************************



*****************************************************************************************************
*****************************************************************************************************
*	Program section										*****
*	The ROM in this BIOS is mapped to the variable						*****
*	ROMBAS.  All executable code is resident in ROM.					*****
												*****
START		EQU	ROMBAS									*****
		DC.L	STACK		INITIAL STACK POINTER					*****
		DC.L	ROMSTART	INITIAL PROGRAM COUNTER					*****
												*****
ROMBUF		DS.L	32		LEAVE A LITTLE SPACE					*****
												*****
MEMDAT		EQU	*		MEMORY EXERCISER DATA					*****	
		DC.B	$5									*****
		DC.B	$A									*****
		DC.B	$0									*****
		DC.B	$F									*****
		DS.L	$100		LEAVE MORE SPACE					*****
												*****
ROMSTART	EQU	*		BEGINNING OF PROGRAM SECTION				*****
		MOVE.L	#RAMBAS,D0	POINT TO BASE OF RAM					*****
		MOVEC.L	D0,VBR		AND INITIALIZE VBR TO POINT THERE			*****
												*****
												*****
****************************************							*****
*   MEMORY EXERCISER 										*****
*	This routine performs a quick check of memory prior to					*****
*	proceeding.  The count of errors is stored in D7 at completion				*****
												*****
		CLR.L	D7		CLEAR ERROR COUNTER					*****
		MOVE.L	#3,D3		INIT OUTER LOOP COUNTER					*****
		LEA.L	RAMBAS,A0	POINT TO BASE OF RAM					*****
		LEA.L	MEMDAT,A1	POINT TO MEMORY EXERCISER DATA				*****
												*****
LOOP0		EQU	*									*****
		MOVE.L	#$3FF,D0	INIT INNER LOOP COUNTER, INITIALLY CHECK 1K		*****
		MOVE.B	(A1,D3),D2	GET MEMORY DATA						*****
												*****
LOOP1		EQU	*									*****
		MOVE.B	D2,(A0,D0)	PUT DATA INTO MEMORY					*****
		CMP.B	(A0,D0),D2	COMPARE WITH STORED DATA				*****
		BEQ.S	LOOP1_1		JUMP IF MATCHES						*****
		ADDQ	#1,D7		ELSE INCREMEMBT ERROR COUNT				*****
												*****
LOOP1_1		EQU	*									*****
		DBRA	D0,LOOP1	TEST ALL OF RAM						*****
		DBRA	D3,LOOP0	FOR ALL DATA TYPES (4 TESTS)				*****
												*****
*	When memory test completes, memory is initialized with NOPs and vector table 		*****
*       is initialized with address of generic handler.						*****
*	After initialization, D7 will contain the number of errors from the test section	*****
												*****
memInit		EQU	*									*****
		LEA.L	RAMBAS,A0	POINT AT BASE OF RAM AGAIN				*****
		MOVE.L	#$1FFE,D0	USE AS LOOP COUNTER FOR MEMINIT				*****
		MOVE.L	#$3FE,D2	POINT TO BOTTOM OF VECTOR TABLE				*****
		MOVE.W	#NOP,D1		FILL NON-VECTOR MEMORY WITH NOPs			*****
												*****
.loop2		EQU	*									*****
		MOVE.W	D1,(A0,D0)	PUT DATA INTO MEMORY					*****
		SUBQ	#2,D0		DECREMENT COUNTER					*****
		CMP.L	D0,D2		LOOK FOR BOTTOM OF VECTOR TABLE				*****
		BNE.S	.loop2		LOOP UNTIL BOTTOM OF VECTOR TABLE			*****
		SUBQ	#2,D0		ELSE MOVE TO LONG-WORD INIT				*****
		MOVE.L	#EXCHND,D1	PUT GENERIC EXCEPTION HANDLER IN REST OF VECTOR TABLE	*****
												*****
.loop3		EQU	*									*****
		MOVE.L	D1,(A0,D0)	PUT HANDLER ADDRESS THERE				*****
		SUBQ	#4,D0		AND DECREMENT POINTER					*****
		BGE.S	.loop3		FILL REST OF MEMORY					*****
												*****
												*****
****************************************							*****
*	Initialize the MC68901 MFP								*****
												*****
		BSR.S	MFPINIT		DO SO AS SUBROUTINE FOR LATER USE			*****
												*****
		TST	D7		NOW CHECK FOR MEMORY ERRORS				*****
		BEQ	NO_ERR		IF NONE, OUTPUT OK MESSAGE				*****
		LEA.L	ERRMSG,A0	POINT TO TOP OF MESSAGE					*****
		BSR.W	printString	PRINT MESSAGE						*****
MEMFAIL		NOP			RAM FAILED IN FIRST 1K SO DO NOTHING, QUICKLY		*****
		BRA.S	MEMFAIL		AND KEEP DOING IT					*****
												*****
NO_ERR		EQU	*		INIT OK!						*****
		LEA.L	OKMSG,A0	POINT TO TOP OF MESSAGE					*****
		BSR.W	printString	Output messsage over serial port			*****
		BSR.W	RAMsizer	Check RAM Size						*****
												*****
												*****      **
		LEA.L	msgBanner,A0	Print Banner						*****    ****
		BSR.W	printString								*****  **********************
PROMPT		LEA.L	msgPrompt,A0	Print prompt						*****************************
		BSR.W	printString								*****  **********************
POLL		EQU	*		POLL SERIAL PORT FOR INPUT				*****    ****		*****
		BTST.B	#3,MFPRSR	CHECK FOR BREAK						*****      **		*****
		BNE.S	BREAK		IF PRESENT, JUMP TO PROCESS				*****			*****
		BTST.B	#7,MFPRSR	ELSE CHECK FOR CHARACTER				*****			*****
		BEQ.S	POLL		LOOP IF NO DATA PRESENT, ELSE DATA PRESENT IN USART RX	*****			*****
*					ADD USER INPUT PROCESSING ROUTINE HERE			*****			*****
												*****			*****
BREAK		EQU	*		BREAK DETECT ROUTINE					*****************************
*					ADD USER BREAK HANDLER HERE				*****************************
		JMP	POLL		AND RETURN WHEN COMPLETED				*****************************


*****************************************************************************************************
*****************************************************************************************************
* EXCEPTION HANDLERS										*****
												*****
****************************************							*****
*   Generic Exception Handler									*****
												*****
EXCHND		EQU	*		GENERIC EXCEPTION HANDLER				*****
		RTE										*****
												*****
****************************************							*****
*   MFP Exception Handler									*****
											     ***********
MFPEXC		EQU	*								      *********
		RTE									       *******
												*****
												 *** 
												  *
*****************************************************************************************************
*****************************************************************************************************
* DEVICE SECTION										*****
												*****
****************************************							*****
*  MC68901 INITIALIZATION									*****
												*****
												*****
MFPINIT		EQU	*		MC68901 INITIALIZATION ROUTINE				*****
		CLR.L	D0		CLEAR D0						*****
		SUBQ	#1,D0		THEN TURN INTO ALL 1's					*****
		MOVE.B	D0,MFPDDR	ALL MFP I/O INITIALIZED TO OUTPUT			*****
		ADDQ	#3,D0		NOW TURN D0 INTO 2					*****
		MOVE.B	D0,MFPTCDR	SELECT 1/4 TX CLOCK					*****
		MOVE.B	D0,MFPTDDR	SEELCT 1/4 RX CLOCK					*****
		MOVE.B	#$11,MFPTCDCR	SELECT DIVIDE BY 4 IN C/D CONTROL REGISTER		*****
		MOVE.B	#$88,MFPUCR	SELECT DIVIDE BY 16, 8-BIT				*****
*					NO PARITY IN USART CONTROL REGISTER			*****
												*****
*		INITIALIZE MFP VECTOR AND HANDLER						*****
		MOVE.L	#MFPVCT,D0	GET VECTOR						*****
		MOVE.B	D0,MFPVR	LOAD INTO MFP						*****
		ASL.L	#2,D0		NOW SHIFT LEFT 2					*****
		ADDI.L  #RAMBAS,D0								*****
		MOVE.L	D0,A0		PUT INTO ADDRESS REGISTER				*****
		MOVE.L	#MFPEXC,(A0)	AND INIT APPROPRIATE VECTOR				*****
												*****
*		START TX/RX CLOCKS								*****
		MOVE.B	#1,MFPRSR	START RECEIVER CLOCK					*****
		MOVE.B	#5,MFPTSR	START TRANSMIT CLOCK				     ***********
		BSET.B	#7,MFPGPIP	RAISE RTS					      *********
											       *******
		RTS			DONE!  RETURN FROM ROUTINE				*****	
												 ***
												  *
*****************************************************************************************************
*****************************************************************************************************
* SUBROUTINE SECTION										*****
												*****
												*****

****************************************							*************************
* RAM sizer and diagnostic									*************************
												*************************
RAMsizer	EQU	*									*****
		LEA.L	msgRamSizing,A0								*****
		BSR.W   printString     Output messsage over serial port			*****
		MOVE.L	#$7FF,D1	Start at 2K mark to avoid disrupting existing 1K stack	*****
		MOVE.L  #$2,D3		D3 stores amount of RAM in KB				*****
		LEA.L	RAMBAS,A1								*****
		MOVE.B	#$76,D2		First test pattern, 0x76				*****
.loopSiz	MOVE.B	D2,(A1,D1)	Write to RAM						*****
		CMP.B	(A1,D1),D2	Compare to see if write was successful			*****
		BNE.S	.reportRAM	If failure, report size of detected RAM			*****
		NOT.B	D2		Invert test pattern					*****
		MOVE.B	D2,(A1,D1)	Retest memory location					*****
		CMP.B	(A1,D1),D2	Compare to see if write was successful			*****
		BNE.S	.reportRAM	If failure, report size of detected RAM			*****
                ADDI.L	#$400,D1	Increment test point by 1KB				*****
		ADDI.W	#$1,D3									*****
		BRA.S	.loopSiz	Loop until failure					*****
												*****
.reportRAM	EQU	*									*****
		SUBI.W	#$1,D3		Correct KB count due to failed test after increment	*****
		LEA.L	msgRamFound1,A0								*****
		BSR.S	printString								*****
		BSR.S	.printVal								*****		**
		LEA.L	msgRamFound2,A0								*****		****
		BSR.S	printString								**********************
		RTS			Exit RAMsizer						************************
												**********************		
.progress	EQU	*									*****		****
												*****		**
.printVal	EQU	*									*****
		EOR.L	D1,D1		Clear D1, will store character to be written		*****
		EOR.L	D2,D2		Clear D2, will be zero until first character written	*****
.loop10000	CMPI.W	#$2710,D3	Compare to 10000					*****
		BLT.S	.print10000	Branch if less than					*****
		SUBI.W	#$2710,D3	Subtract 10000						*****
		ADDI.B	#$1,D1		Increment digit to be written				*****
		BRA.S	.loop10000	Repeat until less than 10000				*****
												*****
.print10000	BSR.S	.printChar								*****
		EOR.L	D1,D1		Clear D1, will store character to be written		*****
.loop1000	CMPI.W	#$3E8,D3	Compare to 1000						*****
		BLT.S	.print1000	Branch if less than					*****
		SUBI.W	#$3E8,D3	Subtract 1000						*****
		ADDI.B	#$1,D1		Increment digit to be written				*****
		BRA.S	.loop1000	Repeat until less than 1000				*****
												*****
.print1000	BSR.S	.printChar								*****
		EOR.L	D1,D1		Clear D1, will store character to be written		*****
.loop100	CMPI.W	#$64,D3		Compare to 100						*****
		BLT.S	.print100	Branch if less than					*****
		SUBI.W	#$64,D3		Subtract 100						*****
		ADDI.B	#$1,D1		Increment digit to be written				*****
		BRA.S	.loop100	Repeat until less than 100				*****
												*****
.print100	BSR.S   .printChar      							*****
                EOR.L   D1,D1           Clear D1, will store character to be written		*****
.loop10		CMPI.W	#$A,D3		Compare to 10						*****
		BLT.S	.print10	Branch if less than					*****
		SUBI.W	#$A,D3		Subtract 10						*****
		ADDI.B	#$1,D1		Increment digit to be written				*****
		BRA.S	.loop10		Repeate until less than 10				*****
												*****
.print10	BSR.S   .printChar								*****
                MOVE.B   D3,D1		Clear D1, will store character to be written		*****
		BSR.S	.printChar	No need to loop on 1s digit				*****
												*****
		RTS			GTFO							*****
												*****
.printChar	TST.B	D1		Test if digit is zero					*****
		BNE.S	.continue	Print if not						*****
		TST.B	D2		Else test if this would be first printed digit		*****
		BNE.S	.continue	Print if not						*****
		RTS			Else return without zero-padding			*****
												*****
.continue	ADDI.B	#'0',D1		Convert to ASCII					*****
.wait		BTST.B  #7,MFPTSR	Check for empty transmit buffer				*****
                BEQ.S   .wait		Loop until ready					*****
                MOVE.B  D1,MFPUDR	Put character into USART data register			*****
		MOVE.B	#$1,D2		Mark first character as written to avoid zero-padding	*****
												*****
.end            RTS                     ELSE RETURN WHEN COMPLETED				*****
										*********************
										*********************
										*********************

****************************************							*************************
* MFP Transmit/Receive										*************************	
												*************************
printString	EQU	*									*****
.loop		MOVE.B	(A0)+,D0	Read character data					*****	
		BEQ.S	.end		Check for null terminator				*****		**
		BSR.S	.outChar	Write the character					*****		****
		BRA.S	.loop		Loop until null found					**********************
		RTS										************************
												**********************
.outChar	EQU	*									*****		****
		BTST.B	#7,MFPTSR	Check for empty transmit buffer				*****		**
		BEQ.S	.outChar	Loop until ready					*****
		MOVE.B	D0,MFPUDR	Put character into USART data register			*****
		RTS			ELSE RETURN WHEN COMPLETED				*****
										*********************
										*********************
										*********************


*****************************************************************************************************
*****************************************************************************************************
* MESSAGES SECTION										*****
												*****
												*****
OKMSG		EQU	*									*****
		DC.B	'<RESET>',CR,LF								*****
		DC.B	'MC68901 Multifunction Peripheral Initialized',CR,LF,LF,0		*****
												*****
ERRMSG		EQU	*									*****
		DC.B	'<RESET>',CR,LF								*****
		DC.B	'MEMORY ERRORS ENCOUNTERED...',CR,LF,'>',0				*****
												*****
msgBanner	EQU	*									*****
		DC.B	'============================',CR,LF					*****
		DC.B	'RHOMBUS 68020 System Monitor',CR,LF					*****
		DC.B	'============================',CR,LF,0					*****
												*****
msgPrompt	EQU	*									*****
		DC.B	'>',0									*****
												*****
msgRamSizing	EQU	*									*****
		DC.B	'RAM detection in progress...',CR,LF,0					*****
												*****
msgRamFound1	EQU	*									*****
		DC.B	CR,'Detected: ',0							*****
msgRamFound2	EQU	*									*****
		DC.B	'KB',CR,LF,LF,0								*****
		END	START									*****
