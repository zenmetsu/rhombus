*****************************************************************************************************
*****************************************************************************************************
*****	Title		: RHOMBUS System Monitor						*****
*****	Written by	: Jason Westervelt							*****
*****	Date		: 10 December 2015							*****
*****	Description	: A ROM monitor for the RHOMBUS minimalist 68020 system			*****
*****			  which was designed around the Motorola Application Note 1015		*****
*****												*****	
*****			  Portions of code borrowed from Hayden Kroepfl (ChartreuseK)		*****
*****												*****	
*****************************************************************************************************
*****************************************************************************************************

						*
					       ***
					      *****
					     *******	
					    **** ****
					   ****   ****
					  ****     ****
					 ****       ****
					****         ****
					 ****       ****
					  ****     ****
					   ****   ****
					    **** ****
					     *******
					      *****
					       ***
					        *

*****************************************************************************************************
*****************************************************************************************************
* Equates section										*****
												*****
												*****
****************************************							*****
*   Addresses											*****
												*****
ROMBAS		EQU	0		ROM BASE ADDRESS					*****
RAMBAS		EQU	$F00000		RAM BASE ADDRESS					*****
STACK		EQU	varLast		INITIAL STACK POINTER $F007AA				*****
MFPBAS		EQU	$EF0000		MFP BASE ADDRESS					*****
MFPVCT		EQU	$40		VECTOR FOR MFP SOURCED INTERRUPT			*****
												*****
****************************************							*****
*   Defines											*****
												*****
NOP		EQU	$4E71		STANDARD 68000 NOP INSTRUCTION				*****
MAX_LINELEN	EQU	80									*****
												*****
****************************************							*****
*   Variables											*****
												*****
varCurAddr	EQU	$F007FC									*****
varLineBuf	EQU	varCurAddr-MAX_LINELEN-2						*****
varLast		EQU	varLineBuf								*****
												*****
****************************************							*****
*   MC68901 MFP Registers									*****
												*****
MFPGPDR		EQU	MFPBAS+$1	GPIO DATA REGISTER					*****
MFPAER		EQU	MFPBAS+$3	ACTIVE EDGE REGISTER					*****
MFPDDR		EQU     MFPBAS+$5	DATA DIRECTION REGISTER					*****
MFPIERA		EQU     MFPBAS+$7	INTERRUPT ENABLE REGISTER A				*****
MFPIERB 	EQU     MFPBAS+$9	INTERRUPT ENABLE REGISTER B				*****
MFPIPRA		EQU     MFPBAS+$B	INTERRUPT PENDING REGISTER A				*****
MFPIPRB		EQU     MFPBAS+$D	INTERRUPT PENDING REGISTER B				*****
MFPISRA		EQU     MFPBAS+$F	INTERRUPT IN SERVICE REGISTER A				*****
MFPISRB		EQU     MFPBAS+$11	INTERRUPT IN SERVICE REGISTER B				*****
MFPIMRA		EQU     MFPBAS+$13	INTERRUPT MASK REGISTER A				*****
MFPIMRB		EQU     MFPBAS+$15	INTERRUPT MASK REGISTER B				*****
MFPVR		EQU     MFPBAS+$17	VECTOR REGISTER						*****
MFPTACR		EQU     MFPBAS+$19	TIMER A CONTROL REGISTER				*****
MFPTBCR		EQU     MFPBAS+$1B	TIMER B CONTROL REGISTER				*****
MFPTCDCR	EQU     MFPBAS+$1D	TIMER C/D CONTROL REGISTER				*****
MFPTADR		EQU     MFPBAS+$1F	TIMER A DATA REGISTER					*****
MFPTBDR		EQU     MFPBAS+$21	TIMER B DATA REGISTER					*****
MFPTCDR		EQU     MFPBAS+$23	TIMER C DATA REGISTER					*****
MFPTDDR		EQU     MFPBAS+$25	TIMER D DATA REGISTER					*****
MFPSCR		EQU     MFPBAS+$27	SYNCHRONOUS CHARACTER REGISTER				*****
MFPUCR		EQU     MFPBAS+$29	USART CONTROL REGISTER					*****
MFPRSR		EQU     MFPBAS+$2B	RECEIVER STATUS REGISTER				*****
MFPTSR		EQU     MFPBAS+$2D	TRANSMITTER STATUS REGISTER				*****
MFPUDR		EQU     MFPBAS+$2F	USART DATA REGISTER					*****
												*****	
****************************************							*****
*   ASCII Equates										*****
												*****
CTRLC		EQU	$03									*****
BEL		EQU	$07									*****
BKSP		EQU	$08		CTRL-H							*****
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
*	We avoid using stack until the first 2K of RAM is deemed safe				*****
		CLR.L	D7		CLEAR ERROR COUNTER					*****
		MOVE.L	#3,D3		INIT OUTER LOOP COUNTER					*****
		LEA.L	RAMBAS,A0	POINT TO BASE OF RAM					*****
		LEA.L	MEMDAT,A1	POINT TO MEMORY EXERCISER DATA				*****
												*****
LOOP0		EQU	*									*****
		MOVE.L	#$7FF,D0	INIT INNER LOOP COUNTER, INITIALLY CHECK 2K		*****
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
		MOVE.L	#$7FE,D0	USE AS LOOP COUNTER FOR MEMINIT				*****
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
****************************************							*****
*	Initialize the TRAP vectors								*****
		BSR.W	setTrap1	Initialize MFP GPIO TRAP1 vector			*****
		BSR.W	setTrap15	Initialize IO TRAP15 vector				*****
												*****
		TST	D7		NOW CHECK IF THERE WERE MEMORY ERRORS			*****
		BEQ	NO_ERR		IF NONE, OUTPUT OK MESSAGE				*****
		LEA.L	ERRMSG,A0	POINT TO TOP OF MESSAGE					*****
		BSR.W	printString	PRINT MESSAGE						*****
MEMFAIL		NOP			RAM FAILED IN FIRST 2K SO DO NOTHING, QUICKLY		*****
		BRA.S	MEMFAIL		AND KEEP DOING IT					*****
												*****
NO_ERR		EQU	*		INIT OK!						*****
		LEA.L	OKMSG,A0	POINT TO TOP OF MESSAGE					*****
		BSR.W	printString	Output messsage over serial port			*****
		BSR.W	RAMsizer	Check RAM Size						*****
		MOVE.L	#RAMBAS,D0	Set D0 to base of RAM					*****
		ADDI.L	#$800,D0	Point D0 at base of non-stack RAM (2K)			*****
		MOVE.L	D3,D1		Copy size of detected RAM to D1				*****
		SUBQ	#2,D1		Subtract 2KB from total RAM as we will not check stack	*****
		MULU.W	#$400,D1	Multiply by 1024 per KB					*****
		SUBQ	#1,D1		Subtract 1, we start counting at zero			*****
		ADD.L	D0,D1		Add size (D1) to non-stack offset (D0) to get high addr	*****	
		BSR.W	RAMcheck	Check remaining RAM for errors				*****
												*****
												*****      **
WARMSTART	LEA.L	msgBanner,A0	Print Banner						*****    ****
		BSR.W	printString								*****  **********************
PROMPT		LEA.L	msgPrompt,A0	Print prompt						*****************************
		BSR.W	printString								*****  **********************
		BSR.W	readLine	Get User Input						*****    ****		*****
		BSR.W	convertCase	Convert to upper-case					*****      **		*****
		BSR.W	parseLine	Then parse user input					*****			*****
												*****			*****
		BRA.S	PROMPT									*****			*****
												*****			*****
												*****			*****
												*****************************
												*****************************
												*****************************


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
		ADDQ	#2,D0		NOW TURN D0 INTO 1					*****
		MOVE.B	D0,MFPTCDR	SELECT 1/2 TX CLOCK					*****
		MOVE.B	D0,MFPTDDR	SEELCT 1/2 RX CLOCK					*****
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
		BSET.B	#7,MFPGPDR	RAISE RTS (INHIBIT RX)				      *********
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
* RAM checker and diagnostic									*************************
*												*************************
*					D0 stores lowest memory address to check		*****
*					D1 stores highest memory address to check		*****
*					D7 returns number of failed bytes			*****
												*****
RAMcheck	MOVEM.L	D2-D4/A0-A1,-(SP)	Save modified registers				*****
		LEA.L   msgRamTest1,A0	Point to top of message					*****
                BSR.W   printString	Print message						*****
		MOVE.L	D0,A0		Point A0 at lowest memory address			*****
                SUB.L	D1,D0		Subtract to get range, yes this will be negative	*****
		NEG.L	D0		Now D0 is positive, unless there was derp		*****
                MOVE.L  D0,D4           Store RAM Size                                          *****
                MOVE.L  #3,D3           Initialize outer loop counter                           *****
                LEA.L   MEMDAT,A1       Point to memory exerciser data                          *****
.chkloop0	MOVE.L  D4,D0           Retrieve memory size                                    *****
                MOVE.B  (A1,D3),D2      Get memory data                                         *****
.chkloop1	MOVE.B  D2,(A0,D0)      Write to memory                                         *****
                CMP.B   (A0,D0),D2      Compare with stored data                                *****
                BEQ.S   .chkloop1_1	Jump if match                                           *****
                ADDQ    #1,D7           Else increment error count                              *****
.chkloop1_1	DBRA    D0,.chkloop1	Test all of RAM                                         *****
                DBRA    D3,.chkloop0	For all data types                                      *****
                MOVE.L  D7,D3                                                                   *****
                BSR.W	printVal								*****
                LEA.L   msgRamTest2,A0                                                          *****		**
                BSR.W   printString                                                             *****		****
		MOVEM.L	(SP)+,D2-D4/A0-A1	Restore modified registers			**********************
                RTS                     Return from RAMcheck					************************
										**************************************
										*********************		****
										*********************		**



****************************************							*************************
* RAM sizer and diagnostic									*************************
*					D3 stores size of detected RAM in KB after completion	*************************

RAMsizer	EQU	*									*****
		LEA.L	msgRamSizing,A0								*****
		BSR.W   printString     Output messsage over serial port			*****
		MOVE.L	#$BFF,D1	Start at 3K mark to avoid disrupting existing 2K stack	*****
		MOVE.L  #$3,D3		D3 stores amount of RAM in KB				*****
		LEA.L	RAMBAS,A1								*****
		MOVE.B	#$76,D2		First test pattern, 0x76				*****
.loopSiz	MOVE.B	D2,(A1,D1)	Write to RAM						*****
		CMP.B	(A1,D1),D2	Compare to see if write was successful			*****
		BNE.S	.reportRAM	If failure, report size of detected RAM			*****
		NOT.B	D2		Invert test pattern					*****
		MOVE.B	D2,(A1,D1)	Retest memory location					*****
		CMP.B	(A1,D1),D2	Compare to see if write was successful			*****
		BNE.S	.reportRAM	If failure, report size of detected RAM			*****
		BSR.S	.progress	Print progess						*****
                ADDI.L	#$400,D1	Increment test point by 1KB				*****
		ADDI.W	#$1,D3									*****
		BRA.S	.loopSiz	Loop until failure					*****
												*****
.reportRAM	EQU	*									*****
		SUBI.W	#$1,D3		Correct KB count due to failed test after increment	*****
		LEA.L	msgRamFound1,A0								*****
		BSR.W	printString								*****
		BSR.S	printVal								*****		**
		LEA.L	msgRamFound3,A0								*****		****
		BSR.W	printString								**********************
		RTS			Return from RAMsizer					************************
												**********************		
												*****		****
												*****		**
												*****
.progress	MOVE.L	A0,-(SP)								*****
		LEA.L	msgRamFound1,A0								*****
		BSR.W	printString								*****
		BSR.S	printVal								*****
		LEA.L	msgRamFound2,A0								*****
		BSR.W	printString								*****
		MOVE.L	(SP)+,A0								*****
		RTS										*****
												*****
*					This is ugly as hell, I am looking at the BCD functions	*****
*					supported by the CPU to clean this up a bit		*****
printVal	MOVEM.L	D1-D3,-(SP)	Store register contents					*****
		EOR.L	D1,D1		Clear D1, will store character to be written		*****
		EOR.L	D2,D2		Clear D2, will be zero until first character written	*****
.loop10000	CMPI.W	#$2710,D3	Compare to 10000					*****
		BCS.S	.print10000	Branch if less than					*****
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
                MOVE.B   D3,D1		Clear D1, will store character to be written		*****			**
		BSR.S	.continue	No need to loop on 1s digit				*****			****
		MOVEM.L	(SP)+,D1-D3	Restore register contents				******************************
		RTS			GTFO							********************************
												******************************
.printChar	TST.B	D1		Test if digit is zero					*****			****
		BNE.S	.continue	Print if not						*****			**
		TST.B	D2		Else test if this would be first printed digit		*****
		BNE.S	.continue	Print if not						*****
		RTS			Else return without zero-padding			*****
												*****
.continue	ADDI.B	#'0',D1		Convert to ASCII					*****
		BSET.B  #7,MFPGPDR	Negate RTS to inhibit receive				*****
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
		BEQ.S	.printEnd	Check for null terminator				*****		**
		BSR.S	outChar		Write the character					*****		****
		BRA.S	.loop		Loop until null found					**********************
.printEnd	RTS										************************
												**********************
outChar		BSET.B  #7,MFPGPDR	Negate RTS to inhibit receiving				*****		****
.outloop	BTST.B	#7,MFPTSR	Check for empty transmit buffer				*****		**
		BEQ.S	.outloop	Loop until ready					*****
		MOVE.B	D0,MFPUDR	Put character into USART data register			*****
		RTS			ELSE RETURN WHEN COMPLETED				*****
										*********************
										*********************
										*********************


inChar		BCLR.B  #7,MFPGPDR	Enable RTS to allow receiving				*****
.poll		BTST.B	#7,MFPRSR	Check for empty receive buffer				*****
		BEQ.S	.poll		Loop until ready					*****
		MOVE.B	MFPUDR,D0								*****
		RTS										*****

		
readLine	MOVEM.L	D2/A2,-(SP)	Preserve registers which will be modified		************************
		LEA	varLineBuf,A2	Point to start of lineBuffer				************************
		EOR.W	D2,D2		Clear the character counter				************************
.loop		BSR.S	inChar		Read a character from the UART buffer			*****
		CMP.B	#BKSP,D0	Is user having difficulty typing?			*****
		BEQ.S	.backspace								*****
		CMP.B	#CTRLX,D0	Is user having second thoughts?				*****
		BEQ.S	.lineclear								*****
		CMP.B	#CR,D0		Did the user finally make up their mind?		*****
		BEQ.S	.endline								*****
		CMP.B	#LF,D0		We don't care about LFs... everything else continues	*****
		BEQ.S	.loop		and doesn't make this loop back				*****
.char		CMP.W	#MAX_LINELEN,D2	If the line is too long...				*****
		BGE.S	.loop		...we're going to stop listening to the user		*****
		MOVE.B	D0,(A2)+	Otherwise, store character				*****
		ADDQ.W	#1,D2		...and increment character count			*****
		BSR.S	outChar		Local echo						*****
		BRA.S	.loop		Get next character					*****
.backspace	TST.W	D2		Are we at beginning of the line?			*****
		BEQ.S	.loop		If so, user has doublefailed and we'll ignore BKSP	*****
*					but seriously, we should send a BEL just for insult	*****
		BSR.S	outChar		Remove the previous character from the user's console	*****
		MOVE.B	#' ',D0		via a complex and convoluted process of writing a space	*****
		BSR.S	outChar		after going back a space...				*****
		MOVE.B	#BKSP,D0	...and going back a space again				*****
		BSR.S	outChar		just to keep things tidy.				*****
		SUBQ.L	#1,A2		Move back in buffer one space				*****
		SUBQ.L	#1,D2		And reduce character count by one			*****
		BRA.S	.loop		Get next character					*****
.lineclear	TST	D2		Is there even data to clear?				*****
		BEQ.S	.loop		If not, just ignore and fetch next character		*****
		SUBA.L	D2,A2		Return to start of the buffer				*****
.lineclearloop	MOVE.B	#BKSP,D0								*****
		BSR.W	outChar		Backspace...						*****
		MOVE.B	#' ',D0									*****
		BSR.W	outChar		Spaaaaaaaaaaaaace....					*****
		MOVE.B	#BKSP,D0								*****
		BSR.W	outChar		Backspace...						*****
		SUBQ.W	#1,D2		Decrement character count				*****
		BNE.S	.lineclearloop	Repeat until start of line				*****
		BRA.S	.loop									*****
.endline	BSR.W	outChar		Echo the character					*****
		MOVE.B	#LF,D0									*****
		BSR.W	outChar		Throwing in a free LF for good measure			*****
		MOVE.B	#0,(A2)		Add null terminator...					*****		**
		MOVEA.L	A2,A0		Ready the pointer to get to the choppah			*****		****
		MOVEM.L	(SP)+,D2/A2	Restore registers				******************************
		RTS									********************************
											******************************
														****
														**


convertCase	LEA	varLineBuf,A0	Get start of line					************************
.loop		MOVE.B	(A0),D0									************************
		CMP.B	#'a',D0		Check if less than 'a'					************************
		BLT.S	.next									*****
		CMP.B	#'z',D0		Check if greater than 'z'				*****
		BGT.S	.next									*****
		SUB.B	#$20,D0		Convert to upper-case					*****		**
.next		MOVE.B	D0,(A0)+	Store the character back in A0, move to next		*****		****
		BNE.S	.loop		Loop until null terminator			******************************
		RTS									********************************
											******************************
														****
														**


parseLine	MOVEM.L	A2-A3,-(SP)	Save registers						************************
		LEA varLineBuf,A0	Get start of line					************************
.findCommand	MOVE.B	(A0)+,D0								************************
		CMP.B	#' ',D0		Ignore spaces						*****
		BEQ.W	.findCommand								*****
		CMP.B	#'E',D0		EXAMINE							*****
		BEQ.W	.examine								*****
		CMP.B	#'D',D0		DEPOSIT							*****
		BEQ.W	.deposit								*****
		CMP.B	#'R',D0		RUN							*****
		BEQ.W	.run									*****
		CMP.B	#'H',D0		HELP							*****
		BEQ.W	.help									*****
		CMP.B	#0,D0		BLANK LINE						*****
		BEQ.S	.exit									*****
.invalid	LEA	msgNoCMD,A0								*****		**
		BSR.W	printString								*****		****			
.exit		MOVEM.L	(SP)+,A2-A3	Restore registers					**********************		
		RTS										************************
												**********************
												*****		****
****************************************							*****		**
*   Examine											*****
*												*****
*   Modes:	e ADDR			Display a single byte					*****
*		e ADDR.			Display a single page (16 lines, 256 bytes)		*****
*		e ADDR-ADDR		Display all bytes between the two addresses		*****
*		e ADDR+LEN		Displays LEN bytes following ADDR			*****
*		e ADDR;			Interactive mode: SPACE shows 16 lines, ENTER shows 1	*****
												*****
.examine	BSR.W	parseNumber	Read start address					*****	
		TST.B	D1		Non-zero return on invalid address			*****
		BNE.W	.invalidAddr								*****
		MOVE.L	D0,A3		Save the start address					*****
.exloop		MOVE.B	(A0)+,D0								*****
		CMP.B	#' ',D0		Ignore spaces						*****
		BEQ.S	.exloop									*****
		CMP.B	#'-',D0		Check if range specified				*****
		BEQ.S	.exrange								*****
		CMP.B	#'+',D0		Check if lenth specified				*****
		BEQ.S	.exlength								*****
		CMP.B	#';',D0		Check if interactive requested				*****
		BEQ.S	.exinter								*****
		CMP.B	#'.',D0		Check if quick 16 is requested				*****
		BEQ.S	.exquick								*****
		MOVE.L	#1,D0		Otherwise read a single byte				*****
		BRA.S	.exend									*****
.exrange	BSR.W	parseNumber	Find the ending address					*****
		TST.B	D1		Non-zero return on invalid address			*****
		BNE.W	.invalidAddr								*****
		SUB.L	A3,D0		Get the length						*****
		BRA.S	.exend									*****
.exquick	MOVE.L	#$10,D0									*****
		BRA.S	.exend									*****
.exlength	BSR.W	parseNumber	Find the length						*****
		TST.B	D1		Non-zero return on invalid address			*****
		BNE.W	.invalidAddr								*****
.exend		MOVE.L	A3,A0		Parameter parsing complete, pass to dumpRAM and return	*****
		BSR.W	dumpRAM									*****
		BRA.S	.exit									*****
.exinter	MOVE.L	A3,A0		Interactive mode, set current address			*****
		MOVE.L	#$10,D0		16 bytes						*****
		BSR.W	dumpRAM									*****
		ADD.L	#$10,A3		Move current address up 16 bytes			*****
.exinterend	BSR.W	inChar									*****
		CMP.B	#CR,D0		Display another line					*****
		BEQ.S	.exinter								*****
		CMP.B	#' ',D0		Display a page (256 bytes)				*****
		BEQ.S	.exinterpage								*****
		CMP.B	#LF,D0		Disregard linefeeds					*****
		BEQ.S	.exinterend	Fetch new character on LF				*****
		BRA.S	.exit		Else exit on any other character			*****
.exinterpage	MOVE.L	A3,A0									*****
		MOVE.L	#$100,D0	256 bytes						*****
		BSR.W	dumpRAM		Dump 16 lines						*****
		ADD.L	#$100,A3	Adjust current address to match				*****
		BRA.S	.exinterend								*****
												  *
												  *
.deposit	MOVE.B	(A0),D0									*****
		CMP.B	#':',D0		Check if continuing from previous address		*****
		BEQ.S	.depCont								*****
												*****
		BSR.W	parseNumber	Read the address					*****
		TST.B	D1		Make certain that it is valid				*****
		BNE.S	.invalidAddr								*****
												*****
		MOVE.L	D0,A3		Save starting address					*****
.depLoop	MOVE.B	(A0),D0									*****
		CMP.B	#';',D0		Check for multi-line continuation			*****
		BEQ.S	.depMultiline								*****
		TST	D0		Check for end of line					*****
		BEQ	.depEnd									*****
												*****
		BSR.S	parseNumber	Read value						*****
		TST.B	D1		Test for validity					*****
		BNE.S	.invalidVal								*****
												*****
		MOVE.B	D0,(A3)+	Store the value into memory				*****
		BRA.S	.depLoop	...and fetch next value					*****
												*****
.depCont	MOVE.L	varCurAddr,A3	Read in the previous address				*****
		ADDQ.L	#1,A0		Skip over the ':'					*****
		BRA.S	.depLoop	...and fetch next value					*****
												*****
.depMultiline	LEA	msgDepPrompt,A0								*****
		BSR.W	printString								*****
		BSR.W	readLine	Read in the next line to be parsed			*****
		BSR.W	convertCase	Convert case						*****
		LEA	varLineBuf,A0	Reset buffer pointer					*****
		BRA.S	.depLoop	Jump back to decoding					*****
												*****
.depEnd		MOVE.L	A3,varCurAddr								*****
		BRA.W	.exit									*****
												  *
												  *
.run		BSR.W	parseNumber	Read in the address					*****
		TST.B	D1		Test for validity					*****
		BNE.S	.invalidAddr								*****
		MOVE.L	D0,A0									*****
		JSR	(A0)		Wheeeeeeeeeeeeeeeeeeeee!!!!				*****
		JSR	WARMSTART	Return to known state					*****
												  *
												  *
.help		LEA	msgHelp,A0								*****
		BSR.W	printString								*****
		BRA.W	.exit									*****
												  *
												  *
.invalidAddr	LEA	msgBadAddr,A0								*****
		BSR.W	printString								*****
		BRA.W	.exit									*****
												  *
												  *
.invalidVal	LEA	msgBadVal,A0								*****
		BSR.W	printString								*****
		BRA.W	.exit									*****
										*********************
										*********************
										*********************


parseNumber	EOR.L	D0,D0		Clear D0						************************
		MOVE.B	(A0)+,D0								************************
		CMP.B	#' ',D0		Ignore leading spaces					************************
		BEQ.S	parseNumber								*****
		CMP.B	#'0',D0		Look for hex digits 0-9					*****
		BLT.S	.invalid								*****
		CMP.B	#'9',D0									*****
		BLE.S	.firstdigit1								*****
		CMP.B	#'A',D0		Look for hex digits A-F					*****
		BLT.S	.invalid								*****
		CMP.B	#'F',D0									*****
		BLE.S	.firstdigit2								*****
.invalid	MOVE.L	#1,D1									*****
		RTS										*****
.firstdigit2	SUB.B	#'7',D0		Convert 'A' to 10, and so on				*****
		BRA.S	.loop									*****
.firstdigit1	SUB.B	#'0',d0		Convert '0' to 0, and so on				*****
.loop		MOVE.B	(A0)+,D1	Read in a digit						*****
		CMP.B	#'0',D1		Look for hex digits 0-9					*****
		BLT.S	.end		End loop on anything else				*****
		CMP.B	#'9',D1									*****
		BLE.S	.digit1									*****
		CMP.B	#'A',D1		Look for hex digits A-F					*****
		BLT.S	.end									*****
		CMP.B	#'F',D1									*****
		BLE.S	.digit2									*****		**
.end		SUBQ.L	#1,A0		Non-hex digit encountered, end parsing			*****		****
		MOVE.L	#0,D1		Move pointer back and clear D1				**********************
		RTS										************************
.digit2		SUB.B	#'7',D1		Convert 'A' to 10, and so on				**********************
		BRA.S	.digit3									*****		****
.digit1		SUB.B	#'0',D1		Convert '0' to 0, and so on				*****		**
.digit3		LSL.L	#4,D0		Shift to the next nybble				*****
		ADD.B	D1,D0		Place in our current nybble				*****
		BRA.S	.loop									*****
										*********************
										*********************
										*********************


dumpRAM		MOVEM.L	D2-D4/A2,-(SP)	Save registers						************************
		MOVE.L	A0,A2		Save start address					************************
		MOVE.L	D0,D2		Save number of bytes					************************
.line		MOVE.L	A2,D0									*****
		BSR.W	printHexLong	Starting address of the line				*****
		LEA	msgColonSpc,A0								*****
		BSR.W	printString								*****
		MOVE.L	#16,D3		16 bytes printed on a line				*****
		MOVE.L	D3,D4		Save number of bytes on this line			*****
.hexbyte	TST.L	D2		Check if out of bytes					*****
		BEQ.S	.endbytesShort								*****
		TST.B	D3		Check if line is finished				*****
		BEQ.S	.endbytes								*****
		MOVE.B	(A2)+,D0	Read in a byte from RAM					*****
		BSR.W	printHexByte	Display it						*****
		MOVE.B	#' ',D0									*****
		BSR.W	outChar		Separate bytes for readability				*****
		SUBQ.L	#1,D3									*****
		SUBQ.L	#1,D2									*****
		BRA.S	.hexbyte								*****
.endbytesShort	SUB.B	D3,D4		Set D4 to actual number of bytes on this line		*****
		MOVE.B	#' ',D0									*****
.endbytesShrtLp	TST.B	D3		Check if line ended					*****
		BEQ.S	.endbytes								*****
		MOVE.B	#' ',D0									*****
		BSR.W	outChar		Pad end with spaces					*****
		MOVE.B 	#' ',D0									*****
		BSR.W	outChar		Pad end with spaces					*****
		MOVE.B 	#' ',D0									*****
		BSR.W	outChar		Pad end with spaces					*****
		SUBQ.B	#1,D3									*****
		BRA.S	.endbytesShrtLp								*****
.endbytes	SUBA.L	D4,A2		Return to the start address of this line		*****
.endbytesLoop	TST.B	D4		Check if done printing ASCII				*****
		BEQ	.endline								*****
		SUBQ.B	#1,D4									*****
		MOVE.B	(A2)+,D0	Read the byte						*****
		CMP.B	#' ',D0		Check if character is in printable range		*****
		BLT.S	.unprintable								*****
		CMP.B	#'~',D0		Highest printable character				*****
		BGT.S	.unprintable								*****
		BSR.W	outChar									*****
		BRA.S	.endbytesLoop								*****
.unprintable	MOVE.B	#'.',D0									*****
		BSR.W	outChar									*****
		BRA.S	.endbytesLoop								*****
.endline	LEA	msgNewline,A0								*****
		BSR.W	printString								*****
		TST.L	D2									*****
		BLE.S	.end									*****		**
		BRA.W	.line									*****		****
.end		MOVEM.L	(SP)+,D2-D4/A2	Restore registers					**********************		
		RTS										************************	
										**************************************
										*********************		****
										*********************		**


printHexWord	MOVE.L	D2,-(SP)	Save D2							*****
		MOVE.L	D0,D2		Save address in D2					*****
		ROL.L	#8,D2		4321 -> 3214						*****
		ROL.L	#8,D2		3214 -> 2143						*****
		BRA.S	printHex_WrdEnt	Print last 16 bits					*****
												 ***
												  *
printHexAddr	MOVE.L	D2,-(SP)	Save D2							*****
		MOVE.L	D0,D2		Save address in D2					*****
		ROL.L	#8,D2		4321 -> 3214						*****
		BRA.S	printHex_AddEnt	Print last 24 bits					*****
												 ***
												  *
printHexLong	MOVE.L  D2,-(SP)	Save D2							*****
		MOVE.L  D0,D2		Save the address in D2					*****
		ROL.L   #8,D2		4321 -> 3214 high byte in low				*****
		MOVE.L  D2,D0									*****
		BSR.S   printHexByte	Print the high byte (24-31)				*****
												*****
printHex_AddEnt ROL.L   #8,D2		3214 -> 2143 middle-high byte in low			*****
		MOVE.L  D2,D0              							*****
		BSR.S   printHexByte	Print the high-middle byte (16-23)			*****
												*****
printHex_WrdEnt ROL.L   #8,D2		2143 -> 1432 Middle byte in low				*****
		MOVE.L  D2,D0									*****
		BSR.S   printHexByte	Print the middle byte (8-15)				*****
		ROL.L   #8,D2									*****
		MOVE.L  D2,D0									*****		**
		BSR.S   printHexByte	Print the low byte (0-7)				*****		****			
    		MOVE.L	(SP)+,D2	Restore D2						**********************
    		RTS										************************
										**************************************
										*********************		****
										*********************		**


printHexByte	MOVE.L	D2,-(SP)								************************
		MOVE.B	D0,D2									************************
		LSR.B	#$4,D0									************************
		ADD.B	#'0',D0									*****
		CMP.B	#'9',D0									*****	
		BLE.S	.second									*****
		ADD.B	#7,D0									*****
.second		BSR.W	outChar									*****
		ANDI.B	#$0F,D2									*****
		ADD.B	#'0',D2									*****
		CMP.B	#'9',D2									*****
		BLE.S	.end									*****
		ADD.B	#7,D2									*****
.end		MOVE.B	D2,D0									*****		**
		BSR.W	outChar									*****		****
		MOVE.L	(SP)+,D2								**********************
		RTS										************************
										**************************************
										*********************		****
										*********************		**

*****************************************************************************************************
*****************************************************************************************************
* TRAPS SECTION											*****
												*****
												*****

setTrap0	RTS										*****

setTrap1	MOVEQ	#32,D0		Traps start at vector 32				*****
		ADDI.B	#1,D0		Add 1 for TRAP 1					*****
		ASL.L	#2,D0		Multiply by 4 since each vector occupies 2 words	*****
		ADDI.L	#RAMBAS,D0	Add RAM offset since vector table starts there		*****
		MOVE.L	D0,A0		Put final location into address register		*****
		MOVE.L	#trap1,(A0)	Initialize with exception handler address		*****
		RTS			Return from subroutine					*****

setTrap15	MOVEQ	#32,D0		Traps start at vector 32				*****
		ADDI.B	#15,D0		Add 15 for TRAP 15, this number chosen to be in line	*****
		ASL.L	#2,D0		with the EAsy68k simulator	- multiply by 4		*****
		ADDI.L	#RAMBAS,D0	since each vector is 2 words.  Add RAM offset		*****
		MOVE.L	D0,A0		Put final location into address register		*****
		MOVE.L	#trap15,(A0)	Initialize with IO exception handler address		*****
		RTS			Return from subroutine					*****
* TRAP 0											*****
												*****

* TRAP 1				MFP GPIO Exception					*****
*					Value in D0 represents a GPIO 8 bit mask		*****
*					Value in D1 determines status of GPIO, 0=off, other=on	*****
trap1		TST.B	D1		Check if turning GPIO pins on or off			*****
		BNE.S	.gpio1		Jump to pin assertion if not zero			*****
		MOVE.B	MFPGPDR,D1	Store GPIO Data Register in D1				*****
		AND.B	D0,D1		Logical AND the contents of MFPGPDR with mask		*****
		SUB.B	D1,MFPGPDR	Subtract result from MFP GPIO Data Register		*****
		RTE			Return from exception					*****
.gpio1		OR.B	D0,MFPGPDR	Logical OR to assert all pins in mask if not already 1	*****
		RTE			Return from exception					*****	


* TRAP 15				IO Exception						*****
*					15 Chosen to match functionality with the EAsy68k sim	*****
*					Value of D0 determines function				*****
*												*****

trap15		CMP.B	#0,D0		D0= 0 Display string at (A1),D1.W bytes long w/CR+LF	*****
		BEQ.S	.io0		Branch to subroutine					*****

		CMP.B	#1,D0		D0= 1 Display string at (A1),D1.W bytes long w/o CR+LF	*****
		BEQ.S	.io1		Branch to subroutine					*****

		CMP.B	#2,D0		D0= 2 Read UART string, store in (A1), null terminated	*****
		BEQ.S	.io2		Branch to subroutine					*****

		CMP.B	#3,D0		D0= 3 Display signed number D1.L dec in smallest field	*****
		BEQ.S	.io3		Branch to subroutine					*****

		CMP.B	#4,D0		D0= 4 Read number from UART into D1.L			*****
		BEQ.S	.io4		Branch to subroutine					*****

		CMP.B	#5,D0		D0= 5 Read single char from UART into D1.B		*****
		BEQ.S	.io5		Branch to subroutine					*****

		CMP.B	#6,D0		D0= 6 Display single character in D1.B			*****
		BEQ.S	.io6		Branch to subroutine					*****

		CMP.B	#7,D0		D0= 6 Set D1.B to 1 if UART RX buffer full, otherwise 0	*****
		BEQ.S	.io7		Branch to subroutine					*****

		CMP.B	#8,D0		D0= 6 Display single character in D1.B			*****
		BEQ.S	.io8		Branch to subroutine					*****


.ioExcEnd	RTE			Return from exception					*****

.io0		BEQ.S	.ioExcEnd	Return from exception					*****

.io1		BEQ.S	.ioExcEnd	Return from exception					*****

.io2		BEQ.S	.ioExcEnd	Return from exception					*****

.io3		BEQ.S	.ioExcEnd	Return from exception					*****

.io4		BEQ.S	.ioExcEnd	Return from exception					*****

												*****
.io5		BTST.B  #7,MFPRSR	Check for empty receive buffer				*****
		BEQ.S	.ioExcEnd	Bail if nothing to return				*****
		MOVE.B  MFPUDR,D1	Else move character to D1				*****
.end		BEQ.S	.ioExcEnd	Return from exception					*****

.io6		BSET.B  #7,MFPGPDR	Negate RTS to inhibit receiving				*****
.outloop	BTST.B  #7,MFPTSR	Check for empty transmit buffer				*****
		BEQ.S   .outloop	Loop until ready					*****
		MOVE.B  D1,MFPUDR	Put character into USART data register			*****
		BEQ.S	.ioExcEnd	Return from exception					*****

.io7		BCLR.B	#7,MFPGPDR	Assert RTS to allow receiving				*****
		EOR.L   D1,D1		D1 will store return, 1 = buffer full			*****
		BTST.B	#7,MFPRSR	Check if character is still in RX buffer		*****
		BEQ.S	.ioExcEnd	Bail if empty						*****
		MOVEQ	#1,D1		Set return to indicate character waiting		*****
		BEQ.S	.ioExcEnd	Return from exception					*****

.io8		BEQ.S	.ioExcEnd	Return from exception					*****

*****************************************************************************************************
*****************************************************************************************************
* MESSAGES SECTION										*****
												*****
												*****
OKMSG		DC.B	'<RESET>',CR,LF								*****
		DC.B	'MC68901 Multifunction Peripheral Initialized',CR,LF,LF,0		*****
												*****
ERRMSG		DC.B	'<RESET>',CR,LF								*****
		DC.B	'MEMORY ERRORS ENCOUNTERED...',CR,LF,'>',0				*****
												*****
msgBanner	DC.B	'============================',CR,LF					*****
		DC.B	'RHOMBUS 68020 System Monitor',CR,LF					*****
		DC.B	'============================',CR,LF					*****
		DC.B	'ver 0.3	  18-DEC-2015',CR,LF,0					*****
												*****
msgPrompt	DC.B	'>',0									*****
												*****
msgRamSizing	DC.B	'RAM detection in progress...',CR,LF,0					*****
												*****
msgRamFound1	DC.B	CR,'Detected: ',0							*****
msgRamFound2	DC.B	'KB      ',CR,0								*****
msgRamFound3	DC.B	'KB',CR,LF,LF,0								*****
												*****
msgRamTest1	DC.B	'Testing RAM...',CR,LF,0						*****
msgRamTest2	DC.B	' errors found',CR,LF,0							*****
msgNoCMD	DC.B	'Invalid Command',CR,LF,0						*****
msgBadAddr	DC.B	'Invalid Address',CR,LF,0
msgBadVal	DC.B	'Invalid Value',CR,LF,0
msgNewline	DC.B	CR,LF,0
msgColonSpc	DC.B	': ',0

msgDepPrompt	DC.B	': ',0

msgHelp		DC.B	'Available Commands: ',CR,LF
		DC.B	' [E]xamine	[D]eposit	[R]un	[H]elp',CR,LF,0
		END	START	

