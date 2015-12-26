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
****************************************							*****
*   Defines											*****
												*****
NOP		EQU	$4E71		STANDARD 68000 NOP INSTRUCTION				*****
												*****
****************************************							*****
*   Addresses											*****
												*****
ROMBAS		EQU	0		ROM BASE ADDRESS					*****
RAMBAS		EQU	$F00000		RAM BASE ADDRESS					*****
MFPBAS		EQU	$EF0000		MFP BASE ADDRESS					*****
MFPVCT		EQU	$40		VECTOR FOR MFP SOURCED INTERRUPT			*****
STACK		EQU	$F00800									*****
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
SPACE		EQU	$20									 ***
CTRLA		EQU	$01									  *

*****************************************************************************************************
*****************************************************************************************************
*	Program section										*****
*	The ROM in this BIOS is mapped to the variable						*****
*	ROMBAS.  All executable code is resident in ROM.					*****
START		EQU	ROMBAS									*****
		DC.L	STACK		INITIAL STACK POINTER					*****
		DC.L	RESET		INITIAL PROGRAM COUNTER					*****
ROMBUF		DS.L	32
MEMDAT		EQU	*		MEMORY EXERCISER DATA					*****	
		DC.B	$5									*****
		DC.B	$A									*****
		DC.B	$0									*****
		DC.B	$F
		DS.L	$100									*****
RESET		EQU	*		COLD ENTRY POINT					*****
LORAMINIT	EQU	*		THIS WILL TEST AND PREP LOWEST 2K OF RAM 		*****
*	This routine performs a quick check of memory prior to					*****
*	proceeding.  The count of errors is stored in D7 at completion				*****
*	We avoid using stack until the first 2K of RAM is deemed safe				*****
		CLR.L	D7		CLEAR ERROR COUNTER					*****
		MOVE.L	#3,D3		INIT OUTER LOOP COUNTER					*****
		LEA.L	RAMBAS,A0	POINT TO BASE OF RAM					*****
		LEA.L	MEMDAT,A1	POINT TO MEMORY EXERCISER DATA				*****
												*****
.loop0		EQU	*									*****
		MOVE.L	#$7FF,D0	INIT INNER LOOP COUNTER, INITIALLY CHECK 2K		*****
		MOVE.B	(A1,D3),D2	GET MEMORY DATA						*****
												*****
.loop1		EQU	*									*****
		MOVE.B	D2,(A0,D0)	PUT DATA INTO MEMORY					*****
		CMP.B	(A0,D0),D2	COMPARE WITH STORED DATA				*****
		BEQ.S	.loop1_1	JUMP IF MATCHES						*****
		ADDQ	#1,D7		ELSE INCREMEMBT ERROR COUNT				*****
												*****
.loop1_1		EQU	*								*****
		DBRA	D0,.loop1	TEST ALL OF RAM						*****
		DBRA	D3,.loop0	FOR ALL DATA TYPES (4 TESTS)				*****
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
* END RAM INITIALIZATION		
		LEA	RAMBAS,A6	POINT A6 TO DATA AREA					*****
		CLR.L	UCOMTAB(A6)	RESET USER COMMAND TABLE POINTER			*****
		BSR.S	CFGUART		CONFIGURE UART						*****
		LEA.L	msgPOR,A0	SEND POWER-ON-RESET MESSAGE
		BSR.W	printString
		LEA.L	msgLORAMck,A0	SEND LOW RAM CHECK MESSAGE
		BSR.W	printString
		LEA.L	msgOK,A0								*****
		BSR.W	printString								*****
		LEA.L	msgEXTABinit,A0								*****
		BSR.W	printString								*****
		BSR	EXCSET		SET UP EXCEPTION TABLE					*****
		LEA.L	msgOK,A0								*****
		BSR.W	printString								*****
		LEA.L	msgDCBinit,A0								*****
		BSR.W	printString								*****
		BSR	SET_DCB		SET UP DCB TABLE IN RAM					*****
		LEA.L	msgOK,A0								*****
		BSR.W	printString								*****
		MOVE.B	#0,ECHO(A6)	ENABLE LOCAL ECHO					*****
		LEA	BANNER,A4	POINT TO BANNER						*****
		BSR.W	HEADING		PRINT THE BANNER					*****
WARM		CLR.L	D7		CLEAR ERROR FLAG					*****
		BSR.W	NEWLINE		PRINT A NEWLINE						*****
		BSR.W	GETLINE		GET A COMMAND LINE					*****
		BSR.W	TIDY		CLEAN UP COMMAND LINE BUFFER				*****
		BSR.W	EXECUTE		INTERPRET AND EXECUTE COMMAND				*****
		BRA	WARM									*****
												 ***
												  *
*****************************************************************************************************
*****************************************************************************************************
* INITIALIZATION SECTION                                                                        *****
                                                                                                *****
CFGUART		EQU	*
		BSR.W	MFPINIT
		RTS


EXCSET		EQU	*		SET UP EXCEPTION TABLE
		MOVE.L	#RAMBAS,D0	POINT TO BASE OF RAM					*****
		MOVEC.L	D0,VBR		INITIALIZE VBR						*****
		LEA	RAMBAS,A0	POINT TO VECTOR BASE					*****
		MOVE.L	#ER_BUS,8(A0)	SET UP BUS ERROR EXCEPTION				*****
		MOVE.L	#ER_ADDR,12(A0)	SET UP ADDRESS ERROR EXCEPTION				*****
		MOVE.L	#ER_ILOP,16(A0) SET UP ILLEGAL OPERATION EXCEPTION			*****
		MOVE.L	#TRACE,36(A0)	SET UP TRACE EXCEPTION					*****
		MOVE.L	#TRAP0,128(A0)	SET UP TRAP #0 EXCEPTION				*****
		MOVE.L	#BRKPT,184(A0)	SET UP TRAP #14 = BREAKPOINT VECTOR			*****
		MOVE.L	#WARM,188(A0)	SET UP TRAP #15 EXCEPTION VECTOR			*****
												 ***
		MOVE.W	#7,D0		CLEAR THE BREAKPOINT TABLE				*****
		LEA	BKPTAB(A6),A0	POINT TO TABLE						*****
EXCSET_2	CLR.L	(A0)+		CLEAR AN ADDRESS ENTRY					*****
		CLR.W	(A0)+		CLEAR A DATA ENTRY					*****
		DBRA	D0,EXCSET_2	REPEAT FOR REMAINING TABLE ENTRIES			*****
		RTS										*****
												 ***
TRAP0		EQU	*									*****
		CMP.B	#0,D1		D1 =  0 = Get character					*****
		BNE.S	TRAP1									*****
		BSR	GETCHAR									*****
		RTE										*****
												 ***
TRAP1		CMP.B	#1,D1		D1 =  1 = Print character				*****
		BNE.S	TRAP2									*****
		BSR	PUTCHAR									*****
		RTE										*****
												 ***
TRAP2		CMP.B	#2,D1		D2 =  2 = Newline					*****
		BNE.S	TRAP3									*****
		BSR	NEWLINE									*****
		RTE										*****
												 ***
TRAP3		RTE
ER_BUS		RTE
ER_ADDR		RTE
ER_ILOP		RTE
TRACE		RTE
BRKPT		RTE


*****************************************************************************************************
*****************************************************************************************************
* INPUT/OUTPUT SUBROUTINES SECTION								*****
												*****
IO_REQ		MOVEM.L	A0-A1,-(A7)	BACK UP REGISTERS					*****
		LEA	8(A0),A1	A1 POINTS TO DEVICE HANDLER FIELD			*****
		MOVE.L	(A1),A1		A1 CONTAINS DEVICE HANDLER ADDRESS			*****
		JSR	(A1)		CALL DEVICE HANDLER					*****
		MOVEM.L	(A7)+,A0-A1	RESTORE REGISTERS					*****
		RTS										*****
												 ***
CON_IN		MOVEM.L	D1/A1,-(A7)	BACK UP REGISTERS					*****
		LEA	12(A0),A1	GET POINTER TO DEVICE FROM DCB				*****
		MOVE.L	(A1),A1		PUT ADDRESS OF DEVICE IN A1				*****
		CLR.B	19(A0)		CLEAR LOGICAL ERROR IN DCB				*****
		BCLR.B	#7,$1(A1)	ENABLE RTS TO ALLOW RECEIVE				*****
CON_I1		MOVE.B	$2B(A1),D1	READ RECEIVE STATUS REGISTER				*****
		BTST	#7,D1		CHECK RECEIVE BUFFER FULL				*****
		BEQ.S	CON_I1		LOOP UNTIL READY					*****
		MOVE.B	D1,18(A0)	STORE RECEIVER STATUS REGISTER IN DCB			*****
		MOVE.B	$2F(A1),D0	READ USART DATA REGISTER				*****
		MOVEM.L	(A7)+,D1/A1	RESTORE REGISTERS					*****
		RTS										*****
												 ***
CON_OUT		MOVEM.L	A1/D1-D2,-(A7)	BACK UP REGISTERS					*****
		LEA	12(A0),A1	GET POINTER TO DEVICE FROM DCB				*****
		MOVE.L	(A1),A1		PUT ADDRESS OF DEVICE IN A1				*****
		CLR.B	19(A0)		CLEAR LOGICAL ERROR IN DCB				*****
		BSET.B	#7,$1(A1)	NEGATE RTS TO INHIBIT RECEIVE				*****
CON_O1		MOVE.B	$2D(A1),D1	READ TRANSMIT STATUS REGISTER				*****
		BTST	#7,D1		CHECK TRANSMIT BUFFER FULL				*****
		BEQ.S	CON_O1		LOOP UNTIL TRANSMIT BUFFER EMPTY			*****
		MOVE.B	D1,18(A0)	STORE STATUS REGISTER IN DCB				*****
		MOVE.B	D0,$2F(A1)	PUT CHARACTER INTO USART DATA REGISTER			*****
		MOVEM.L	(A7)+,A1/D1-D2	RESTORE WORKING REGISTERS				*****
		RTS										*****
												 ***
BUFF_IN		LEA	12(A0),A1	A1 POINTS TO INPUT BUFFER				*****
		MOVE.L	(A1),A2		A2 HAS INPUT POINTER FROM BUFFER			*****
		MOVE.B	-(A2),D0	READ CHAR FROM BUFFER AND MOVE A2			*****
		MOVE.L	A2,(A1)		RESTORE POINTER IN BUFFER				*****
		RTS										*****
												 ***
BUFF_OUT	LEA	12(A0),A1	A1 POINTS TO OUTPUT BUFFER				*****
		MOVE.L	4(A1),A2	A2 HAS OUTPUT POINTER FROM BUFFER			*****
		MOVE.B	D0,(A2)+	PUT CHAR INTO BUFFER AND MOVE A2			*****
		MOVE.L	A2,(A1)		RESTORE POINTER IN BUFFER				*****
		RTS										*****
												 ***
GETCHAR		MOVE.L	A0,-(A7)	BACK UP REGISTER					*****
		MOVE.L	CONiVEC(A6),A0	A0 POINTS TO NAME OF CONSOLE DCB			*****
		BSR.S	IO_OPEN		OPEN CONSOLE BY PLACING DCB ADDRESS IN A0		*****
		BTST	#3,D7		THIS BIT SET IF OPEN ERROR				*****
		BNE.S	GETCHAR3	BAIL IF ERROR						*****
		BSR	IO_REQ		OTHERWISE DO I/O TRANSACTION				*****
		AND.B	#$7F,D0		STRIP MSB OF INPUT					*****
		TST.B	UCASE(A6)	TEST FOR UPPER -> LOWER CONVERSION			*****
		BNE.S	GETCHAR2	IF FLAG SET, DO NOT PERFORM CONVERSION			*****
		BTST	#5,D0		TEST IF LOWER CASE					*****
		BEQ.S	GETCHAR2	IF UPPER CASE, DO NOT PERFORM CONVERSION		*****
		AND.B	#%11011111,D0	CLEAR BIT 5 TO CONVERT TO UPPER CASE			*****
GETCHAR2	TST.B	ECHO(A6)	DO WE WANT ECHO?					*****
		BNE.S	GETCHAR3	NOT ZERO = NO ECHO					*****
		CMP.B	#BKSP,D0	CHECK FOR BACKSPACE, WE WON'T PRINT IT HERE, BUT RATHER	*****
		BEQ.S	GETCHAR3	HANDLE IT LATER IN GETLINE SUBROUTINE			*****
		BSR.S	PUTCHAR		OTHERWISE ECHO						*****
GETCHAR3	MOVE.L	(A7)+,A0	RESTORE REGISTER					*****
		RTS										*****
												 ***
PUTCHAR		MOVE.L	A0,-(A7)	BACK UP REGISTER					*****
		MOVE.L	CONoVEC(A6),A0	A0 POINTS TO NAME OF CONSOLE OUTPUT DCB			*****	
		BSR.S	IO_OPEN		OPEN CONSOLE						*****
		BSR	IO_REQ		PERFORM OUTPUT						*****
		MOVE.L	(A7)+,A0	RESTORE REGISTER					*****
		RTS										*****
												 ***
IO_OPEN		MOVEM.L	A1-A3/D0-D4,-(A7)							*****
		LEA	FIRST(A6),A1	A1 POINTS TO FIRST DCB IN RAM				*****
OPEN1		LEA	(A1),A2		A2 IS TEMP COPY OF POINTER TO DCB			*****
		LEA	(A0),A3		A3 IS TEMP COPY OF POINTER TO DCB NAME			*****
		MOVE.W	#7,D0		MATCH UP TO 8 CHARACTERS OF DCB NAME			*****
OPEN2		MOVE.B	(A2)+,D4	COMPARE DCB NAME WITH STRING				*****
		CMP.B	(A3)+,D4								*****
		BNE.S	OPEN3		NO MATCH, TRY NEXT DCB					*****
		DBRA	D0,OPEN2	ELSE REPEAT UNTIL ALL CHARS MATCHED			*****
		LEA	(A1),A0		MATCH, MOVE THIS DCB ADDRESS TO A0			*****
		BRA.S	OPEN4									*****
OPEN3		EQU	*		NO MATCH, FIND ADDRESS OF NEXT DCB			*****
		MOVE.W	16(A1),D1	GET PARAMETER BLOCK SIZE OF DCB				*****
		LEA	18(A1,D1.W),A1	A1 POINTS TO POINTER OF NEXT DCB			*****
		MOVE.L	(A1),A1		A1 POINTS TO NEXT DCB					*****
		CMP.L	#0,A1		TEST FOR END OF DCB LIST				*****
		BNE	OPEN1		TRY NEXT DCB IF NOT AT END				*****
		OR.B	#8,D7		ELSE FLAG ERROR AND BAIL				*****
OPEN4		MOVEM.L	(A7)+,A1-A3/D0-D4							*****
		RTS										*****
												 ***
NEWLINE		EQU	*		MOVE CURSOR TO BEGINNING OF NEW LINE			*****
		MOVEM.L	A4,-(A7)	SAVE REGISTER						*****
		LEA	CRLF,A4		POINT TO CR/LF STRING					*****
		BSR.S	PRINTSTRING	PRINT STRING						*****
		MOVEM.L	(A7)+,A4	RESTORE REGISTER					*****
		RTS										*****
												 ***
GETLINE		LEA	LNBUF(A6),A1	A1 POINTS TO START OF LINE BUFFER			*****
		LEA	(A1),A3		A3 POINTS TO START					*****
		LEA	MAXCHR(A1),A2	A2 POINTS TO END					*****
GLN2		BSR	GETCHAR		GET A CHARACTER						*****
		CMP.B	#LF,D0		IGNORE LINEFEEDS					*****
		BEQ.S	GLN2		AND FETCH ANOTHER CHARACTER				*****
		CMP.B	#CTRLA,D0	IF CONTROL-A, REJECT LINE				*****
		BEQ.S	GLN5		AND FETCH A NEW LINE					*****
		CMP.B	#BKSP,D0	IF BACKSPACE, THEN ROLL BACK POINTER			*****
		BNE.S	GLN3		ELSE SKIP OVERLY-COMPLEX DELETE PROCESS			*****
		CMP.L	A1,A3		CHECK FOR EMPTY BUFFER FIRST TO AVOID BAD THINGS	*****
		BEQ	GLN2		IF ALREADY EMPTY, FETCH ANOTHER CHARACTER		*****
		MOVE.B	#BKSP,D0	DELETE THE SPACE TO MOVE CURSOR BACK ON SPACE		*****
		BSR.W	PUTCHAR									*****
		MOVE.B	#' ',D0		WRITE OVER DELETED CHARACTER WITH A SPACE		*****
		BSR.W	PUTCHAR									*****
		MOVE.B	#BKSP,D0	AND DELETE THE SPACE TO MOVE CURSOR BACK ON SPACE	*****
		BSR.W	PUTCHAR									*****
		LEA	-1(A3),A3	ELSE DECREMENT POINTER					*****
		BRA	GLN2		AND FETCH ANOTHER CHARACTER				*****
GLN3		MOVE.B	D0,(A3)+	STORE CHARACTER INTO BUFFER				*****
		CMP.B	#CR,D0		CHECK IF END OF LINE					*****
		BNE.S	GLN4		SKIP EXIT IF NOT CR					*****
*		BRA	NEWLINE		PRINT NEW LINE						*****
		RTS			RETURN ON CR						*****
GLN4		CMP.L	A2,A3		TEST FOR BUFFER OVERFLOW				*****
		BNE	GLN2		IF NOT FULL, GET NEW CHARACTER				*****
GLN5		BSR	NEWLINE		ELSE MOVE TO NEXT LINE					*****
		BRA	GETLINE		AND REPEAT						*****
												 ***
PRINTSTRING	EQU	*		DISPLAY STRING REFERENCED BY A4				*****
		MOVE.L	D0,-(A7)	SAVE REGISTER						*****
PRNT1		MOVE.B	(A4)+,D0	GET CHARACTER TO BE PRINTED				*****
		BEQ.S	PRNT2		BAIL IF NULL TERMINATOR					*****
		BSR	PUTCHAR		OTHERWISE PRINT						*****
		BRA	PRNT1		LOOP UNTIL NULL TERMINATOR FOUND			*****
PRNT2		MOVE.L	(A7)+,D0	RESTORE REGISTER					*****
		RTS										*****
												 ***
HEADING		BSR	PRINTSTRING								*****
		BRA	NEWLINE									*****
												 ***
TIDY		LEA	LNBUF(A6),A0	POINT A0 TO LINE BUFFER					*****
		LEA	(A0),A1		POINT A1 TO START OF LINE BUFFER			*****
TIDY1		MOVE.B	(A0)+,D0	READ A CHARACTER INTO D0				*****
		CMP.B	#SPACE,D0	CHECK FOR LEADING WHITESPACE				*****
		BEQ.S	TIDY1		LOOP UNTIL NON-WHITESPACE CHARACTER FOUND		*****
		LEA	-1(A0),A0	MOVE POINTER BACK TO FIRST CHARACTER			*****
TIDY2		MOVE.B	(A0)+,D0	MOVE STRING LEFT TO REMOVE LEADING SPACES		*****
		MOVE.B	D0,(A1)+								*****
		CMP.B	#SPACE,D0	CHECK FOR MID-LINE SPACE				*****
		BNE.S	TIDY4		CHECK FOR CR IF NOT					*****
TIDY3		CMP.B	#SPACE,(A0)+	SKIP OVER CONSEQUTIVE MID-LINE SPACES			*****
		BEQ.S	TIDY3									*****
		LEA	-1(A0),A0	MOVE POINTER BACK					*****
TIDY4		CMP.B	#CR,D0		TEST FOR CR						*****
		BNE.S	TIDY2		IF NO MATCH, READ NEXT CHARACTER			*****
		LEA	LNBUF(A6),A0	RESTORE BUFFER POINTER					*****
TIDY5		CMP.B	#CR,(A0)	TEST FOR CR						*****
		BEQ.S	TIDY6		BAIL IF CR MATCHED					*****
		CMP.B	#SPACE,(A0)+	TEST FOR COMMAND DELIMITER				*****
		BNE.S	TIDY5		REPEAT UNTIL DELIMITER OR CR MATCHED			*****
TIDY6		MOVE.L	A0,BUFPT(A6)	UPDATE BUFFER POINTER					*****
		RTS										*****
												 ***
EXECUTE		RTS										*****
												 ***
												  *
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
*****************************************************************************************************
*****************************************************************************************************
* DEVICE SECTION										*****
												*****
****************************************							*****
*  DEVICE CONTROL BLOCKS									*****
												*****
SET_DCB		MOVEM.L	A0-A3/D0-D3,-(A7)							*****
		LEA	FIRST(A6),A0	POINTER TO FIRST DDB RAM DESTINATION			*****
		LEA	DCB_LST,A1	POINTER TO FIRST DCB ROM DECLARATION			*****
		MOVE.W	#3,D0		4 DCB(s) TO SET UP (N-1)				*****
SET_DCB1	MOVE.W	#15,D1		16 BYTE DCB HEADER					*****
SET_DCB2	MOVE.B	(A1)+,(A0)+	MOVE BYTE FROM ROM TO RAM				*****
		DBRA	D1,SET_DCB2	REPEAT FOR ENTIRE HEADER				*****
		MOVE.W	(A1)+,D3	GET NUMBER OF BYTES IN PARAMETER BLOCK			*****
		MOVE.W	D3,(A0)		STORE SIZE OF DCB IN RAM				*****
		LEA	2(A0,D3.W),A0	A0 NOW POINTS TO END OF DCB IN RAM			*****
		LEA	4(A0),A3	A3 POINTS TO ADDRESS OF NEXT DCB IN RAM			*****
		MOVE.L	A3,(A0)		STORE POINTER TO NEXT DCB IN THIS ONE			*****
		LEA	(A3),A0		A0 NOW POINTS TO NEXT DCB IN RAM			*****
		DBRA	D0,SET_DCB1	REPEAT FOR ALL DCBs					*****
		LEA	-4(A3),A3	A3 POINTS TO LAST DCB POINTER				*****
		CLR.L	(A3)		LAST POINTER IS ZEROED FOR NULL TERMINATION		*****
		MOVE.L	#DCB1,CONiVEC(A6)	SET VECTOR TO CONSOLE INPUT DCB			*****
		MOVE.L	#DCB2,CONoVEC(A6)	SET VECTOR TO CONSOLE OUTPUT DCB		*****
		MOVEM.L	(A7)+,A0-A3/D0-D3							*****
		RTS										*****
												*****
DCB_LST		EQU	*									*****
DCB1		DC.B	'CON_IN  '	DEVICE NAME						*****
		DC.L	CON_IN,MFPBAS	ADDRESS OF DRIVER ROUTINE, DEVICE			*****
		DC.W	2		NUMBER OF PARAMETER WORDS				*****
DCB2		DC.B	'CON_OUT '	DEVICE NAME						*****
		DC.L	CON_OUT,MFPBAS	ADDRESS OF DRIVER ROUTINE, DEVICE			*****
		DC.W	2		NUMBER OF PARAMETER WORDS				*****
DCB3		DC.B	'BUF_IN  '	DEVICE NAME						*****
		DC.L	BUFF_IN,BUFFER	ADDRESS OF DRIVER ROUTINE, DEVICE			*****
		DC.W	2		NUMBER OF PARAMETER WORDS				*****
DCB4		DC.B	'BUF_OUT '	DEVICE NAME						*****
		DC.L	BUFF_OUT,BUFFER	ADDRESS OF DRIVER ROUTINE, DEVICE			*****
		DC.W	2		NUMBER OF PARAMETER WORDS				*****


****************************************							*****
*  MC68901 INITIALIZATION									*****
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
* STRINGS AND FIXED PARAMETERS									*****
												*****
msgPOR		DC.B	'<RESET>',CR,LF,0
msgLORAMck	DC.B	'Checking low memory...            ',0					*****
msgOK		DC.B	'OK',CR,LF,0
msgEXTABinit	DC.B	'Initializing Exception Table...   ',0					*****
msgDCBinit	DC.B	'Creating Device Control Blocks... ',0					*****

BANNER		DC.B	'RHOMBUS Monitor version 0.2015.12.26.0',0,0				*****
CRLF		DC.B	CR,LF,'>',0								*****

****************************************							*****
*   Environment Parameter Equates								*****
												*****
MAXCHR		EQU	64		MAXIMUM LENGTH OF COMMAND LINE				*****
		ORG	$C00
DATA		EQU	$00000C00	DATA ORIGIN						*****
LNBUF		DS.B	MAXCHR		COMMAND LINE BUFFER					*****
BUFEND		EQU	LNBUF+MAXCHR-1	END OF COMMAND LINE POINTER				*****
BUFPT		DS.L	1		COMMAND LINE POINTER					*****
PARAM		DS.L	1		LAST COMMAND LINE PARAMETER				*****
ECHO		DS.B	1		LOCAL ECHO ON CLEAR					*****
UCASE		DS.B	1		UPPER CASE CONVERSION FLAG				*****
UCOMTAB		DS.L	1		USER COMMAND TABLE POINTER				*****
CONiVEC		DS.L	1		CONSOLE INPUT DCB POINTER				*****
CONoVEC		DS.L	1		CONSOLE OUTPUT DCB POINTER				*****
TSK_T		DS.W	37		FRAME: D0-D7/A0-A6/USB/SSP/SW/PC			*****
BKPTAB		DS.W	24		BREAKPOINT TABLE					*****
FIRST		DS.B	512		DCB AREA						*****
BUFFER		DS.B	256		256 BYTE I/O BUFFER					*****
												*****

		END	START												  
