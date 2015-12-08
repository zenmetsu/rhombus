*	Equates section

ROMBAS		EQU	0		ROM BASE ADDRESS
RAMBAS		EQU	$F00000		RAM BASE ADDRESS
STACK		EQU	$F003FF		INITIAL STACK POINTER
MFPBAS		EQU	$EF0000		MFP BASE ADDRESS
MFPVCT		EQU	$40		VECTOR FOR MFP SOURCED INTERRUPT
NOP		EQU	$4E71		STANDARD 68000 NOP INSTRUCTION

*	MC68901 MFP Registers

MFPGPIP		EQU	MFPBAS+$1	GPIP DATA
MFPAER		EQU	MFPBAS+$3	ACTIVE EDGE
MFPDDR		EQU     MFPBAS+$5
MFPIERA		EQU     MFPBAS+$7
MFPIERB 	EQU     MFPBAS+$9
MFPIPRA		EQU     MFPBAS+$B
MFPIPRB		EQU     MFPBAS+$D
MFPISRA		EQU     MFPBAS+$F
MFPISRB		EQU     MFPBAS+$11
MFPIMRA		EQU     MFPBAS+$13
MFPIMRB		EQU     MFPBAS+$15
MFPVR		EQU     MFPBAS+$17
MFPTACR		EQU     MFPBAS+$19
MFPTBCR		EQU     MFPBAS+$1B
MFPTCDCR	EQU     MFPBAS+$1D
MFPTADR		EQU     MFPBAS+$1F
MFPTBDR		EQU     MFPBAS+$21
MFPTCDR		EQU     MFPBAS+$23
MFPTDDR		EQU     MFPBAS+$25
MFPSCR		EQU     MFPBAS+$27
MFPUCR		EQU     MFPBAS+$29
MFPRSR		EQU     MFPBAS+$2B
MFPTSR		EQU     MFPBAS+$2D
MFPUDR		EQU     MFPBAS+$2F

*	Program section
*	The ROM in this BIOS is mapped to the variable
*	ROMBAS.  All executable code is resident in ROM.

START		EQU	ROMBAS
		DC.L	STACK		INITIAL STACK POINTER
		DC.L	ROMSTART	INITIAL PROGRAM COUNTER

ROMBUF		DS.L	32		LEAVE A LITTLE SPACE

MEMDAT		EQU	*		MEMORY EXERCISER DATA
*					THIS IS USED TO CHECK MEMORY
		DC.B	$5
		DC.B	$A
		DC.B	$0
		DC.B	$F
		DS.L	$100		LEAVE MORE SPACE

ROMSTART	EQU	*		BEGINNING OF PROGRAM SECTION
*					POINT TO BASE OF RAM
		MOVE.L	#RAMBAS,D0
		MOVEC.L	D0,VBR		AND INITIALIZE VBR TO POINT THERE

*					*** MEMORY EXERCISER ***
*	This routine performs a quick check of memory prior to
*	proceeding.  The count of errors is stored in D7 at completion

		CLR.L	D7		CLEAR ERROR COUNTER
		MOVE.L	#3,D3		INIT OUTER LOOP COUNTER
		LEA.L	RAMBAS,A0	POINT TO BASE OF RAM
		LEA.L	MEMDAT,A1	POINT TO MEMORY EXERCISER DATA

LOOP0		EQU	*
		MOVE.L	#$1FFF,D0	INIT INNER LOOP COUNTER
		MOVE.B	(A1,D3),D2	GET MEMORY DATA

LOOP1		EQU	*
		MOVE.B	D2,(A0,D0)	PUT DATA INTO MEMORY
		CMP.B	(A0,D0),D2	COMPARE WITH STORED DATA
		BEQ.S	LOOP1_1		JUMP IF MATCHES
		ADDQ	#1,D7		ELSE INCREMEMBT ERROR COUNT

LOOP1_1		EQU	*
		DBRA	D0,LOOP1	TEST ALL OF RAM
		DBRA	D3,LOOP0	FOR ALL DATA TYPES (4 TESTS)

*	When memory test completes, memory is initialized with NOPs and vector table initialized with address of generic handler.
*	After initialization, D7 will contain the number of errors from the test section

MEMINIT		EQU	*
		LEA.L	RAMBAS,A0	POINT AT BASE OF RAM AGAIN
		MOVE.L	#$1FFE,D0	USE AS LOOP COUNTER FOR MEMINIT
		MOVE.L	#$3FE,D2	POINT TO BOTTOM OF VECTOR TABLE
		MOVE.W	#NOP,D1		FILL NON-VECTORY MEMORY WITH NOPs

LOOP2		EQU	*
		MOVE.W	D1,(A0,D0)	PUT DATA INTO MEMORY
		SUBQ	#2,D0		DECREMENT COUNTER
		CMP.L	D0,D2		LOOK FOR BOTTOM OF VECTOR TABLE
		BNE.S	LOOP2		LOOP UNTIL BOTTOM OF VECTOR TABLE
		SUBQ	#2,D0		ELSE MOVE TO LONG-WORD INIT
		MOVE.L	#EXCHND,D1	PUT GENERIC EXCEPTION HANDLER IN REST OF VECTOR TABLE

LOOP3		EQU	*
		MOVE.L	D1,(A0,D0)	PUT HANDLER ADDRESS THERE
		SUBQ	#4,D0		AND DECREMENT POINTER
		BGE.S	LOOP3		FILL REST OF MEMORY

*	Memory check/initialization complete

*	Initialize the MC68901 MFP

		JSR	MFPINIT		DO SO AS SUBROUTINE FOR LATER USE
		TST	D7		NOW CHECK FOR MEMORY ERRORS
		BEQ	NO_ERR		IF NONE, OUTPUT OK MESSAGE
		LEA.L	ERRMSG,A0	POINT TO TOP OF MESSAGE
		LEA.L	ERRMND,A1	AND POINT TO END
		BRA.S	INITND		JUMP TO END OF INIT ROUTINE

NO_ERR		EQU	*		INIT OK!
		LEA.L	OKMSG,A0	POINT TO TOP OF MESSAGE
		LEA.L	ERRMSG-1,A1	AND POINT TO END

INITND		EQU	*
		JSR	SEROUT		OUTPUT MESSAGE OVER SERIAL PORT
		
POLL		EQU	*		POLL SERIAL PORT FOR INPUT
		BTST.B	#3,MFPRSR	CHECK FOR BREAK
		BNE.S	BREAK		IF PRESENT, JUMP TO PROCESS
		BTST.B	#7,MFPRSR	ELSE CHECK FOR CHARACTER
		BEQ.S	POLL		LOOP IF NO DATA PRESENT, ELSE DATA PRESENT IN USART RX
*					ADD USER INPUT PROCESSING ROUTINE HERE

BREAK		EQU	*		BREAK DETECT ROUTINE
*					ADD USER BREAK HANDLER HERE
		JMP	POLL		AND RETURN WHEN COMPLETED

SEROUT		EQU	*		OUTPUT MESSAGE VIA MC68901 USART
*					MESSAGE CONTAINED A0 THROUGH A1
		BTST.B	#7,MFPTSR	CHECK FOR BUFFER EMPTY
		BEQ.S	SEROUT		AND LOOP UNTIL SO

		MOVE.B	(A0)+,D0	GET DATA INDICATED BY A0
		MOVE.B	D0,MFPUDR	AND PUT INTO USART DATA REGISTER
		CMPA	A1,A0		COMPARE CURRENT ADDRESS WITH END
		BGE.S	SEROUT		LOOP UNTIL DONE
		RTS			ELSE RETURN WHEN COMPLETED

*	Generic Exception Handler

EXCHND		EQU	*		GENERIC EXCEPTION HANDLER
		RTE

MFPINIT		EQU	*		MC68901 INITIALIZATION ROUTINE
		CLR.L	D0		CLEAR D0
		SUBQ	#1,D0		THEN TURN INTO ALL 1's
		MOVE.B	D0,MFPDDR	ALL MFP I/O INITIALIZED TO OUTPUT
		ADDQ	#3,D0		NOW TURN D0 INTO 2
		MOVE.B	D0,MFPTCDR	SELECT 1/4 TX CLOCK
		MOVE.B	D0,MFPTDDR	SEELCT 1/4 RX CLOCK
		MOVE.B	#$11,MFPTCDCR	SELECT DIVIDE BY 4 IN C/D CONTROL REGISTER
		MOVE.B	#$88,MFPUCR	SELECT DIVIDE BY 16, 8-BIT
*					NO PARITY IN USART CONTROL REGISTER

*		INITIALIZE MFP VECTOR AND HANDLER
		MOVE.L	#MFPVCT,D0	GET VECTOR
		MOVE.B	D0,MFPVR	LOAD INTO MFP
		ASL.L	#2,D0		NOW SHIFT LEFT 2
		MOVE.L	D0,A0		PUT INTO ADDRESS REGISTER
		MOVE.L	#MFPEXC,(A0)	AND INIT APPROPRIATE VECTOR

*		START TX/RX CLOCKS
		MOVE.B	#1,MFPRSR	START RECEIVER CLOCK
		MOVE.B	#5,MFPTSR	START TRANSMIT CLOCK
		BSET.B	#7,MFPGPIP	RAISE RTS

		RTS			DONE!  RETURN FROM ROUTINE

*		MFP EXCEPTION HANDLER
MFPEXC		EQU	*
		RTE


*	MESSAGES SECTION

OKMSG		EQU	*
		DC.B	'WELCOME TO RHOMBUS CONFIGURATION SYSTEM >'

ERRMSG		EQU	*
		DC.B	'MEMORY ERRORS ENCOUNTERED...'

ERRMND		EQU	*
		DC.B	'>'

		END	START
