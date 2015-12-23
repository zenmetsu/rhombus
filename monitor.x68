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
*   Addresses											*****
												*****
ROMBAS		EQU	0		ROM BASE ADDRESS					*****
RAMBAS		EQU	$F00000		RAM BASE ADDRESS					*****
MFPBAS		EQU	$EF0000		MFP BASE ADDRESS					*****
MFPVCT		EQU	$40		VECTOR FOR MFP SOURCED INTERRUPT			*****
STACK		EQU	$800									*****

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
*   Environment Parameter Equates								*****
												*****
MAXCHR		EQU	64		MAXIMUM LENGTH OF COMMAND LINE				*****
DATA		EQU	$800		DATA ORIGIN						*****
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
												*****
START		EQU	ROMBAS									*****
		DC.L	STACK		INITIAL STACK POINTER					*****
		DC.L	RESET		INITIAL PROGRAM COUNTER					*****
RESET		EQU	*		COLD ENTRY POINT					*****
		LEA	DATA,A6		POINT A6 TO DATA AREA					*****
		CLR.L	UCOMTAB(A6)	RESET USER COMMAND TABLE POINTER			*****
		BSR.S	CFGUART		CONFIGURE UART						*****
WARM		EQU	*
		NOP
		BRA	WARM

*****************************************************************************************************
*****************************************************************************************************
* INITIALIZATION SECTION                                                                        *****
                                                                                                *****
CFGUART		EQU	*
		RTS

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
