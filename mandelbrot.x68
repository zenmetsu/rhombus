*****************************************************************************************************
*****************************************************************************************************
*****	Title		: RHOMBUS Mandelbrot Viewer						*****
*****	Written by	: Jason Westervelt							*****
*****	Date		: 12 July 2017								*****
*****	Description	: A mandelbrot viewer for the RHOMBUS minimalist 68020 system		*****
*****			  which was designed around Motorola Application Note 1015		*****
*****												*****
*****	Integer math, using 28 bits of decimal precision					*****
***** 	For instance, 4.5 would be represented as  [ 4].[5                      	 ]	*****
*****                                              0100 1000 0000 0000 0000 0000 0000 0000      *****
*****   -2.3 would be represented as               [-2].[3                               ]      *****
*****                                              1101 1011 0011 0011 0011 0011 0011 0011      *****
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

START		
*****************************************************************************************************
*****************************************************************************************************
* Equates section										*****
												*****
****************************************							*****
*   Defines											*****
NOP		EQU	$4E71		STANDARD 68000 NOP INSTRUCTION				*****
SCRX		EQU	$A0		COLUMN COUNT (0x50=80)					*****
SCRY		EQU	$28		ROW COUNT (0x28=40)					*****
IMAX		EQU	$5D		Max iterations						*****	
												*****
RCENT		EQU	$F02000		Pointer to real center memory location			*****
ICENT		EQU	$F02004		Pointer to imaginary center memory location		*****
REXT		EQU	$F02008		Pointer to real center memory location			*****
IEXT		EQU	$F0200C		Pointer to imaginary center memory location		*****
INITR		EQU	$F02010		Pointer to real start value memory location		*****
INITI		EQU	$F02014		Pointer to imaginary start value memory location	*****
XSCALE		EQU	$F02018		Pointer to XSCALE memory location			*****
YSCALE		EQU	$F0201C		Pointer to YSCALE memory location			*****
IMAX_SCL	EQU	$F02020		Pointer to IMAX Scaling variable			*****
												*****
****************************************							*****
*   Addresses											*****
												*****
GETCHAR		EQU	$716		ROM INSTRUCTION						*****
PUTCHAR		EQU	$760		ROM INSTRUCTION						*****
NEWLINE		EQU	$7AE		ROM INSTRUCTION						*****
												*****
****************************************							*****
*   ASCII Equates										*****
												*****
CTRLC		EQU	$03									*****
BEL		EQU	$07									*****
BKSP		EQU	$08		CTRL-H							*****
TAB		EQU	$09									*****
LF		EQU	$0A								     	*****
CR		EQU	$0D								      	*****
CTRLX		EQU	$18								       	*****
ESC		EQU	$1B									*****
SPACE		EQU	$20									*****
CTRLA		EQU	$01									*****

							*********************************************
							*	Register usage
			 				*
			 				*	D0	scratch space
							*	D1,D2	zr,zi
							*	D3,D4	zr2,zi2
							*	D5,D6	cr,ci
							*	D7	iteration, decrement and exit on zero
							*	The following 2 index the last one we did
							*	to facilitate end-testing.
							*	A0	(4.0) for quick test
							*	A2	column counter
							*	A3	stores max X column
							*	A4	row counter
							*
							*********************************************
							
			MOVE.L	#RCENT,A5		* point A0 to RCENT	
			MOVE.L	#$F8000000,(A5)+	* set initial real center to -0.5
			MOVE.L	#$0,(A5)+		* set initial imaginary center to 0
			MOVE.L	#$18000000,(A5)+	* set initial real extent to 1.5
			MOVE.L	#$10000000,(A5)+	* set initial imaginary extent to 1
			MOVE.L	#$4D4874,(XSCALE)
			MOVE.L	#$D20D21,(YSCALE)
			MOVE.L	#$0,(IMAX_SCL)	
REINIT			MOVE.L	(RCENT),D0		* load RCENT into D0
			SUB.L	(REXT),D0		* subtract REXT to find new INITR
			MOVE.L	D0,(INITR)		* and store INITR
			MOVE.L	(ICENT),D6		* load ICENT into D0
			ADD.L	(IEXT),D6		* add IEXT to find new INITI
			MOVE.L	D6,(INITI)		* and store INITI

			MOVEA.L	#$4000000,A0		* "4" for quick test
			MOVEA.L #$0,A4			* reset row counter

			MOVEA.L	#SCRX,A3		* set max column counter
			BRA.S	YLOOP0			* jump over looped increment section for the first Y iteration
YLOOP			ADDA	#$1,A4			* increment row counter
			SUB.L	(YSCALE),D6		* update ci by decrementing by Y step
YLOOP0			MOVEA.L	#$0,A2			* reset column counter
                        MOVE.L	#CR,D0			* output CR+LF for next row
			JSR	PUTCHAR			* using ROM monitor function
			MOVE.L	#LF,D0			*
                        JSR	PUTCHAR			* 
XINIT			MOVE.L	(INITR),D5		* put left-hand x starting value into D5
                        BRA.S	XLOOP0			* jump over the looped increment section for first X iteration
XLOOP			ADD.L	(XSCALE),D5		* add X step
XLOOP0			MOVE.L	D5,D1			* zr = cr
			MOVE.L	D6,D2			* zi = ci

			MOVE.L	#IMAX,D7		* iteration = IMAX
			MOVE.L  (IMAX_SCL),D0	
			ASL.L	D0,D7			* multiply by IMAX scalar
			BRA	ITERATE
							* D1, D2 already swapped
ILOOP			muls	D1,D2			* zi = zr * zi
			asl.l	#5,D2			* adjust and * 2
			add.l	D6,D2			* + ci

			move.l	D3,D1			* zr = zr2
			sub.l	D4,D1			* - zi2
			asl.l	#4,D1			* adjustment of D3,D4
			add.l	D5,D1			* + cr
ITERATE			SWAP	D1
			MOVE.L	D1,D3
			MULS	D3,D3			* zr2 = zr * zr

			SWAP	D2
			MOVE.W	D2,D4
			MULS	D4,D4			* zi2 = zi * zi
							* D3 and D4 not adjusted yet
			MOVE.L	D3,D0			* if (zr2
			ADD.L	D4,D0			* +zi2
			CMP.L	A0,D0			* >="4"
			DBHI	D7,ILOOP		* || iteration==IMAX) break;

							* D7==-1 if iterations ran out before point went outside 4.0
							* D7==0 means it went outside on the last iteration.
							* IMAX>D7>0 if it went outside.
							* D7==IMAX means it was already outside before iteration
ENDITER			ADDI.L	#$1,D7			* "color" mapping function
			MOVE.L	(IMAX_SCL),D0		*
			ASR.L	D0,D7			* if D7==-1, point is in set and
			ADDI.L	#$20,D7			* adding 0x21 will result in a space
							* character being drawn
			MOVE.L  D7,D0			* put resulting value into D0 for putchar call
			JSR	PUTCHAR			* draw it using ROM monitor function
			ADDA	#$1,A2			* increment column counter
			CMPA.L	A2,A3			* test for end of x-loop
			BNE	XLOOP           	* if not, repeat
NEWROW			CMPA.L  #SCRY,A4		* Check if finished with final row
			BNE	YLOOP			* loop Y if not
			CLR.L	D7			* clear D7, GETCHAR references D7, might be a bug in GETCHAR, will investigate

WHATNEXT		JSR	NEWLINE			* present user with prompt
			JSR	GETCHAR			* get input
			CMP.B	#'W',D0			* if W, PAN UP
			BEQ.S	PAN_UP			*
			CMP.B	#'S',D0			* if S, PAN DOWN
			BEQ.S	PAN_DOWN		*
			CMP.B	#'A',D0			* if A, PAN LEFT
			BEQ.S	PAN_LEFT		*
			CMP.B	#'D',D0			* if D, PAN RIGHT
			BEQ.S	PAN_RIGHT		*
			CMP.B	#'Q',D0			* if Q, ZOOM OUT
			BEQ.W	ZOOM_OUT		*
			CMP.B	#'E',D0			* if E, ZOOM IN
			BEQ.W	ZOOM_IN			*
			CMP.B	#'Z',D0			* if Z, DECREASE_IMAX
			BEQ.W	DEC_IMAX		*
			CMP.B	#'C',D0			* if C, INCREASE_IMAX
			BEQ.W	INC_IMAX		*
			CMP.B	#'X',D0			* if X, EXIT
			BEQ.W	EXIT			*
			BRA.S	WHATNEXT		* else if no match, prompt again

PAN_UP			MOVE.L  (IEXT),D0		* put IEXT in D0
			ASR.L	#1,D0			* and divide it by two
			ADD.L	(ICENT),D0		* and add to ICENT to set new center
			MOVE.L	D0,(ICENT)		* and store new ICENT
			JMP	REINIT			* and restart	

PAN_DOWN		CLR.L	D0			* clear D0
			SUB.L	(IEXT),D0		* subtract IEXT
			ASR.L	#1,D0			* and divide it by two
			ADD.L	(ICENT),D0		* and add to ICENT to set new center
			MOVE.L	D0,(ICENT)		* and store new ICENT
			BRA.W	REINIT			* and restart	

PAN_RIGHT		MOVE.L  (REXT),D0		* put REXT in D0
			ASR.L	#1,D0			* and divide it by two
			ADD.L	(RCENT),D0		* and add to RCENT to set new center
			MOVE.L	D0,(RCENT)		* and store new RCENT
			JMP	REINIT			* and restart	

PAN_LEFT		CLR.L	D0			* clear D0
			SUB.L	(REXT),D0		* subtract REXT
			ASR.L	#1,D0			* and divide it by two
			ADD.L	(RCENT),D0		* and add to RCENT to set new center
			MOVE.L	D0,(RCENT)		* and store new RCENT
			BRA.W	REINIT			* and restart	

ZOOM_OUT		MOVE.L	(XSCALE),D0	
			ASL.L	#1,D0			* multiply XSCALE by two
			MOVE.L	D0,(XSCALE)		* and store it
			MOVE.L	(YSCALE),D0
			ASL.L	#1,D0			* multiply YSCALE by two
			MOVE.L	D0,(YSCALE)		* and store it
			MOVE.L	(REXT),D0	
			ASL.L	#1,D0			* multiply REXT by two (this is redundant, need to calculate XSCALE from REXT)
			MOVE.L	D0,(REXT)		* and store it
			MOVE.L	(IEXT),D0	
			ASL.L	#1,D0			* multiply REXT by two (this is redundant, need to calculate XSCALE from REXT)
			MOVE.L	D0,(IEXT)		* and store it
			BRA.W	REINIT

ZOOM_IN			MOVE.L	(XSCALE),D0	
			ASR.L	#1,D0			* divide XSCALE by two
			MOVE.L	D0,(XSCALE)		* and store it
			MOVE.L	(YSCALE),D0
			ASR.L	#1,D0			* divide YSCALE by two
			MOVE.L	D0,(YSCALE)		* and store it
			MOVE.L	(REXT),D0	
			ASR.L	#1,D0			* divide REXT by two (this is redundant, need to calculate XSCALE from REXT)
			MOVE.L	D0,(REXT)		* and store it
			MOVE.L	(IEXT),D0	
			ASR.L	#1,D0			* divide REXT by two (this is redundant, need to calculate XSCALE from REXT)
			MOVE.L	D0,(IEXT)		* and store it
			BRA.W	REINIT

DEC_IMAX		MOVE.L	(IMAX_SCL),D0
			CMP.L	#$0,D0			* check if IMAX scalar is already '1'
			BEQ.W	REINIT			* restart if so
			SUBI.L	#$1,D0			* subtract '1' if not
			MOVE.L	D0,(IMAX_SCL)		* and store it
			BRA.W	REINIT			* and restart						

INC_IMAX		MOVE.L	(IMAX_SCL),D0
			ADDI.L	#$1,D0			* add '1'
			MOVE.L	D0,(IMAX_SCL)		* and store it
			BRA.W	REINIT			* and restart					

EXIT			RTS				* finished, return to ROM monitor

	END	START
