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

*****************************************************************************************************
*****************************************************************************************************
* Equates section										*****
												*****
****************************************							*****
*   Defines											*****
NOP		EQU	$4E71		STANDARD 68000 NOP INSTRUCTION				*****
SCRX		EQU	$100		COLUMN COUNT (0x50=80)					*****
SCRY		EQU	$28		ROW COUNT (0x28=40)					*****
IMAX		EQU	$5D		Max iterations						*****
RCENT		EQU	$F8000000	Real Center (28bit decimal, F8000000 = -0.5		*****
ICENT		EQU	$0		Imaginary Center					*****
REXT		EQU	$18000000	Real extent (0.5 * (rmax-rmin))				*****
IEXT		EQU	$10000000	Imaginary extent					*****
* xscale = 2 * r_ext / (SCRX - 1) = 2 * 1.5 / (80 - 1) = 0.0379746835443 =  0x9B8B5 		*****
* yscale = 2 * i_ext / (SCRY - 1) = 2 * 1 / (40 - 1 ) =  0.1025641025641 = 0xd20d21		*****
XSCALE		EQU	$4CCCCC									*****
YSCALE		EQU	$D20D21									*****
* INITR = (RCENT - REXT)    As iterated, working value will be (RCENT - REXT) + X * xscale	*****
* INITI = (ICENT - IEXT)    As iterated, working value will be (ICENT + IEXT) - Y * yscale	*****
INITR		EQU	$E0000000	Initial Real value (-2.0)				*****
INITI		EQU	$10000000	Intial Imaginary value (1.0)				*****	
****************************************							*****
*   Addresses											*****
PROGBAS		EQU	$F01000									*****
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
LF		EQU	$0A								     ***********
CR		EQU	$0D								      *********
CTRLX		EQU	$18								       *******
ESC		EQU	$1B									*****
SPACE		EQU	$20									 ***
CTRLA		EQU	$01									  *


*****************************************************************************************************
*****************************************************************************************************
*	Program section										*****
*	The starting RAM address is mapped to the variable					*****
*	PROGBAS.  All executable code is resident in RAM.					*****
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

START			MOVE.L	#INITI,D6	* get initial imaginary value
			MOVEA.L	#$4000000,A0	* FIX(4) for quick test
			MOVEA.L #$0,A4		* reset row counter

			MOVEA.L	#SCRX,A3	* set max column counter
			BRA.S	YLOOP0		* jump over looped increment section for the first Y iteration
YLOOP			ADDA	#$1,A4		* increment row counter
			SUBI.L	#YSCALE,D6	* update ci by decrementing by Y step
YLOOP0			MOVEA.L	#$0,A2		* reset column counter
                        JSR	NEWLINE		* print a newline character using ROM monitor function
XINIT			MOVE.L	#INITR,D5	* put left-hand x starting value into D5
                        BRA.S	XLOOP0		* jump over the looped increment section for first X iteration
XLOOP			ADDI.L	#XSCALE,D5	* add X step
XLOOP0			MOVE.L	D5,D1		* zr = cr
			MOVE.L	D6,D2		* zi = ci

			MOVE.W	#IMAX,D7	* iteration = IMAX;
			BRA	ITERATE
						* D1, D2 already swapped
ILOOP			muls	D1,D2		* zi = zr * zi
			asl.l	#5,D2		* adjust and * 2
			add.l	D6,D2		* + ci

			move.l	D3,D1		* zr = zr2
			sub.l	D4,D1		* - zi2
			asl.l	#4,D1		* adjustment of D3,D4
			add.l	D5,D1		* + cr
ITERATE			SWAP	D1
			MOVE.L	D1,D3
			MULS	D3,D3		* zr2 = zr * zr

			SWAP	D2
			MOVE.W	D2,D4
			MULS	D4,D4		* zi2 = zi * zi
						* D3 and D4 not adjusted yet
			MOVE.L	D3,D0		* if (zr2
			ADD.L	D4,D0		* +zi2
			CMP.L	A0,D0		* >= FIX(4)
			DBHI	D7,ILOOP	* || iteration==IMAX) break;

			* D7==-1 if iterations ran out before point went outside 4.0
			* D7==0 means it went outside on the last iteration.
			* IMAX>D7>0 if it went outside.
			* D7==IMAX means it was already outside before iteration
ENDITER			ADDI.B	#$21,D7		* "color" mapping function
						* if D7==-1, point is in set and
						* adding 0x21 will result in a space
						* character being drawn
			MOVE.L  D7,D0		* put resulting value into D0 for putchar call
			JSR	PUTCHAR		* draw it using ROM monitor function
			ADDA	#$1,A2		* increment column counter
			CMPA.L	A2,A3		* test for end of x-loop
			BNE	XLOOP           * if not, repeat
NEWROW			CMPA.L  #SCRY,A4	* Check if finished with final row
			BNE	YLOOP		* loop Y if not
DONE			RTS			* finished, return to ROM monitor
