
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		main.asm
;	Contents:	Base source
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; ASM68K build options
; -------------------------------------------------------------------------

	opt	l.				; Use "." for local labels
	opt	op+				; Optimize to PC relative addressing
	opt	os+				; Optimize short branches
	opt	ow+				; Optimize absolute long addressing
	opt	oz+				; Optimize zero displacements
	opt	oaq+				; Optimize to addq
	opt	osq+				; Optimize to subq
	opt	omq+				; Optimize to moveq
	opt	ae-				; Disable automatic evens

; -------------------------------------------------------------------------
; Includes
; -------------------------------------------------------------------------

	include	"../md/constants.asm"		; Contants
	include	"../md/macro.asm"		; Macros
	include	"../md/z80.asm"			; Z80 macro set
	include	"../md/debugger.asm"		; Debugger macro set
	include	"../md/ram.asm"			; System RAM
	include	"_inc/const.asm"		; Constant definitions
	include	"_inc/ram.asm"			; RAM definitions
	include	"_inc/macro.asm"		; Macro definitions
	include	"includes.asm"			; Shared user includes
	include	"config.asm"			; Configuration

; -------------------------------------------------------------------------
; Vector table
; -------------------------------------------------------------------------

	org	0

	dc.l	r_Stack				; Stack pointer
	dc.l	ICD_BLK				; Code start

	dc.l	Exception			; Bus error
	dc.l	AddrError			; Address error
	dc.l	BadInstr			; Illegal instruction
	dc.l	DivideBy0			; Division by zero error
	dc.l	Exception			; CHK exception
	dc.l	Exception			; TRAPV exception
	dc.l	Exception			; Privilege violation
	dc.l	Exception			; TRACE exception
	dc.l	Exception			; Line A emulator
	dc.l	Exception			; Line F emulator

	dc.b	"ALL IN ALL, IT'S JUST ANOTHER BRICK IN THE WALL."

	dc.l	Exception			; Spurious exception
	dc.l	Exception			; IRQ level 1
	dc.l	Exception			; External interrupt
	dc.l	Exception			; IRQ level 3
	dc.l	r_HInt				; Horizontal interrupt
	dc.l	Exception			; IRQ level 5
	dc.l	r_VInt				; Vertical interrupt
	dc.l	Exception			; IRQ level 7

	dc.l	Exception			; TRAP 00
	dc.l	Exception			; TRAP 01
	dc.l	Exception			; TRAP 02
	dc.l	Exception			; TRAP 03
	dc.l	Exception			; TRAP 04
	dc.l	Exception			; TRAP 05
	dc.l	Exception			; TRAP 06
	dc.l	Exception			; TRAP 07
	dc.l	Exception			; TRAP 08
	dc.l	Exception			; TRAP 09
	dc.l	Exception			; TRAP 10
	dc.l	Exception			; TRAP 11
	dc.l	Exception			; TRAP 12
	dc.l	Exception			; TRAP 13
	dc.l	Exception			; TRAP 14
	dc.l	Exception			; TRAP 15

	dc.b	"GO AWAY, HACKER FUCKER"
	align	$100

; -------------------------------------------------------------------------
; Header
; -------------------------------------------------------------------------

	dc.b	"SEGA MEGA DRIVE "		; Hardware system ID

	dc.b	"(C)T-00 "			; Company
hyr	=	(_year+1900)%10000
hmth	substr	1+((_month-1)*3), 3+((_month-1)*3), &
	"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC"
	dcb.b	4-strlen("\#hyr"), "0"
	dc.b	"\#hyr\.\hmth"			; Year and month

hgname	substr	1, $30, "\GAME_NAME"
	dc.b	"\hgname"			; Japanese game name
	dcb.b	$30-strlen("\hgname"), " "
	dc.b	"\hgname"			; Overseas game name
	dcb.b	$30-strlen("\hgname"), " "

hday	equs	"\#_day"
	if _day<10
hday	equs	"0\#_day"
	endif
hhr	equs	"\#_hours"
	if _hours<10
hhr	equs	"0\#_hours"
	endif
hmin	equs	"\#_minutes"
	if _minutes<10
hmin	equs	"0\#_minutes"
	endif
hsec	equs	"\#_seconds"
	if _seconds<10
hsec	equs	"0\#_seconds"
	endif
	dc.b	"GM "				; Game type
	dc.b	"\hday\\hhr\\hmin\\hsec\-00"	; Game version

	dc.w	0				; Checksum

hiosup	substr	1, $10, "\IO_SUPPORT"
	dc.b	"\hiosup"			; I/O support
	dcb.b	$10-strlen("\hiosup"), " "

	dc.l	ROM_START, ROM_END-1		; ROM start and end addresses
	dc.l	RAM_START, RAM_END-1		; RAM start and end addresses

	dc.l	SRAM_SUPPORT			; SRAM support
	dc.l	SRAM_START, SRAM_END		; SRAM start and end addresses

	dc.b	"            "			; Modem information
	dc.b	"        "

hnotes	substr	1, $20, "\NOTES"		; Notes
	dc.b	"\hnotes"
	dcb.b	$20-strlen("\hnotes"), " "

	if LOCKOUT				; Region code
hregion		substr	REGIONS+1,REGIONS+1,"0123456789ABCDEF"
		dc.b	"\hregion               "
	else
		dc.b	"F               "
	endif

; -------------------------------------------------------------------------
; Initialization
; -------------------------------------------------------------------------

ICD_BLK:
	; Standard initialization

	movea.l	ROM_START.w,sp			; Reset stack pointer
	include	"../md/icd.asm"			; Initialization

	di					; Disable interrupts
	waitDMA					; Wait for DMA to finish

	; Region lockout check

	if LOCKOUT
		include	"../md/lock.asm"	; Lockout
	endif

; -------------------------------------------------------------------------

	; Check the checksum value (Code by MarkeyJester)
	if CHECKSUM
		lea	$200.w,a0		; Prepare start address
		move.l	$1A4.w,d7		; Load size
		sub.l	a0,d7			; Subtract start address
		move.b	d7,d5			; Copy end nibble
		andi.w	#$F,d5
		lsr.l	#4,d7			; Divide the size by 16
		move.w	d7,d6			; Load lower word size
		swap	d7			; Get upper word size
		moveq	#0,d0			; Clear d0

.ChecksumModular:
		rept	8
			add.w	(a0)+,d0	; Modular checksum (8 words)
		endr
		dbf	d6,.ChecksumModular	; Repeat until all main block sections are done
		dbf	d7,.ChecksumModular
		subq.w	#1,d5			; Decrease remaining nibble for dbf
		bpl.s	.ChkChecksum		; If there is none, branch

.CheckRemain:
		add.w	(a0)+,d0		; Add remaining words
		dbf	d5,.CheckRemain		; Repeat until the remaining words are done

.ChkChecksum:
		cmp.w	$18E.w,d0		; Does the checksum match?
		beq.s	.ChecksumMatches	; If so, branch

		vdpCmd	move.l,0,CRAM,WRITE,VDP_CTRL
		move.w	#$E,VDP_DATA		; Set screen to red
		halt				; Loop here forever

.ChecksumMatches:
	endif

	; Clear RAM

	clrRAM	RAM_START,RAM_END

	; Set up interrupts

	move.w	#$4EF9,d0			; JMP instruction
	move.w	d0,r_HInt.w			; Set for H-INT
	move.w	d0,r_VInt.w			; Set for V-INT
	move.l	#Int_Blank,r_HInt+2.w		; Set pointer for H-INT
	move.l	#VInt_Std,r_VInt+2.w		; Set pointer for V-INT

	; Get region bits

	move.b	HW_VERSION,r_Region.w

	; Initialize stuff

	z80ResOff
	bsr.w	InitCtrls			; Initialize controllers
	bsr.w	InitScreen			; Initialize the screen

	move.w	#$8134,VDP_CTRL			; Enable V-INT and DMA

	; Go to the main routine

	jmp	Main				; Go to main

; -------------------------------------------------------------------------
; Libraries
; -------------------------------------------------------------------------

	include	"../lib/ctrl.asm"		; Controller I/O library
	include	"../lib/vdp.asm"		; VDP library
	include	"../lib/int.asm"		; Interrupt library
	include	"../lib/decomp.asm"		; Decompression library

; -------------------------------------------------------------------------
; Main source
; -------------------------------------------------------------------------

	include	"main.asm"

; -------------------------------------------------------------------------
; Error handler
; -------------------------------------------------------------------------

	include	"../md/error.asm"

; -------------------------------------------------------------------------
