
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		int.asm
;	Contents:	Interrupt library
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Acknowledge VSync and handle counters
; -------------------------------------------------------------------------

ackVSync macro

	addq.l	#1,r_Frame_Cnt.w		; Increment frame counter
	move.b	#80-1,r_Sprs_Left.w		; Reset sprites left to draw

	if LAGMETER
		move.w	#$9193,VDP_CTRL		; Enable lag meter
	endif
	
	endm

; -------------------------------------------------------------------------
; Perform VSync
; -------------------------------------------------------------------------

VSync:
	movem.l	d0-d1,-(sp)			; Save registers
	if LAGMETER
		move.w	#$9100,VDP_CTRL		; Disable lag meter
	endif

	ei					; Enable interrupts
	move.b	r_Frame_Cnt+3.w,d0		; Get current frame count

.Wait:
	cmp.b	r_Frame_Cnt+3.w,d0		; Has the V-INT run yet?
	beq.s	.Wait				; If not, wait

	movem.l	(sp)+,d0-d1			; Restore registers

	rts

; -------------------------------------------------------------------------
; Standard V-INT routine
; -------------------------------------------------------------------------

VInt_Std:
	di					; Disable interrupts
	pusha					; Push all registers

	bsr.w	ReadCtrls			; Read controller data
	bsr.w	ProcessDMA			; Process DMA queue
	bsr.w	DMAPalette			; Transfer palette data

	ackVSync				; Acknowledge VSync
	popa					; Pop all registers

Int_Blank:
	rte

; -------------------------------------------------------------------------
