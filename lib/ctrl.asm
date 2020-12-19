
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		ctrl.asm
;	Contents:	Controller library (Courtesy of AURORA*FIELDS)
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Initialize controllers
; -------------------------------------------------------------------------

InitCtrls:
	z80Stop					; Stop Z80
	moveq	#0,d2				; TH low
	lea	r_Ctrl_States.w,a0		; Controller states

	clr.w	(a0)				; Reset states
	lea	IO_A_DATA,a1			; Port A
	bsr.s	.Ctrl				; Get state
	addq.w	#2,a1				; Port B
	bsr.s	.Ctrl				; Get state
	z80Start				; Start Z80
	rts

.Ctrl:
	move.b	#IO_TH,6(a1)			; Set TH pin as output
	or.l	d0,d0				; Delay
	move.b	d2,(a1)				; Pull TH line low
	nop
	moveq	#IO_TH,d3			; TH high
	move.b	d3,(a1)				; Pull TH line high
	or.l	d0,d0				; Delay
	move.b	d2,(a1)				; Pull TH line low
	or.l	d0,d0				; Delay
	move.b	d3,(a1)				; Pull TH line high
	or.l	d0,d0				; Delay
	move.b	d2,(a1)				; Pull TH line low
	or.l	d0,d0				; Delay

	move.b	(a1),d0				; 6 button
	move.b	d3,(a1)				; Pull TH line high
	andi.b	#$F,d0
	seq	d1				; If 6 button, set d1
	add.b	d1,(a0)+			; $FF if 6 button, 0 if 3 button
	rts

; -------------------------------------------------------------------------
; Read controllers
; -------------------------------------------------------------------------

ReadCtrls:
	tst.b	r_Ctrl_Chg.w			; Are we polling for controller changes?
	beq.s	.NoInit				; If not, skip
	moveq	#7,d0
	and.b	r_Frame_Cnt+3.w,d0		; Only run once every 8 frames
	beq.w	InitCtrls			; If we are in a frame to do that, then do it

.NoInit:
	lea	IO_A_DATA,a1			; Port A
	lea	r_Ctrl.w,a0			; Controller button bits
	lea	r_Ctrl_States.w,a2		; Controller states

	moveq	#IO_TH,d3			; TH high
	moveq	#0,d2				; TH low
	bsr.s	.Ctrl3				; Read data
	addq.w	#2,a1				; Port B

.Ctrl3:
	tst.b	(a2)+				; Check if 6 button controller
	bmi.s	.Ctrl6				; If so, branch

	move.b	d2,(a1)				; Set TH low
	moveq	#IO_TL|IO_TR,d0			; Prepare d0
	nop					; Delay
	and.b	(a1),d0				; Get controller data (start/A)
	move.b	d3,(a1)				; Set TH high
	lsl.b	#2,d0

	moveq	#IO_TL|IO_TR|$F,d1
	and.b	(a1),d1				; Get controller data (B/C/Dpad)
	or.b	d1,d0				; Combine controller bits
	not.b	d0				; Invert

	clr.b	(a0)+				; Clear high bits
	move.b	(a0),d1				; Get presses button data
	eor.b	d0,d1				; Toggle off inputs that are being held
	move.b	d0,(a0)+			; Store held button bits
	and.b	d0,d1				; Only allow buttons pressed in this frame
	clr.b	(a0)+				; Clear high bits
	move.b	d1,(a0)+			; Store pressed button bits

	rts

.Ctrl6:
	move.b	d3,(a1)				; Set TH high
	moveq	#0,d0
	moveq	#0,d1

	move.b	(a1),d1				; Read first 6 buttons
	move.b	d2,(a1)				; Set TH low
	andi.b	#IO_TL|IO_TR|$F,d1

	move.b	(a1),d0				; Read second 2 buttons
	move.b	d3,(a1)				; Set TH high
	andi.b	#IO_TL|IO_TR,d0
	move.b	d2,(a1)				; Set TH low
	lsl.b	#2,d0
	move.b	d3,(a1)				; Set TH high
	or.l	d0,d1				; Combine basic 8 buttons

	move.b	d2,(a1)				; Set TH low
	or.l	d0,d0				; Delay
	move.b	d3,(a1)				; Set TH high

	moveq	#$F,d0				; Prepare d0
	nop
	and.b	(a1),d0				; Read extra buttons
	move.b	d3,(a1)				; Set TH high
	lsl.w	#8,d0				; Shift it by 8 bits
	or.w	d1,d0				; Combine with basic buttons
	not.w	d0				; Invert

	move.w	(a0),d1				; Get presses button data
	eor.w	d0,d1				; Toggle off inputs that are being held
	move.w	d0,(a0)+			; Store held button bits
	and.w	d0,d1				; Only allow buttons pressed in this frame
	move.w	d1,(a0)+			; Store pressed button bits

	rts

; -------------------------------------------------------------------------
