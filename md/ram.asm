
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		ram.asm
;	Contents:	System RAM
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; RAM definitions
; -------------------------------------------------------------------------

r_Buffer	EQU	RAM_START		; General buffer

USER_RAM	EQU	$FF000000|(RAM_START+$8000)

	rsset	$FF000000|(RAM_START+$F5C4)

SYS_RAM		EQU	__rs

r_HScrl		rs.b	$380			; HScroll buffer
r_HScrl_End	rs.b	0
r_HScrl_A	EQU	r_HScrl			; HScroll A
r_HScrl_B	EQU	r_HScrl+2		; HScroll B

r_VScrl		rs.b	$50			; VScroll buffer
r_VScrl_End	rs.b	0
r_VScrl_A	EQU	r_VScrl			; VScroll A
r_VScrl_B	EQU	r_VScrl+2		; VScroll B

r_Sprites	rs.b	$280			; Sprite data buffer
r_Sprites_End	rs.b	0

r_Palette	rs.b	$80			; Palette buffer
r_Fade_Pal	rs.b	$80			; Fade palette buffer

r_DMA_Queue	rs.b	($20*$E)		; DMA queue
r_DMA_Slot	rs.w	1			; DMA queue slot

r_Spr_Chk_Bnd	rs.b	1			; Check sprite boundary flag
		rsEven

r_Ctrl		rs.b	0			; Controller data
r_P1_Ctrl	rs.b	0			; P1 controller data
r_P1_Hold	rs.w	1			; P1 held buttons
r_P1_Press	rs.w	1			; P1 pressed buttons
r_P2_Ctrl	rs.b	0			; P2 controller data
r_P2_Hold	rs.w	1			; P2 held buttons
r_P2_Press	rs.w	1			; P2 pressed buttons
r_Ctrl_States	rs.b	0			; Controller states
r_P1_State	rs.b	1			; P1 controller state
r_P2_State	rs.b	1			; P2 controller state
r_Ctrl_Chg	rs.b	1			; Poll controller change flag

r_Sprs_Left	rs.b	1			; Sprites left available to draw

r_Plane_Delta	rs.l	1			; Plane draw VDP command delta

r_Pal_Fade	rs.b	0			; Palette fade data
r_Fade_Start	rs.b	1			; Palette fade start offset
r_Fade_Len	rs.b	1			; Palette fade length

r_Frame_Cnt	rs.l	1			; Frame count

r_RNG_Seed	rs.l	1			; RNG seed

r_Region	rs.b	1			; Hardware region ID

r_SRAM_Found	rs.b	1			; SRAM found flag

r_HInt		rs.b	6			; H-INT jump instruction
r_VInt		rs.b	6			; V-INT jump instruction

		rs.b	$100			; Stack area
r_Stack		rs.b	0			; Stack base

	if (__rs&$FFFFFF)<>0
		if (__rs&$FFFFFF)<RAM_START
			inform 3,"System RAM goes $%h byte(s) beyond the end of RAM", __rs&$FFFFFF
		elseif (__rs&$FFFFFF)<RAM_END
			inform 1,"System RAM is $%h byte(s) behind the end of RAM", RAM_END-(__rs&$FFFFFF)
		endif
	endif

; -------------------------------------------------------------------------
