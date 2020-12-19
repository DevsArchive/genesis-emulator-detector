
; -------------------------------------------------------------------------
;
;	Genesis Emulator Detector
;		By Ralakimus 2019
;
;	File:		main.asm
;	Contents:	Main source
;
; -------------------------------------------------------------------------

	rsset	r_Local

r_Emulator	rs.b	1

; -------------------------------------------------------------------------
; Main routine
; -------------------------------------------------------------------------

Main:
	di

	bsr.w	DetectEmulator
	move.b	d0,r_Emulator.w

	lea	ArtKos_Font(pc),a0		; Load font
	lea	r_Buffer,a1
	lea	$0000.w,a2
	bsr.w	LoadKosArt

	lea	Pal_Font(pc),a0			; Load palette
	move.w	#$80/2-1,d0
	jsr	LoadPalette.w

	lea	Emulators,a0
	moveq	#0,d0
	move.b	r_Emulator.w,d0
	add.w	d0,d0
	add.w	d0,d0
	movea.l	(a0,d0.w),a0

	move.l	#$41040003,VDP_CTRL

.Draw:
	moveq	#0,d0
	move.b	(a0)+,d0
	beq.s	.Done
	subi.b	#$20,d0
	move.w	d0,VDP_DATA
	bra.s	.Draw

.Done:
	move.l	#$81748702,VDP_CTRL
	bsr.w	VSync
	bra.w	*

; -------------------------------------------------------------------------

	include	"emudetect.asm"

; -------------------------------------------------------------------------
; Macro to convert a normal string into a textbox string and store it
; -------------------------------------------------------------------------
; PARAMETERS:
;	text	- String to convert and store
; -------------------------------------------------------------------------

convStr macro &
	text

; EPIC HACK! Done to allow the " and ' characters to coexist in the same string :^)
cind	EQUS	' !"'
cind	EQUS	"\cind\#$ &'()*+,-. 0123456789     : ;<=>?@ABCDEFGHIJKLMNOPQRST   / ;<=>?@ABCDEFGHIJKLMNOPQRST   %"
c	= 1

	rept	strlen(\text)
ochr		substr c, c, \text
char		substr "\ochr"-$1F, "\ochr"-$1F, "\cind"
		dc.b	"\char"
c		= c+1
	endr

	endm

; -------------------------------------------------------------------------
; Emulators
; -------------------------------------------------------------------------

Emulators:
	dc.l	.Hardware
	dc.l	.GPGX
	dc.l	.Regen
	dc.l	.Kega
	dc.l	.Gens
	dc.l	.BlastEm
	dc.l	.Exodus
	dc.l	.MegaSg
	dc.l	.Steam
	dc.l	.Picodrive
	dc.l	.Flashback
	dc.l	.Firecore
	dc.l	.Genecyst
.Hardware:
	convStr	"HARDWARE"
	dc.b	0
	even
.GPGX:
	convStr	"GENESIS PLUS GX"
	dc.b	0
	even
.Regen:
	convStr	"REGEN"
	dc.b	0
	even
.Kega:
	convStr	"KEGA FUSION"
	dc.b	0
	even
.Gens:
	convStr	"GENS"
	dc.b	0
	even
.BlastEm:
	convStr	"BLASTEM"
	dc.b	0
	even
.Exodus:
	convStr	"EXODUS"
	dc.b	0
	even
.MegaSg:
	convStr	"MEGA SG"
	dc.b	0
	even
.Steam:
	convStr	"STEAM"
	dc.b	0
	even
.Picodrive:
	convStr	"PICODRIVE"
	dc.b	0
	even
.Flashback:
	convStr	"ATGAMES FLASHBACK"
	dc.b	0
	even
.Firecore:
	convStr	"ATGAMES FIRECORE"
	dc.b	0
	even
.Genecyst:
	convStr	"GENECYST"
	dc.b	0
	even

; -------------------------------------------------------------------------
; Data
; -------------------------------------------------------------------------

ArtKos_Font:
	incbin	"data/font.kos"
	even
Pal_Font:
	incbin	"data/palette.pal"
	even

; -------------------------------------------------------------------------
