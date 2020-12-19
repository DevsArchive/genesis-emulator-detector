
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		decomp.asm
;	Contents:	Decompression library
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Kosinski decompression (General purpose)
; New faster version by written by vladikcomper,
; with additional improvements by MarkeyJester and Flamewing
; -------------------------------------------------------------------------
; PARAMETERS:
;	a0.l	- Source address
;	a1.l	- Destination address
; RETURNS:
;	a1.l	- End of decompressed data address
; -------------------------------------------------------------------------

_Kos_UseLUT		EQU	1
_Kos_LoopUnroll		EQU	3
_Kos_ExtremeUnrolling	EQU	1

; -------------------------------------------------------------------------

_Kos_RunBitStream macro &

	dbra	d2,.skip\@
	moveq	#7,d2				; Set repeat count to 8.
	move.b	d1,d0				; Use the remaining 8 bits.
	not.w	d3				; Have all 16 bits been used up?
	bne.s	.skip\@				; Branch if not.
	move.b	(a0)+,d0			; Get desc field low-byte.
	move.b	(a0)+,d1			; Get desc field hi-byte.
	if _Kos_UseLUT=1
		move.b	(a4,d0.w),d0		; Invert bit order...
		move.b	(a4,d1.w),d1		; ... for both bytes.
	endif
.skip\@:

	endm

; -------------------------------------------------------------------------

_Kos_ReadBit macro &

	if _Kos_UseLUT=1
		add.b	d0,d0				; Get a bit from the bitstream.
	else
		lsr.b	#1,d0				; Get a bit from the bitstream.
	endif

	endm

; -------------------------------------------------------------------------

KosDec:
	moveq	#(1<<_Kos_LoopUnroll)-1,d7
	if _Kos_UseLUT=1
		moveq	#0,d0
		moveq	#0,d1
		lea	KosDec_ByteMap(pc),a4	; Load LUT pointer.
	endif
	move.b	(a0)+,d0			; Get desc field low-byte.
	move.b	(a0)+,d1			; Get desc field hi-byte.
	if _Kos_UseLUT=1
		move.b	(a4,d0.w),d0		; Invert bit order...
		move.b	(a4,d1.w),d1		; ... for both bytes.
	endif
	moveq	#7,d2				; Set repeat count to 8.
	moveq	#0,d3				; d3 will be desc field switcher.
	bra.s	.FetchNewCode

.FetchCodeLoop:
	; Code 1 (Uncompressed byte).
	_Kos_RunBitStream
	move.b	(a0)+,(a1)+
 
.FetchNewCode:
	_Kos_ReadBit
	bcs.s	.FetchCodeLoop			; If code = 1, branch.
 
	; Codes 00 and 01.
	moveq	#-1,d5
	lea	(a1),a5
	_Kos_RunBitStream

	if _Kos_ExtremeUnrolling=1
		_Kos_ReadBit
		bcs.w	.Code_01
 
		; Code 00 (Dictionary ref. short).
		_Kos_RunBitStream
		_Kos_ReadBit
		bcs.s	.Copy45
		_Kos_RunBitStream
		_Kos_ReadBit
		bcs.s	.Copy3
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
		adda.w	d5,a5
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		bra.s	.FetchNewCode

.Copy3:
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
		adda.w	d5,a5
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		bra.w	.FetchNewCode

.Copy45:
		_Kos_RunBitStream
		_Kos_ReadBit
		bcs.s	.Copy5
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
		adda.w	d5,a5
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		bra.w	.FetchNewCode

.Copy5:
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
		adda.w	d5,a5
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		bra.w	.FetchNewCode

	else
		moveq	#0,d4			; d4 will contain copy count.
		_Kos_ReadBit
		bcs.s	.Code_01
 
		; Code 00 (Dictionary ref. short).
		_Kos_RunBitStream
		_Kos_ReadBit
		addx.w	d4,d4
		_Kos_RunBitStream
		_Kos_ReadBit
		addx.w	d4,d4
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
 
.StreamCopy:
		adda.w	d5,a5
		move.b	(a5)+,(a1)+		; Do 1 extra copy (to compensate +1 to copy counter).
 
.copy:
		move.b	(a5)+,(a1)+
		dbra	d4,.copy
		bra.w	.FetchNewCode
	endif

.Code_01:
	moveq	#0,d4				; d4 will contain copy count.
	; Code 01 (Dictionary ref. long / special).
	_Kos_RunBitStream
	move.b	(a0)+,d6			; d6 = %LLLLLLLL.
	move.b	(a0)+,d4			; d4 = %HHHHHCCC.
	move.b	d4,d5				; d5 = %11111111 HHHHHCCC.
	lsl.w	#5,d5				; d5 = %111HHHHH CCC00000.
	move.b	d6,d5				; d5 = %111HHHHH LLLLLLLL.
	if _Kos_LoopUnroll=3
		and.w	d7,d4			; d4 = %00000CCC.
	else
		andi.w	#7,d4
	endif
	bne.s	.StreamCopy			; if CCC=0, branch.
 
	; special mode (extended counter)
	move.b	(a0)+,d4			; Read cnt
	beq.s	.Quit				; If cnt=0, quit decompression.
	subq.b	#1,d4
	beq.w	.FetchNewCode			; If cnt=1, fetch a new code.
 
	adda.w	d5,a5
	move.b	(a5)+,(a1)+			; Do 1 extra copy (to compensate +1 to copy counter).
	move.w	d4,d6
	not.w	d6
	and.w	d7,d6
	add.w	d6,d6
	lsr.w	#_Kos_LoopUnroll,d4
	jmp	.LargeCopy(pc,d6.w)

.LargeCopy:
	rept (1<<_Kos_LoopUnroll)
		move.b	(a5)+,(a1)+
	endr
	dbra	d4,.LargeCopy
	bra.w	.FetchNewCode

	if _Kos_ExtremeUnrolling=1
.StreamCopy:
		adda.w	d5,a5
		move.b	(a5)+,(a1)+		; Do 1 extra copy (to compensate +1 to copy counter).
		if _Kos_LoopUnroll=3
			eor.w	d7,d4
		else
			eori.w	#7,d4
		endif
		add.w	d4,d4
		jmp	.MediumCopy(pc,d4.w)

.MediumCopy:
		rept 8
			move.b	(a5)+,(a1)+
		endr
		bra.w	.FetchNewCode
	endif

.Quit:
	rts
; -------------------------------------------------------------------------

	if _Kos_UseLUT=1
KosDec_ByteMap:
		dc.b	$00,$80,$40,$C0,$20,$A0,$60,$E0,$10,$90,$50,$D0,$30,$B0,$70,$F0
		dc.b	$08,$88,$48,$C8,$28,$A8,$68,$E8,$18,$98,$58,$D8,$38,$B8,$78,$F8
		dc.b	$04,$84,$44,$C4,$24,$A4,$64,$E4,$14,$94,$54,$D4,$34,$B4,$74,$F4
		dc.b	$0C,$8C,$4C,$CC,$2C,$AC,$6C,$EC,$1C,$9C,$5C,$DC,$3C,$BC,$7C,$FC
		dc.b	$02,$82,$42,$C2,$22,$A2,$62,$E2,$12,$92,$52,$D2,$32,$B2,$72,$F2
		dc.b	$0A,$8A,$4A,$CA,$2A,$AA,$6A,$EA,$1A,$9A,$5A,$DA,$3A,$BA,$7A,$FA
		dc.b	$06,$86,$46,$C6,$26,$A6,$66,$E6,$16,$96,$56,$D6,$36,$B6,$76,$F6
		dc.b	$0E,$8E,$4E,$CE,$2E,$AE,$6E,$EE,$1E,$9E,$5E,$DE,$3E,$BE,$7E,$FE
		dc.b	$01,$81,$41,$C1,$21,$A1,$61,$E1,$11,$91,$51,$D1,$31,$B1,$71,$F1
		dc.b	$09,$89,$49,$C9,$29,$A9,$69,$E9,$19,$99,$59,$D9,$39,$B9,$79,$F9
		dc.b	$05,$85,$45,$C5,$25,$A5,$65,$E5,$15,$95,$55,$D5,$35,$B5,$75,$F5
		dc.b	$0D,$8D,$4D,$CD,$2D,$AD,$6D,$ED,$1D,$9D,$5D,$DD,$3D,$BD,$7D,$FD
		dc.b	$03,$83,$43,$C3,$23,$A3,$63,$E3,$13,$93,$53,$D3,$33,$B3,$73,$F3
		dc.b	$0B,$8B,$4B,$CB,$2B,$AB,$6B,$EB,$1B,$9B,$5B,$DB,$3B,$BB,$7B,$FB
		dc.b	$07,$87,$47,$C7,$27,$A7,$67,$E7,$17,$97,$57,$D7,$37,$B7,$77,$F7
		dc.b	$0F,$8F,$4F,$CF,$2F,$AF,$6F,$EF,$1F,$9F,$5F,$DF,$3F,$BF,$7F,$FF
	endif

; -------------------------------------------------------------------------
; Enigma decompression (Mainly for plane mappings)
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- Base tile properties (tile ID, flags, etc.)
;	a0.l	- Source address
;	a1.l	- Destination address
; RETURNS:
;	a1.l	- End of decompressed data address
; -------------------------------------------------------------------------

EniDec:
	movem.l d0-d7/a1-a5,-(sp)
	movea.w d0,a3				; Store base tile properties
	move.b	(a0)+,d0
	ext.w	d0
	movea.w d0,a5				; Store first byte, extended to word
	move.b	(a0)+,d4			; Store second byte
	lsl.b	#3,d4				; Multiply by 8
	movea.w (a0)+,a2			; Store third and fourth byte
	adda.w	a3,a2				; Add base tile properties
	movea.w (a0)+,a4			; Store fifth and sixth byte
	adda.w	a3,a4				; Add base tile properties
	move.b	(a0)+,d5			; Store seventh byte
	asl.w	#8,d5				; Shift up by a byte
	move.b	(a0)+,d5			; Store eigth byte in lower register byte
	moveq	#$10,d6				; 16 bits = 2 bytes

EniDec_Loop:
	moveq	#7,d0				; Process 7 bits at a time
	move.w	d6,d7
	sub.w	d0,d7
	move.w	d5,d1
	lsr.w	d7,d1
	andi.w	#$7F,d1				; Keep only lower 7 bits
	move.w	d1,d2
	cmpi.w	#$40,d1				; Is Bit 6 set?
	bcc.s	.getnext			; If so, branch
	moveq	#6,d0				; If not, process 6 bits instead of 7
	lsr.w	#1,d2				; Bitfield now becomes TTSSSS isntead of TTTSSSS

.getnext:
	bsr.w	EniDec_ChkGetNextByte
	andi.w	#$F,d2				; Keep only lower nibble
	lsr.w	#4,d1				; Store upper nibble (max value = 7)
	add.w	d1,d1
	jmp	EniDec_JmpTable(pc,d1.w)

EniDec_Sub0:
	move.w	a2,(a1)+			; Write to destination
	addq.w	#1,a2				; Increment
	dbf	d2,EniDec_Sub0			; Repeat
	bra.s	EniDec_Loop

EniDec_Sub4:
	move.w	a4,(a1)+			; Write to destination
	dbf	d2,EniDec_Sub4			; Repeat
	bra.s	EniDec_Loop

EniDec_Sub8:
	bsr.w	EniDec_GetInlineCopyVal

.loop1:
	move.w	d1,(a1)+
	dbf	d2,.loop1
	bra.s	EniDec_Loop

EniDec_SubA:
	bsr.w	EniDec_GetInlineCopyVal

.loop2:
	move.w	d1,(a1)+
	addq.w	#1,d1
	dbf	d2,.loop2
	bra.s	EniDec_Loop

EniDec_SubC:
	bsr.w	EniDec_GetInlineCopyVal

.loop3:
	move.w	d1,(a1)+
	subq.w	#1,d1
	dbf	d2,.loop3
	bra.s	EniDec_Loop

EniDec_SubE:
	cmpi.w	#$F,d2
	beq.s	EniDec_End

.loop4:
	bsr.w	EniDec_GetInlineCopyVal
	move.w	d1,(a1)+
	dbf	d2,.loop4
	bra.s	EniDec_Loop

; -------------------------------------------------------------------------

EniDec_JmpTable:
	bra.s	EniDec_Sub0
	bra.s	EniDec_Sub0
	bra.s	EniDec_Sub4
	bra.s	EniDec_Sub4
	bra.s	EniDec_Sub8
	bra.s	EniDec_SubA
	bra.s	EniDec_SubC
	bra.s	EniDec_SubE

; -------------------------------------------------------------------------

EniDec_End:
	subq.w	#1,a0
	cmpi.w	#16,d6				; Were we going to start on a completely new byte?
	bne.s	.norollback			; If not, branch
	subq.w	#1,a0

.norollback:
	move.w	a0,d0
	lsr.w	#1,d0				; Are we on an odd byte?
	bcc.s	.evendest			; If not, branch
	addq.w	#1,a0				; Ensure we're on an even byte

.evendest:
	movem.l	(sp)+,d0-d7/a1-a5
	rts

EniDec_GetInlineCopyVal:
	move.w	a3,d3				; Store base tile properties
	move.b	d4,d1
	add.b	d1,d1
	bcc.s	.nopriority			; If d4 was < $80
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.nopriority			; If not, branch
	ori.w	#(1<<15),d3			; Set high priority bit

.nopriority:
	add.b	d1,d1
	bcc.s	.nopal1				; If d4 < $40
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.nopal1				; If not, branch
	addi.w	#(2<<13),d3			; Set the second palette bit

.nopal1:
	add.b	d1,d1
	bcc.s	.nopal0				; If d4 was < $20
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.nopal0				; If not, branch
	addi.w	#(1<<13),d3			; Set the first palette bit

.nopal0:
	add.b	d1,d1
	bcc.s	.noyflip			; If d4 was < $10
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.noyflip			; If not, branch
	ori.w	#(1<<12),d3			; Set the Y flip bit

.noyflip:
	add.b	d1,d1
	bcc.s	.noxflip			; If d4 was < 8
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.noxflip			; If not, branch
	ori.w	#(1<<11),d3			; Set the X flip bit

.noxflip:
	move.w	d5,d1
	move.w	d6,d7				; Get remaining bits
	sub.w	a5,d7				; Subtract minimum bit number
	bcc.s	.GotEnoughBits			; If we're beyond that, branch
	move.w	d7,d6
	addi.w	#16,d6				; 16 bits = 2 bytes
	neg.w	d7				; Calculate bit deficit
	lsl.w	d7,d1				; Make space for this many bits
	move.b	(a0),d5				; Get next byte
	rol.b	d7,d5				; Make the upper X bits the lower X bits
	add.w	d7,d7
	and.w	EniDec_AndVals-2(pc,d7.w),d5	; Only keep X lower bits
	add.w	d5,d1				; Compensate for the bit deficit

.AddBits:
	move.w	a5,d0
	add.w	d0,d0
	and.w	EniDec_AndVals-2(pc,d0.w),d1	; Only keep as many bits as required
	add.w	d3,d1				; Add base tile properties
	move.b	(a0)+,d5			; Get current byte, move onto next byte
	lsl.w	#8,d5				; Shift up by a byte
	move.b	(a0)+,d5			; Store next byte in lower register byte
	rts

.GotEnoughBits:
	beq.s	.GotExactCount			; If the exact number of bits are leftover, branch
	lsr.w	d7,d1				; Remove unneeded bits
	move.w	a5,d0
	add.w	d0,d0
	and.w	EniDec_AndVals-2(pc,d0.w),d1	; Only keep as many bits as required
	add.w	d3,d1				; Add base tile properties
	move.w	a5,d0				; Store number of bits used up by inline copy
	bra.s	EniDec_ChkGetNextByte		; Move onto next byte

.GotExactCount:
	moveq	#16,d6				; 16 bits = 2 bytes
	bra.s	.AddBits

; -------------------------------------------------------------------------

EniDec_AndVals:
	dc.w	$0001, $0003, $0007, $000F
	dc.w	$001F, $003F, $007F, $00FF
	dc.w	$01FF, $03FF, $07FF, $0FFF
	dc.w	$1FFF, $3FFF, $7FFF, $FFFF

; -------------------------------------------------------------------------

EniDec_ChkGetNextByte:
	sub.w	d0,d6
	cmpi.w	#9,d6
	bcc.s	.Done
	addq.w	#8,d6				; 8 bits = 1 byte
	asl.w	#8,d5				; Shift up by a byte
	move.b	(a0)+,d5			; Store next byte in lower register byte

.Done:
	rts

; -------------------------------------------------------------------------
; Load Kosinski compressed art into VRAM
; -------------------------------------------------------------------------
; PARAMETERS:
;	a0.l	- Source address
;	a1.l	- Decompression buffer
;	a2.w	- VRAM address
; -------------------------------------------------------------------------

LoadKosArt:
	movea.l	a1,a3				; Save destination address
	bsr.w	KosDec				; Decompress the art

	suba.l	a3,a1				; Get size of decompressed art
	move.w	a1,d3
	lsr.w	#1,d3				; Divide by 2 for DMA
	move.l	a3,d1				; Use destination address for DMA source
	move.w	a2,d2				; Get destination VRAM address
	bsr.w	QueueDMA			; Queue a DMA transfer
	bra.w	ProcessDMA			; Process the DMA queue

; -------------------------------------------------------------------------
; Load an Enigma compressed map into VRAM (for )
; -------------------------------------------------------------------------
; PARAMETERS:
;	a0.l	- Source address
;	a1.l	- Decompression buffer
;	d0.l	- VDP command for storing plane data
;	d1.w	- Plane width (minus 1)
;	d2.w	- Plane height (minus 1)
;	d3.w	- Base tile ID
;	d4.l	- VDP command delta for new rows
;		  (only if not already provided)
; -------------------------------------------------------------------------

LoadEniMapH20:
	move.l	#$400000,d4			; H40 delta
	bra.s	LoadEniMapHXX

LoadEniMapH40:
	move.l	#$800000,d4			; H40 delta
	bra.s	LoadEniMapHXX

LoadEniMapH80:
	move.l	#$1000000,d4			; H80 delta
	bra.s	LoadEniMapHXX

LoadEniMap:
	move.l	r_Plane_Delta.w,d4		; Get delta

LoadEniMapHXX:
	movem.l	d0-d4/a1,-(sp)			; Save registers
	moveq	#0,d0				; Base tile ID for compressed plane data
	bsr.w	EniDec				; Decompress mappings
	movem.l	(sp)+,d0-d4/a1			; Restore registers

	bra.w	DrawPlaneHXX			; Draw the plabe

; -------------------------------------------------------------------------
