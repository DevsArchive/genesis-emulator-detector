
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		vdp.asm
;	Contents:	VDP library (Ultra DMA queue by Flamewing)
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Add a DMA transfer command to the DMA queue
; -------------------------------------------------------------------------
; PARAMETERS:
;	d1.l	- Source in 68000 memory
;	d2.w	- Destination in VRAM
;	d3.w	- Transfer length in words
; -------------------------------------------------------------------------

; This option makes the function work as a drop-in replacement of the original
; functions. If you modify all callers to supply a position in words instead of
; bytes (i.e., divide source address by 2) you can set this to 0 to gain 10(1/0)
AssumeSourceAddressInBytes	EQU	1

; This option (which is disabled by default) makes the DMA queue assume that the
; source address is given to the function in a way that makes them safe to use
; with RAM sources. You need to edit all callers to ensure this.
; Enabling this option turns off UseRAMSourceSafeDMA, and saves 14(2/0).
AssumeSourceAddressIsRAMSafe	EQU	0

; This option (which is enabled by default) makes source addresses in RAM safe
; at the cost of 14(2/0). If you modify all callers so as to clear the top byte
; of source addresses (i.e., by ANDing them with $FFFFFF).
UseRAMSourceSafeDMA		EQU	1&(AssumeSourceAddressIsRAMSafe=0)

; This option breaks DMA transfers that crosses a 128kB block into two. It is disabled by default because you can simply align the art in ROM
; and avoid the issue altogether. It is here so that you have a high-performance routine to do the job in situations where you can't align it in ROM.
Use128kbSafeDMA			EQU	1

; Option to mask interrupts while updating the DMA queue. This fixes many race conditions in the DMA funcion, but it costs 46(6/1) cycles. The
; better way to handle these race conditions would be to make unsafe callers (such as S3&K's KosM decoder) prevent these by masking off interrupts
; before calling and then restore interrupts after.
UseVIntSafeDMA			EQU	1

; Like vdpComm, but starting from an address contained in a register

vdpCommReg macro &
	reg, type, rwd, clr

	lsl.l	#2,\reg				; Move high bits into (word-swapped) position, accidentally moving everything else
    if ((v\type\&v\rwd\)&3)<>0
	addq.w	#(v\type\&v\rwd\)&3,\reg	; Add upper access type bits
    endif
	ror.w	#2,\reg				; Put upper access type bits into place, also moving all other bits into their correct (word-swapped) places
	swap	\reg				; Put all bits in proper places
    if \clr<>0
	andi.w	#3,\reg				; Strip whatever junk was in upper word of reg
    endif
    if ((v\type\&v\rwd\)&$FC)=$20
	tas.b	\reg				; Add in the DMA flag -- tas fails on memory, but works on registers
    elseif ((v\type\&v\rwd\)&$FC)<>0
	ori.w	#((v\type\&v\rwd\)&$FC)<<2,\reg	; Add in missing access type bits
    endif

	endm

; -------------------------------------------------------------------------

	rsreset
DMAEntry.Reg94		rs.b	1
DMAEntry.Size		rs.b	0
DMAEntry.SizeH		rs.b	1
DMAEntry.Reg93		rs.b	1
DMAEntry.Source		rs.b	0
DMAEntry.SizeL		rs.b	1
DMAEntry.Reg97		rs.b	1
DMAEntry.SrcH		rs.b	1
DMAEntry.Reg96		rs.b	1
DMAEntry.SrcM		rs.b	1
DMAEntry.Reg95		rs.b	1
DMAEntry.SrcL		rs.b	1
DMAEntry.Command	rs.l	1
DMAEntry.len		rs.b	0

; -------------------------------------------------------------------------

QueueSlotCount	EQU	(r_DMA_Slot-r_DMA_Queue)/DMAEntry.len

; -------------------------------------------------------------------------

loadDMA macro &
	src, length, dest

	if ((\src)&1)<>0
		inform 2,"DMA queued from odd source $\$src\!"
	endif
	if ((\length)&1)<>0
		inform 2,"DMA an odd number of bytes $\length\!"
	endif
	if (\length)=0
		inform 2,"DMA transferring 0 bytes (becomes a 128kB transfer). If you really mean it, pass 128kB instead."
	endif
	if (((\src)+(\length)-1)>>17)<>((\src)>>17)
		inform 2,"DMA crosses a 128kB boundary. You should either split the DMA manually or align the source adequately."
	endif
	if UseVIntSafeDMA=1
		move.w	sr,-(sp)		; Save current interrupt mask
		di				; Mask off interrupts
	endif
	movea.w	r_DMA_Slot.w,a1
	cmpa.w	#r_DMA_Slot,a1
	beq.s	.Done\@				; Return if there's no more room in the queue

						; Write top byte of size/2
	move.b	#((((\length)>>1)&$7FFF)>>8)&$FF,DMAEntry.SizeH(a1)
						; Set d0 to bottom byte of size/2 and the low 3 bytes of source/2
	move.l	#(((((\length)>>1)&$7FFF)&$FF)<<24)|(((\src)>>1)&$7FFFFF),d0
	movep.l	d0,DMAEntry.SizeL(a1)		; Write it all to the queue
	lea	DMAEntry.Command(a1),a1		; Seek to correct RAM address to store VDP DMA command
	vdpCmd	move.l,\dest,VRAM,DMA,(a1)+	; Write VDP DMA command for destination address
	move.w	a1,r_DMA_Slot.w			; Write next queue slot

.Done\@:
	if UseVIntSafeDMA=1
		move.w	(sp)+,sr		; Restore interrupts to previous state
	endif

	endm

; -------------------------------------------------------------------------

resetDMA macros

	move.w	#r_DMA_Queue,r_DMA_Slot.w

; -------------------------------------------------------------------------

QueueDMA:
	if UseVIntSafeDMA=1
		move.w	sr,-(sp)		; Save current interrupt mask
		di				; Mask off interrupts
	endif
	movea.w	r_DMA_Slot.w,a1
	cmpa.w	#r_DMA_Slot,a1
	beq.s	.Done				; Return if there's no more room in the queue

	if AssumeSourceAddressInBytes<>0
		lsr.l	#1,d1			; Source address is in words for the VDP registers
	endif
	if UseRAMSourceSafeDMA<>0
		bclr.l	#23,d1			; Make sure bit 23 is clear (68k->VDP DMA flag)
	endif
	movep.l	d1,DMAEntry.Source(a1)		; Write source address; the useless top byte will be overwritten later
	moveq	#0,d0				; We need a zero on d0

	if Use128kbSafeDMA<>0
		; Detect if transfer crosses 128KB boundary
		; Using sub+sub instead of move+add handles the following edge cases:
		; (1) d3.w == 0 => 128kB transfer
		;   (a) d1.w == 0 => no carry, don't split the DMA
		;   (b) d1.w != 0 => carry, need to split the DMA
		; (2) d3.w != 0
		;   (a) if there is carry on d1.w + d3.w
		;     (* ) if d1.w + d3.w == 0 => transfer comes entirely from current 128kB block, don't split the DMA
		;     (**) if d1.w + d3.w != 0 => need to split the DMA
		;   (b) if there is no carry on d1.w + d3.w => don't split the DMA
		; The reason this works is that carry on d1.w + d3.w means that
		; d1.w + d3.w >= $10000, whereas carry on (-d3.w) - (d1.w) means that
		; d1.w + d3.w > $10000.
		sub.w	d3,d0			; Using sub instead of move and add allows checking edge cases
		sub.w	d1,d0			; Does the transfer cross over to the next 128kB block?
		bcs.s	.doubletransfer		; Branch if yes
	endif
	; It does not cross a 128kB boundary. So just finish writing it.
	movep.w	d3,DMAEntry.Size(a1)		; Write DMA length, overwriting useless top byte of source address

.finishxfer:
	; Command to specify destination address and begin DMA
	move.w	d2,d0				; Use the fact that top word of d0 is zero to avoid clearing on vdpCommReg
	vdpCommReg d0,VRAM,DMA,0		; Convert destination address to VDP DMA command
	lea	DMAEntry.Command(a1),a1		; Seek to correct RAM address to store VDP DMA command
	move.l	d0,(a1)+			; Write VDP DMA command for destination address
	move.w	a1,r_DMA_Slot.w			; Write next queue slot

.Done:
	if UseVIntSafeDMA=1
		move.w	(sp)+,sr		; Restore interrupts to previous state
	endif
	rts

	if Use128kbSafeDMA=1
.doubletransfer:
	; We need to split the DMA into two parts, since it crosses a 128kB block
	add.w	d3,d0				; Set d0 to the number of words until end of current 128kB block
	movep.w	d0,DMAEntry.Size(a1)		; Write DMA length of first part, overwriting useless top byte of source addres

	cmpa.w	#r_DMA_Slot-DMAEntry.len,a1	; Does the queue have enough space for both parts?
	beq.s	.finishxfer			; Branch if not

	; Get second transfer's source, destination, and length
	sub.w	d0,d3				; Set d3 to the number of words remaining
	add.l	d0,d1				; Offset the source address of the second part by the length of the first part
	add.w	d0,d0				; Convert to number of bytes
	add.w	d2,d0				; Set d0 to the VRAM destination of the second part

	; If we know top word of d2 is clear, the following vdpCommReg can be set to not
	; clear it. There is, unfortunately, no faster way to clear it than this.
	vdpCommReg d2,VRAM,DMA,1		; Convert destination address of first part to VDP DMA command
	move.l	d2,DMAEntry.Command(a1)		; Write VDP DMA command for destination address of first part

	; Do second transfer
						; Write source address of second part; useless top byte will be overwritten later
	movep.l	d1,DMAEntry.len+DMAEntry.Source(a1)
						; Write DMA length of second part, overwriting useless top byte of source address
	movep.w	d3,DMAEntry.len+DMAEntry.Size(a1)

	; Command to specify destination address and begin DMA
	vdpCommReg d0,VRAM,DMA,0		; Convert destination address to VDP DMA command; we know top half of d0 is zero
						; Seek to correct RAM address to store VDP DMA command of second part
	lea	DMAEntry.len+DMAEntry.Command(a1),a1
	move.l	d0,(a1)+			; Write VDP DMA command for destination address of second part

	move.w	a1,r_DMA_Slot.w			; Write next queue slot
	if UseVIntSafeDMA=1
		move.w	(sp)+,sr		; Restore interrupts to previous state
	endif
	rts
	endif

; -------------------------------------------------------------------------
; Process all the DMA commands queued
; -------------------------------------------------------------------------

ProcessDMA:
	lea	VDP_CTRL,a5
	movea.w	r_DMA_Slot.w,a1
	jmp	.jump_table-r_DMA_Queue(a1)

; -------------------------------------------------------------------------

.jump_table:
	rts
	rept 6
		rts											; Just in case
	endr

; -------------------------------------------------------------------------

c = 1
	rept QueueSlotCount
		lea	VDP_CTRL,a5
		lea	r_DMA_Queue.w,a1
		if c<>QueueSlotCount
			bra.w	.jump0-(c*8)
		endif
c = c+1
	endr

; -------------------------------------------------------------------------

	rept QueueSlotCount
		move.l	(a1)+,(a5)									; Transfer length
		move.l	(a1)+,(a5)									; Source address high
		move.l	(a1)+,(a5)									; Source address low + destination high
		move.w	(a1)+,(a5)									; Destination low, trigger DMA
	endr

.jump0:
	resetDMA
	rts

; -------------------------------------------------------------------------
; Initialize the DMA queue
; -------------------------------------------------------------------------

InitDMA:
	lea	r_DMA_Queue.w,a0
	move.b	#$94,d0
	move.l	#$93979695,d1
c = 0
	rept QueueSlotCount
		move.b	d0,c+DMAEntry.Reg94(a0)
		movep.l	d1,c+DMAEntry.Reg93(a0)
c = c+DMAEntry.len
	endr

	resetDMA
	rts

; -------------------------------------------------------------------------
; Initailize sprite data
; -------------------------------------------------------------------------

InitSprites:
	moveq	#0,d0
	lea	r_Sprites.w,a0			; Sprite table buffer
	moveq	#1,d1				; Link value
	moveq	#($280/8)-1,d2			; Number of sprites

.InitLoop:
	move.l	d0,(a0)				; Move off screen
	move.l	d0,4(a0)
	move.b	d1,3(a0)			; Set link value
	addq.w	#1,d1				; Increment link value
	addq.w	#8,a0				; Next sprite
	dbf	d2,.InitLoop			; Loop
	move.b	d0,-5(a0)			; Set final link value to 0

	move.b	#80-1,r_Sprs_Left.w		; Reset sprites left to draw
	rts

; -------------------------------------------------------------------------
; Clear the screen
; -------------------------------------------------------------------------

InitScreen:
	lea	VDP_CTRL,a0
	move.w	#$8F01,(a0)			; Set autoincrement to 1
	dmaFill	0,$8000,$8000,a0		; Clear planes, sprites, and HScroll
	move.w	#$8F02,(a0)			; Set autoincrement to 2

	clrRAM	r_HScrl,r_VScrl_End		; Clear scroll RAM
	
	bsr.s	InitSprites			; Initialize sprite data
	bra.w	InitDMA				; Initialize the DMA queue

; -------------------------------------------------------------------------
; Draw a plane map
; -------------------------------------------------------------------------
; PARAMETERS:
;	a1.l	- Pointer to plane map
;	d0.l	- VDP command
;	d1.w	- Width in tiles (minus 1)
;	d2.w	- Height in tiles (minus 1)
;	d3.w	- Base tile
;	d4.l	- VDP command delta for new rows
;		  (only if not already provided)
; -------------------------------------------------------------------------

DrawPlaneH20:
	move.l	#$400000,d4			; H20 delta
	bra.s	DrawPlaneHXX

DrawPlaneH40:
	move.l	#$800000,d4			; H40 delta
	bra.s	DrawPlaneHXX

DrawPlaneH80:
	move.l	#$1000000,d4			; H80 delta
	bra.s	DrawPlaneHXX

DrawPlane:
	move.l	r_Plane_Delta.w,d4		; Get delta

DrawPlaneHXX:
	move.l	d0,VDP_CTRL			; Set VDP command
	move.w	d1,d6				; Copy width

.RowLoop:
	move.w	(a1)+,d5			; Get tile
	add.w	d3,d5				; Add base tile
	move.w	d5,VDP_DATA			; Draw it
	dbf	d6,.RowLoop			; Loop for next tile
	add.l	d4,d0				; Next row
	dbf	d2,DrawPlaneHXX			; Loop for next row
	rts

; -------------------------------------------------------------------------
; Fill a region of a plane
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- VDP command
;	d1.w	- Width in tiles (minus 1)
;	d2.w	- Height in tiles (minus 1)
;	d3.w	- Tile to fill plane with
;	d4.l	- VDP command delta for new rows
;		  (only if not already provided)
; -------------------------------------------------------------------------

FillPlaneH20:
	move.l	#$400000,d4			; H20 delta
	bra.s	FillPlaneHXX

FillPlaneH40:
	move.l	#$800000,d4			; H40 delta
	bra.s	FillPlaneHXX

FillPlaneH80:
	move.l	#$1000000,d4			; H80 delta
	bra.s	FillPlaneHXX

FillPlane:
	move.l	r_Plane_Delta.w,d4		; Get delta

FillPlaneHXX:
	move.l	d0,VDP_CTRL			; Set VDP command
	move.w	d1,d5				; Copy width

.RowLoop:
	move.w	d3,VDP_DATA			; Fill plane with tile
	dbf	d5,.RowLoop			; Loop for next tile
	add.l	d4,d0				; Next row
	dbf	d2,FillPlane			; Loop for next row
	rts

; -------------------------------------------------------------------------
; Clear a region of a plane
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- VDP command
;	d1.w	- Width in tiles (minus 1)
;	d2.w	- Height in tiles (minus 1)
;	d4.l	- VDP command delta for new rows
;		  (only if not already provided)
; -------------------------------------------------------------------------

ClrPlaneH20:
	moveq	#0,d3				; Fill with 0
	bra.s	FillPlaneH20			; H20

ClrPlaneH40:
	moveq	#0,d3				; Fill with 0
	bra.s	FillPlaneH40			; H40

ClrPlaneH80:
	moveq	#0,d3				; Fill with 0
	bra.s	FillPlaneH80			; H80

ClrPlane:
	moveq	#0,d3				; Fill with 0
	bra.s	FillPlane

; -------------------------------------------------------------------------
; Set the size of a plane
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- Width (0 = 256, 1 = 512, 2 = 1024)
;	d1.w	- Height (0 = 256, 1 = 512, 2 = 1024)
; -------------------------------------------------------------------------

SetPlaneSz:
	lsl.w	#4,d0				; Get table offset
	lsl.w	#6,d1
	or.w	d1,d0
	move.l	.Vals(pc,d0.w),VDP_CTRL		; Set registers
	move.l	.Vals+4(pc,d0.w),VDP_CTRL
	move.l	.Vals+8(pc,d0.w),r_Plane_Delta.w; Set plane delta
	rts

; -------------------------------------------------------------------------

.Vals:
	dc.w	$9000, $8230, $8334, $8407	; 32x32
	dc.l	$400000, 0
	dc.w	$9001, $8230, $8334, $8407	; 64x32
	dc.l	$800000, 0
	dc.w	$9003, $8228, $8320, $8406	; 128x32
	dc.l	$1000000, 0
	dc.w	$9001, $8230, $8334, $8407	; Invalid
	dc.l	$800000, 0
	dc.w	$9010, $8230, $8334, $8407	; 32x64
	dc.l	$400000, 0
	dc.w	$9011, $8228, $8320, $8406	; 64x64
	dc.l	$800000, 0
	dc.w	$9001, $8230, $8334, $8407	; Invalid
	dc.l	$800000, 0
	dc.w	$9001, $8230, $8334, $8407	; Invalid
	dc.l	$800000, 0
	dc.w	$9030, $8228, $8320, $8406	; 32x128
	dc.l	$400000, 0
	dc.w	$9001, $8230, $8334, $8407	; Invalid
	dc.l	$800000, 0
	dc.w	$9001, $8230, $8334, $8407	; Invalid
	dc.l	$800000, 0
	dc.w	$9001, $8230, $8334, $8407	; Invalid
	dc.l	$800000, 0
	
; -------------------------------------------------------------------------
; Load a palette into the primary palette buffer
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- Size in words (minus 1)
;	a0.l	- Palette data
; -------------------------------------------------------------------------

LoadPalette:
	lea	r_Palette.w,a1			; Palette buffer
	bra.s	LoadPal

; -------------------------------------------------------------------------
; Load a palette into the primary fade palette buffer
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- Size in words (minus 1)
;	a0.l	- Palette data
; -------------------------------------------------------------------------

LoadFadePal:
	lea	r_Fade_Pal.w,a1			; Fade palette buffer

; -------------------------------------------------------------------------
; Load a palette into a buffer
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- Size in words (minus 1)
;	a0.l	- Palette data
;	a1.l	- Palette buffer
; -------------------------------------------------------------------------

LoadPal:
	move.w	(a0)+,(a1)+			; Copy palette data
	dbf	d0,LoadPal			; Loop
	rts

; -------------------------------------------------------------------------
; DMA the correct palette buffer to CRAM
; -------------------------------------------------------------------------

DMAPalette:
	dma68k	r_Palette,0,$80,CRAM		; Transfer primary palette data
	rts

; -------------------------------------------------------------------------
; Fade the palette to black
; -------------------------------------------------------------------------
; PARAMETERS:
;	Nothing
; RETURNS:
;	Nothing
; -------------------------------------------------------------------------

FadeToBlack:
	move.w	#$003F,r_Pal_Fade.w		; Set to fade everything

FadeToBlack_Range:
	moveq	#7,d4				; Set repeat times
		
.FadeLoop:
	bsr.w	VSync				; VSync
	bsr.w	VSync
	bsr.s	FadeToBlack_Once		; Fade the colors once
	dbf	d4,.FadeLoop			; Loop until we are done
	rts

; -------------------------------------------------------------------------

FadeToBlack_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	rts

.FadeColor:
	move.w	(a0),d5				; Load color
	beq.s	.NoRed				; If the color is already black, branch
	move.w	d5,d1				; Copy color
	move.b	d1,d2				; Load green and red
	move.b	d1,d3				; Load only red

	andi.w	#$E00,d1			; Get only blue
	beq.s	.NoBlue				; If blue is finished, branch
	subi.w	#$200,d5			; Decrease blue

.NoBlue:
	andi.b	#$E0,d2				; Get only green
	beq.s	.NoGreen			; If green is finished, branch
	subi.w	#$20,d5				; Decrease green

.NoGreen:
	andi.b	#$E,d3				; Get only red
	beq.s	.NoRed				; If red is finished, branch
	subq.w	#2,d5				; Decrease red

.NoRed:
	move.w	d5,(a0)+			; Save the color
	rts

; -------------------------------------------------------------------------
; Fade the palette from black to the target palette
; -------------------------------------------------------------------------
; PARAMETERS:
;	Nothing
; RETURNS:
;	Nothing
; -------------------------------------------------------------------------

FadeFromBlack:
	move.w	#$003F,r_Pal_Fade.w		; Set to fade everything

FadeFromBlack_Range:
	moveq	#$E,d4				; Maximum color check

.FadeLoop:
	bsr.w	VSync				; VSync
	bsr.w	VSync
	bsr.s	FadeFromBlack_Once		; Fade the colors once
	subq.b	#2,d4				; Decrement color check
	bne.s	.FadeLoop			; If we are not done, branch
	bra.w	VSync				; Do VSync so that the colors transfer

; -------------------------------------------------------------------------

FadeFromBlack_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	lea	r_Fade_Pal.w,a1			; Target palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	adda.w	d0,a1
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	rts

.FadeColor:
	move.b	(a1),d5				; Load blue
	move.w	(a1)+,d1			; Load green and red
	move.b	d1,d2				; Load red
	lsr.b	#4,d1				; Get only green
	andi.b	#$E,d2				; Get only red

	move.w	(a0),d3				; Load current color
	cmp.b	d5,d4				; Should the blue fade?
	bhi.s	.NoBlue				; If not, branch
	addi.w	#$200,d3			; Increase blue

.NoBlue:
	cmp.b	d1,d4				; Should the green fade?
	bhi.s	.NoGreen			; If not, branch
	addi.w	#$20,d3				; Increase green

.NoGreen:
	cmp.b	d2,d4				; Should the red fade?
	bhi.s	.NoRed				; If not, branch
	addq.w	#2,d3				; Increase red

.NoRed:
	move.w	d3,(a0)+			; Save the color
	rts

; -------------------------------------------------------------------------
; Fade the palette to white
; -------------------------------------------------------------------------
; PARAMETERS:
;	Nothing
; RETURNS:
;	Nothing
; -------------------------------------------------------------------------

FadeToWhite:
	move.w	#$003F,r_Pal_Fade.w		; Set to fade everything

FadeToWhite_Range:
	moveq	#7,d4				; Set repeat times

.FadeLoop:
	bsr.w	VSync				; VSync
	bsr.w	VSync
	bsr.s	FadeToWhite_Once		; Fade the colors once
	dbf	d4,.FadeLoop			; Loop until we are done
	rts

; -------------------------------------------------------------------------

FadeToWhite_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	rts

.FadeColor:
	move.w	(a0),d5				; Load color
	cmpi.w	#$EEE,d5			; Is it already white?
	beq.s	.NoRed				; If so, branch
	move.w	d5,d1				; Copy color
	move.b	d1,d2				; Load green and red
	move.b	d1,d3				; Load only red

	andi.w	#$E00,d1			; Get only blue
	cmpi.w	#$E00,d1			; Is blue finished?
	beq.s	.NoBlue				; If do, branch
	addi.w	#$200,d5			; Increase blue

.NoBlue:
	andi.b	#$E0,d2				; Get only green
	cmpi.b	#$E0,d2				; Is green finished?
	beq.s	.NoGreen			; If so, branch
	addi.w	#$20,d5				; Increase green

.NoGreen:
	andi.b	#$E,d3				; Get only red
	cmpi.b	#$E,d3				; Is red finished?
	beq.s	.NoRed				; If so, branch
	addq.w	#2,d5				; Increase red

.NoRed:
	move.w	d5,(a0)+			; Save the color
	rts

; -------------------------------------------------------------------------
; Fade the palette from white to the target palette
; -------------------------------------------------------------------------
; PARAMETERS:
;	Nothing
; RETURNS:
;	Nothing
; -------------------------------------------------------------------------

FadeFromWhite:
	move.w	#$003F,r_Pal_Fade.w		; Set to fade everything

FadeFromWhite_Range:
	moveq	#0,d4				; Minimum color check

.FadeLoop:
	bsr.w	VSync				; VSync
	bsr.w	VSync
	bsr.s	FadeFromWhite_Once		; Fade the colors once
	addq.b	#2,d4				; Decrement color check
	cmpi.b	#$E,d4				; Are we done?
	bne.s	.FadeLoop			; If not, branch
	bra.w	VSync				; Do VSync so that the colors transfer

; -------------------------------------------------------------------------

FadeFromWhite_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	lea	r_Fade_Pal.w,a1			; Target palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	adda.w	d0,a1
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	rts

.FadeColor:
	move.b	(a1),d5				; Load blue
	move.w	(a1)+,d1			; Load green and red
	move.b	d1,d2				; Load red
	lsr.b	#4,d1				; Get only green
	andi.b	#$E,d2				; Get only red

	move.w	(a0),d3				; Load current color
	cmp.b	d5,d4				; Should the blue fade?
	bcs.s	.NoBlue				; If not, branch
	subi.w	#$200,d3			; Decrease blue

.NoBlue:
	cmp.b	d1,d4				; Should the green fade?
	bcs.s	.NoGreen			; If not, branch
	subi.w	#$20,d3				; Decrease green

.NoGreen:
	cmp.b	d2,d4				; Should the red fade?
	bcs.s	.NoRed				; If not, branch
	subq.w	#2,d3				; Decrease red

.NoRed:
	move.w	d3,(a0)+			; Save the color
	rts

; -------------------------------------------------------------------------
; Fade the palette from the current palette to the target palette
; -------------------------------------------------------------------------
; PARAMETERS:
;	Nothing
; RETURNS:
;	Nothing
; -------------------------------------------------------------------------

FadeToPal:
	move.w	#$003F,r_Pal_Fade.w		; Set to fade everything

FadeToPal_Range:
	moveq	#7,d4				; Set repeat times

.FadeLoop:
	bsr.w	VSync				; VSync
	bsr.w	VSync
	bsr.s	FadeToPal_Once			; Fade the colors once
	dbf	d4,.FadeLoop			; Loop until we are done
	rts

; -------------------------------------------------------------------------

FadeToPal_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	lea	r_Fade_Pal.w,a1			; Target palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	adda.w	d0,a1
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	rts

.FadeColor:
	move.w	(a0),d3				; Get color
	cmp.w	(a1)+,d3			; Has the color already reached the target color?
	beq.s	.NoRed				; If so, branch
	
	move.w	-2(a1),d1			; Get green and red
	move.b	d1,d2				; Get red only
	andi.b	#$E,d2
	lsr.b	#4,d1				; Get green only

	move.b	-2(a1),d5			; Get blue
	cmp.b	(a0),d5				; Does blue need to fade?
	beq.s	.NoBlue				; If not, branch
	bcs.s	.DecBlue			; If it needs to be decreased, branch
	addi.w	#$200,d3			; Increase blue
	bra.s	.NoBlue				; Continue

.DecBlue:
	subi.w	#$200,d3			; Decrease blue

.NoBlue:
	move.w	(a0),d5				; Get green
	lsr.b	#4,d5
	cmp.b	d5,d1				; Does green need to fade?
	beq.s	.NoGreen			; If not, branch
	bcs.s	.DecGreen			; If it needs to be decreased, branch
	addi.b	#$20,d3				; Increase green
	bra.s	.NoGreen			; Continue

.DecGreen:
	subi.b	#$20,d3				; Decrease green

.NoGreen:
	move.w	(a0),d5				; Get red
	andi.b	#$E,d5
	cmp.b	d5,d2				; Does red need to fade?
	beq.s	.NoRed				; If not, branch
	bcs.s	.DecRed				; If it needs to be decreased, branch
	addq.b	#2,d3				; Increase red
	bra.s	.NoRed				; Continue

.DecRed:
	subq.b	#2,d3				; Decrease red

.NoRed:
	move.w	d3,(a0)+			; Save new color
	rts

; -------------------------------------------------------------------------
; Draw the sprites from mappings
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- X position
;	d1.w	- Y position
;	d4.w	- Number of sprites to draw
;	d5.w	- Sprite tile properties
;	d6.b	- Render flags
;	a1.l	- Mappings frame data
;	a6.l	- Sprite table buffer
; -------------------------------------------------------------------------

DrawSprite:
	lsr.b	#1,d6				; Is this sprite flipped horizontally?
	bcs.s	.FlipX				; If so, branch
	lsr.b	#1,d6				; Is this sprite flipped vertically?
	bcs.w	.FlipY				; If so, branch

.Loop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	add.w	d1,d2				; Add onto Y position
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,(a6)+			; Store sprite size
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	add.w	d0,d2				; Add onto X position
	move.w	d2,(a6)+			; Store in sprite table
	subq.b	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.Loop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.FlipX:
	lsr.b	#1,d6				; Is this sprite flipped vertically?
	bcs.s	.FlipXY				; If so, branch

.FlipXLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	add.w	d1,d2				; Add onto Y position
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,d6			; Get sprite size
	move.b	d6,(a6)+			; Store in sprite table
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$800,d2			; Flip horizontally
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	neg.w	d2				; Negate it
	move.b	.XFlipOff(pc,d6.w),d6		; Get the X offset to apply
	sub.w	d6,d2				; Subtract the new X offset
	add.w	d0,d2				; Add onto X position
	move.w	d2,(a6)+			; Store in sprite table
	subq.b	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipXLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.XFlipOff:
	dc.b	8, 8, 8, 8
	dc.b	$10, $10, $10, $10
	dc.b	$18, $18, $18, $18
	dc.b	$20, $20, $20, $20

; -------------------------------------------------------------------------

.FlipXY:
.FlipXYLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	neg.w	d2				; Negate it
	move.b	(a1),d6				; Get sprite sizes
	move.b	.YFlipOff(pc,d6.w),d6		; Get the Y offset to apply
	sub.w	d6,d2				; Subtract from the Y offset
	add.w	d1,d2				; Add onto Y position
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,d6			; Get sprite size
	move.b	d6,(a6)+			; Store in sprite table
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$1800,d2			; Flip horizontally and vertically
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	neg.w	d2				; Negate it
	move.b	.XFlipOff(pc,d6.w),d6		; Get the X offset to apply
	sub.w	d6,d2				; Subtract the new X offset
	add.w	d0,d2				; Add onto X position
	move.w	d2,(a6)+			; Store in sprite table
	subq.b	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipXYLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.YFlipOff:
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20

; -------------------------------------------------------------------------

.FlipY:
.FlipYLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	neg.w	d2				; Negate it
	move.b	(a1)+,d6			; Get sprite sizes
	move.b	d6,2(a6)			; Store in sprite table
	move.b	.YFlipOff(pc,d6.w),d6		; Get the Y offset to apply
	sub.w	d6,d2				; Subtract from the Y offset
	add.w	d1,d2				; Add onto Y position
	move.w	d2,(a6)+			; Store in sprite table
	addq.w	#2,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$1000,d2			; Flip vertically
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	add.w	d0,d2				; Add onto X position
	move.w	d2,(a6)+			; Store in sprite table
	subq.b	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipYLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------
; Draw the sprites from mappings (with boundary checks)
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- X position
;	d1.w	- Y position
;	d4.w	- Number of sprites to draw
;	d5.w	- Sprite tile properties
;	d6.b	- Render flags
;	a1.l	- Mappings frame data
;	a6.l	- Sprite table buffer
; -------------------------------------------------------------------------

DrawSpr_BndChk:
	tst.b	r_Spr_Chk_Bnd.w			; Should we check boundaries?
	beq.w	DrawSprite			; If not, branch

	lsr.b	#1,d6				; Is this sprite flipped horizontally?
	bcs.s	.FlipX				; If so, branch
	lsr.b	#1,d6				; Is this sprite flipped vertically?
	bcs.w	.FlipY				; If so, branch

.Loop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	add.w	d1,d2				; Add onto Y position
	cmpi.w	#-32+128,d2			; Is it above the screen?
	ble.s	.Next_YOffScr			; If so, branch
	cmpi.w	#224+128,d2			; Is it below the screen?
	bge.s	.Next_YOffScr			; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,(a6)+			; Store sprite size
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	add.w	d0,d2				; Add onto X position
	cmpi.w	#-32+128,d2			; Is it left of the screen?
	ble.s	.Next_XOffScr			; If so, branch
	cmpi.w	#320+128,d2			; Is it right of the screen?
	bge.s	.Next_XOffScr			; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	subq.b	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.Loop			; Loop if there are still enough sprites left
	rts

.Next_XOffScr:
	subq.w	#6,a6				; Go back to the start of the current sprite entry
	dbf	d4,.Loop			; Loop if there are still enough sprites left
	rts

.Next_YOffScr:
	addq.w	#5,a1				; Go to the next sprite in the mappings in the mappings
	dbf	d4,.Loop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.FlipX:
	lsr.b	#1,d6				; Is this sprite flipped vertically?
	bcs.s	.FlipXY				; If so, branch

.FlipXLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	add.w	d1,d2				; Add onto Y position
	cmpi.w	#-32+128,d2			; Is it above the screen?
	ble.s	.Next_YOffScrFlipX		; If so, branch
	cmpi.w	#224+128,d2			; Is it below the screen?
	bge.s	.Next_YOffScrFlipX		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,d6			; Get sprite size
	move.b	d6,(a6)+			; Store in sprite table
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$800,d2			; Flip horizontally
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	neg.w	d2				; Negate it
	move.b	.XFlips(pc,d6.w),d6		; Get the X offset to apply
	sub.w	d6,d2				; Subtract the new X offset
	add.w	d0,d2				; Add onto X position
	cmpi.w	#-32+128,d2			; Is it left of the screen?
	ble.s	.Next_XOffScrFlipX		; If so, branch
	cmpi.w	#320+128,d2			; Is it right of the screen?
	bge.s	.Next_XOffScrFlipX		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	subq.b	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipXLoop			; Loop if there are still enough sprites left
	rts

.Next_XOffScrFlipX:
	subq.w	#6,a6				; Go back to the start of the current sprite entry
	dbf	d4,.FlipXLoop			; Loop if there are still enough sprites left
	rts

.Next_YOffScrFlipX:
	addq.w	#5,a1				; Go to the next sprite in the mappings
	dbf	d4,.FlipXLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.XFlips:
	dc.b	8, 8, 8, 8
	dc.b	$10, $10, $10, $10
	dc.b	$18, $18, $18, $18
	dc.b	$20, $20, $20, $20

; -------------------------------------------------------------------------

.FlipXY:
.FlipXYLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	neg.w	d2				; Negate it
	move.b	(a1),d6				; Get sprite sizes
	move.b	.YFlips(pc,d6.w),d6		; Get the Y offset to apply
	sub.w	d6,d2				; Subtract from the Y offset
	add.w	d1,d2				; Add onto Y position
	cmpi.w	#-32+128,d2			; Is it above the screen?
	ble.s	.Next_YOffScrFlipXY		; If so, branch
	cmpi.w	#224+128,d2			; Is it below the screen?
	bge.s	.Next_YOffScrFlipXY		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,d6			; Get sprite size
	move.b	d6,(a6)+			; Store in sprite table
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$1800,d2			; Flip horizontally and vertically
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	neg.w	d2				; Negate it
	move.b	.XFlips(pc,d6.w),d6		; Get the X offset to apply
	sub.w	d6,d2				; Subtract the new X offset
	add.w	d0,d2				; Add onto X position
	cmpi.w	#-32+128,d2			; Is it left of the screen?
	ble.s	.Next_XOffScrFlipXY		; If so, branch
	cmpi.w	#320+128,d2			; Is it right of the screen?
	bge.s	.Next_XOffScrFlipXY		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	subq.b	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipXYLoop			; Loop if there are still enough sprites left
	rts

.Next_XOffScrFlipXY:
	subq.w	#6,a6				; Go back to the start of the current sprite entry
	dbf	d4,.FlipXYLoop			; Loop if there are still enough sprites left
	rts

.Next_YOffScrFlipXY:
	addq.w	#5,a1				; Go to the next sprite in the mappings
	dbf	d4,.FlipXYLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.YFlips:
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20

; -------------------------------------------------------------------------

.FlipY:
.FlipYLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	neg.w	d2				; Negate it
	move.b	(a1)+,d6			; Get sprite sizes
	move.b	d6,2(a6)			; Store in sprite table
	move.b	.YFlips(pc,d6.w),d6		; Get the Y offset to apply
	sub.w	d6,d2				; Subtract from the Y offset
	add.w	d1,d2				; Add onto Y position
	cmpi.w	#-32+128,d2			; Is it above the screen?
	ble.s	.Next_YOffScrFlipY		; If so, branch
	cmpi.w	#224+128,d2			; Is it below the screen?
	bge.s	.Next_YOffScrFlipY		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	addq.w	#2,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$1000,d2			; Flip vertically
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	add.w	d0,d2				; Add onto X position
	cmpi.w	#-32+128,d2			; Is it left of the screen?
	ble.s	.Next_XOffScrFlipY		; If so, branch
	cmpi.w	#320+128,d2			; Is it right of the screen?
	bge.s	.Next_XOffScrFlipY		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	subq.b	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipYLoop			; Loop if there are still enough sprites left
	rts

.Next_XOffScrFlipY:
	subq.w	#6,a6				; Go back to the start of the current sprite entry
	dbf	d4,.FlipYLoop			; Loop if there are still enough sprites left
	rts

.Next_YOffScrFlipY:
	addq.w	#5,a1				; Go to the next sprite in the mappings
	dbf	d4,.FlipYLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------
