
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		macro.asm
;	Contents:	Macro definitions
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Align
; -------------------------------------------------------------------------
; PARAMETERS:
;	bound	- Size boundary
; -------------------------------------------------------------------------

align macros &
	bound
	
	cnop	0,\bound			; Align data

; -------------------------------------------------------------------------
; Align (custom byte fill)
; -------------------------------------------------------------------------
; PARAMETERS:
;	bound	- Size boundary
; -------------------------------------------------------------------------

align2 macro &
	bound, value
	
	if ((*)%(\bound))>0
		dcb.b	\bound-((*)%(\bound)),\value
	endif

	endm

; -------------------------------------------------------------------------
; Push all registers to the stack
; -------------------------------------------------------------------------

pusha macros

	movem.l	d0-a6,-(sp)			; Push registers

; -------------------------------------------------------------------------
; Pop all registers from the stack
; -------------------------------------------------------------------------

popa macros

	movem.l	(sp)+,d0-a6			; Pop registers

; -------------------------------------------------------------------------
; Pad RS to even address
; -------------------------------------------------------------------------

rsEven macros
	
	rs.b	__rs&1				; Align RS

; -------------------------------------------------------------------------
; Clear a section of memory
; -------------------------------------------------------------------------
; PARAMETERS:
;	saddr	- Address to start clearing memory at
;	eaddr	- Address to finish clearing memory at
;		  (not required if [saddr]_end exists)
; -------------------------------------------------------------------------

clrRAM macro &
	saddr, eaddr
	
	local	endaddr
	if narg<2
endaddr		EQUS	"\saddr\_end"		; Use [saddr]_end
	else
endaddr		EQUS	"\eaddr"		; Use eaddr
	endif
clrsize		=	(\endaddr-\saddr)&$FFFFFF

	moveq	#0,d0

	if (((\saddr)&$8000)&((\saddr)<0))=0	; Optimize setting saddr to a1
		lea	\saddr,a1
	else
		lea	(\saddr).w,a1
	endif

	move.w	#clrsize>>2-1,d1

.Clear\@:
	move.l	d0,(a1)+			; Clear data
	dbf	d1,.Clear\@			; Loop until data is cleared

	if clrsize&2
		move.w	d0,(a1)+		; Clear remaining word of data
	endif
	if clrsize&1
		move.b	d0,(a1)+		; Clear remaining byte of data
	endif

	endm

; -------------------------------------------------------------------------
; Disable SRAM access
; -------------------------------------------------------------------------

sramOff macros

	move.b	#0,SRAM_ENABLE			; Disable SRAM access

; -------------------------------------------------------------------------
; Enable SRAM access
; -------------------------------------------------------------------------

sramOn macros

	move.b	#1,SRAM_ENABLE			; Enable SRAM access

; -------------------------------------------------------------------------
; Request Z80 bus access
; -------------------------------------------------------------------------

z80Bus macros

	move.w	#$100,Z80_BUS			; Request Z80 bus access

; -------------------------------------------------------------------------
; Wait for Z80 bus request acknowledgement
; -------------------------------------------------------------------------

z80Ack macro
	
	btst	#0,Z80_BUS			; Was the request acknowledged?
	bne.s	*-8				; If not, wait

	endm

; -------------------------------------------------------------------------
; Request Z80 bus access
; -------------------------------------------------------------------------

z80Stop macro

	z80Bus					; Request Z80 bus access
	z80Ack					; Wait for acknowledgement

	endm

; -------------------------------------------------------------------------
; Release the Z80 bus
; -------------------------------------------------------------------------

z80Start macros

	move.w	#0,Z80_BUS			; Release the bus

; -------------------------------------------------------------------------
; Cancel Z80 reset
; -------------------------------------------------------------------------

z80ResOff macros

	move.w	#$100,Z80_RESET			; Cancel Z80 reset

; -------------------------------------------------------------------------
; Request Z80 reset
; -------------------------------------------------------------------------

z80Reset macros

	move.w	#0,Z80_RESET			; Request Z80 reset

; -------------------------------------------------------------------------
; Wait for DMA to finish
; -------------------------------------------------------------------------
; PARAMETERS:
;	ctrl	- VDP control port as an address register
;		  (If left blank, this just uses VDP_CTRL instead)
; -------------------------------------------------------------------------

waitDMA macro &
	ctrl

.Wait\@:
	if narg>0
		move.w	(\ctrl),-(sp)	; Get VDP status
	else
		move.w	VDP_CTRL,-(sp)	; Get VDP status
	endif
	andi.w	#2,(sp)+		; Is DMA active?
	bne.s	.Wait\@			; If so, wait

	endm

; -------------------------------------------------------------------------
; VDP command instruction
; -------------------------------------------------------------------------
; PARAMETERS:
;	addr	- Address in VDP memory
;	type	- Type of VDP memory
;	rwd	- VDP command
; -------------------------------------------------------------------------

VVRAM		EQU	%100001			; VRAM
VCRAM		EQU	%101011			; CRAM
VVSRAM		EQU	%100101			; VSRAM
VREAD		EQU	%001100			; VDP read
VWRITE		EQU	%000111			; VDP write
VDMA		EQU	%100111			; VDP DMA

; -------------------------------------------------------------------------

vdpCmd macro &
	ins, addr, type, rwd, end, end2
	
	if narg=5
		\ins	#((((V\type&V\rwd)&3)<<30)|((\addr&$3FFF)<<16)|(((V\type&V\rwd)&$FC)<<2)|((\addr&$C000)>>14)), \end
	elseif narg>=6
		\ins	#((((V\type&V\rwd)&3)<<30)|((\addr&$3FFF)<<16)|(((V\type&V\rwd)&$FC)<<2)|((\addr&$C000)>>14))\end, \end2
	else
		\ins	((((V\type&V\rwd)&3)<<30)|((\addr&$3FFF)<<16)|(((V\type&V\rwd)&$FC)<<2)|((\addr&$C000)>>14))
	endif

	endm


; -------------------------------------------------------------------------
; VDP DMA from 68000 memory to VDP memory
; -------------------------------------------------------------------------
; PARAMETERS:
;	src	- Source address in 68000 memory
;	dest	- Destination address in VDP memory
;	len	- Length of data in bytes
;	type	- Type of VDP memory
;	ctrl	- VDP control port as an address register
;		  (If left blank, this just uses VDP_CTRL instead)
; -------------------------------------------------------------------------

dma68k macro &
	src, dest, len, type, ctrl

	if narg>4
		move.l	#$94009300|((((\len)/2)&$FF00)<<8)|(((\len)/2)&$FF),(\ctrl)
		move.l	#$96009500|((((\src)/2)&$FF00)<<8)|(((\src)/2)&$FF),(\ctrl)
		move.w	#$9700|(((\src)>>17)&$7F),(\ctrl)
		vdpCmd	move.w,\dest,\type,DMA,>>16,(\ctrl)
		vdpCmd	move.w,\dest,\type,DMA,&$FFFF,-(sp)
		move.w	(sp)+,(\ctrl)
	else
		move.l	#$94009300|((((\len)/2)&$FF00)<<8)|(((\len)/2)&$FF),VDP_CTRL
		move.l	#$96009500|((((\src)/2)&$FF00)<<8)|(((\src)/2)&$FF),VDP_CTRL
		move.w	#$9700|(((\src)>>17)&$7F),VDP_CTRL
		vdpCmd	move.w,\dest,\type,DMA,>>16,VDP_CTRL
		vdpCmd	move.w,\dest,\type,DMA,&$FFFF,-(sp)
		move.w	(sp)+,VDP_CTRL
	endif

	endm

; -------------------------------------------------------------------------
; Fill VRAM with byte
; Auto-increment should be set to 1 beforehand
; -------------------------------------------------------------------------
; PARAMETERS:
;	byte	- Byte to fill VRAM with
;	addr	- Address in VRAM
;	len	- Length of fill in bytes
;	ctrl	- VDP control port as an address register
;		  (If left blank, this just uses VDP_CTRL instead)
; -------------------------------------------------------------------------

dmaFill macro &
	byte, addr, len, ctrl

	if narg>3
		move.l	#$94009300|((((\len)-1)&$FF00)<<8)|(((\len)-1)&$FF),(\ctrl)
		move.w	#$9780,(\ctrl)
		move.l	#$40000080|(((\addr)&$3FFF)<<16)|(((\addr)&$C000)>>14),(\ctrl)
		move.w	#(\byte)<<8,-4(\ctrl)
		waitDMA	\ctrl
	else
		move.l	#$94009300|((((\len)-1)&$FF00)<<8)|(((\len)-1)&$FF),VDP_CTRL
		move.w	#$9780,VDP_CTRL
		move.l	#$40000080|(((\addr)&$3FFF)<<16)|(((\addr)&$C000)>>14),VDP_CTRL
		move.w	#(\byte)<<8,VDP_DATA
		waitDMA
	endif

	endm

; -------------------------------------------------------------------------
; Copy a region of VRAM to a location in VRAM
; Auto-increment should be set to 1 beforehand
; -------------------------------------------------------------------------
; PARAMETERS:
;	src	- Source address in VRAM
;	dest	- Destination address in VRAM
;	len	- Length of copy in bytes
;	ctrl	- VDP control port as an address register
;		  (If left blank, this just uses the address instead)
; -------------------------------------------------------------------------

dmaCopy macro &
	src, dest, len, ctrl
	
	if narg>3
		move.l	#$94009300|((((\len)-1)&$FF00)<<8)|(((\len)-1)&$FF),(\ctrl)
		move.l	#$96009500|(((\src)&$FF00)<<8)|((\src)&$FF),(\ctrl)
		move.w	#$97C0,(\ctrl)
		move.l	#$000000C0|(((\dest)&$3FFF)<<16)|(((\dest)&$C000)>>14),(\ctrl)
		waitDMA	\ctrl
	else
		move.l	#$94009300|((((\len)-1)&$FF00)<<8)|(((\len)-1)&$FF),VDP_CTRL
		move.l	#$96009500|(((\src)&$FF00)<<8)|((\src)&$FF),VDP_CTRL
		move.w	#$97C0,VDP_CTRL
		move.l	#$000000C0|(((\dest)&$3FFF)<<16)|(((\dest)&$C000)>>14),VDP_CTRL
		waitDMA
	endif

	endm

; -------------------------------------------------------------------------
