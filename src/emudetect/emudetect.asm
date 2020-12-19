
; -------------------------------------------------------------------------
;
;	Genesis Emulator Detector
;		By Ralakimus 2019
;
;	Special thanks to BigEvilCorporation for the
;	AtGames detection code
;
;	See LICENSE for licensing details
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Emulator/hardware IDs
; -------------------------------------------------------------------------

EMU_HARDWARE	equ	$00			; Hardware
EMU_GPGX	equ	$01			; Genesis Plus GX
EMU_REGEN	equ	$02			; Regen
EMU_KEGA	equ	$03			; Kega Fusion
EMU_GENS	equ	$04			; Gens
EMU_BLASTEM_OLD	equ	$05			; Old versions of BlastEm
EMU_EXODUS	equ	$06			; Exodus
EMU_MEGASG	equ	$07			; Mega Sg
EMU_STEAM	equ	$08			; Steam
EMU_PICODRIVE	equ	$09			; Picodrive
EMU_FLASHBACK	equ	$0A			; AtGames Flashback
EMU_FIRECORE	equ	$0B			; AtGames Firecore
EMU_GENECYST	equ	$0C			; Genecyst

; -------------------------------------------------------------------------
; Configuration
; -------------------------------------------------------------------------

VDPDEBUG_WRITE		equ	$01		; What gets written into the VDP debug register
DMAHALT_THRESHOLD	equ	$80		; DMA Z80 halt counter threshold

; -------------------------------------------------------------------------
; Emulator/hardware detector
; -------------------------------------------------------------------------
; NOTICE: DESTROYS VRAM AND OVERWRITES Z80 RAM
; -------------------------------------------------------------------------
; RETURNS:
;	d0.b	- Emulator/Hardware ID
; -------------------------------------------------------------------------

DetectEmulator:
	move	#$2700,sr			; Disable interrupts

	; Detect improper IYH emulation (where accessing IYH yields garbage results)
	; This is a known issue in the AtGames Flashback system.

	bsr.w	CheckIYH			; Test IYH
	tst.b	d0				; Was it properly emulated?
	beq.w	.FoundFlashback			; If not, then AtGames Flashback has been detected

	; Detect improper ABCD emulation (where the results are not as they should be)
	; This is a known issue in the AtGames Firecore system.

	lea	FirecoreABCD(pc),a0		; Test ABCD (for AtGames Firecore)
	bsr.w	CheckABCD
	tst.b	d0				; Were the results what the AtGames Firecore would return?
	bne.w	.FoundFirecore			; If so, then AtGames Firecore has been detected

	; Check what reading the VDP debug register does (check ReadVDPDebugReg's notes for more details)
	; This is a known issue in Kega Fusion, Steam, old versions of BlastEm, Gens, Picodrive, Genecyst,
	; and Mega Sg. Here, we only check the nonzero results, since multiple emulators will return 0, and thus
	; need further checks to be done first.

	bsr.w	ReadVDPDebugReg			; Read VDP debug register
	move.w	d0,d7				; Save for later checks
	cmpi.w	#$FFFF,d0			; Did it return -1?
	beq.w	.FoundKega			; If so, then Kega Fusion has been detected
	cmpi.w	#$4E71,d0			; Did it return the NOP opcode?
	beq.w	.FoundSteam			; If so, then Steam has been detected
	cmpi.w	#VDPDEBUG_WRITE,d0		; Did it return what it was last written?
	beq.w	.FoundOldBlastEm		; If so, then an old version of BlastEm has been detected

	; Detect DMA interruption (check CheckDMAZ80Halt's notes for more details)
	; Only BlastEm, Exodus, Mega Sg, and hardware will make the Z80 halt during the DMA.
	; BlastEm should have already been detected through the VDP debug register, so
	; we only need to worry about the other 3 now.

	bsr.w	CheckDMAZ80Halt			; Test DMA Z80 halt
	tst.b	d0				; Did the Z80 get halted?
	beq.s	.NoDMAHalt			; If not, then the platform is not hardware, Exodus, or Mega Sg

	; Check the VDP debug register read value again here, with the DMA Z80 halt being
	; detected now. Out of BlastEm, Exodus, Mega Sg, and hardware, only Mega Sg
	; will have it be 0.

	tst.w	d7				; Did the VDP debug register return 0?
	beq.w	.FoundMegaSg			; If so, then Mega Sg has been detected

	; Check if the Z80 can read the hardware version register. Exodus will
	; not allow that to happen, but on hardware, it's allowed.

	bsr.w	CheckZ80HWVerRead		; Test Z80 hardware version register read
	tst.b	d0				; Was the Z80 able to read the register?
	beq.w	.FoundExodus			; If not, Exodus has been detected

; -------------------------------------------------------------------------
; Found hardware
; -------------------------------------------------------------------------

	moveq	#EMU_HARDWARE,d0
	rts

; -------------------------------------------------------------------------

.NoDMAHalt:
	; What's left to sift out now is Regen, Gens, Genecyst, Picodrive, and Genesis Plus GX.
	; First thing we are going to do is check the VDP debug register again. Gens,
	; Genecyst, and Picodrive will have it return 0, while Regen and Genesis Plus GX will
	; have it return the 68000 instrucion prefetch.

	tst.w	d7				; Did the VDP debug register return 0?
	beq.w	.TestOddAddr			; If so, branch

	; All there's left now is Regen and Genesis Plus GX. Here, we will check Timer B in
	; the YM2612. Regen's emulation of that is very much broken.
	; (Also check CheckYMTimerB's notes for more details)

	bsr.w	CheckYMTimerB			; Test YM2612 Timer B
	tst.b	d0				; Was it emulated correctly?
	beq.s	.FoundRegen			; If not, Regen has been detected

; -------------------------------------------------------------------------
; Found Genesis Plus GX
; -------------------------------------------------------------------------

.FoundGPGX:
	moveq	#EMU_GPGX,d0
	rts

; -------------------------------------------------------------------------
; Check for Gens, Genecyst, or Picodrive
; -------------------------------------------------------------------------

.TestOddAddr:
	; The remaining 3 platforms to check are Gens, Genecyst, and Picodrive.
	; None of those 3 emulate odd addressing properly, and each of them
	; behave differently when handling it. We do not worry about dealing
	; with address errors here, then.

	; See CheckOddAddressing's notes for more details

	bsr.w	CheckOddAddressing		; Test odd addressing
	cmpi.b	#1,d0				; Was it Genecyst behavior?
	beq.s	.FoundGenecyst			; If so, Genecyst has been detected
	cmpi.b	#2,d0				; Was it Picodrive behavior?
	beq.s	.FoundPico			; If so, Picodrive has been detected

; -------------------------------------------------------------------------
; Found Gens
; -------------------------------------------------------------------------

.FoundGens:
	moveq	#EMU_GENS,d0
	rts

; -------------------------------------------------------------------------
; Found Picodrive
; -------------------------------------------------------------------------

.FoundPico:
	moveq	#EMU_PICODRIVE,d0
	rts

; -------------------------------------------------------------------------
; Found Genecyst
; -------------------------------------------------------------------------

.FoundGenecyst:
	moveq	#EMU_GENECYST,d0
	rts

; -------------------------------------------------------------------------
; Found AtGames Flashback
; -------------------------------------------------------------------------

.FoundFlashback:
	moveq	#EMU_FLASHBACK,d0
	rts

; -------------------------------------------------------------------------
; Found AtGames Firecore
; -------------------------------------------------------------------------

.FoundFirecore:
	moveq	#EMU_FIRECORE,d0
	rts

; -------------------------------------------------------------------------
; Found Regen
; -------------------------------------------------------------------------

.FoundRegen:
	moveq	#EMU_REGEN,d0
	rts

; -------------------------------------------------------------------------
; Found Kega Fusion
; -------------------------------------------------------------------------

.FoundKega:
	moveq	#EMU_KEGA,d0
	rts

; -------------------------------------------------------------------------
; Found an old version of BlastEm
; -------------------------------------------------------------------------

.FoundOldBlastEm:
	moveq	#EMU_BLASTEM_OLD,d0
	rts

; -------------------------------------------------------------------------
; Found Exodus
; -------------------------------------------------------------------------

.FoundExodus:
	moveq	#EMU_EXODUS,d0
	rts

; -------------------------------------------------------------------------
; Found Mega Sg
; -------------------------------------------------------------------------

.FoundMegaSg:
	moveq	#EMU_MEGASG,d0
	rts

; -------------------------------------------------------------------------
; Found Steam
; -------------------------------------------------------------------------

.FoundSteam:
	moveq	#EMU_STEAM,d0
	rts

; -------------------------------------------------------------------------
; Read VDP debug register
; -------------------------------------------------------------------------
; Reading the VDP debug register yields different results for different
; hardware, clones, and emulators.
; -------------------------------------------------------------------------
; Kega will return -1, some will return 0, some will return the
; 68000 instruction prefetch (like hardware does), Steam will
; return the NOP opcode, and old versions of BlastEm will return the last
; thing written to it.
; -------------------------------------------------------------------------
; RETURNS:
;	d0.w	- Value read from the VDP debug register
; -------------------------------------------------------------------------

ReadVDPDebugReg:
	move.w	#VDPDEBUG_WRITE,$C0001C		; Write to the VDP debug register (for BlastEm detection)
	move.w	$C0001C,d0			; Read VDP debug register
	move.w	#0,$C0001C			; Reset VDP debug register
	rts

; -------------------------------------------------------------------------
; Check odd addressing.
; -------------------------------------------------------------------------
; NOTICE: WILL CAUSE AN ADDRESS ERROR ON MORE ACCURATE EMULATORS
; -------------------------------------------------------------------------
; Different emulators react to odd addressing differently. For instance,
; Genecyst and Kega Fusion will just go on with it like it's nothing,
; Picodrive will mask out the address' bit 0 before it does anyhing, and
; Gens will kinda just flip out.
; -------------------------------------------------------------------------
; RETURNS:
;	d0.b	- 0 = Address error (Not implemented yet!)
;		  1 = Genecyst/Kega Fusion behavior
;		  2 = Picodrive behavior
;                 3 = Gens behavior
; -------------------------------------------------------------------------

CheckOddAddressing:
	lea	$FF0000,a0			; Save set of RAM to be restored
	move.l	(a0),d1
	move.l	4(a0),d2

	move.w	#$6968,3(a0)			; Write to an odd address
	move.l	2(a0),d0			; Read the result of that write
	swap	d0

	move.l	d1,(a0)				; Restore previous set of RAM
	move.l	d2,4(a0)

	cmpi.w	#$6968,d0			; Was the address masked?
	beq.s	.Pico				; If so, then it's Picodrive behavior
	cmpi.l	#$68000069,d0			; Was the value just written there?
	beq.s	.GenecystKega			; If so, then it's Genecyst/Kega Fusion behavior

	moveq	#3,d0				; Gens behavior
	rts

.Pico:
	moveq	#2,d0				; Picodrive behavior
	rts

.GenecystKega:
	moveq	#1,d0				; Genecyst/Kega Fusion behavior
	rts

; -------------------------------------------------------------------------
; Check DMA Z80 halt
; -------------------------------------------------------------------------
; NOTICE: DESTROYS VRAM AND OVERWRITES Z80 RAM
; -------------------------------------------------------------------------
; On hardware, while there is a DMA from 68000 memory, the Z80 should
; halt if it also tries to access 68000 memory at the same time, at least
; until it is finished with the DMA. Most emulators do not emulate this.
; -------------------------------------------------------------------------
; To detect it, there's a Z80 program that constantly reads from 68000
; memory while also incrementing a counter, and then do 2 very large
; DMA transfers from 68000. If the Z80 is halted during the DMAs, then the
; counter should not go very high at all, otherwise, it should skyrocket.
; -------------------------------------------------------------------------
; RETURNS:
;	d0.b	- 0 = DMA did not make the Z80 halt
;		  1 = DMA did make the Z80 halt
; -------------------------------------------------------------------------

CheckDMAZ80Halt:
	lea	Z80DMAHaltTest(pc),a0		; Load the Z80 program
	move.w	#Z80DMAHaltTest_End-Z80DMAHaltTest-1,d0
	bsr.w	EmuDet_LoadZ80

	lea	$C00004,a1			; Prepare the first DMA
	move.l	#$81148F02,(a1)
	move.l	#$947F93FF,(a1)
	move.l	#$96009500,(a1)
	move.w	#$9700,(a1)
	move.w	#$4000,(a1)
	move.w	#$0080,-(sp)

	move.w	#$100,$A11100			; Make the Z80 go into the counter loop

.StopZ80:
	btst	#0,$A11100
	bne.s	.StopZ80
	move.b	#$00,Z80DMAHaltTest_Loop1
	move.b	#$00,Z80DMAHaltTest_Loop1+1
	move.b	#$00,Z80DMAHaltTest_Loop1+2
	move.w	#0,$A11100

	move.w	(sp)+,(a1)			; Start the first DMA

	move.l	#$947F93FF,(a1)			; Start the second DMA
	move.l	#$96009500,(a1)
	move.w	#$9700,(a1)
	move.w	#$4000,(a1)
	move.w	#$0080,-(sp)
	move.w	(sp)+,(a1)

	move.w	#$100,$A11100			; Make the Z80 finish up

.StopZ80_2:
	btst	#0,$A11100
	bne.s	.StopZ80_2
	move.b	#$00,Z80DMAHaltTest_Loop2Ins
	move.b	#$00,Z80DMAHaltTest_Loop2Ins+1
	move.b	#$00,Z80DMAHaltTest_Loop2Ins+2
	move.w	#0,$A11100

	moveq	#$7F,d0				; Ensure that the Z80 transfers the counter to RAM first
	dbf	d0,*

	move.w	#$100,$A11100			; Get the Z80 counter

.StopZ80_3:
	btst	#0,$A11100
	bne.s	.StopZ80_3
	moveq	#0,d0
	move.b	Z80DMAHaltTest_Cnt+1,d0
	lsl.w	#8,d0
	move.b	Z80DMAHaltTest_Cnt,d0
	move.w	#0,$A11100

	move.w	#$8F01,(a1)			; Clear VRAM
	move.l	#$94FF93FF,(a1)
	move.l	#$96009500,(a1)
	move.w	#$9780,(a1)
	move.w	#$4000,(a1)
	move.w	#$0080,-(sp)
	move.w	(sp)+,(a1)
	move.w	#0,-4(a1)

.WaitVRAMClr:
	move.w	(a1),d1
	btst	#1,d1
	bne.s	.WaitVRAMClr
	move.w	#$8F02,(a1)

	cmpi.w	#DMAHALT_THRESHOLD,d0		; Check if the Z80 DMA test counter was a low value
	bcc.s	.NoDMAHalt			; If not, then the Z80 did not halt
	moveq	#1,d0
	rts

.NoDMAHalt:
	moveq	#0,d0
	rts

; -------------------------------------------------------------------------
; Z80 side of the DMA interrupt detector
; -------------------------------------------------------------------------

Z80DMAHaltTest:
	dc.b	$F3		; di		; Disable interrupts

	; Z80DMAHaltTest_Loop1:			; Wait for the 68000 to start with
	dc.b	$C3,$01,$00	; jp Z80DMAHaltTest_Loop1
						; the DMA thing

	dc.b	$21,$00,$00	; ld hl,$0000	; Initialize the counter	

	; Z80DMAHaltTest_Loop2:
	dc.b	$3A,$00,$80	; ld a,($8000)	; Attempt a read from 68000 memory
	dc.b	$23		; inc hl	; Increment the counter

	; Z80DMAHaltTest_Loop2Ins:		; Loop
	dc.b	$C3,$07,$00	; jp Z80DMAHaltTest_Loop2

	dc.b	$7D		; ld a,l	; Transfer the counter over to RAM
	dc.b	$32,$19,$00	; ld (Z80DMAHaltTest_Cnt),a
	dc.b	$7C		; ld a,h
	dc.b	$32,$1A,$00	; ld (Z80DMAHaltTest_Cnt+1),a

	; Z80DMAHaltTest_Loop3:			; Loop here forever
	dc.b	$C3,$16,$00	; jp Z80DMAHaltTest_Loop3

	; Z80DMAHaltTest_Cnt:
	dc.b	$00,$00		; dw $0000	; Finalized Z80 counter
Z80DMAHaltTest_End:
	even

Z80DMAHaltTest_Loop1		equ	$A00001
Z80DMAHaltTest_Loop2		equ	$A00007
Z80DMAHaltTest_Loop2Ins		equ	$A0000B
Z80DMAHaltTest_Cnt		equ	$A00019

; -------------------------------------------------------------------------
; Check if the Z80 can read the hardware version
; -------------------------------------------------------------------------
; NOTICE: OVERWRITES Z80 RAM
; -------------------------------------------------------------------------
; It should allow the Z80 to do that if emulated correctly.
; -------------------------------------------------------------------------
; RETURNS:
;	d0.b	- 0 = Z80 did not read it
;		  1 = Z80 did read it
; -------------------------------------------------------------------------

CheckZ80HWVerRead:
	lea	Z80HWVerTest(pc),a0		; Load the Z80 program
	move.w	#Z80HWVerTest_End-Z80HWVerTest-1,d0
	bsr.w	EmuDet_LoadZ80

	moveq	#$7F,d0				; Wait for the program to finish
	dbf	d0,*

	move.w	#$100,$A11100			; Get Z80 read value

.StopZ80:
	btst	#0,$A11100
	bne.s	.StopZ80
	move.b	Z80HWVerTest_Val,d0
	move.w	#0,$A11100

	move.b	$A10001,d1			; Was the value read the actual hardware version?
	cmp.b	d0,d1
	bne.s	.NoRead
	moveq	#1,d0
	rts

.NoRead:
	moveq	#0,d0
	rts

; -------------------------------------------------------------------------
; Z80 side of the hardware version register test
; -------------------------------------------------------------------------

Z80HWVerTest:
	dc.b	$F3		; di		; Disable interrupts

	dc.b	$11,$00,$60	; ld de, $6000	; Set bank
	dc.b	$AF		; xor a
	dc.b	$12		; ld (de),a
	dc.b	$3E,$01		; ld a,1
	dc.b	$12		; ld (de),a
	dc.b	$AF		; xor a
	dc.b	$12		; ld (de),a
	dc.b	$12		; ld (de),a
	dc.b	$12		; ld (de),a
	dc.b	$12		; ld (de),a
	dc.b	$3E,$01		; ld a,1
	dc.b	$12		; ld (de),a
	dc.b	$AF		; xor a
	dc.b	$12		; ld (de),a
	dc.b	$3E,$01		; ld a,1
	dc.b	$12		; ld (de),a

	dc.b	$3A,$01,$80	; ld a,($8001)	; Get hardware version
	dc.b	$32,$1F,$00	; ld (Z80HWVerTest_Val),a

	; EmuDetZ80HWVer_Loop:			; Loop here forever
	dc.b	$C3,$1C,$00	; jp EmuDetZ80HWVer_Loop

	; Z80HWVerTest_Val:
EmuDetZ80HWVer_Val:
	dc.b	$00		; db $00
Z80HWVerTest_End:
	even

Z80HWVerTest_Val		equ	$A0001F

; -------------------------------------------------------------------------
; Check if YM2612 Timer B is properly emulated
; -------------------------------------------------------------------------
; On Regen, the YM timers do not function accurately. In this case here,
; It should take YM Timer B quite a bit of time before it overflows, but
; on Regen, it does it rather... quickly.
; -------------------------------------------------------------------------
; A test was done beforehand to compare Timer B to Timer A. Timer B was set to
; $F0, and Timer A was set to $3F0. On Regen, Timer B overflowed first. On
; Genesis Plus GX, Timer A overflowed first, and even then, it took quite a
; bit of time to overflow on that.
; -------------------------------------------------------------------------
; RETURNS:
;	d0.b	- 0 = Timer B emulation is broken
;		  1 = Timer B emulation is okay
; -------------------------------------------------------------------------

CheckYMTimerB:
	move.w	#$100,$A11100			; Stop the Z80

.StopZ80:
	btst	#0,$A11100
	bne.s	.StopZ80

	move.b	#$26,$A04000			; Set Timer B to $F0
	move.b	#$F0,$A04001

.WaitYM:
	tst.b	$A04000				; Wait for the YM2612 to not be busy
	bmi.s	.WaitYM
	
	move.b	#$27,$A04000			; Enable Timer B
	move.b	#%00101010,$A04001

	moveq	#$1F,d0				; Delay for a bit
	dbf	d0,*

	move.b	$A04000,d0			; Get YM status
	move.w	#0,$A11100			; Start the Z80
	
	andi.b	#2,d0				; Set the Timer B overflow flag as the return value
	bchg	#1,d0
	lsr.b	#1,d0
	rts

; -------------------------------------------------------------------------
; Check if IYH is properly emulated
; -------------------------------------------------------------------------
; NOTICE: OVERWRITES Z80 RAM
; -------------------------------------------------------------------------
; AtGames Flashback doesn't emulate accessing the IYH register
; correctly. Here, we have the Z80 access that register and then
; check the results.
; -------------------------------------------------------------------------
; RETURNS:
;	d0.b	- 0 = IYH emulation is broken
;		  1 = IYH emulation is okay
; -------------------------------------------------------------------------

CheckIYH:
	lea	Z80IYHTest(pc),a0		; Load the Z80 program
	move.w	#Z80IYHTest_End-Z80IYHTest-1,d0
	bsr.w	EmuDet_LoadZ80

	moveq	#$7F,d0				; Wait for the program to finish
	dbf	d0,*

	move.w	#$100,$A11100			; Get IYH value

.StopZ80:
	btst	#0,$A11100
	bne.s	.StopZ80
	move.b	Z80IYHTest_Val,d0
	move.w	#0,$A11100

	cmpi.b	#$40,d0				; Was the IYH register access emulated properly?
	bne.s	.BadIYH				; If not, branch
	moveq	#1,d0				; IYH emulation is okay
	rts

.BadIYH:
	moveq	#0,d0				; IYH emulation is broken
	rts

; -------------------------------------------------------------------------
; Z80 side of the IYH test for AtGames Flashback
; (Credit to Matt Philips/BigEvilCorporation for this detection code!)
; -------------------------------------------------------------------------

Z80IYHTest:
	dc.b	$F3		; di		; Disable interrupts

	dc.b	$FD,$21,$34,$12	; ld iy,$1234	; Set IY to whatever
	dc.b	$FD,$26,$40	; ld iyh,$40	; Set IYH
	dc.b	$FD,$7C		; ld a,iyh	; Read IYH
	dc.b	$32,$10,$00	; ld (Z80IYHTest_Val),a

	; Z80IYHTest_Loop:			; Loop here forever
	dc.b	$C3,$0D,$00	; jp Z80IYHTest_Loop

	; Z80IYHTest_Val:			; IYH read value
	dc.b	$00		; db $00
Z80IYHTest_End:
	even

Z80IYHTest_Val	equ	$A00010

; -------------------------------------------------------------------------
; Check ABCD
; -------------------------------------------------------------------------
; Runs ABCD multiple times and checks the results. Sometimes, ABCD
; isn't emulated properly on some platforms (such as the AtGames
; Firecore).
; -------------------------------------------------------------------------
; (Special thanks to Matt Philips (aka BigEvilCorporation) for this)
; -------------------------------------------------------------------------
; PARAMETERS:
;	a0.l	- Expected ABCD results table
; -------------------------------------------------------------------------
; RETURNS:
;	d0.b	- 0 = Results don't match
;		  1 = Results matched
; -------------------------------------------------------------------------

CheckABCD:
	moveq	#0,d0				; Clear result and sr
	moveq	#0,d3

	lea	ABCDTestTbl(pc),a1		; Get test table ready
	moveq	#(ABCDTestTbl_End-ABCDTestTbl)/3-1,d7

.TestLp:
	move.b	(a1)+,d1			; Source operand
	move.b	(a1)+,d2			; Destination operand
	move.b	(a1)+,d3			; CCR

	move	d3,ccr				; Do ABCD
	abcd.b	d1,d2
	move	sr,d6				; Get SR

	move.b	(a0)+,d4			; Get expected result
	move.b	(a0)+,d5			; Get expected SR

	cmp.b	d2,d4				; Are the results the same?
	bne.s	.ABCDFail			; If not, branch
	cmp.b	d6,d5
	bne.s	.ABCDFail			; If not, branch

	dbf	d7,.TestLp			; Next set

	moveq	#1,d0				; Success

.ABCDFail:
	rts

; -------------------------------------------------------------------------
; ABCD test table
; -------------------------------------------------------------------------

ABCDTestTbl:
	;	D0,  D1,  CCR
	dc.b	$4A, $4A, $00
	dc.b	$4A, $4A, $1F
	dc.b	$4E, $4E, $00
	dc.b	$4E, $4E, $1F
	dc.b	$FF, $FF, $00
	dc.b	$FF, $FF, $1F
ABCDTestTbl_End:

; -------------------------------------------------------------------------
; Expected ABCD results from an AtGames Firecore system
; -------------------------------------------------------------------------

FirecoreABCD:
	;	Result, SR
	dc.b	$94,    $08
	dc.b	$95,    $0A
	dc.b	$9C,    $08
	dc.b	$9D,    $0A
	dc.b	$FE,    $08
	dc.b	$FF,    $0A

; -------------------------------------------------------------------------
; Load a Z80 program
; -------------------------------------------------------------------------
; PARAMETERS:
;	a0.l	- Z80 program
;	d0.w	- Z80 program size minus 1
; -------------------------------------------------------------------------

EmuDet_LoadZ80:
	move.w	#$100,$A11100			; Request Z80 bus
	move.w	#$100,$A11200			; Z80 reset off

.Z80Bus:					; Wait for Z80 bus
	btst	#0,$A11100
	bne.s	.Z80Bus

	lea	$A00000,a1			; Load the Z80 program

.Load:
	move.b	(a0)+,(a1)+
	dbf	d0,.Load

	move.w	#0,$A11200			; Z80 reset on
	moveq	#$7F,d0				; Wait for Z80 reset
	dbf	d0,*

	move.w	#0,$A11100			; Z80 start
	move.w	#$100,$A11200			; Z80 reset off
	rts

; -------------------------------------------------------------------------