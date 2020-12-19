
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		const.asm
;	Contents:	Constant definitions
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; ROM
; -------------------------------------------------------------------------

ROM_START	EQU	$000000			; ROM start
ROM_END		EQU	$400000			; ROM end

; -------------------------------------------------------------------------
; Sound
; -------------------------------------------------------------------------

YM_ADDR_0	EQU	$A04000			; YM address port 0
YM_DATA_0	EQU	$A04001			; YM data port 0
YM_ADDR_1	EQU	$A04002			; YM address port 1
YM_DATA_1	EQU	$A04002			; YM data port 1
PSG_CTRL	EQU	$C00011			; PSG control port

; -------------------------------------------------------------------------
; I/O
; -------------------------------------------------------------------------

HW_VERSION	EQU	$A10001			; Hardware version
IO_A_DATA	EQU	$A10003			; I/O port A data port
IO_B_DATA	EQU	$A10005			; I/O port B data port
IO_C_DATA	EQU	$A10007			; I/O port C data port
IO_A_CTRL	EQU	$A10009			; I/O port A control port
IO_B_CTRL	EQU	$A1000B			; I/O port B control port
IO_C_CTRL	EQU	$A1000D			; I/O port C control port
CART_MODE	EQU	$A11000			; Cart mode (D-RAM/ROM)
SRAM_ENABLE	EQU	$A130F1			; SRAM enable port
TMSS_SEGA	EQU	$A14000			; TMSS "SEGA" register
TMSS_MODE	EQU	$A14100			; TMSS bus mode

; -------------------------------------------------------------------------
; Z80
; -------------------------------------------------------------------------

Z80_RAM		EQU	$A00000			; Z80 RAM start
Z80_END		EQU	$A02000			; Z80 RAM end
Z80_BUS		EQU	$A11100			; Z80 bus request
Z80_RESET	EQU	$A11200			; Z80 reset

; -------------------------------------------------------------------------
; VDP
; -------------------------------------------------------------------------

VDP_DATA	EQU	$C00000			; VDP data port
VDP_CTRL	EQU	$C00004			; VDP control port
VDP_HV		EQU	$C00008			; VDP H/V counter
VDP_DEBUG	EQU	$C0001C			; VDP debug

; -------------------------------------------------------------------------
; RAM
; -------------------------------------------------------------------------

RAM_START	EQU	$FF0000			; RAM start
RAM_END		EQU	$1000000		; RAM end

; -------------------------------------------------------------------------
; Controller I/O
; -------------------------------------------------------------------------

	rsreset
JbU		rs.b	1			; Bit up
JbD		rs.b	1			; Bit down
JbL		rs.b	1			; Bit left
JbR		rs.b	1			; Bit right
JbB		rs.b	1			; Bit B
JbC		rs.b	1			; Bit C
JbA		rs.b	1			; Bit A
JbS		rs.b	1			; Bit start
JbZ		rs.b	1			; Bit Z
JbY		rs.b	1			; Bit Y
JbX		rs.b	1			; Bit X
JbM		rs.b	1			; Bit mode

J_U		EQU	(1<<JbU)		; Up
J_D		EQU	(1<<JbD)		; Down
J_L		EQU	(1<<JbL)		; Left
J_R		EQU	(1<<JbR)		; Right
J_B		EQU	(1<<JbB)		; B
J_C		EQU	(1<<JbC)		; C
J_A		EQU	(1<<JbA)		; A
J_S		EQU	(1<<JbS)		; Start
J_Z		EQU	(1<<JbZ)		; Z
J_Y		EQU	(1<<JbY)		; Y
J_X		EQU	(1<<JbX)		; X
J_M		EQU	(1<<JbM)		; Mode

IObTH		EQU	6			; TH pin bit
IObTR		EQU	5			; TR pin bit
IObTL		EQU	4			; TL pin bit
IO_TH		EQU	1<<IObTH		; TH pin
IO_TR		EQU	1<<IObTR		; TR pin
IO_TL		EQU	1<<IObTL		; TL pin

; -------------------------------------------------------------------------
