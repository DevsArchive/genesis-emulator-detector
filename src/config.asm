
; -------------------------------------------------------------------------
;
;	Genesis Emulator Detector
;		By Ralakimus 2019
;
;	File:		config.asm
;	Contents:	Configuration
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Header
; -------------------------------------------------------------------------

; Game name
GAME_NAME	EQUS	"EMULATOR DETECTOR"
; I/O support
IO_SUPPORT	EQUS	"J"
; SRAM support
SRAM_SUPPORT	EQU	$20202020
; SRAM start address
SRAM_START	EQU	$20202020
; SRAM end address
SRAM_END	EQU	$20202020
; Regions allowed
REGIONS		EQU	%1111
; Notes
NOTES		EQUS	""

; -------------------------------------------------------------------------
; Flags
; -------------------------------------------------------------------------

; Enable lockout code flag
LOCKOUT		EQU	0
; Enable lag meter flag
LAGMETER	EQU	0
; Debug build flag
DEBUG		EQU	1
; Checksum check flag
CHECKSUM	EQU	1

; -------------------------------------------------------------------------
; User defined
; -------------------------------------------------------------------------

; Insert user defined flags here

; -------------------------------------------------------------------------
