Wait:
_
	ld a,(mpIntMaskedStatus)
	rra
	ret c
	ld a,(mpKeypadGrp6)
	rra
	jr c,-_
_
	ld a,(mpIntMaskedStatus)
	rra
	ret c
	ld a,(mpKeypadGrp6)
	rra
	jr nc,-_
	ret

#ifdef DEBUG
debug_printf:
	ld ix,text_buffer
	ex (sp),ix
	call _sprintf
	pop hl
	push ix
	
debug_print:
	; HL points to string
_
	ld a,(hl)
	inc hl
	ld (mpCEmuDbg),a
	or a
	jr nz,-_
	ret
#endif
	
SetEmulatorMessage:
	ld a,120
	
	; Call like printf, DE=format string (offset into appvar), A=duration
SetEmulatorMessageWithDuration:
	ld ix,(ArcBase)
	add ix,de
	ex (sp),ix
	ld hl,emulatorMessageDuration
	ld (hl),a
	inc hl
	push hl
	 ld a,(GameMessageDisplay)
	 cp $FF
	 jr nz,_
	 ld a,(MessageDisplay)
_
	 or a
	 call nz,_sprintf
	pop hl
	ex (sp),ix
	jp reset_preserved_area
	
PutEmulatorMessage:
	xor a
	ld (cursorCol),a
	ld (cursorRow),a
	ld a,(active_scaling_mode)
	or a
	ld a,BLACK_BYTE
	jr nz,_
	; Modify PutChar to use 8-bit pixels
	ld a,8
	ld (PutChar_8BitSMC1),a
	ld (PutChar_8BitSMC2),a
	ld a,$37	;SCF
	ld (PutChar_8BitSMC3),a
	ld (PutChar_8BitSMC3+1),a
	ld a,160-8
	ld (PutChar_8BitSMC4),a
	ld a,BLACK
_
	call SetStringBgColor
	ld a,WHITE
	ACALL(PutStringColor)
	; Restore PutChar to 4-bit
	ld a,4
	ld (PutChar_8BitSMC1),a
	ld (PutChar_8BitSMC2),a
	ld a,$CB	;SLA C
	ld (PutChar_8BitSMC3),a
	ld a,$21	;SLA C
	ld (PutChar_8BitSMC3+1),a
	ld a,160-4
	ld (PutChar_8BitSMC4),a
	
	ld a,BLUE_BYTE
	call SetStringBgColor

	ld hl,(cursorCol)
	add hl,hl
	add hl,hl
	ld h,10
	jp PutEmulatorMessageRet
	
	
	; Call like printf, A=color
PutStringFormatColor:
	call SetStringColor
	
	; Call like printf
PutStringFormat:
	ld ix,text_buffer
	ex (sp),ix
	call _sprintf
	pop hl
	push ix
	jr PutString
	
	; HL points to string, A=color
PutStringColor:
	call SetStringColor
	
	; HL points to string
PutString:
	ld a,(hl)
	inc hl
	or a
	ret z
	push hl
	 call PutChar
	pop hl
	jr PutString
	
PutNStringColor:
	call SetStringColor
	
PutNString:
	ld b,(hl)
	inc hl
PutNString_B:
	push bc
	 ld a,(hl)
	 inc hl
	 push hl
	  call PutChar
	 pop hl
	pop bc
	djnz PutNString_B
	ret
	
PutNewLine:
	push hl
	 ld hl,cursorCol
	 ld (hl),0
	 inc hl
	 ld a,(hl)
	 add a,10
	 cp 240
	 jr nc,_
	 ld (hl),a
_
	pop hl
	ret

font:
	#import "font.bin"
	
digits_encoded:
	.db %11101010,%10101010,%11100000
	.db %01001100,%01000100,%11100000
	.db %11000010,%01001000,%11100000
	.db %11000010,%01000010,%11000000
	.db %10101010,%11100010,%00100000
	.db %11101000,%11000010,%11000000
	.db %01101000,%11101010,%11100000
	.db %11100010,%01001000,%10000000
	.db %11101010,%11101010,%11100000
	.db %11101010,%11100010,%11000000
	.db %10100010,%01001000,%10100000
	
generate_digits:
	APTR(digits_encoded)
	ex de,hl
	ld hl,digits
	ld (display_digit_smc_1),a
	ld (display_digit_smc_2),a
	cpl
	add a,161
	ld (display_digit_smc_3),a
	cp 160-4
	jr z,generate_digits_8bit
	ld b,11
_
	push bc
	 ld b,3
_
	 ld a,(de)
	 inc de
	 ld c,a
_
	 sla c
	 sbc a,a
	 or BLACK+(15-WHITE)
	 sub 15-WHITE
	 ld (hl),a
	 sla c
	 sbc a,a
	 or BLACK+(15-WHITE)
	 sub 15-WHITE
	 rld
	 inc hl
	 ld a,l
	 and 3
	 jr nz,-_
	 djnz --_
	 ld c,12
	 add hl,bc
	pop bc
	djnz ---_
	ret
	
generate_digits_8bit:
	ld b,3*11
_
	ld a,(de)
	inc de
	ld c,a
_
	sla c
	sbc a,a
	or BLACK-(1+WHITE)
	add a,1+WHITE
	ld (hl),a
	inc hl
	ld a,l
	and 7
	jr nz,-_
	djnz --_
	ret
	
#ifdef DEBUG
StartText:
	.db "Starting!\n",0
	
WaitLoopSearchMessage:
	.db "Searching for waitloop at %04X.\n",0
	
WaitLoopIdentifiedMessage:
	.db "Setting waitloop at %04X, var %04X.\n",0
	
ByteFormat:
	.db "%02X",0

LookupGBMessage:
	.db "Looking up GB address from %04X\n",0
	
LookupGBFoundMessage:
	.db "Found GB %04X @ %04X\n",0
	
FlushMessage:
	.db "Flushing recompiled code!\n",0
	
CacheMissMessage:
	.db "Cache miss at %06X\n",0
	
LookupMessage:
	.db "Looking up GB code at %06X\n",0
	
RecompileMessage:
	.db "Recompiling %06X to %04X\n",0
	
RecompileRamMessage:
	.db "Recompiling RAM:%06X to %04X\n",0
	
CoherencyFailedMessage:
	.db "RAM coherency failed at %06X, routine=%04X\n",0
	
PaddingUpdateMessage:
	.db "RAM block padding increased to %06X\n",0
	
RuntimeErrorMessage:
	.db "Runtime error occurred!\n",0
	
InvalidOpcodeErrorMessage:
	.db "Encountered invalid opcode at %04X\n",0
#endif