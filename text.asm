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
	 jr nc,ScrollUp
	 ld (hl),a
	pop hl
	ret
	
ScrollUp:
	 push bc
	  push de
	   ld hl,(current_buffer)
	   ld de,text_frame_1 - gb_frame_buffer_1
	   add hl,de
	   ex de,hl
	   ld hl,160*10
	   add hl,de
	   ld bc,160*(90-10)
	   ldir
	   push de
	   pop hl
	   inc de
	   ld (hl),BLACK_BYTE
	   ld bc,160*10-1
	   ldir
	  pop de
	 pop bc
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
	.db "Found GB %06X @ %04X\n",0
	
FlushMessage:
	.db "Flushing recompiled code!\n",0
	
CacheMissMessage:
	.db "Cache miss at %02X:%04X\n",0
	
LookupMessage:
	.db "Looking up GB code at %02X:%04X\n",0
	
RecompileMessage:
	.db "Recompiling %02X:%04X (%06X) to %04X\n",0
	
RecompileRamMessage:
	.db "Recompiling RAM:%04X (%06X) to %04X\n",0
	
CoherencyFailedMessage:
	.db "RAM coherency failed, routine=%04X\n",0
	
PaddingUpdateMessage:
	.db "RAM block padding increased to %06X\n",0
	
RuntimeErrorMessage:
	.db "Runtime error occurred!\n",0
	
InvalidOpcodeErrorMessage:
	.db "Encountered invalid opcode at %04X\n",0
#endif