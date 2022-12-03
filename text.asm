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
	inc hl ;emulatorMessageText
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
	ld a,BLACK
	call SetStringBgColor
	; Modify PutChar to use a 160-pixel wide buffer
	push hl
	 xor a
	 ld hl,PutChar_SmallBufferSMC1
	 ld (hl),a			; nop
#if (PutChar_SmallBufferSMC1>>8)!=(PutChar_SmallBufferSMC2>>8)
.error "(PutChar_SmallBufferSMC1>>8)!=(PutChar_SmallBufferSMC2>>8)"
#endif
	 ld l,PutChar_SmallBufferSMC2&$FF
	 ld (hl),(160-8)/2
	 ld c,a
	 ld b,a
	 ld a,WHITE
	 ex (sp),hl
	 ACALL(PutStringColorXY)
	pop hl
	; Restore PutChar to use a 320-pixel wide buffer
	ld (hl),(320-8)/2
	ld l,PutChar_SmallBufferSMC1&$FF
	ld (hl),$09			; add hl,bc
	
	ld a,BLUE
	call SetStringBgColor
	
	ld hl,(cursorCol)
	add hl,hl
	ld h,10
	jp PutEmulatorMessageRet

	; Call like printf, A=color, H=x (column), L=y
PutStringFormatColorXY:
	ld (cursorRowCol),hl

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

	; HL points to string, A=color, B=x (column), C=y
PutStringColorXY:
	ld (cursorRowCol),bc

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

PutNStringColorXY:
	ld (cursorRowCol),bc

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
	ld hl,cursorRowCol+1
	ld (hl),1
	dec hl
	ld a,(hl)
	cp a,240
	ret nc
	add a,10
	ld (hl),a
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
	.db "Flushing recompiled code\n",0
	
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

#ifdef SCHEDULER_LOG	
SchedulerLogMessage:
	.db "%06X: AF=%04X, BC=%04X, DE=%04X, HL=%04X, SP=%04X\n",0
#endif
#endif
