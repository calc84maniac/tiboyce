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
	
#ifdef CEMU
	; HL points to string
_
	ld a,(hl)
	inc hl
	ld (mpCEmuDbg),a
	or a
	jr nz,-_
	ret
#else
	push hl
	 ld a,(current_buffer+1)
	 push af
	  xor (gb_frame_buffer_1 ^ gb_frame_buffer_2)>>8
	  ld (current_buffer+1),a
	  ld hl,(cursorCol)
	  push hl
	   ld a,WHITE
	   ACALL(PutStringColor)
	  pop hl
	  ld (cursorCol),hl
	 pop af
	 ld (current_buffer+1),a
	pop hl
	jr PutString
#endif
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
	   ld hl,text_frame_1 + (160*10)
	   ld de,text_frame_1
	   ld bc,160*(90-10)
	   ldir
	   push de
	   pop hl
	   inc de
	   ld (hl),BLUE_BYTE
	   ld bc,160*10-1
	   ldir
	   
	   ld hl,text_frame_2 + (160*10)
	   ld de,text_frame_2
	   ld bc,160*(90-10)
	   ldir
	   push de
	   pop hl
	   inc de
	   ld (hl),BLUE_BYTE
	   ld bc,160*10-1
	   ldir
	  pop de
	 pop bc
	pop hl
	ret

font:
	#import "font.bin"
	
generate_digits:
	APTR(font + (('0'-' ')*10))
	ex de,hl
	ld hl,digits
	ld b,10*10
_
	ld a,(de)
	inc de
	ld c,a
_
	sla c
	sbc a,a
	or WHITE+(15-BLACK)
	sub 15-BLACK
	ld (hl),a
	sla c
	sbc a,a
	or WHITE+(15-BLACK)
	sub 15-BLACK
	rld
	inc hl
	ld a,l
	and 3
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
#endif