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
	ld a,(hl)
	inc hl
	ld (mpCEmuDbg),a
	or a
	jr nz,PutString
	ret
#else
	push hl
	 ld a,(current_buffer+1)
	 xor (gb_frame_buffer_1 ^ gb_frame_buffer_2)>>8
	 ld (current_buffer+1),a
	 push af
	  ld hl,(cursorCol)
	  push hl
	   ld a,WHITE
	   call PutStringColor
	  pop hl
	  ld (cursorCol),hl
	 pop af
	 ld (current_buffer+1),a
	pop hl
	jr PutString
#endif
#endif
	
SetStringColor:
	inc a
	ld (PutChar_ColorSMC1),a
	ld (PutChar_ColorSMC2),a
	ret
	
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
	
	; A = character to display
	; (cursorRow), (cursorCol) is location to display
PutChar:
	or a
	jr z,_
	sub ' '
	jr c,PutNewLine
_
	ld c,a
	ld b,10
	mlt bc
	ld hl,font
	add hl,bc
	ex de,hl
	
	ld hl,(cursorCol)
	ld a,l
	cp 40
	ret nc
	ld l,160
	mlt hl
	ld bc,(current_buffer)
	add hl,bc
	ld c,a
	ld b,4
	mlt bc
	add hl,bc
	
	ld b,10
PutCharRowLoop:
	push bc
	 ld a,(de)
	 inc de
	 ld b,4
	 ld c,a
PutCharPixelLoop:
	 sla c
	 sbc a,a
PutChar_ColorSMC1 = $+1
	 or WHITE+1
	 dec a
	 ld (hl),a
	 sla c
	 sbc a,a
PutChar_ColorSMC2 = $+1
	 or WHITE+1
	 dec a
	 rld
	 inc hl
	 djnz PutCharPixelLoop
	 ld c,160-4
	 add hl,bc
	pop bc
	djnz PutCharRowLoop
	ld hl,cursorCol
	inc (hl)
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
	
cursorCol:
	.db 0
cursorRow:
	.db 0

text_buffer:
	.block 42

font:
	#import "font.bin"
	
generate_digits:
	ld hl,digits
	ld de,font + (('0'-' ')*10)
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
	
	; Digit in A, output at offset DE
display_digit:
	ld hl,(current_buffer)
	add hl,de
	ex de,hl
	ld hl,digits
	ld c,a
	ld b,40
	mlt bc
	add hl,bc
	ld a,10
_
	ld bc,160
	ldi
	ldi
	ldi
	ldi
	ex de,hl
	add hl,bc
	ex de,hl
	dec a
	jr nz,-_
	ret