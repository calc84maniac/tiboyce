#ifdef DEBUG
yourkidding:
	push bc
	push de
	push ix
	push hl
	ld hl,yourkiddingtext
	push hl
	call printf
	pop hl
	pop hl
	pop ix
	pop de
	pop bc
	;call Wait
	ret.l
	
yourkiddingtext:
	.db "You're kidding! %04X\n",0

printf:
	ld ix,text_buffer
	ex (sp),ix
	call _sprintf
	ex (sp),ix
	ld hl,text_buffer
	
#ifdef CEMU
	; HL points to string
PutString:
	ld a,(hl)
	inc hl
	ld (mpCEmuDbg),a
	or a
	jr nz,PutString
	ret
#else
	; HL points to string
PutString:
	ld a,(cursorCol)
	cp 40
	call nc,PutNewLine
	ld a,(hl)
	inc hl
	or a
	ret z
	push hl
	 call PutChar
	pop hl
	jr PutString
	
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
	ld c,a
	ld b,4
	mlt bc
	add hl,bc
	ld bc,text_frame_1
	add hl,bc
	
	ld bc,256+10
PutCharRowLoop:
	push bc
	 ld a,(de)
	 inc de
	 ld b,4
	 ld c,a
PutCharPixelLoop:
	 sla c
	 sbc a,a
	 sub 2
	 rld
	 sla c
	 sbc a,a
	 sub 2
	 rld
	 inc hl
	 djnz PutCharPixelLoop
	 ld c,160-4
	 add hl,bc
	pop bc
	dec c
	jr nz,PutCharRowLoop
	djnz _
	ld bc,text_frame_2 - text_frame_1 - (160*10)
	add hl,bc
	ex de,hl
	ld bc,10
	sbc hl,bc
	ex de,hl
	jr PutCharRowLoop
_
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
	 cp 90
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
	   ld (hl),$EE
	   ld bc,160*10-1
	   ldir
	   
	   ld hl,text_frame_2 + (160*10)
	   ld de,text_frame_2
	   ld bc,160*(90-10)
	   ldir
	   push de
	   pop hl
	   inc de
	   ld (hl),$EE
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
#endif

text_buffer:
	.block 42
#endif

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
	sub 2
	rld
	sla c
	sbc a,a
	sub 2
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