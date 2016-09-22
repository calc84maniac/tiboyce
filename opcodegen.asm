; Allocate at 256-byte aligned cursor memory

opgenroutines:
opgenE9:
	ld a,1
	call opgen_finisher
	ex de,hl
	ld (hl),$C3
	inc hl
	ld (hl),ophandlerE9 & $FF
	inc hl
	ld (hl),ophandlerE9 >> 8
	ex de,hl
	ret
	
opgenE0:
	jp opgenFFwrite
opgenEA:
	jp opgenCONSTwrite
opgenF0:
	jp opgenFFread
opgenFA:
	jp opgenCONSTread
	
opgenCALLcond:
	jp _opgenCALLcond
opgenCALL:
	jp _opgenCALL
opgenRST:
	ld c,decode_rst & $FF
	ld a,decode_rst >> 8
	jp opgen_emit_call
opgenJP:
	jp _opgenJP
opgenJR:
	jp _opgenJR
opgenRET:
	jp _opgenRET

opgen08:
	call opgenroutinecall2byte_5cc
	.dw ophandler08
opgen31:
	call opgenroutinecall2byte_3cc
	.dw ophandler31
opgenE8:
	call opgenroutinecall1byte_4cc
	.dw ophandlerE8
opgenF8:
	call opgenroutinecall1byte_3cc
	.dw ophandlerF8
opgen36:
	call opgenroutinecall1byte_3cc
	.dw ophandler36
opgen34:
	call opgenroutinecall_3cc
	.dw ophandler34
opgen35:
	call opgenroutinecall_3cc
	.dw ophandler35
opgen39:
	call opgenroutinecall_2cc
	.dw ophandler39
opgen76:
	call opgenroutinecall_1cc
	.dw ophandler76
opgenE2:
	call opgenroutinecall_2cc
	.dw ophandlerE2
opgenF2:
	call opgenroutinecall_2cc
	.dw ophandlerF2
opgenF3:
	call opgenroutinecall_1cc
	.dw ophandlerF3
opgenF9:
	call opgenroutinecall_2cc
	.dw ophandlerF9
opgenEI:
	call opgenroutinecall_1cc
	.dw ophandlerEI
	
opgenMEM:
	ld a,RST_MEM
	ld (de),a
	inc de
	ldi
	inc de
	dec iy
	jr opgen_next_fast
	
opgenPUSH:
	ldi
	ld a,RST_PUSH
	ld (de),a
	inc de
	lea iy,iy-3
	jr opgen_next_fast
	
opgenPOP:
	ld a,RST_POP
	ld (de),a
	inc de
opgen1byte_3cc:
	lea iy,iy-2
	jr opgen1byte
	
opgen33:
opgen3B:
	dec iy
	ex de,hl
	ld (hl),$D9	; EXX
	inc hl
	ld (hl),$5B	; .LIL
	inc hl
	res 4,c
	ld (hl),c	; INC/DEC HL
	inc hl
	ld (hl),$D9	; EXX
opgen_next_swap_skip:
	ex de,hl
opgen_next_skip:
	inc de
opgen0byte:
	inc hl
opgen_next:
	ld bc,opgentable
	jr opgen_next_fast
	
opgenCB:
	ldi
	ld a,(hl)
	and $07
	cp $06
	jr nz,opgen1byte
opgenswap:
	dec de
	ld a,RST_BITS
	ld (de),a
	inc de
	jr opgen1byte
	
opgen1byte_2cc:
	dec iy
	jr opgen1byte
	
opgen3byte_low:
	inc b
opgen3byte:
	ldi
opgen2byte:
	ldi
opgen1byte:
	ldi
opgen_next_fast:
	ld c,(hl)
	ld a,(bc)
	ld ixl,a
	jp (ix)
	
opgenRETI:
	jr _opgenRETI
opgenINVALID:
	jr _opgenINVALID
opgenJRcond:
	jr _opgenJRcond
opgenJPcond:
	jr _opgenJPcond
opgenRETcond:
	jr _opgenRETcond
	
	.echo "Opgen routine size: ", $ - opgenroutines
	
_opgenJRcond:
	ld a,c
	xor $20 ^ $28
	ld (de),a
	inc de
	ld a,7
	ld (de),a
	inc de
	call _opgenJR
	jr opgen_next_skip
	
_opgenJPcond:
	ld a,c
	xor $C2 ^ $28
	ld (de),a
	inc de
	ld a,7
	ld (de),a
	inc de
	call _opgenJP
	jr opgen_next_skip
	
_opgenRETcond:
	dec iy
	ld a,c
	xor $C0 ^ $28
	ld (de),a
	inc de
	ld a,4
	ld (de),a
	inc de
	call _opgenRET
	jr opgen_next_skip
	
_opgenRETI:
	call _opgenRET
	ex de,hl
	ld (hl),$C3 ;JP ophandlerRETI
	inc hl
	ld (hl),ophandlerRETI & $FF
	inc hl
	ld (hl),ophandlerRETI >> 8
	ex de,hl
	ret
	
_opgenINVALID:
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),ophandlerINVALID & $FF
	inc hl
	ld (hl),ophandlerINVALID >> 8
	ex de,hl
	ret