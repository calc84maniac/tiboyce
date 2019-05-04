; Allocate at 256-byte aligned cursor memory

opgenroutines:
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
	jp _opgenRST
opgenJR:
opgenJP:
	jp opgen_emit_unconditional_jump
opgenRET:
	jp _opgenRET
opgenRETcond:
	jp _opgenRETcond

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
	call opgenroutinecallsplit_1cc
	.dw ophandler76
opgenE2:
	call opgenroutinecall_2cc
	.dw ophandlerE2
opgenF1:
	call opgenroutinecall_3cc
	.dw ophandlerF1
opgenF2:
	call opgenroutinecall_2cc
	.dw ophandlerF2
opgenF3:
	call opgenroutinecall_1cc
	.dw ophandlerF3
opgenF5:
	call opgenroutinecall_4cc
	.dw ophandlerF5
opgenF9:
	call opgenroutinecall_2cc
	.dw ophandlerF9
opgenEI:
	call opgenroutinecallsplit_1cc
	.dw ophandlerEI
	
opgenE9:
	call opgenblockend
	.dw ophandlerE9
opgenRETI:
	call opgenblockend
	.dw ophandlerRETI
	
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
	inc hl
opgen_next:
	ld bc,opgentable
	jr opgen_next_fast
	
opgenNOP:
	xor a
	ld (de),a
	jr opgen_next_skip
	
opgenCB:
	ldi
	ld a,(hl)
	and $07
	cp $06
	jr nz,opgen1byte
	jr _opgenCB
	
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
	ld a,l
	sub iyl
	ret m
	jp (ix)
	
opgenINVALID:
	jr _opgenINVALID
opgenJRcond:
	jr _opgenJRcond
opgenJPcond:
	jr _opgenJPcond
	
opgen27:
	call opgenroutinecall_1cc
	.dw ophandler27
	
	.echo "Opgen routine size: ", $ - opgenroutines
	
_opgenCB:
	dec iy
	ld a,(hl)
	add a,$40
	jp pe,_
	dec iy
_
	dec de
	ld a,RST_BITS
	ld (de),a
	inc de
	jr opgen1byte
	
_opgenJRcond:
	ld a,$20 ^ $28
_
	xor c
	ex de,hl
	ld (hl),a
	inc hl
	ld (hl),10
	inc hl
	xor $28 ^ $C2
	call opgen_emit_jump_swapped
	jp opgen_emit_block_bridge
	
_opgenJPcond:
	ld a,$C2 ^ $28
	jr -_
	
_opgenINVALID:
	ex de,hl
	ld (hl),RST_INVALID_OPCODE
	inc hl
	jp opgen_reset_cycle_count