; Allocate at 256-byte aligned cursor memory

opgenroutines:
opgen08:
	jp _opgen08
opgen36:
	jp _opgen36
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
	jp opgen_emit_jump
opgenRETcond:
	jp opgen_emit_cond_ret
	
opgenE9:
opgenRET:
opgenRETI:
	jp opgenblockend

opgen34:
	ld c,$30
	jr opgenF1
	
opgen35:
	ld c,$37
	jr opgenF1
	
opgenE8:
	dec iyl
opgenF8:
	dec iyl
	call opgenroutinecall
	djnz opgen1byte

opgen31:
	call opgenroutinecall
	djnz opgen2byte
	
opgenC5:
opgenD5:
opgenE5:
opgenF5:
	dec iyl
opgenF1:
	dec iyl
opgen39:
opgen33:
opgen3B:
opgenE2:
opgenF2:
opgenF9:
	dec iyl
opgen27:
opgenF3:
opgenEI:
	call opgenroutinecall
	djnz opgen_next_fast
	
opgen01:
#ifdef USE_IX
	ld a,$DD
	ld (de),a
	inc de
#endif
	ld a,$21
	ld (de),a
	inc de
	inc hl
	inc c
	jr opgen2byte
	
opgen3byte_remap:
	inc b
	ld a,(bc)
	ld (de),a
	inc de
	inc hl
	djnz opgen2byte
	
opgen2byte_remap_ix:
#ifdef USE_IX
	ld a,$DD
	ld (de),a
	inc de
#endif
opgen2byte_remap:
	inc b
	ld a,(bc)
	ld (de),a
	inc de
	inc hl
	djnz opgen1byte
	
opgen1byte_remap_ix:
#ifdef USE_IX
	ld a,$DD
	ld (de),a
	inc de
#endif
opgen1byte_remap:
	inc b
	ld a,(bc)
	ld (de),a
	inc de
	inc hl
	djnz opgen_next_fast
	
opgenPOP:
	xor a
	ld (de),a
	inc de
	ld a,RST_POP
	ld (de),a
	inc de
	dec iyl
	jr opgen1byte_2cc_remap
	
opgen1byte_2cc_remap_ix:
#ifdef USE_IX
	ld a,$DD
	ld (de),a
	inc de
#endif
opgen1byte_2cc_remap:
	inc b
	ld a,(bc)
	ld (de),a
	inc de
	inc hl
	dec iyl
	djnz opgen_next_fast
	
opgenMEM:
	inc hl
opgen36_finish:
	ld a,RST_MEM
	ld (de),a
	inc de
	inc b
	ld a,(bc)
	ld (de),a
	inc de
	inc de
	dec iyl
	djnz opgen_next_fast
	
opgenNOP:
	inc hl
	jr opgen_next_fast
	
opgen76:
	jp _opgen76
	
opgenCB:
	ldi
	ld a,(hl)
	xor $36
	ld c,a
	and $F8
	jr z,opgenCB_swap
	xor c
	jr z,opgenCB_mem
	srl a
	jr z,opgen1byte
	add a,-2
	adc a,a
	inc a
	and 6
	xor (hl)
	ld (de),a
	inc hl
	inc de
	jr opgen_next_fast
	
opgen2byte:
	ldi
opgen1byte:
	ldi
opgen_next_fast:
	ld c,(hl)
	ld a,iyh
	sub l
	ret m
opgen_next_no_bound_check:
	ld a,(bc)
	ld ixl,a
	jp (ix)
	
opgen09:
	ex de,hl
#ifdef USE_IX
	ld (hl),$DD ;LEA HL,IX
	inc hl
	ld (hl),$22
	inc hl
	ld (hl),$00
#endif
	ld c,$19
	jr _
opgen19:
opgen29:
	res 4,c
	ex de,hl
#ifndef USE_IX
_
#endif
	ld (hl),$EB ;EX DE,HL
	inc hl
#ifdef USE_IX
_
#endif
	ld (hl),c
	inc hl
	ld (hl),$EB ;EX DE,HL
	dec iyl
opgen_next_swap_skip:
	ex de,hl
opgen_next_skip:
	inc de
	inc hl
opgen_next:
	ld bc,opgentable
	jr opgen_next_fast
	
opgenINVALID:
	jp opgenblockend_invalid
opgen3F:
	jr _opgen3F
opgenJRcond:
	jr _opgenJRcond
opgenJPcond:
	jr _opgenJPcond
	
opgenROT:
	ex de,hl
	
	.echo "Opgen routine size: ", $ - opgenroutines
	
	bit 4,c
	jr z,opgenROT_rlca_rrca
	ld (hl),$CC	;CALL Z,reset_z_flag
	inc hl
	ld (hl),reset_z_flag & $FF
	inc hl
	ld (hl),reset_z_flag >> 8
	jr _
opgenROT_rlca_rrca:
	ld (hl),$37 ;SCF
	inc hl
	ld (hl),$8F ;ADC A,A
opgen3F_finish:
	inc hl
	ld (hl),$1F ;RRA
_
	inc hl
	ld (hl),c
	inc hl
	inc de
	ex de,hl
	jr opgen_next_fast
	
opgenCB_mem:
	jp _opgenCB_mem

opgenCB_swap:
	jp _opgenCB_swap
	
_opgen3F:
	ex de,hl
	ld (hl),c
	; Reset H and N flags, preserve Z and C flags
	ld c,$17	;RLA
	jr opgen3F_finish
	
_opgenJRcond:
	ld a,$20 ^ $28
_
	xor c
	ld (de),a
	inc de
	ld a,11
	ld (de),a
	inc de
	call opgen_emit_jump
	ld a,e
	sub iyh
	jp m,opgen_emit_subblock_bridge
	; If block is ending, combine the subblock and end-of-block bridges
	jp opgen_emit_subblock_combined_bridge
	
_opgenJPcond:
	ld a,$C2 ^ $28
	jr -_
	