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
	
opgenHLread:
	jp opgen_emit_hl_read
opgenHLwrite:
	jp opgen_emit_hl_write
opgenHLreadwrite:
	jp opgen_emit_hl_readwrite
opgenHLread_post:
	jp opgen_emit_hl_read_post
opgenHLwrite_post:
	jp opgen_emit_hl_write_post
opgenHLread_bc:
	jp opgen_emit_hl_read_bc
opgenHLwrite_bc:
	jp opgen_emit_hl_write_bc
opgenDEread:
	jp opgen_emit_de_read
opgenDEwrite:
	jp opgen_emit_de_write
opgenBCread:
	jp opgen_emit_bc_read
opgenBCwrite:
	jp opgen_emit_bc_write
	
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
	
opgenJPHL:
opgenRET:
opgenRETI:
	jp opgenblockend

opgenE8:
	dec iyl
opgenF8:
	call opgenroutinecall_displacement
	djnz opgen_next_fast

opgen31:
	call _opgen31
	djnz opgen_next_fast
	
opgenPUSH:
	dec iyl
opgenF1:
	dec iyl
opgen39:
opgenF9:
	dec iyl
opgenDAA:
opgenDI:
opgenEI:
	call opgenroutinecall
	djnz opgen_next_fast
	
opgenPOP:
	dec iyl
opgen33:
opgen3B:
	call opgenroutinecall_2cc
	ld a,$08 ;EX AF,AF'
	ld (de),a
	inc de
	djnz opgen_next_fast
	
opgen01:
	ld a,$DD
	ld (de),a
	inc de
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
	ld a,$DD
	ld (de),a
opgen2byte_remap_inc:
	inc de
opgen2byte_remap:
	inc b
	ld a,(bc)
	ld (de),a
	inc de
	inc hl
	djnz opgen1byte
	
opgen1byte_remap_ix:
	ld a,$DD
	ld (de),a
opgen1byte_remap_inc:
	inc de
opgen1byte_remap:
	inc b
	ld a,(bc)
	ld (de),a
	inc de
	inc hl
	djnz opgen_next_fast
	
opgen1byte_2cc_remap_ix:
	ld a,$DD
	ld (de),a
opgen1byte_2cc_remap_inc:
	inc de
opgen1byte_2cc_remap:
	inc b
	ld a,(bc)
	ld (de),a
	inc de
	inc hl
	dec iyl
	djnz opgen_next_fast
	
opgenNOP:
	inc hl
	jr opgen_next_fast
	
opgenCB:
	jp _opgenCB
	
opgenHALT_STOP:
	jp _opgenHALT_STOP
	
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
	ld (hl),$ED ;LEA HL,IX
	inc hl
	ld (hl),$22
	inc hl
	ld (hl),$00
	ld c,$19
	jr _
opgen19:
opgen29:
	res 4,c
	ex de,hl
	ld (hl),$EB ;EX DE,HL
_
	inc hl
	ld (hl),c
opgen_next_ex_swap_skip_1cc:
	inc hl
	ld (hl),$EB ;EX DE,HL
opgen_next_swap_skip_1cc:
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
opgenE2:
opgenF2:
	jr _opgenLDH_C
opgenJRcond:
	jr _opgenJRcond
opgenJPcond:
	jr _opgenJPcond
	
opgenROT:
	ex de,hl
	
	.echo "Opgen routine size: ", $ - opgenroutines
	
	bit 4,c
	jr z,opgenROT_rlca_rrca
	ld (hl),$2E ;LD L,
	inc hl
	ld (hl),c
	inc hl
	ld (hl),$2C ;INC L
	jr opgenROTfinish
opgenROT_rlca_rrca:
	ld (hl),$ED ;LD HL,I
	inc hl
	ld (hl),$D7
opgenROTfinish:
	inc hl
	ld (hl),c
	inc hl
	inc de
	ex de,hl
	jr opgen_next_fast
	
_opgenLDH_C:
	bit 4,c
	jp z,_opgenE2
	call opgen_emit_load_cycle_offset_swap_1cc
	ex de,hl
	call opgenroutinecall
	djnz opgen_next_fast
	
_opgen3F:
	ex de,hl
	ld (hl),c
	inc hl
	; Reset H and N flags, preserve Z and C flags
	ld (hl),$1F ;RRA
	ld c,$17	;RLA
	jr opgenROTfinish
	
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
	