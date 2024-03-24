overlapped_op_1_1_fixup:
	call fixup_gb_address_bc
	jr overlapped_op_1_1_fixup_continue

overlapped_op_1_1_mismatch:
	lea hl,ix-3
	ld (hl),a
	lea bc,ix-7
	; Recompile an overlapped instruction again
	; Input: BC=start of overlapped handler call
	;        HL=overlap point in copied instruction
	;        IX=recompiled code start
	;        DE=cycle count
opgen_overlap_rerecompile:
	push bc
	 exx
	pop hl
	exx
	ld a,l
	ld (opgen_base_address_smc_1),a
	ld a,h
	lea hl,ix-2
	sub (hl)
	ld (opgen_base_address_smc_2),a
	ld ix,opgenroutines
	inc bc
	inc bc
	inc bc
	push de
	 push iy
	  call opgen_emit_overlapped_opcode
	 pop iy
	pop de
	jp handle_overlapped_op_done

overlapped_op_1_2_mismatch:
	lea hl,ix-4
	ld.s (hl),bc
	lea bc,ix-8
	jr opgen_overlap_rerecompile

handle_overlapped_op_1_1_helper:
	ex af,af'
	lea hl,ix
	exx
	ld e,a
	ld bc,z80codebase
	add ix,bc
	ld.s bc,(ix-3)
	ld hl,z80codebase+mem_read_lut
	ld a,c
	ld c,l ;C=0
	ld l,b
	ld l,(hl)
	inc h ;mem_get_ptr_routines
	inc l \ inc l
	ld hl,(hl)
	add hl,bc
	jr c,overlapped_op_1_1_fixup
overlapped_op_1_1_fixup_continue:
	cp (hl)
	jr nz,overlapped_op_1_1_mismatch
	ld a,(ix-1)
	add a,e
	jr nc,handle_overlapped_op_done
	inc d
	jr nz,handle_overlapped_op_done
	lea hl,ix-4
	jr schedule_overlapped_event_helper_x_1

overlapped_op_2_1_mismatch:
	lea hl,ix-3
	ld (hl),a
	lea bc,ix-8
	jr opgen_overlap_rerecompile

handle_overlapped_op_1_2_helper:
	ex af,af'
	lea hl,ix
	exx
	ld e,a
	ld bc,z80codebase
	add ix,bc
	ld.s bc,(ix-3)
	ld hl,z80codebase+mem_read_lut
	ld a,c
	ld c,l ;C=0
	ld l,b
	ld l,(hl)
	inc h ;mem_get_ptr_routines
	inc l \ inc l
	ld hl,(hl)
	add hl,bc
	jr c,overlapped_op_1_2_fixup
overlapped_op_1_2_fixup_continue:
	ld bc,(hl)
	ld h,a
	ld l,(ix-4)
	sbc.s hl,bc
	jr nz,overlapped_op_1_2_mismatch
	ld a,(ix-1)
	add a,e
	jr nc,handle_overlapped_op_done
	inc d
	jr nz,handle_overlapped_op_done
	lea hl,ix-5
	inc.s bc
	ld b,e
	lea de,ix-4
	jr schedule_overlapped_event_helper

handle_overlapped_op_2_1_helper:
	ex af,af'
	lea hl,ix
	exx
	ld e,a
	ld bc,z80codebase
	add ix,bc
	ld.s bc,(ix-3)
	ld hl,z80codebase+mem_read_lut
	ld a,c
	ld c,l ;C=0
	ld l,b
	ld l,(hl)
	inc h ;mem_get_ptr_routines
	inc l \ inc l
	ld hl,(hl)
	add hl,bc
	jr c,overlapped_op_2_1_fixup
overlapped_op_2_1_fixup_continue:
	cp (hl)
	jr nz,overlapped_op_2_1_mismatch
	ld a,(ix-1)
	add a,e
	jr c,_
handle_overlapped_op_done:
	exx
	ex af,af'
	pop.s ix
	jp.s (hl)
_
	inc d
	jr nz,handle_overlapped_op_done
	lea hl,ix-5
schedule_overlapped_event_helper_x_1:
	ld b,e
	lea de,ix-3
; Inputs: HL = pointer to first byte of copied opcode
;         DE = pointer to first overlapping byte of copied opcode
;         IX = HL' = starting recompiled address
;         BCU = 0
;         B = cycle count before executing overlapped opcode (< 0)
;         A = cycle count at end of overlapped opcode (>= 0)
schedule_overlapped_event_helper:
	ld c,a
	ld a,d
	sub (ix-2)
	ld d,a
	; Check for a prefixed opcode
	ld a,(hl)
	cp $CB
	ld a,b
	jr nz,schedule_event_later_resolved
	; Prefixed overlapped opcodes are always 8 bytes recompiled
	lea ix,ix+8
	inc hl
	inc hl
	sbc hl,de
	ld d,0
	ld a,e
	jp.sis schedule_event_finish

overlapped_op_1_2_fixup:
	call fixup_gb_address_bc
	jr overlapped_op_1_2_fixup_continue

overlapped_op_2_1_fixup:
	call fixup_gb_address_bc
	jr overlapped_op_2_1_fixup_continue

; Inputs: DE = Game Boy address of jump instruction
;         IX = HL' = starting recompiled address
;         BCU = 0
;         B = cycles until end of sub-block (plus jump cycles, if applicable)
;         C = cycle count at end of sub-block (>= 0)
schedule_jump_event_helper:
	dec b
	dec b
	dec b
schedule_jump_event_helper_adjusted:
	GET_GB_ADDR_FAST_SWAPPED
	bit 7,(hl)
	inc hl
	jr nz,schedule_jump_event_absolute
	push de
	 ld a,(hl)
	 inc hl
	 ex de,hl
	 rla
	 sbc hl,hl
	 rra
	 ld l,a
	 add hl,de
#ifdef VALIDATE_SCHEDULE
	pop de
	call validate_schedule_resolved
	push de
#endif
	 ld a,c
	 sub b
	 jr c,schedule_event_later_resolved_pushed
	pop de
	sbc hl,de
schedule_event_now_unresolved:
	ld.sis (event_gb_address),hl
	ld e,b
	ld d,0
	ld ixl,a
	ld ixh,d
#ifdef DEBUG
	ld hl,event_debug_address
	ld.sis (event_address),hl
#endif
	; This is a code path that could target the flush handler
flush_event_smc_1 = $+1
	jp.sis do_event_pushed
	
schedule_jump_event_absolute:
	dec b
	ld hl,(hl)
#ifdef VALIDATE_SCHEDULE
	inc hl
	dec.s hl
	ex de,hl
	call validate_schedule
	ex de,hl
#endif
	ld a,c
	sub b
	jr nc,schedule_event_now_unresolved
	inc hl
	dec.s hl
	add hl,de
schedule_event_later_resolved:
	push de
schedule_event_later_resolved_pushed:
	 ld de,opcounttable
	 ld b,e
	 push bc
	  ld c,3
	  call opcycle_first
	 pop de
	pop bc
	or a
	sbc hl,bc
	jp.sis schedule_event_finish

; Inputs: DE = Game Boy address at conditional branch
;         IX = HL' =  recompiled address after conditional branch
;         BCU = 0
;         B = cycles until end of sub-block (including conditional branch cycles)
;         C = cycle count at end of sub-block (>= 0)
schedule_subblock_event_helper:
	GET_GB_ADDR_FAST_SWAPPED
	ld a,(hl)
	rlca
	jr nc,_
	bit 2,a
	jr z,++_
	;jp cond
	inc hl
	dec b
_
	;jr cond
	inc hl
_
	;ret cond
	inc hl
	dec b
	dec b
#ifdef VALIDATE_SCHEDULE
	call validate_schedule_resolved
#endif
	ld a,c
	sub b
	jr c,schedule_event_later_resolved
	sbc hl,de
	ld.sis (event_gb_address),hl
	ld e,b
	ld d,0
	ld ixl,a
	ld ixh,d
#ifdef DEBUG
	ld hl,event_debug_address
	ld.sis (event_address),hl
#endif
	jp.sis do_event_pushed

schedule_jump_event_relative_slow:
	inc de
	ld l,(hl)
	ld a,l
	rla
	sbc a,a
	ld h,a
	add.s hl,de
	ex de,hl
	jr schedule_event_helper

; Inputs: DE = Game Boy address at second byte of call instruction
;         IX = HL' =  starting recompiled address
;         BCU = 0
;         B = cycles until end of sub-block (plus jump cycles, if applicable)
;         C = cycle count at end of sub-block (>= 0)
schedule_call_event_helper:
	GET_GB_ADDR_FAST
	inc e
	jr nz,schedule_call_event_fast
schedule_jump_event_absolute_slow:
	; Handle possibly overlapped memory region
	ld a,(hl)
	inc d
	GET_GB_ADDR_FAST
	ld e,a
	jr schedule_event_helper_slow_finish

; Inputs: DE = Game Boy address of jump instruction plus 1
;         IX = HL' = starting recompiled address
;         BCU = 0
;         B = cycles until end of sub-block (plus 1 for non-taken jump)
;         C = cycle count at end of sub-block (>= 0)
;         A = negative cycles for jump
;  (-1 for untaken JR/RET, -2 for untaken JP, -3 for taken JR, -4 for taken JP)
schedule_slow_jump_event_helper:
	add a,2
	jr c,schedule_bridge_event_slow
	inc.s de
	GET_GB_ADDR_FAST
	inc a
	jr z,schedule_jump_event_relative_slow
	; Check if jump target may overlap memory regions
	inc e
	jr z,schedule_jump_event_absolute_slow
schedule_call_event_fast:
	ld e,(hl)
	inc hl
schedule_event_helper_slow_finish:
	ld d,(hl)

; Inputs:  DE = starting Game Boy address
;          IX = HL' = starting recompiled address
;          B = cycles until end of sub-block
;          C = cycle count at end of sub-block (>= 0)
;          (SPS+1) = stack overflow counter
;          (SPS+2) = Game Boy BC
; Outputs: HL = event Game Boy address
;          IX = event recompiled address
;          A = event cycles to sub-block end
;          DE = cycle count at end of sub-block (>= 0)
schedule_event_helper:
	ld a,c
schedule_event_helper_a:
	sub b
	jr nc,schedule_event_now
schedule_event_later:
#ifdef VALIDATE_SCHEDULE
	call validate_schedule
#endif
	GET_BASE_ADDR_FAST
	push hl
	 add hl,de
	 jr c,schedule_event_fixup_address
schedule_event_fixup_address_continue:
	 ld de,opcounttable
	 ld b,e
	 push bc
	  ld c,3
	  call opcycle_first
	 pop de
	pop bc
	or a
	sbc hl,bc
	jp.sis schedule_event_finish

schedule_bridge_event_slow:
	dec b
	ld a,c
	sub b
	jr c,schedule_event_later
schedule_event_now:
#ifdef VALIDATE_SCHEDULE
	call validate_schedule
#endif
	ld.sis (event_gb_address),de
	ld e,b
	ld d,0
	ld ixl,a
	ld ixh,d
#ifdef DEBUG
	ld hl,event_debug_address
	ld.sis (event_address),hl
#endif
	; This is a code path that could target the flush handler
flush_event_smc_2 = $+1
	jp.sis do_event_pushed

schedule_event_fixup_address:
	pop de
	call fixup_gb_address_swapped
	push de
	 jr schedule_event_fixup_address_continue

#ifdef VALIDATE_SCHEDULE
validate_schedule_resolved:
	push af
	 push ix
	  push hl
	   push de
	    or a
	    sbc hl,de
	    push hl
	    jr _

validate_schedule:
	push af
	 push ix
	  push hl
	   push de
	    push de
_
	     push bc
	      ; Verify that BCU=0
	      ld hl,$FF0000
	      add hl,bc
	      ASSERT_NC
	      ; Check if target block is expected to be 0 cycles
	      ld a,b
	      or a
	      lea.s bc,ix
	      jr nz,_
	      ; Skip reverse lookup if the flush handler is the target
	      ; This is only valid when the target block is 0 cycles
	      ld hl,flush_handler
	      sbc hl,bc
	      ld.sis de,(flush_address)
_
	      call nz,lookup_gb_code_address
	     pop bc
	     sub b
	     call nz,validate_schedule_nops
	    pop hl
	    sbc hl,de
	    jr nz,$
	   pop de
	  pop hl
	 pop ix
	pop af
	ex (sp),hl
	inc sp \ inc sp \ inc sp
	ex (sp),hl
	dec sp \ dec sp \ dec sp
	pop hl
	ret

validate_schedule_nops:
	jr c,$
	; Special case to handle NOPs, ugh
	push bc
	 ld b,a
	 ld ix,opcounttable
	 GET_GB_ADDR_FAST
_
	 ld a,(hl)
	 ld ixl,a
	 ld a,(ix)
	 sub opcycleNOP - opcycleroutines
	 jr nz,$
	 inc hl
	 inc de
	 djnz -_
	pop bc
	ret
#endif
