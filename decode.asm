decode_jump_helper:
	ld bc,recompile_struct
	ld hl,z80codebase+mem_read_lut
	ld l,d
	jr c,decode_block_bridge_helper
	add ix,bc
	; Save original opcode in B
	ld b,a
	ld c,(hl)
	inc l
	ld a,(hl)
	inc h ;mem_get_ptr_routines
	ld l,c
	inc l \ inc l
	cp c
	ld hl,(hl)
	add hl,de
	jr nz,decode_jump_maybe_overlap
	; Check whether original opcode was JR or JP
	bit 7,b
	ld b,-3 ; Taken JR cycles (negative)
	jr z,decode_relative_jump
decode_absolute_jump:
	; Check if the actual opcode byte is HALT (if so, this is the HALT bug)
	bit 7,(hl)
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	; In the case of the HALT bug, always emit a slow jump
	jr z,decode_slow_jump_for_jp
	; Adjust B to taken JP cycles (negative)
	djnz decode_jump_common ; Always jumps
	
decode_jump_no_overlap:
	; Recover whether it was a JR or JP
	scf
	sbc a,e
	ld b,-3 ; Taken JR cycles (negative)
	jr nz,decode_absolute_jump
decode_relative_jump:
	; No special handling is needed for the HALT bug in this path,
	; since a HALT opcode will be detected as a JR (top bit reset)
	inc hl
	ld l,(hl)
	ld a,l
	rla
	sbc a,a
	ld h,a
	inc de
	inc de
	add.s hl,de
	ex de,hl
decode_jump_common:
	; Check if the target is in a different bank than the jump instruction
	ld hl,z80codebase+mem_read_lut
	ld l,d
	ld a,(hl)
	cp c
	jr nz,decode_jump_bank_switch
	push bc
	 push de
	  call lookup_code_link_internal
	 pop de
	 push af
	  call c,identify_waitloop
	 pop af
	pop bc
	jp.sis decode_jump_return

decode_block_bridge_helper:
	add ix,bc
	; Place (negative) number of extra cycles in B
	cpl
	inc a
	ld b,a
	; Get the target memory region and check if it differs from the
	; containing code block's memory region (which happens on overlap)
	ld a,(hl)
	ld l,(ix+3)
	cp (hl)
	jr nz,decode_jump_bank_switch
	; If there's no overlap but extra cycles are present, emit a slow jump
	inc b
	djnz decode_jump_bank_switch_fixed
	call lookup_code_link_internal
	jp.sis decode_block_bridge_return

decode_jump_maybe_overlap:
	; Check if the opcode is a JR or JP
	sla b
	; Save the base index of the next region
	ld b,a
	; Check if the jump overlaps to the next region
	ld a,e
	adc a,1
	jr nc,decode_jump_no_overlap
	; Go ahead and move D to the next region
	inc d
	; Recover whether it was a JR or JP
	inc e
	cp e
	; Read the next byte, check if it overlapped later
	inc hl
	ld a,(hl)
	; Get the base address for the next region
	ld hl,z80codebase+mem_get_ptr_routines
	ld l,b
	inc l \ inc l
	ld hl,(hl)
	ld b,-3 ; Taken JR cycles (negative)
	jr z,decode_overlapped_jr
	; Get the second immediate byte (always in the next region)
	inc e
	add hl,de
	ld d,(hl)
	jr z,_
	; If the first byte overlapped, get it from the next region
	dec hl
	ld a,(hl)
_
	ld e,a
decode_slow_jump_for_jp:
	; Adjust to taken JP cycles (negative)
	djnz decode_slow_jump
decode_overlapped_jr:
	; Switch to next region unconditionally
	add hl,de
	; Get low byte of JR, offset from $xx01
	ld e,(hl)
	inc e
	; Adjust top byte of JR target for signed offset
	ld a,e
	cp $81 ; Low bytes of $81 and higher mean upper byte decreases
	jr nc,decode_overlapped_jr_backward
decode_slow_jump:
	; Get which memory region is being jumped to
	ld hl,z80codebase+mem_read_lut
	ld l,d
	ld a,(hl)
	; If the target memory region is the same as the jump instruction,
	; just emit a normal overlapped jump with no bank checking
	cp c
	jr z,decode_jump_bank_switch_fixed
decode_jump_bank_switch:
	cp rom_banked_get_ptr & $FF
	jr nz,decode_jump_bank_switch_fixed
	ld hl,do_rom_bank_jump
	ld a,(z80codebase+curr_rom_bank)
	jr decode_jump_bank_switch_continue
decode_overlapped_jr_backward:
	dec d
	; An overlapped JR jumping backward implicitly targets the same region
decode_jump_bank_switch_fixed:
	ld hl,do_overlapped_jump
	; Bank id is irrelevant, so left uninitialized
decode_jump_bank_switch_continue:
	ld c,a
	push bc
	 push hl
	  call lookup_code_link_internal
	 pop de
	pop bc
	sub b
	bit 1,b
	; If B is 0, -3, or -4, it represents actual jump cycles
	jp.sis z,decode_bank_switch_return
	; If B is -1 or -2, it represents jump cycles minus 1
	inc a
	jp.sis decode_bank_switch_return
	
decode_call_helper:
	push hl
	 GET_BASE_ADDR_FAST
	 add hl,de
	 ld a,(hl)
	 dec de
	 GET_BASE_ADDR_FAST
	 add hl,de
	 dec de
	 ld b,d
	 ld e,(hl)
	 ld d,a
	 ; Check if jumping from outside banked ROM to banked ROM
	 xor b
	 and $C0
	 jr z,_
	 ld a,d
	 sub $40
	 cp $40
_
	 ; Carry is reset if no banking
	 push af
	  ld a,b
	  rla
	  jr nc,_
	  call.il lookup_code_cached
	  scf
_
	  call nc,lookup_code
	  add a,6	; Taken call eats 6 cycles
	  ld c,a
	 pop af
	 ex (sp),ix
	 exx
	pop hl
	jp.sis decode_call_return
	
decode_rst_helper:
	ex af,af'
	exx
	ld e,a
	push de
	 push ix
	  exx
	  push hl
	   ld a,l
	   exx
	   add a,10*4-1
	   rra
	   ld l,a
	   ld h,dispatch_rst_00 >> 8
	   ld.s de,(hl)
	   call lookup_code
	   ex (sp),ix
	  pop hl
	  add a,4	; Taken rst eats 4 cycles
	  ld.s (ix-1),a
	  ld.s (ix+1),hl
	 pop ix
	pop de
	add a,e
	jp.sis decode_rst_return
	
banked_jump_mismatch_helper:
	push bc
	 push hl
	  ; Look up the old target
	  ld.s de,(ix+6)
	  ; Save the new bank index
	  ld c,a
	  ; Check the number of cycles taken by the jump
	  ; -4 is JP, -3 is JR, -2 is untaken JP, -1 is untaken JR or RET, 0 is a bridge
	  ld.s b,(ix+5)
	  ld a,b
	  or a
	  jr z,banked_jump_mismatch_resolved
	  add a,2
	  jr c,banked_jump_mismatch_untaken
	  inc de
	  GET_BASE_ADDR_FAST
	  add hl,de
	  inc de
	  ; Check whether it's JP or JR
	  inc a
	  jr z,banked_jump_mismatch_jr
	  ld a,(hl)
	  GET_BASE_ADDR_FAST
	  add hl,de
	  ld d,(hl)
	  ld e,a
	  jr banked_jump_mismatch_resolved
	  
banked_jump_mismatch_untaken:
	  ; Decrement to get the actual negative cycle count
	  djnz banked_jump_mismatch_resolved
	  
banked_jump_mismatch_jr:
	  ld a,(hl)
	  ld l,a
	  rla
	  sbc a,a
	  ld h,a
	  add.s hl,de
	  ex de,hl
banked_jump_mismatch_resolved:
	  push ix
	   push bc
	    call.il lookup_code_cached
	   pop de
	   ; Calculate total cycles
	   sub d
	   ld d,a
	   ex (sp),ix
	   ; Save new bank index / cycles
	   ld.s (ix+3),de
	  pop hl
	  ; Save new JIT target
	  ld.s (ix+1),hl
	 pop hl
	pop bc
	jp.sis banked_jump_mismatch_continue
	
	
banked_call_mismatch_helper:
	push bc
	 push ix
	  push hl
	   ld.s (hl),a  ; Update bank value
	   dec de
	   dec.s de
	   GET_BASE_ADDR_FAST
	   add hl,de
	   ld a,(hl)
	   inc hl
	   inc e
	   jr nz,_
	   inc d
	   GET_BASE_ADDR_FAST
	   add hl,de
_
	   ld d,(hl)
	   ld e,a
	   call.il lookup_code_cached
	   add a,6	; Taken call eats 6 cycles
	   ex (sp),ix
	   ; Update JIT target
	   exx
	  pop hl
	  ld.s (ix-9),hl  ; Update JIT target
	  exx
	  ; Update cycle count
	  lea hl,ix-3
	  ld.s (hl),a
	 pop ix
	pop bc
	pop.s de
	jp.sis banked_call_mismatch_continue
	
	
decode_halt_helper:
	exx
	ex af,af'
	ld e,a
	push de
	 push ix
	  ld.s de,(ix+3)
	  call lookup_code
	  ex (sp),ix
	 pop hl
	pop de
	jp.sis decode_halt_continue
	