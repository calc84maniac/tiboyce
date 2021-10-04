decode_jump_helper:
	ld bc,recompile_struct
	ld hl,mem_region_lut
	ld l,d
	jr c,decode_block_bridge_helper
	add ix,bc
	; Save original opcode in B
	ld b,a
	ld c,(hl)
	inc l
	ld a,(hl)
	dec h
	ld l,c
	cp l
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
	ld hl,mem_region_lut
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
	ld hl,mem_region_lut-1
	ld l,b
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
	ld hl,mem_region_lut
	ld l,d
	ld a,(hl)
	; If the target memory region is the same as the jump instruction,
	; just emit a normal overlapped jump with no bank checking
	cp c
	jr z,decode_jump_bank_switch_fixed
decode_jump_bank_switch:
	cp rom_bank_base & $FF
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
	
decode_rst_helper:
	ex af,af'
	exx
	ld c,a
	push bc
	 push hl
	  push ix
	   ; Get the offset of the RST dispatch
	   lea de,ix+(-(dispatch_rst_00 + $80) & $FF) - $80
	   ld d,0
	   ; Get a pointer to the corresponding JR offset
	   ld hl,z80codebase + do_rst_08 - 1
	   add hl,de
	   ld a,e
	   srl e
	   add hl,de
	   ; Get the RST target address
	   add a,a
	   ld e,a
	   ; Adjust the JR offset
	   ld a,(do_rst-1) & $FF
	   sub l
	   ld (hl),a
	   call lookup_code
	   ex (sp),ix
	  pop hl
	  ; Emit the cycle count
	  add a,4
	  ld.s (ix+3),a
	  ; Emit the jump target
	  ld.s (ix+1),hl
	 pop hl
	pop bc
	ld a,c
	jp.sis do_rst_decoded
	
decode_call_helper:
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
	xor b
	and $C0
	jr nz,decode_call_bank_switch
decode_call_common:
	ld a,b
	rla
	jr nc,_
	call.il lookup_code_cached
	scf
_
	call nc,lookup_code
_
	add a,6	; Taken call eats 6 cycles
	; Carry is reset
	ret.l
	
decode_call_flush:
	pop hl
	call prepare_flush
	xor a
	jr -_
	
decode_call_bank_switch:
	ld a,d
	sub $40
	cp $40
	jr nc,decode_call_common
	
	push de
	 ld hl,(z80codebase+memroutine_next)
	 ld de,-5
	 add hl,de	;Sets C flag
	
	 ; Bail if not enough room for trampoline
	 ex de,hl
	 ld hl,(recompile_struct_end)
	 ld hl,(hl)
	 sbc hl,de	;Carry is set
	 ex de,hl
	 ld (z80codebase+memroutine_next),hl
	 jr nc,decode_call_flush
	 ld de,ERROR_CATCHER
	 ld (hl),de
	 inc hl
	 inc hl
	 inc hl
	pop de
	; Code lookup may overwrite the trampoline area, so wait to write it out
	push hl
	 ld a,b
	 rla
	 jr nc,_
	 call.il lookup_code_cached
	 scf
_
	 call nc,lookup_code
	 sub -6	; Taken call eats 6 cycles
	 ex (sp),ix
	pop hl
	ld (ix),$C3
	ld (ix+1),hl  ;JIT target
	ld.sis hl,(curr_rom_bank-1)
	ld l,a
	ld.s (ix+3),hl  ;ROM bank and taken cycle count
	ld bc,do_rom_bank_call
	; Carry is set
	ret.l
	
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
	 push de
	  push hl
	   ld.s (ix+4),a  ; Update bank value
	   inc hl
	   ld.s de,(hl)
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
	   push ix
	    call.il lookup_code_cached
	    add a,6	; Taken call eats 6 cycles
	    ex (sp),ix
	   pop hl
	   ld.s (ix+3),a  ; Update cycle count
	   ld.s (ix+1),hl  ; Update JIT target
	  pop hl
	 pop de
	pop bc
	jp.sis banked_call_mismatch_continue
	
decode_intcache_helper:
	push bc
	 push de
	  lea de,ix+(-(dispatch_rst_00 + $80) & $FF) - $80
	  ld d,a
	  sla e
	  push ix
	   call lookup_code
	   ; Spend 5 cycles for interrupt dispatch overhead
	   add a,5
	   ex (sp),ix
	  pop hl
	 pop de
	pop bc
	ret.l
	
decode_halt_helper:
	push hl
	 ex af,af'
	 ld c,a
	 push bc
	  call lookup_code
	 pop bc
	 jp.sis decode_halt_continue
	
; Most emitted single-byte memory access instructions consist of RST_MEM
; followed inline by the opcode byte in question (and one padding byte).
;
; Each unique combination of an opcode byte and a memory region can receive
; its own memory access routine that incorporates both. This routine
; determines the routine associated with the instruction in question
; and returns it to be patched into a direct call.
;
; Opcode identifiers:
;   0-1 = ld (bc),a \ ld a,(bc)
;   2-3 = ld (de),a \ ld a,(de)
;   4-5 = ldi (hl),a \ ldi a,(hl)
;   6-7 = ldd (hl),a \ ldd a,(hl)
;   8-13,15 = ld r,(hl)
;   16-23 = op a,(hl)
;   24-29,31 = ld (hl),r
;   30 = ld (hl),nn
;   14 = unused
;
; Memory region identifiers:
;   0 = HRAM ($FF80 - $FFFE)
;   1 = Static ROM ($0000-$3FFF)
;   2 = OAM/MMIO/HRAM ($FE00-$FFFF), region 0 takes precedence
;   3 = Banked ROM ($4000-$7FFF)
;   4 = VRAM ($8000-$9FFF)
;   5 = Cart RAM ($A000-$BFFF)
;   6 = WRAM ($C000-$DFFF)
;   7 = WRAM Mirror ($E000-$FDFF)
;
; The memory access routines grow backwards in the JIT code region, and if
; the JIT code and the memory access routines overlap a flush must occur.
; If this case is detected, a pointer to a flush handler is returned instead.
;
; When a memory access routine is called with an address outside its assigned
; memory region, decode_mem is called with the new region and the direct call
; is patched again with the new memory access routine.
;
; Inputs:  IX = address following the RST_MEM instruction
;          BCDEHL = Game Boy BCDEHL
; Outputs: IX = address of the RST_MEM instruction
;          DE = address of the memory access routine, or flush_mem_handler
;          When flush_mem_handler is returned, Z is set for 2-byte instructions
; Destroys AF,HL
decode_mem_helper:
	dec ix
	
	; Get index 0-31
	ld.s a,(ix+1)
	sub $70
	cp 8
	jr c,++_
_
	add a,$70+$40
	rrca
	rrca
	rrca
	or $E0
_
	add a,24
	
	; Check for BC access
	cp 2
	jr nc,_
	ld d,b
	ld e,c
_
	; Check for HL access 
	cp 4
	jr c,_
	ex de,hl
_
	
	; Address is now in DE
	ld hl,memroutineLUT
	ld l,a
	
	; Get memory region, 0-7
	ld a,d
	cp $FE
	jr c,++_
	inc e	; Exclude $FFFF from region 0
	jr z,_
	dec e
_
	rrca
	and e
	rrca
	cpl
	and $40
	jr ++_
_
	and $E0
	jp m,_
	set 5,a
_
	or l
	ld l,a
	
	; Get routine address
	ld e,(hl)
	inc h
	ld d,(hl)
	ld a,d
	or e
	ret.l nz
	
	; Routine doesn't exist, let's generate it!
	push bc \ push hl
	 ex de,hl
	 ld.s d,(ix+1)
	 ; Save on the stack in case the original memory location is overwritten
	 push de
	 
	  ; Emit RET and possible post-increment/decrement
	  ld hl,(z80codebase+memroutine_next)
	  inc hl
	  inc hl
	  ld (hl),$C9	;RET
	  ld a,e
	  and $1C
	  cp 4
	  jr nz,++_
	  ld a,e
	  rra
	  ld d,$77	;LD (HL),A
	  jr nc,_
	  ld d,$7E	;LD A,(HL)
_
	  dec hl
	  rra
	  ld (hl),$23 ;INC HL
	  jr nc,_
	  ld (hl),$2B ;DEC HL
_
	  dec hl
	  
	  ; Get register pair index (BC=-4, DE=-2, HL=0)
	  ld a,e
	  and $1E
	  sub 4
	  jr c,_
	  xor a
_
	  ld c,a
	 
	  ld a,e
	  rlca
	  rlca
	  rlca
	  and 7
	  jr nz,memroutine_gen_not_high
	 
	  ld (hl),d   ;opcode
	  dec hl
	  ld (hl),$F1 ;POP AF
	  ld a,d
	  cp $76
	  jr nz,_
	  inc hl
	  ld (hl),$F1 ;POP AF
	  dec hl
	  ld (hl),$77 ;LD (HL),A
	  dec hl
	  ld (hl),$7D
	  dec hl
	  ld (hl),$FD ;LD A,IYL
_
	  dec hl
	  ld (hl),-10
	  dec hl
	  ld (hl),$20 ;JR NZ,$-8
	  dec hl
	  ld (hl),$3C ;INC A
	  dec hl
	  ld a,c
	  add a,$A4 ;AND B/D/H
	  ld (hl),a
	  dec hl
	  ld (hl),$9F ;SBC A,A
	  dec hl
	  ld (hl),$17 ;RLA
	  dec hl
	  add a,$7D-$A4 ;LD A,C/E/L
	  ld (hl),a
	 
memroutine_gen_end_push:
	  dec hl
	  ld (hl),$F5 ;PUSH AF
memroutine_gen_end:
	 pop af
	 push hl
	  dec hl
	  ld (hl),a
	  dec hl
	  ld (hl),RST_MEM
	  dec hl
	  dec hl
	  dec hl
	  ld de,ERROR_CATCHER
	  ld (hl),de
	  ld (z80codebase+memroutine_next),hl
	  ex de,hl
	  ld hl,(recompile_struct_end)
	  ld hl,(hl)
	  scf
	  sbc hl,de
	 pop de
	 jr nc,memroutine_gen_flush
_
	pop hl \ pop bc
	
	ld (hl),d
	dec h
	ld (hl),e
	cp $76
	ret.l
	
memroutine_gen_flush:
	 ld de,flush_mem_handler
	 jr -_
	
memroutine_gen_not_high:
	  ld b,a
	 
	  ; Get HL-based access instruction for BC/DE accesses
	  ld a,e
	  and $1C
	  jr nz,_
	  bit 0,e
	  ld d,$77	;LD (HL),A
	  jr z,_
	  ld d,$7E	;LD A,(HL)
_
	  ; Set carry if write instruction
	  ld a,d
	  sub $70
	  sub 8
	  
	  djnz memroutine_gen_not_cart0
	  jr c,memroutine_gen_write_cart
	  
	  call memroutine_gen_index
rom_start_smc_1 = $+1
	  ld de,0
	  ld (hl),de
	  dec hl
	  ld (hl),$21
	  dec hl
	  ld (hl),$DD
	  dec hl
	  ld (hl),$5B	;LD.LIL IX,ACTUAL_ROM_START
	  dec hl
	  ld (hl),-8
	  dec hl
	  ld (hl),$30	;JR NC,$-6
	  dec hl
	  ld (hl),$40
	  dec hl
	  ld (hl),$FE	;CP $40
	  dec hl
	  ld a,c
	  add a,$7C	;LD A,B/D/H
	  ld (hl),a
	  jr memroutine_gen_end_push
	  
memroutine_gen_write_hmem:
	  ld de,mem_write_hmem
	  ; B = NO_CYCLE_INFO - 1
memroutine_gen_write:
	  sra c
	  call nz,memroutine_gen_restore_hl
	  inc a
	  jr z,_
	  ld (hl),$F1	;POP AF
	  dec hl
_
	  ld (hl),d
	  dec hl
	  ld (hl),e
	  dec hl
	  ld (hl),$CD	;CALL routine
	  jr z,++_
	  dec hl
	  inc a
	  jr nz,_
	  ld (hl),$7D
	  dec hl
	  ld a,$FD-$7E	;LD A,IYL
_
	  add a,$7E	;LD A,r
	  ld (hl),a
	  dec hl
	  ld (hl),$F5	;PUSH AF
_
	  sra c
	  call nz,memroutine_gen_swap_hl
	  inc b
	  call nz,memroutine_gen_no_cycle_info
	  jp memroutine_gen_end
	  
memroutine_gen_write_cart:
	  ld de,mem_write_cart
	  djnz memroutine_gen_write ; Set B=-1, don't emit cycle info
	  
memroutine_gen_write_vram:
	  ld de,mem_write_vram
	  ld b,(NO_RESCHEDULE | NO_CYCLE_INFO) - 1
	  jr memroutine_gen_write
	  
memroutine_gen_not_cart0:
	  djnz memroutine_gen_not_hmem
	  jr c,memroutine_gen_write_hmem
	  
	  sra c
	  call nz,memroutine_gen_restore_hl
	  ld (hl),d	;op r,(HL)
	  dec hl
	  ld (hl),mem_update_hmem >> 8
	  dec hl
	  ld (hl),mem_update_hmem & $FF
	  dec hl
	  ld (hl),$CD	;CALL mem_update_hmem
	  sra c
	  call nz,memroutine_gen_swap_hl
	  ld b,NO_CYCLE_INFO | NO_RESCHEDULE
	  call memroutine_gen_no_cycle_info
	  jp memroutine_gen_end
	  
memroutine_gen_not_cart_bank:
	  djnz memroutine_gen_not_vram
	  jr c,memroutine_gen_write_vram
	  
	  call memroutine_gen_index
	  ld de,vram_base
	  ld (hl),de
	  dec hl
	  ld (hl),$21
	  dec hl
	  ld (hl),$DD
	  dec hl
	  ld (hl),$5B	;LD.LIL IX,vram_base
	  ex de,hl
	  ld hl,-9
	  add hl,de
	  ex de,hl
	  dec hl
	  ld (hl),d
	  dec hl
	  ld (hl),e
	  dec hl
	  ld (hl),$E2	;JP PO,$-6
	  dec hl
	  ld (hl),$20
	  dec hl
	  ld (hl),$D6	;SUB $20
	  dec hl
	  ld a,c
	  add a,$7C	;LD A,B/D/H
	  ld (hl),a
	  jp memroutine_gen_end_push
	  
memroutine_gen_not_hmem:
	  djnz memroutine_gen_not_cart_bank
	  jr c,memroutine_gen_write_cart
	  
	  call memroutine_gen_index
	  ld de,rom_bank_base
	  ld (hl),de
	  dec hl
	  ld (hl),$2A
	  dec hl
	  ld (hl),$DD
	  dec hl
	  ld (hl),$5B	;LD.LIL IX,(rom_bank_base)
	  ex de,hl
	  ld hl,-9
	  add hl,de
	  ex de,hl
	  dec hl
	  ld (hl),d
	  dec hl
	  ld (hl),e
	  dec hl
	  ld (hl),$E2	;JP PO,$-6
	  dec hl
	  ld (hl),$40
	  dec hl
	  ld (hl),$C6	;ADD A,$40
	  dec hl
	  ld a,c
	  add a,$7C	;LD A,B/D/H
	  ld (hl),a
	  jp memroutine_gen_end_push
	  
memroutine_gen_not_vram:
	  djnz memroutine_gen_not_cram
	  
	  sbc a,a
memroutine_rtc_smc_1 = $+1
	  and 0	; 5 when RTC bank selected
	  call memroutine_gen_index_offset
	  ld de,cram_bank_base
	  ld (hl),de
memroutine_rtc_smc_2 = $
	  jr _   ; JR C when RTC bank selected
	  ld de,4
	  cp 1
	  push hl
	   adc hl,de
	   ld a,(hl)
	   xor $DD ^ $FE	;ADD.L IX,rr vs CP.L nn
	   ld (hl),a
	  pop hl
_
	  dec hl
	  ld (hl),$2A
	  dec hl
	  ld (hl),$DD
	  dec hl
	  ld (hl),$5B	;LD.LIL IX,(cram_bank_base)
	  dec hl
	  ld (hl),-10
	  dec hl
	  ld (hl),$30	;JR NC,$-8
	  dec hl
	  ld (hl),$20
	  dec hl
	  ld (hl),$FE	;CP $20
	  dec hl
	  ld (hl),$A0
	  dec hl
	  ld (hl),$D6	;SUB $A0
	  dec hl
	  ld a,c
	  add a,$7C	;LD A,B/D/H
	  ld (hl),a
	  jp memroutine_gen_end_push
	  
memroutine_gen_not_cram:
	  ;We're in RAM, cool!
	  call memroutine_gen_index
	  ;Mirrored RAM
	  ld de,wram_base-$2000
	  ld a,$1E
	  djnz _
	  ;Unmirrored RAM
	  ld de,wram_base
	  ld a,$20
_
	  ld (hl),de
	  dec hl
	  ld (hl),$21
	  dec hl
	  ld (hl),$DD
	  dec hl
	  ld (hl),$5B	;LD.LIL IX,wram_base
	  dec hl
	  ld (hl),-10
	  dec hl
	  ld (hl),$30	;JR NC,$-8
	  dec hl
	  ld (hl),a
	  dec hl
	  ld (hl),$FE	;CP $1E / $20
	  dec hl
	  cpl
	  and $E0
	  ld (hl),a
	  dec hl
	  ld (hl),$D6	;SUB $E0 / $C0
	  dec hl
	  ld a,c
	  add a,$7C	;LD A,B/D/H
	  ld (hl),a
	  jp memroutine_gen_end_push
	
memroutine_gen_index:
	xor a
memroutine_gen_index_offset:
	ld (hl),a	;offset
	dec hl
	ld (hl),d	;opcode
	ld a,d
	cp $76
	jr nz,_
	inc hl
	ld a,(hl)
	ld (hl),$F1	;POP AF
	dec hl
	ld (hl),a	;offset
	dec hl
	ld (hl),$77	;LD (IX),A
_
	dec hl
	ld (hl),$DD	;IX prefix
	dec hl
	ld (hl),$5B	;.LIL prefix
	dec hl
	ld (hl),$F1	;POP AF
	jr nz,_
	ld (hl),$7D
	dec hl
	ld (hl),$FD	;LD A,IYL
_
	dec hl
	ld a,c
	or a
	jr nz,_
	ld (hl),$EB	;EX DE,HL	(if accessing HL)
	dec hl
	ld a,-2
_
	add a,a
	add a,a
	add a,a
	add a,$29
	ld (hl),a
	dec hl
	ld (hl),$DD
	dec hl
	ld (hl),$5B	;ADD.LIL IX,BC/DE/DE
	dec hl
	ld a,c
	or a
	jr nz,_
	ld (hl),$EB
	dec hl
_
	dec hl
	dec hl
	ret
	
memroutine_gen_swap_hl:
	dec hl
	ld (hl),$EB	;EX DE,HL
	ret c
	ld (hl),$60	;LD H,B
	dec hl
	ld (hl),$69	;LD L,C
	dec hl
	ld (hl),$E5	;PUSH HL
	ret
	
memroutine_gen_restore_hl:
	ld (hl),$EB	;EX DE,HL
	dec hl
	ret pe
	inc hl
	ld (hl),$E1	;POP HL
	dec hl
	ret
	
memroutine_gen_no_cycle_info:
	dec hl
	ld (hl),b
	dec hl
	ld (hl),$2E
	dec hl
	ld (hl),$DD ;LD IXL,NO_CYCLE_INFO[ | NO_RESCHEDULE]
	ret