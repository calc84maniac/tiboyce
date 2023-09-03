	.assume adl=0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Stack pointer access routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; ADD HL,SP
ophandler39:
	push de
	 ; The starting Game Boy address of the current stack window, minus $7F
stack_window_base = $+1
	 ld hl,0
	 ; Get the current stack window offset and add it
	 ld e,iyl
	 ld d,0
	 add hl,de
	pop de
	add hl,de
	ex de,hl
	ret
	
	; LD HL,SP+0
ophandlerF8_zero:
	; Get the value of SP in DE
	ld hl,(stack_window_base)
	ld d,e
	ld e,iyl
	add hl,de
	ex de,hl
	; Reset all flags
	ld hl,i
	ret
	
	; LD HL,SP-nn
ophandlerF8_negative:
	ld d,a
	; Get the value of SP minus 256 in HL
	; This transforms it into an unsigned addition
	ld hl,(stack_window_base)
	ld a,l
	add a,iyl
	jr c,_
	dec h
	jr _
	
	; LD HL,SP+nn
ophandlerF8_positive:
	ld d,a
	; Get the value of SP in HL
	ld hl,(stack_window_base)
	ld a,l
	add a,iyl
	jr nc,_
	inc h
_
	; Add the positive offset and set N/H/C flags
	add a,e
	ld l,a
	ld a,d
	; Put result in DE
	ex de,hl
	; If no carry, we are guaranteed NZ because we added at least one
	ret nc
	; Increment D without destroying flags
	ex af,af'
	inc d
	ex af,af'
	; If NZ, flags are correct
	ret nz
	; Recalculate in a way that generates the same carries and a result of 1.
	xor a
ophandlerE8_finish_z:
	sub l ; Sets carry since the offset is known to be non-zero.
	adc a,l
	ld a,h
	ret
	
	; LD [nnnn],SP
ophandler08_fast:
	ex af,af'
	ld b,d
	ld c,a
	ex de,hl
	call try_get_mem_readwrite_ptr_swapped
	ex af,af'
	; Calculate the real value of SP
	ld de,(stack_window_base)
	ld a,e
	add a,iyl
	jr nc,_
	inc d
_
	; Write to the memory addresses
	ld.l (hl),a
	inc.l hl
	ld.l (hl),d
	ld d,b
	ld a,c
	ex af,af'
	exx
	ret
	
	; LD [FFnn],sp
ophandler08_hram:
	ex af,af'
	ld e,a
	ld bc,(stack_window_base)
	ld a,c
	add a,iyl
	ld c,a
	jr nc,_
	inc b
_
	ld (hl),bc
	ld a,e
	ex af,af'
	exx
	ret
	
	; LD [nnnn],SP
ophandler08_slow:
	; Save the GB address provided by the trampoline
	ld (generic_write_gb_address),hl
	; Get the cycle offset and write address
	pop hl
	ld e,(hl)
	inc hl
	ld bc,(hl)
	inc hl
	inc hl
	push hl
	; Save the JIT address
	ld (generic_write_jit_address),hl
	push af
	 ; Calculate and save the end-of-instruction offset
	 ld a,-2
	 sub e
	 ld (generic_write_instr_cycle_offset),a
	 ; Calculate the real value of SP
	 ld hl,(stack_window_base)
	 ld a,l
	 add a,iyl
	 jr nc,_
	 inc h
_
	 ; Write to the memory addresses
	 push bc
	  ld l,e
	  push hl
	   call write_mem_any
	  pop hl
	  ld a,h
	  ld e,l
	 pop bc
	 ; Increment the address and cycle offset
	 inc e
	 inc bc
	 call write_mem_any
	pop af
	exx
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Stack adjustment routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; ADD SP,-nn
ophandlerE8_negative:
	ld h,a
	; Add the negative offset to IYL and check for a bounds overflow
	ld a,iyl
	add a,l
	ld iyl,a
	; Results between $7F and $FE are valid
	inc a
	call p,shift_stack_window_lower_preserved_a	
ophandlerE8_finish:
	; Get the current LSB of the stack pointer
	ld a,(stack_window_base)
	add a,iyl
	jr z,ophandlerE8_finish_z
	; Subtract the offset and re-add to get half-carry and carry flags
	sub l
	add a,l
	ld a,h
	ret

	; ADD SP,+nn
ophandlerE8_positive:
	ld h,a
	; Add the offset to IYL and check for a bounds overflow
	ld a,iyl
	add a,l
	ld iyl,a
	; Results between $80 and $FF are valid
	jr nc,ophandlerE8_finish
	call shift_stack_window_higher_preserved_a
	jr ophandlerE8_finish

	; INC SP
ophandler33:
	ex af,af'
	inc iyl
	; Results between $80 and $FF are valid
	ret m
	; Inputs:  IYL=out-of-bounds stack offset
	; Outputs: IY and stack access SMC are adjusted
	;          H = Input value of A
	; Destroys: F, BC', E', HL'
shift_stack_window_higher:
	ld h,a
shift_stack_window_higher_preserved_a:
	exx
#ifdef FASTLOG
	ld b,iyl
	push bc
	inc sp
	ld hl,(stack_window_base)
	push hl
	FASTLOG_EVENT_Z80(SHIFT_STACK_HIGHER, 7)
	dec sp \ dec sp \ dec sp \ dec sp
#endif
	ld hl,stack_window_base
	; Attempt shifting the stack window by 64 bytes
	ld a,iyl
	sub $40
	jr nc,shift_stack_window_higher_2
	ld e,a
	ld a,(hl)
	; Check for moving forward from $80, causing a new page overlap
	dec a
	jr z,shift_stack_window_higher_check_overlap
	add a,$41
	ld (hl),a
	jr nc,shift_stack_window_higher_finish
	; Increment the MSB of the window base
	inc hl
	inc (hl)
shift_stack_window_higher_finish:
	ld hl,do_pop_any_ptr_offset_smc
	ld a,(hl)
	xor $80
	ld iyl,a
	sub $40
	lea.l iy,iy+$40
	or a
shift_stack_window_higher_any_finish:
	ld (hl),a
	ld iyl,e
	jp apply_stack_offset_smc
	
shift_stack_window_higher_2:
	; Shift the stack window by 128 bytes
	sub $40
	ld e,a
	ld a,(hl)
	add a,$80
	ld (hl),a
	inc hl
	jr nc,_
	; Increment the MSB of the window base
	inc (hl)
_
	; Check for moving forward from $40 or $80, causing a new page overlap
	; This means a transition from $C1 to $41 or $01 to $81, so check parity
	or a
	jp pe,shift_stack_window_higher_2_check_overlap
shift_stack_window_higher_2_finish:
	ld hl,do_pop_any_ptr_offset_smc
	ld bc,$0080
	ld a,(hl)
	xor c
	ld iyl,a
	add.l iy,bc
	jr shift_stack_window_higher_any_finish
	
shift_stack_window_higher_check_overlap:
	ld (hl),$41
	inc hl
	; Compare the current and next stack regions
	ld l,(hl)
	ld h,mem_read_lut >> 8
	ld a,(hl)
	inc l
	cp (hl)
	jr z,shift_stack_window_higher_finish
#ifdef SHADOW_STACK
	; Check and see if we want to try shifting the shadow stack
	cp shadow_stack_get_ptr & $FF
	jr z,shift_shadow_stack_higher
#endif
shift_stack_window_higher_overlap:
	ld b,l
	ld a,(stack_window_base)
	add a,e
	ld c,a
	exx
	ld a,h
	exx
	scf
	jp set_gb_stack
	
shift_stack_window_higher_2_check_overlap:
	; Compare the current and next stack regions
	ld l,(hl)
	ld h,mem_read_lut >> 8
	ld a,(hl)
	inc l
	cp (hl)
	jr z,shift_stack_window_higher_2_finish
#ifdef SHADOW_STACK
	; Check and see if we want to try shifting the shadow stack
	cp shadow_stack_get_ptr & $FF
	jr nz,shift_stack_window_higher_overlap
shift_shadow_stack_higher:
	FIXME
#else
	jr shift_stack_window_higher_overlap
#endif
	
	; DEC SP
ophandler3B:
	ex af,af'
	dec iyl
	; Results between $7F and $FE are valid
	ret m
	ret pe
	; Inputs:  IYL=out-of-bounds stack offset
	; Outputs: IY and stack access SMC are adjusted
	;          H = Input value of A
	; Destroys: F, BC', E', HL'
shift_stack_window_lower:
	ld h,a
shift_stack_window_lower_preserved_a:
	exx
shift_stack_window_lower_preserved_a_swapped:
#ifdef FASTLOG
	ld b,iyl
	push bc
	inc sp
	ld hl,(stack_window_base)
	push hl
	FASTLOG_EVENT_Z80(SHIFT_STACK_LOWER, 7)
	dec sp \ dec sp \ dec sp \ dec sp
#endif
	ld hl,stack_window_base
	; Attempt shifting the stack window by 64 bytes
	ld a,iyl
	add a,$41
	jp po,shift_stack_window_lower_2
	dec a
	ld e,a
	ld a,(hl)
	; Check for moving backard from $00, causing a new page overlap
	sub $40
	ld (hl),a
	jp pe,shift_stack_window_lower_check_overlap
	jr nc,shift_stack_window_lower_finish
	inc hl
	; Special case for HRAM, force a region transition immediately
	ld c,(hl)
	inc c
	jr z,shift_stack_window_lower_io
	; Decrement the MSB of the window base
	dec (hl)
shift_stack_window_lower_finish:
	ld hl,do_pop_any_ptr_offset_smc
	ld a,(hl)
	xor $80
	ld iyl,a
	add a,$40
	lea.l iy,iy-$40
	or a
shift_stack_window_lower_any_finish:
	ld (hl),a
	ld iyl,e
	jp apply_stack_offset_smc

shift_stack_window_lower_2:
	; Shift the stack window by 128 bytes
	add a,$3F
	ld e,a
	ld a,(hl)
	; Check for moving backward from $00 or $40, causing a new page overlap
	sub $80
	ld (hl),a
	jr nc,shift_stack_window_lower_2_check_overlap
	inc hl
	; Special case for HRAM, force a region transition immediately
	ld c,(hl)
	inc c
	jr z,shift_stack_window_lower_io
	; Decrement the MSB of the window base
	dec (hl)
shift_stack_window_lower_2_finish:
	ld hl,do_pop_any_ptr_offset_smc
	ld a,(hl)
	xor $80
	ld iyl,a
	lea.l iy,iy-$80
	jr shift_stack_window_lower_any_finish

shift_stack_window_lower_check_overlap:
	; Compare the next and current pages
	ld b,mem_read_lut >> 8
	inc hl
	ld c,(hl)
	ld a,(bc)
	ld l,a
	inc c
	ld a,(bc)
	cp l
	jr z,shift_stack_window_lower_finish
#ifdef SHADOW_STACK
	; Check and see if we want to try shifting the shadow stack
	ld a,l
	cp shadow_stack_get_ptr & $FF
	jr z,shift_shadow_stack_lower
#endif
shift_stack_window_lower_overlap:
	ld a,(stack_window_base)
	add a,e
	jr c,++_
_
	dec c
_
	ld b,c
	ld c,a
	exx
	ld a,h
	exx
	scf
	jp set_gb_stack

shift_stack_window_lower_io:
	add a,e
	jr --_

shift_stack_window_lower_2_check_overlap:
	; Compare the next and current pages
	ld b,mem_read_lut >> 8
	inc hl
	ld c,(hl)
	ld a,(bc)
	ld l,a
	inc c
	ld a,(bc)
	cp l
	jr z,shift_stack_window_lower_finish
#ifdef SHADOW_STACK
	; Check and see if we want to try shifting the shadow stack
	ld a,l
	cp shadow_stack_get_ptr & $FF
	jr nz,shift_stack_window_lower_overlap
shift_shadow_stack_lower:
	FIXME
#else
	jr shift_stack_window_lower_overlap
#endif

	; LD SP,HL
ophandlerF9:
	push de
	 exx
	pop bc
	; LD SP,nnnn
ophandler31:
; Get a literal 24-bit pointer to the Game Boy stack.
; Does not use a traditional call/return, must be jumped to directly.
;
; This routine is invoked whenever SP is set to a new value which may be outside
; its current bank. If the bank has changed, any relevant stack routines are modified.
;
; Inputs:  BC' = 16-bit Game Boy SP
;          BCDEHL' have been swapped
; Outputs: IY = 24-bit literal SP
;          BCDEHL' have been unswapped
;          SMC applied to stack operations
; Destroys: HL, BC', E', HL'
set_gb_stack:
	push af
#ifdef FASTLOG
	 push bc
	 FASTLOG_EVENT_Z80(SET_STACK, 2)
#endif
	 ; Determine the new stack window start address.
	 ; Aligns to 128 bytes by default for simplicity.
	 ; If the new stack pointer is on a 128-byte boundary, the preceding
	 ; window is selected (assuming the bottom of the stack is being set).
	 ld e,c
	 dec bc
	 ld a,c
	 ld hl,$FF80
	 and l
	 inc a
	 ld c,a
	 add hl,bc
	 ld (stack_window_base),hl
	 ; Get the low byte relative to the window start
	 ld a,e
	 sub l
	 ld e,a
	 ; Get the memory region directly preceding the stack pointer
	 ld.lil hl,z80codebase+mem_read_lut
	 ld l,b
	 ld l,(hl)
	 inc h ;mem_get_ptr_routines
	 inc l \ inc l
	 ld.l iy,(hl)
	 ; Calculate the 24-bit stack window base
	 add.l iy,bc
	 ; Check if the type of stack region changed, to apply SMC
	 ld a,l
	 ; Special-case the $FF00 area
	 inc b
	 jr nz,_
	 ; Check for I/O (setting A to io_region_base=0 if so)
	 cp c
	 sbc a,a
	 and l
_
curr_gb_stack_region = $+1
	 cp $FF
	 jr nz,set_gb_stack_region
set_gb_stack_region_finish:
	 ; Get the new offset from the window base
	 ld a,iyl
	 xor $80
	 ; Set the low byte of the stack index
	 ld iyl,e
	 ; Apply offset SMC if the offset has changed
	 ld hl,do_pop_any_ptr_offset_smc
	 cp (hl)
	 ld (hl),a
	 jp nz,pop_apply_stack_offset_smc
	 exx
	pop af
	ret

	 ; Apply SMC as needed to stack accesses when SP changes banks.
set_gb_stack_region:
	 ld (curr_gb_stack_region),a
	 ld bc,$2525 ; DEC H \ DEC H
	 ; Check for read-only vs. read-write
	 cp cram_bank_base & $FF
	 jr c,set_gb_stack_region_read_only
	 ; Special-case cartridge RAM based on current mapping
	 jr z,set_gb_stack_region_cram_rtc
	 ; Check for long vs. short pointer
	 cp shadow_stack_base & $FF
	 jr c,set_gb_stack_region_long_ptr
	 ; Set push handlers
	 ld a,do_push_bc_short_ptr - push_routines_start
	 ld (ophandlerC5_smc),a
	 ld a,do_push_de_short_ptr - push_routines_start
	 ld (ophandlerD5_smc),a
	 ld a,do_push_hl_short_ptr - push_routines_start
	 ld (ophandlerE5_smc),a
	 ld a,do_push_any_short_ptr - push_routines_start
	 ld (ophandlerF5_smc),a
	 ld hl,$FD49 ;LD.LIS (IY),...
	 ld (do_call_shadow_stack_smc),hl
	 ld (do_rst_shadow_stack_smc),hl
	 ld (do_push_for_interrupt_shadow_stack_smc),hl
	 .db $21
	  pop.l hl
	 ld (callstack_ret_shadow_stack_smc),hl
	 ld (callstack_ret_cond_shadow_stack_smc),hl
	 ld a,apply_stack_offset_smc_short_ptr - (apply_stack_offset_smc_offset_smc+1)
	 ld l,pop_short_ptr_src - pop_routines_start
	 jr set_gb_stack_region_apply_pop_smc
	 
set_gb_stack_region_read_only:
	 ; Check for I/O region
	 or a
set_gb_stack_region_io:
	 ; Set push handlers
	 ld a,do_push_bc_slow - push_routines_start
	 ld (ophandlerC5_smc),a
	 ld a,do_push_de_slow - push_routines_start
	 ld (ophandlerD5_smc),a
	 ld a,do_push_hl_slow - push_routines_start
	 ld (ophandlerE5_smc),a
	 ld a,do_push_instr_slow - push_routines_start
	 ld (ophandlerF5_smc),a
	 ld hl,$18 | ((do_call_no_shadow_stack - (do_call_shadow_stack_smc+2)) << 8)
	 ld (do_call_shadow_stack_smc),hl
	 ld h,do_rst_no_shadow_stack - (do_rst_shadow_stack_smc+2)
	 ld (do_rst_shadow_stack_smc),hl
	 ld h,do_push_for_interrupt_no_shadow_stack - (do_push_for_interrupt_shadow_stack_smc+2)
	 ld (do_push_for_interrupt_shadow_stack_smc),hl
	 ld h,callstack_ret_no_shadow_stack - (callstack_ret_shadow_stack_smc+2)
	 ld (callstack_ret_shadow_stack_smc),hl
	 ld h,callstack_ret_cond_no_shadow_stack - (callstack_ret_cond_shadow_stack_smc+2)
	 ld (callstack_ret_cond_shadow_stack_smc),hl
	 ld a,apply_stack_offset_smc_read_only - (apply_stack_offset_smc_offset_smc+1)
	 jr nz,set_gb_stack_long_ptr_finish
	 ld a,apply_stack_offset_smc_slow - (apply_stack_offset_smc_offset_smc+1)
	 ld l,pop_slow_src - pop_routines_start
	 jr set_gb_stack_region_apply_pop_smc
	 
set_gb_stack_region_cram_rtc:
	 ; Check if an RTC register is mapped (or MBC2 RAM is present)
	 ld a,(cram_banked_get_ptr_rtc_smc)
	 rra ;JR vs. ADD.L
	 jr nc,set_gb_stack_region_io ; Note: Z is already set
set_gb_stack_region_cram:
	 ; Check if cart RAM is currently protected
	 ld a,(cram_banked_get_ptr_protect_smc)
	 cp $18 ;JR
	 jr z,set_gb_stack_region_io
set_gb_stack_region_long_ptr:
	 cp wram_bank_base & $FF
	 jr nz,_
	 ld bc,$18 | ((writeSVBK_stack - (writeSVBK_stack_smc+2)) << 8)
_
	 ; Set push handlers
	 ld a,do_push_bc_long_ptr - push_routines_start
	 ld (ophandlerC5_smc),a
	 ld a,do_push_de_long_ptr - push_routines_start
	 ld (ophandlerD5_smc),a
	 ld a,do_push_hl_long_ptr - push_routines_start
	 ld (ophandlerE5_smc),a
	 ld a,do_push_any_long_ptr - push_routines_start
	 ld (ophandlerF5_smc),a
#ifdef SHADOW_STACK
	 ld hl,$18 | ((do_call_set_shadow_stack - (do_call_shadow_stack_smc+2)) << 8)
	 ld (do_call_shadow_stack_smc),hl
	 ld h,do_rst_set_shadow_stack - (do_rst_shadow_stack_smc+2)
	 ld (do_rst_shadow_stack_smc),hl
	 ld h,do_push_for_interrupt_set_shadow_stack - (do_push_for_interrupt_shadow_stack_smc+2)
	 ld (do_push_for_interrupt_shadow_stack_smc),hl
	 ld h,callstack_ret_set_shadow_stack - (callstack_ret_shadow_stack_smc+2)
	 ld (callstack_ret_shadow_stack_smc),hl
	 ld h,callstack_ret_cond_set_shadow_stack - (callstack_ret_cond_shadow_stack_smc+2)
	 ld (callstack_ret_cond_shadow_stack_smc),hl
#else
	 ld hl,$FD49 ;LD.LIS (IY),...
	 ld (do_call_shadow_stack_smc),hl
	 ld (do_rst_shadow_stack_smc),hl
	 ld (do_push_for_interrupt_shadow_stack_smc),hl
	 .db $21
	  pop.l hl
	 ld (callstack_ret_shadow_stack_smc),hl
	 ld (callstack_ret_cond_shadow_stack_smc),hl
#endif
	 ld a,apply_stack_offset_smc_long_ptr - (apply_stack_offset_smc_offset_smc+1)
set_gb_stack_long_ptr_finish:
	 ld l,pop_long_ptr_src - pop_routines_start
set_gb_stack_region_apply_pop_smc:
	 ; Apply WRAM banking SMC
	 ld (writeSVBK_stack_smc),bc
	 ; Select the appropriate stack offset routine
	 ld (apply_stack_offset_smc_offset_smc),a
	 ; Get the new offset from the window base
	 ld a,iyl
	 xor $80
	 ld (do_pop_any_ptr_offset_smc),a
	 ; Set the low byte of the stack index
	 ld iyl,e
	 ; Apply pop SMC
	 push de
	  ld de,ophandlerC1_smc
	  ld h,d
	  ld bc,ophandlerC1_smc_size
	  ldir
	  ld e,ophandlerD1_smc & $FF
	  ld c,ophandlerD1_smc_size
	  ldir
	  ld e,ophandlerE1_smc & $FF
	  ld c,ophandlerE1_smc_size
	  ldir
	 pop de
	 ld hl,(hl)
	 ld (ophandlerF1_smc),hl
pop_apply_stack_offset_smc:
	 ld l,a
	pop af
	exx
	ld h,a
	exx
	ld a,l
apply_stack_offset_smc:
#ifdef FASTLOG
	push af
	ld b,a
	ld c,iyl
	push bc
	ld hl,(stack_window_base)
	push hl
	FASTLOG_EVENT_Z80(APPLY_STACK_OFFSET, 4)
	pop af
#endif
	exx
apply_stack_offset_smc_offset_smc = $+1
	jr apply_stack_offset_smc_short_ptr

apply_stack_offset_smc_short_ptr:
	ld (do_push_short_ptr_offset_smc_1),a
	ld (do_push_short_ptr_offset_smc_2),a
	ld (do_push_short_ptr_offset_smc_3),a
	ld (do_push_short_ptr_offset_smc_4),a
	ld (do_pop_short_ptr_offset_smc_1),a
	ld (do_pop_short_ptr_offset_smc_2),a
	ld (do_pop_short_ptr_offset_smc_3),a
	ld (callstack_ret_pop_offset_smc),a
	ld (callstack_ret_cond_pop_offset_smc),a
	ld (do_call_push_offset_smc_1),a
	ld (do_call_push_offset_smc_2),a
	ld (do_rst_push_offset_smc_1),a
	ld (do_rst_push_offset_smc_2),a
	ld (trigger_interrupt_push_offset_smc_1),a
	ld (trigger_interrupt_push_offset_smc_2),a
apply_stack_offset_smc_slow:
	ld a,h
	ret

apply_stack_offset_smc_long_ptr:
	ld (do_push_long_ptr_offset_smc_1),a
	ld (do_push_long_ptr_offset_smc_2),a
	ld (do_push_long_ptr_offset_smc_3),a
	ld (do_push_long_ptr_offset_smc_4),a
	ld (do_push_long_ptr_offset_smc_5),a
	ld (do_push_long_ptr_offset_smc_6),a
	ld (callstack_ret_pop_offset_smc),a
	ld (callstack_ret_cond_pop_offset_smc),a
	ld (do_call_push_offset_smc_1),a
	ld (do_call_push_offset_smc_2),a
	ld (do_rst_push_offset_smc_1),a
	ld (do_rst_push_offset_smc_2),a
	ld (trigger_interrupt_push_offset_smc_1),a
	ld (trigger_interrupt_push_offset_smc_2),a
apply_stack_offset_smc_read_only:
	ld (do_pop_long_ptr_offset_smc_1),a
	ld (do_pop_long_ptr_offset_smc_2),a
	ld (do_pop_long_ptr_offset_smc_3),a
	ld a,h
	ret

#ifdef SHADOW_STACK
set_shadow_stack_rollback:
	dec iyl
	dec iyl
set_shadow_stack:
	push hl
	 exx
	 push af
	  push bc
	   push de
	    push hl
	     ; Get the current shadow stack region and base
curr_shadow_stack_region = $+1
	     ld de,$FF00 | (hmem_get_ptr & $FF)
	     ; Get the stack pointer in IY
	     ld bc,(stack_window_base)
	     ld iyh,0
	     add iy,bc
	     ; Get the MSB preceding the end of the stack window
	     dec bc
	     dec bc
	     ld a,(curr_gb_stack_region)
	     ld.lil hl,z80codebase+mem_read_lut+2
	     sub l ;2
	     ld c,a
_
	     ld (curr_shadow_stack_region),bc
	     ; Check if the first 256 bytes are in the current stack region.
	     ; The second 256 bytes are already guaranteed to be in that region.
	     ld l,b
	     sub (hl)
	     jp.lil z,set_shadow_stack_contiguous_helper
	     ; Start the shadow stack 256 bytes higher.
	     ; All eligible regions are at least 512 bytes large so this will
	     ; force the shadow stack into a contiguous region.
	     ; In the case the following 256 bytes are already in the shadow
	     ; stack, for now don't bother optimizing the stack move.
	     inc b
	     ld a,c
	     jr -_
	
set_shadow_stack_finish:
	     call set_gb_stack
	     exx
	    pop hl
	   pop de
	  pop bc
	 pop af
	 exx
	pop hl
	ret
#endif

do_push_for_call_slow_swap:
	exx
	; Input: E = cycles for specific call (RST, CALL, interrupt)
	;        HL = value to push, DA = cycle counter, AFBCDEHL' are swapped
	; Output: AFBCDEHL' are unswapped, cycle counter updated if event triggered
do_push_for_call_slow:
	; Add cycles for the specific call type
	add a,e
#ifdef DEBUG
	; Prevent assertions when handling events during a memory access
	ld (event_cycle_count),a
#endif
	jr nc,_
	inc d
_
	ex af,af'
	push af
	 ; Save the call cycle offset
	 push de
	  push hl
	   ld a,h
	   ; Ensure a triggered event won't clobber any code
	   ld hl,generic_write_instr_cycle_offset
	   ld (generic_write_jit_address),hl
#ifdef DEBUG
	   ; Be consistent with debug assertions...
	   inc d
	   dec d
	   jr z,_
	   ld hl,event_debug_address
_
#endif
	   ld (event_address),hl
	   ld e,-2 ; First call push is two cycles before the end
	   ld hl,(stack_window_base)
	   ld c,iyl
	   ld b,0
	   add hl,bc
	   ld b,h
	   ld c,l
do_push_instr_get_cycle_offset_return:
	   push de
	    push bc
	     call write_mem_any
	    pop bc
	   pop hl
	   ; Increment cycle offset, decrement stack pointer and address
	   ld e,l
	   inc e
	   dec iyl
	   dec bc
	  pop hl
	  ld a,l
	  call write_mem_any
	 pop hl
	pop af
	; Subtract the call cycle offset again
	ex af,af'
	sub l
	jr nc,_
	dec d
_
	ex af,af'
	exx
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Pop routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.echo "Wasted space before pop routines: ", (-$)&$FF
	.block (-$)&$FF
pop_routines_start:

	; POP BC
ophandlerC1:
	ex af,af'
ophandlerC1_smc = $
do_pop_short_ptr_offset_smc_1 = $+2
	;ld ix,(iy)
do_pop_long_ptr_offset_smc_1 = $+3
	ld.l ix,(iy)
	inc iyl
	inc iyl
ophandlerC1_smc_size = $-ophandlerC1_smc
	ret m
	call shift_stack_window_higher
	ret nc
do_pop_bc_slow:
	exx
	ld c,$C1 ;POP BC
	call do_pop_instr_slow
	push hl
	pop ix
	ret
	
ophandlerC1_slow:
	call p,shift_stack_window_higher
	jr do_pop_bc_slow
	
	; POP DE
ophandlerD1:
	ex af,af'
ophandlerD1_smc = $
do_pop_short_ptr_offset_smc_2 = $+2
	;ld bc,(iy)
do_pop_long_ptr_offset_smc_2 = $+3
	ld.l bc,(iy)
	inc bc \ dec bc ;BCU=0
	inc iyl
ophandlerD1_smc_size = $+1-ophandlerD1_smc
	inc iyl
	ret m
	call shift_stack_window_higher
	ret nc
do_pop_de_slow:
	exx
	ld c,$D1 ;POP DE
	call do_pop_instr_slow
	ld b,h
	ld c,l
	ret
	
ophandlerD1_slow:
	call shift_stack_window_higher
	jr do_pop_de_slow
	
	; POP HL
ophandlerE1:
	ex af,af'
ophandlerE1_smc = $
do_pop_short_ptr_offset_smc_3 = $+2
	;ld de,(iy)
do_pop_long_ptr_offset_smc_3 = $+3
	ld.l hl,(iy)
	ex de,hl ;DEU=0
	inc iyl
	inc iyl
ophandlerE1_smc_size = $-ophandlerE1_smc
	ret m
	call shift_stack_window_higher
	ret nc
do_pop_hl_slow:
	exx
	ld c,$E1 ;POP HL
	call do_pop_instr_slow
	ex de,hl
	ret
	
ophandlerE1_slow:
	call shift_stack_window_higher
	jr do_pop_hl_slow
	
	; POP AF
ophandlerF1:
do_pop_any_ptr_offset_smc = $+3
	ld.l hl,(iy)
	inc iyl
	inc iyl
ophandlerF1_smc = $
	jp p,ophandlerF1_overflow
do_pop_af_slow_finish:
	ld a,h
do_pop_af_finish:
	ld h,flags_lut >> 8
	res 3,l
	ld l,(hl)
	ld h,a
	push hl
	pop af
	ret
	
ophandlerF1_slow:
	call p,shift_stack_window_higher_preserved_a
	jr do_pop_af_slow
ophandlerF1_overflow:
	call shift_stack_window_higher_preserved_a
	jr nc,do_pop_af_finish
do_pop_af_slow:
	exx
	ld c,$F1 ;POP AF
	ex af,af'
	call do_pop_instr_slow
	ex af,af'
	jr do_pop_af_slow_finish
	
do_pop_instr_slow:
	ld b,a
	ld a,(curr_gb_stack_region)
	or a ;cp io_region_base
	jr z,do_pop_instr_get_cycle_offset
do_pop_instr_get_cycle_offset_return:
	ld a,b
do_pop_any_slow:
	ex af,af'
	push af
	 ld a,c
	 ld hl,(stack_window_base)
	 lea bc,iy-2
	 ld b,0
	 add hl,bc
	 ld b,h
	 ld c,l
	 call read_mem_any
	 push af
	  call read_mem_any_stale_bus_next
	  exx
	 pop hl
	 ld l,h
	 ld h,a
	pop af
	ex af,af'
	ret
	
do_pop_instr_get_cycle_offset:
	push bc
	jp.lil do_pop_instr_get_cycle_offset_helper
	
	; SMC to apply to pop routines
pop_short_ptr_src:
	;ophandlerC1_smc
	ld ix,(iy)
	inc iyl
	inc iyl
	ret m
	;ophandlerD1_smc
	ld bc,(iy)
	inc iyl
	inc iyl
	ret m
	.db $21 ;LD HL,
	;ophandlerE1_smc
	ld de,(iy)
	inc iyl
	inc iyl
	ret m
	.db $2E ;LD L,
	;ophandlerF1_smc
	.db $F2, ophandlerF1_overflow & $FF ;JP P,ophandlerF1_overflow
	
pop_long_ptr_src:
	;ophandlerC1_smc
	ld.l ix,(iy)
	inc iyl
	inc iyl
	;ophandlerD1_smc
	ld.l bc,(iy)
	inc bc \ dec bc
	inc iyl
	.db $FD ;INC IYL
	;ophandlerE1_smc
	ld.l hl,(iy)
	ex de,hl
	inc iyl
	inc iyl
	;ophandlerF1_smc
	.db $F2, ophandlerF1_overflow & $FF ;JP P,ophandlerF1_overflow

pop_slow_src:
	;ophandlerC1_smc
	inc iyl
	inc iyl
	jr $+(ophandlerC1_slow-(ophandlerC1_smc+4))
	nop
	nop
	;ophandlerD1_smc
	inc iyl
	inc iyl
	jp m,do_pop_de_slow
	jr $+(ophandlerD1_slow-(ophandlerD1_smc+7))
	;ophandlerE1_smc
	inc iyl
	inc iyl
	jp m,do_pop_hl_slow
	jr $+(ophandlerE1_slow-(ophandlerE1_smc+7))
	;ophandlerF1_smc
	.db $18, ophandlerF1_slow - (ophandlerF1_smc+2) ;JR ophandlerF1_slow
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Push routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.block (pop_routines_start+256)-$
push_routines_start:
audio_port_value_base:
hdma_port_value_base:
	
do_push_bc_long_ptr:
	lea hl,ix
do_push_any_long_ptr:
do_push_long_ptr_offset_smc_1 = $+3
	ld.l (iy),h
	dec iyl
do_push_long_ptr_offset_smc_2 = $+3
	ld.l (iy),l
	ex af,af'
	ret

	.block (audio_port_value_base + (NR10-ioregs)) - $
	; The current values of audio registers
audio_port_values:
	.block NR52 - NR10

do_push_de_long_ptr:
do_push_long_ptr_offset_smc_3 = $+3
	ld.l (iy),b
	dec iyl
do_push_long_ptr_offset_smc_4 = $+3
	ld.l (iy),c
	ex af,af'
	ret

do_push_hl_long_ptr:
do_push_long_ptr_offset_smc_5 = $+3
	ld.l (iy),d
	dec iyl
do_push_long_ptr_offset_smc_6 = $+3
	ld.l (iy),e
	ex af,af'
	ret
	
do_push_any_short_ptr:
	dec iyl
do_push_short_ptr_offset_smc_1 = $+2
	ld (iy),hl
	ex af,af'
	ret

do_push_bc_short_ptr:
	dec iyl
do_push_short_ptr_offset_smc_2 = $+2
	ld (iy),ix
	ex af,af'
	ret

	.block (hdma_port_value_base + (HDMA1-ioregs)) - $
	; The current values of HDMA registers
hdma_src_ptr:
	.dw 0
hdma_dst_ptr:
	.dw 0

do_push_de_short_ptr:
	dec iyl
do_push_short_ptr_offset_smc_3 = $+2
	ld (iy),bc
	ex af,af'
	ret
	
do_push_hl_short_ptr:
	dec iyl
do_push_short_ptr_offset_smc_4 = $+2
	ld (iy),de
	ex af,af'
	ret
	
	; PUSH AF
ophandlerF5:
	push af
	pop hl
	ld h,flags_lut >> 8
	; Bit 3 of F was set by the previous pop af
	ld l,(hl)
	ld h,a
	ex af,af'
ophandlerF5_retry:
	dec iyl
ophandlerF5_smc = $+1
	jp m,do_push_any_long_ptr
	call shift_stack_window_lower
	ex af,af'
	ld h,a
	ex af,af'
	; If possible after the window shift, do a direct write
	inc iyl
	jr nz,ophandlerF5_retry
	dec iyl
	jr do_push_instr_slow
	
	; PUSH HL
ophandlerE5:
	ex af,af'
ophandlerE5_retry:
	dec iyl
ophandlerE5_smc = $+1
	jp m,do_push_hl_long_ptr
	call shift_stack_window_lower
	; If possible after the window shift, do a direct write
	inc iyl
	jr nz,ophandlerE5_retry
	dec iyl
do_push_hl_slow:
	ld h,d
	ld l,e
	jr do_push_instr_slow
	
	.block (audio_port_values + $80) - $
	; Masks to apply to readable audio registers
audio_port_masks:
	;NR10 - NR14
	.db $80, $3F, $00, $FF, $BF
	;unused, NR21 - NR24
	.db $FF, $3F, $00, $FF, $BF
	;NR30 - NR34
	.db $7F, $FF, $9F, $FF, $BF
	;unused, NR41 - NR44
	.db $FF, $FF, $00, $00, $BF
	;NR50 - NR51
	.db $00, $00
	
	; PUSH DE
ophandlerD5:
	ex af,af'
ophandlerD5_retry:
	dec iyl
ophandlerD5_smc = $+1
	jp m,do_push_de_long_ptr
	call shift_stack_window_lower
	; If possible after the window shift, do a direct write
	inc iyl
	jr nz,ophandlerD5_retry
	dec iyl
do_push_de_slow:
	ld h,b
	ld l,c
	jr do_push_instr_slow
	
	; PUSH BC
ophandlerC5:
	ex af,af'
ophandlerC5_retry:
	dec iyl
ophandlerC5_smc = $+1
	jp m,do_push_bc_long_ptr
	call shift_stack_window_lower
	; If possible after the window shift, do a direct write
	inc iyl
	jr nz,ophandlerC5_retry
	dec iyl
do_push_bc_slow:
	lea hl,ix
do_push_instr_slow:
	exx
	ld b,d
	ld c,a
	ld hl,(stack_window_base)
	ld e,iyl
	ld d,0
	add hl,de
	ex de,hl
	call try_get_mem_readwrite_ptr_swapped
	ex af,af'
	jr c,do_push_instr_get_cycle_offset
	exx
	ld a,h
	exx
	ld.l (hl),a
	dec de
	call try_get_mem_readwrite_ptr_swapped
	ex af,af'
	jr c,do_push_instr_get_cycle_offset_2
	dec iyl
	exx
	ld a,l
	exx
	ld.l (hl),a
	ld d,b
	ld a,c
	exx
	ex af,af'
	ret
	
do_push_instr_get_cycle_offset_2:
	inc de
do_push_instr_get_cycle_offset:
	; Intentionally use stack instead of loading to ensure stack is allocated
	push bc
	pop hl
	pop bc
	push bc
	jp.lil do_push_instr_get_cycle_offset_helper
	