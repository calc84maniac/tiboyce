z80code:
	.assume adl=0
	.org 0
r_bits:
	ex af,af'
	ex (sp),hl
	ld iyl,a
	ld a,(hl)
	jp do_bits
	
	.block $08-$
r_mem:
	pop ix
	ex af,af'
	ld iyl,a
	jp decode_mem
	
	.block $10-$
r_pop:
	ex af,af'
	exx
	inc b
do_pop_jump_smc_1 = $+1
	djnz do_pop_hmem
	jr do_pop_check_overflow
	
	;.block $18-$
	; Currently unused
	
	.block $20-$
r_call:
	ex af,af'
	ld ix,(-call_stack_lower_bound) & $FFFF
	add ix,sp
	jr c,do_call
	call.il callstack_overflow_helper
	jr do_call
	
	;.block $28-$
	; Currently taken by previous entry
	
	.block $30-$
r_event:
	exx
	push.l hl
	pop hl
	dec hl
	jp do_event
	
	.block $38-$
r_cycle_check:
rst38h:
	inc iyh
	ret nz
	ld iyl,a
	pop ix
	exx
	jp nc,cycle_overflow_for_jump
	ld c,(ix-3)
	ld de,(ix-7)
	inc ix
	push ix
#ifdef VALIDATE_SCHEDULE
	call.il schedule_subblock_event_helper
#else
	jp.lil schedule_subblock_event_helper
#endif
	
do_pop_check_overflow:
	ld c,a
	ld a,h
do_pop_bound_smc_1 = $+1
	cp 0
_
	jp p,do_pop_overflow
	ld a,c
do_pop_jump_smc_2 = $+1
	jr do_pop_hmem
do_pop_z80:
	bit 6,b
	jr nz,-_
	inc b
	ld ix,(hl)
	inc hl
	inc hl
	ex (sp),ix
	exx
	ex af,af'
	jp (ix)
	
do_pop_adl:
	ld.l ix,(hl)
_
	inc b
	inc.l hl
	inc.l hl
	ex (sp),ix
	exx
	ex af,af'
	jp (ix)
	
do_pop_rtc:
	ld ix,(sp_base_address)
	ld ix,(ix)
	ld ixh,ixl
	jr -_
	
do_pop_hmem:
	ld iyl,a
	; Advance the return address to the end of the instruction,
	; and execute the skipped pop opcode here in case it's overwritten
	pop de
	ld a,(de)
	ld (do_pop_hmem_smc),a
	inc de
	push de
	call pop_hmem
	push de
	 exx
do_pop_hmem_smc = $
	pop bc
	ret
	
do_call_maybe_overflow:
	inc iyh
	jr nz,do_call_no_overflow
	call cycle_overflow_for_call
	jr callstack_ret
	
callstack_ret_bound:
	ld a,h
do_pop_bound_smc_4 = $+1
	cp 0
callstack_ret_overflow:
	jp p,_callstack_ret_overflow
	or a
	jr callstack_ret_nobound
	
do_call:
	pop ix
	exx
	ld de,(ix+3)
	add a,e ; Count cycles for taken CALL
	ld c,d  ; Cycles for taken RET
	ld de,(ix+5)  ; Game Boy return address
	pea ix+7  ; Cache JIT return address
	jr c,do_call_maybe_overflow
do_call_no_overflow:
	call do_push_for_call
	; BCDEHL' are swapped, D=cached stack offset, E=cached RET cycles
callstack_ret:
	ex af,af'
	ld c,a  ; Save cycle counter
	ld a,d  ; Compare the cached stack offset to the current one
	cp b
	jr nz,callstack_ret_stack_mismatch
callstack_ret_check_overflow_smc = $+1
	and $FF ; Check if the stack may be overflowing its bounds
	jr z,callstack_ret_bound
callstack_ret_nobound:
	ld a,e  ; Save the RET cycle count
	; Pop the return address into DE
callstack_ret_pop_prefix_smc_1 = $
	ld.l e,(hl)
	inc.l hl
callstack_ret_pop_prefix_smc_2 = $
	ld.l d,(hl)
	inc.l hl
	inc b   ; Increment the stack bound counter
callstack_ret_do_compare:
	; Save the GB stack pointer and get the cached return address.
	; The high byte of this address is non-zero if and only if
	; the mapped bank is different than when the call occurred.
	ex.l (sp),hl
	; Both compare the return addresses and ensure the bank has not changed.
	sbc.l hl,de
	jr nz,callstack_ret_target_mismatch
	add a,c  ; Count cycles
	jr c,callstack_ret_maybe_overflow
callstack_ret_no_overflow:
	pop.l hl  ; Restore GB stack pointer
	exx
	ex af,af'
	ret
	
callstack_ret_stack_mismatch:
	; Get the previous top of the stack
	ld ix,-4
	add ix,sp
	ld a,e  ; Get the requested cycles taken
	jp m,callstack_ret_skip
	; Restore the stack to that position, preserving the present values
	ld sp,ix
	ld iyl,c  ; Transfer the cycle count
	sub (ix)  ; Get the taken RET cycles
	add a,4
	ld c,a
	jr do_ret_full
	
banked_call_stack_overflow:
	call.il callstack_overflow_helper
	jr banked_call_stack_overflow_continue
	
do_rom_bank_call:
	ex af,af'
	exx
	ld c,a  ; Save cycle count
rom_bank_check_smc_1 = $+1
	ld a,0  ; Get current bank
banked_call_common:
	ex.l de,hl
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr nc,banked_call_stack_overflow
banked_call_stack_overflow_continue:
	pop hl  ; Get return address
	ld ix,(hl)  ; Get pointer to associated data
	inc hl
	inc hl
	cp (ix+4)  ; Validate the current bank
	jr nz,banked_call_mismatch
banked_call_mismatch_continue:
	ld a,c  ; Restore cycle count
	ld c,(hl)  ; Cycles for taken RET
	inc hl
	inc hl
	inc hl
	push hl  ; JIT return address
	dec hl
	dec hl
	ld hl,(hl)  ; Game Boy return address
	ex.l de,hl
	add a,(ix+3)  ; Count cycles for taken CALL
	jr nc,do_call_no_overflow
	inc iyh
	jr nz,do_call_no_overflow
	call cycle_overflow_for_call
	jr callstack_ret

callstack_ret_target_mismatch:
	ld iyl,c  ; Transfer the cycle count
	; If the subtraction carried, the high byte of HL was definitely zero
	jr c,callstack_ret_bank_mismatch_continue
	; Check if the bank difference is non-zero
	add.l hl,de  ; Restore the original high byte while keeping carry clear
	ld h,d
	ld l,e
	sbc.l hl,de
	jr nz,callstack_ret_bank_mismatch
callstack_ret_bank_mismatch_continue:
	ld hl,-4
	sub l     ; Add 4 cycles for the RET itself
	add hl,sp ; Get the old stack pointer
	sub (hl)  ; Get the taken RET cycles
	ld c,a
	pop hl    ; Remove the cached JIT return address
	pop.l hl  ; Restore GB stack pointer
	jr do_ret_full_continue
	
callstack_ret_maybe_overflow:
	inc iyh
	jr nz,callstack_ret_no_overflow
	ld iyl,a
	; HL was 0, get the stack pointer and get the JIT target address
	add hl,sp
	ld ix,(hl)
	; Get the original cycle count (unmodified by conditional RET)
	dec hl
	dec hl
	dec hl
	dec hl
	ld a,(hl)
	sub 4  ; Subtract taken RET cycles to get the block cycle offset
	ld c,a
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_helper
#else
	jp.lil schedule_event_helper
#endif
	
ophandlerRET:
	ld sp,myz80stack-4  ; Restore the stack to above this default handler
	ld c,e  ; Save the taken cycle count (4=unconditional, 5=conditional)
	ex af,af'
	ld iyl,a  ; Save the cycle count
do_ret_full:
	inc b
do_pop_for_ret_jump_smc_1 = $+1
	djnz do_pop_for_ret_overflow
	ld a,h
do_pop_bound_smc_2 = $+1
	cp 0
	jp p,do_pop_for_ret_overflow
do_pop_for_ret_jump_smc_2 = $+1
	jr do_pop_for_ret_overflow
	
do_pop_for_ret_adl:
	inc b
	ld.l e,(hl)
	inc.l hl
	ld.l d,(hl)
	inc.l hl
do_ret_full_continue:
	push bc
	 call.il lookup_code_cached
	pop bc
	add a,c  ; Add the taken cycles for RET
	add a,iyl  ; Count cycles
	jr c,do_ret_full_maybe_overflow
do_ret_full_no_overflow:
	exx
	ex af,af'
	jp (ix)
	
banked_call_mismatch:
	jp.lil banked_call_mismatch_helper
	
callstack_ret_bank_mismatch:
	call.il callstack_ret_bank_mismatch_helper
	jr callstack_ret_bank_mismatch_continue
	
do_pop_for_ret_z80:
	bit 6,b
	jr nz,do_pop_for_ret_overflow
	inc b
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	jr do_ret_full_continue
	
do_pop_for_ret_overflow:
	call pop_overflow
	ex af,af'
	exx
	pop de
	jr do_ret_full_continue
	
do_ret_full_maybe_overflow:
	inc iyh
	jr nz,do_ret_full_no_overflow
	push.l hl
	; Get the total number of taken cycles
	ex de,hl
	ld e,iyl
	ld iyl,a
	sub e
	sub c ; Subtract out taken cycles from RET itself
	ld c,a
	ex de,hl  ; Clears top byte of DE
	push ix
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_helper
#else
	jp.lil schedule_event_helper
#endif
	
callstack_ret_skip:
	sub (ix) ; Get the conditional RET cycle offset
	inc sp  ; Skip the JIT return address
	inc sp
	pop de  ; Prepare the next return inputs
	add a,e ; Add the conditional offset
	ld e,a
	; Skip the Game Boy return address, but make sure
	; to propagate any bank mismatch
	inc.l sp
	pop.l af
	dec.l sp
	or a
	jr nz,callstack_ret_skip_propagate
	ld a,c  ; Restore the cycle counter
	ex af,af'
	ret
	
callstack_ret_skip_propagate:
	; Get the full return address
	dec.l sp
	dec.l sp
	dec.l sp
	 ex.l (sp),hl
	 call.il callstack_ret_skip_propagate_helper
	pop.l hl
	ex af,af'
	ret
	
_callstack_ret_overflow:
	push de  ; Save the requested RET taken cycles
	dec sp
	dec sp   ; Preserve the original RET taken cycles
	call pop_overflow_for_callstack_ret
	ex af,af'
	exx
	ld c,iyl ; Restore the cycle count
	pop de  ; Get popped GB address
	inc sp
	pop af  ; Pop requested taken cycles into A
	inc sp
	or a
	jp callstack_ret_do_compare
	
cycle_overflow_for_call:
	ld iyl,a
	dec b
	push bc
	inc b
	push ix
	push de
	dec de
	ld a,(ix+3)
	sub 6
	ld c,a
	ld ix,(ix+1)
#ifdef VALIDATE_SCHEDULE
	call.il schedule_call_event_helper
#else
	jp.lil schedule_call_event_helper
#endif
	
do_rom_bank_jump:
	ex af,af'
	exx
	ld c,a
	pop ix
rom_bank_check_smc_2 = $+1
	ld a,0
	ld de,(ix+3)
	xor e
	jr nz,banked_jump_mismatch
	ld a,d
banked_jump_mismatch_continue:
	add a,c
	jr c,++_
_
	exx
	ex af,af'
	jp (ix)
_
	inc iyh
	jr nz,--_
	ld iyl,a
	ld a,(ix)
	ld c,d
	ld de,(ix+6)
	ld ix,(ix+1)
	push ix
#ifdef VALIDATE_SCHEDULE
	call.il schedule_jump_event_helper
#else
	jp.lil schedule_jump_event_helper
#endif
	
banked_jump_mismatch:
	jp.lil banked_jump_mismatch_helper
	
cycle_overflow_for_jump:
	xor a
	sub (ix-3)
	ld c,a
	ld a,(ix+1)
	ld de,(ix+4)
	ld ix,(ix+2)
	push ix
#ifdef VALIDATE_SCHEDULE
	call.il schedule_jump_event_helper
#else
	jp.lil schedule_jump_event_helper
#endif
	
schedule_event_finish:
	ld (event_cycle_count),a
	ld (event_gb_address),hl
#ifdef DEBUG
	ld a,(event_address+1)
	cp event_value >> 8
	jr nz,$
#endif
	lea hl,ix
	ld (event_address),hl
	ld a,(hl)
	ld (event_value),a
	ld (hl),RST_EVENT
	ld a,iyl
schedule_event_finish_no_schedule:
	ex af,af'
	pop.l hl
	exx
	ret
	
Z80InvalidOpcode:
	jp.lil Z80InvalidOpcode_helper
	
Z80Error:
	jp.lil runtime_error
	
	  ; If the cycle count underflowed, modify event trigger logic
	  ; to force it to recognize the count as an overflow
do_push_overflow_underflow:
	  ex af,af'
	  xor a
	  ld (trigger_event_remove_smc),a
	  ex af,af'
	  call mem_write_any
	  ld a,trigger_event_no_remove - (trigger_event_remove_smc+1)
	  ld (trigger_event_remove_smc),a
	  jr do_push_overflow_continue
	
do_push_for_call_overflow:
	push ix
do_push_overflow:
	ld iyl,c
	ex af,af'
	push af
	 ld b,e	; B' can be used for safe storage, since set_gb_stack restores it
	 ld a,d
	 ld de,(sp_base_address_neg)
	 add hl,de
	 push hl
	  ; We need special handling if an event is triggered by the first write
	  ld hl,event_cycle_count
	  dec (hl)
	  ld de,-1
	  add iy,de
	  exx
	  ex (sp),hl
	  dec hl
	  jr nc,do_push_overflow_underflow
	  call mem_write_any
do_push_overflow_continue:
	  exx
	  ; Restore the event cycle count, or increment if an event was triggered
	  ld hl,event_cycle_count
	  inc (hl)
	  ld a,b
	  exx
	  dec hl
	  call mem_write_any_after_read
	  ex (sp),hl
	  exx
	 pop hl
	 jr set_gb_stack_pushed

do_pop_overflow:
	ld iyl,c
	; Advance the return address to the end of the instruction,
	; and execute the skipped pop opcode here in case it's overwritten
	pop de
	ld a,(de)
	ld (do_pop_overflow_smc),a
	inc de
	push de
	call pop_overflow
do_pop_overflow_smc = $
	pop bc
	ret

pop_overflow_for_callstack_ret:
	ld iyl,c  ; Make the full cycle count available
	ld c,e  ; Make the requested cycles available
pop_overflow:
	push bc
	 ld de,(sp_base_address_neg)
	 add hl,de
	 push hl
	  exx
	  ex (sp),hl
	  call mem_read_any_before_write
	  inc hl
	  inc iy
	  push af
	   call mem_read_any
	  pop ix
	  ld ixl,ixh
	  ld ixh,a
	  inc hl
	  ex (sp),hl
	  exx
	 pop hl
	pop bc
	ex (sp),ix
	push ix
	ld a,iyl
	ex af,af'

; Get a literal 24-bit pointer to the Game Boy stack.
; Does not use a traditional call/return, must be jumped to directly.
;
; This routine is invoked whenever SP is set to a new value which may be outside
; its current bank. If the bank has changed, any relevant stack routines are modified.
;
; Inputs:  HL = 16-bit Game Boy SP
;          BCDEHL' have been swapped
; Outputs: HL' = 24-bit literal SP
;          B' = stack overflow counter
;          C' is preserved, IX is destroyed
;          BCDEHL' have been unswapped
;          SMC applied to stack operations
set_gb_stack:
	push af
set_gb_stack_pushed:
	 ; Get memory region, 0-7
	 ; This is the same as memroutines, except $FFFF is included in region 0 because
	 ; accesses would always be handled by the overflow handler
	 ld a,h
	 cp $FE
	 jr c,_
	 rrca
	 and l
	 rrca
	 cpl
	 and $40
	 jr ++_
_
	 and $E0
	 jp m,_
	 set 5,a
_
curr_gb_stack_bank = $+1
	 cp 1	; Default value forces a mismatch
	 jr nz,set_gb_stack_bank
	 or a
set_gb_stack_bank_done:
	 ; Calculate the new stack bound counter
	 ld a,h
	 rra
	 ld a,l
	 rra
	 jr nz,_
	 ; Special-case for HRAM, base the counter at $FF80
	 and $3F
_
	 inc a
	 ; Put the direct stack pointer in HL'
sp_base_address = $+2
	 ld.lil de,0
	 add.l hl,de
	 ; Put the new stack bound counter in B'
	 ld b,a
	 exx
	pop af
	ret

set_gb_stack_bank:
	ld (curr_gb_stack_bank),a
	push bc
	 call.il set_gb_stack_bounds_helper
	 call.il set_gb_stack_bank_helper
	pop bc
	jr set_gb_stack_bank_done	
	
do_event:
event_value = $+1
	ld (hl),0
	push hl
#ifdef DEBUG
	ld hl,event_value
	ld (event_address),hl
#endif
	ex af,af'
event_cycle_count = $+2
	ld iyl,0
	sub iyl
	ld c,a
	ASSERT_NC
do_event_pushed:
#ifdef DEBUG
	inc iyh
	dec iyh
	jr nz,$
#endif
	push bc

	 ; Check scheduled events
	 ld hl,i
	 ld (event_save_sp),sp
event_expired_loop:
ei_delay_event_smc = $+1
	 ld sp,event_counter_checkers

	 ld b,h
	 ld c,l
vblank_counter = $+1
	 ld de,0
	 sbc hl,de
	 ex de,hl
	 ret nz
	 jp.lil vblank_helper

event_expired_more_events:
	 or a
	 sbc hl,de
	 or a
	 jr event_expired_loop

event_counter_checkers_done:
	 ld h,b
	 ld l,c
	 add iy,de
	 jr c,event_expired_more_events
	 sbc hl,de
	 ld i,hl
event_save_sp = $+1
	 ld sp,0   
	pop bc
event_not_expired:
	ld hl,IE
	ld a,(hl)
	ld l,IF - ioregs
	and (hl)
intstate_smc_2 = $+1
	jr nz,trigger_interrupt
	ld a,iyl
	add a,c
	jr c,event_maybe_reschedule
event_no_reschedule:
	pop.l hl
	exx
	ex af,af'
	ret
	
start_emulation:
	call set_gb_stack
	ex af,af'
	exx
	push.l hl
	jr event_not_expired
	
event_maybe_reschedule:
	inc iyh
	jr nz,event_no_reschedule
	ld iyl,a
	ld de,(event_gb_address)
	pop ix
	push ix
	; This is guaranteed to carry, so the event cannot be now
	sub c
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_later
#else
	jp.lil schedule_event_later
#endif
	
trigger_int_callstack_overflow:
	pop.l hl
	call.il callstack_overflow_helper
	push.l hl
	jr trigger_int_selected
	
trigger_interrupt:
	rrca
	jr c,trigger_vblank
	rrca
	jr c,trigger_stat
	rrca
	jr c,trigger_timer
	rrca
	jr c,trigger_serial
trigger_joypad:
	res 4,(hl)
	ld ix,dispatch_joypad
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr c,trigger_int_selected
	jr trigger_int_callstack_overflow
trigger_serial:
	res 3,(hl)
	ld ix,dispatch_serial
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr c,trigger_int_selected
	jr trigger_int_callstack_overflow
trigger_timer:
	res 2,(hl)
	ld ix,dispatch_timer
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr c,trigger_int_selected
	jr trigger_int_callstack_overflow
trigger_stat:
	res 1,(hl)
	ld ix,dispatch_stat
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr c,trigger_int_selected
	jr trigger_int_callstack_overflow
trigger_vblank:
	res 0,(hl)
	ld ix,dispatch_vblank
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr nc,trigger_int_callstack_overflow
trigger_int_selected:
event_gb_address = $+1
	ld de,event_gb_address
	; Disable interrupts
	ld a,$08 ;EX AF,AF'
	ld (intstate_smc_1),a
	; Get the number of cycles to be taken by RET
	rrca	;ld a,4
	add a,c
	ld c,a
	; More disabling interrupts
	xor a
	ld (intstate_smc_2),a
	; Get number of cycles to be taken, and check if zero
	or (ix+3)
cpu_halted_smc = $
	jr z,decode_intcache  ; Replaced with JR exit_halt when halted
dispatch_int_decoded:
	add a,iyl
	jr c,dispatch_int_maybe_overflow
dispatch_int_no_overflow:
	pop.l hl
	call do_push_for_call
callstack_reti:
	jp callstack_ret
	
dispatch_int_maybe_overflow:
	inc iyh
	jr nz,dispatch_int_no_overflow
	; Push the special return value used as a sentinel
	ld hl,callstack_reti
	push hl
	; Reset carry
	or a	
cycle_overflow_for_rst_or_int:
	ld iyl,a
	dec b
	push bc
	inc b
	push ix
	push de
	ld a,(ix+3)
	; Subtract 4 for RST, 5 for CALL
	adc a,-5
	ld c,a
	lea de,ix+(-(dispatch_rst_00 + $80) & $FF) - $80
	ld d,0
	sla e
	ld ix,(ix+1)
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_helper_for_call
#else
	jp.lil schedule_event_helper_for_call
#endif
	
exit_halt:
	; Undo SMC for halted state
	ld hl,((decode_intcache - (cpu_halted_smc+2)) << 8) | $28 ;JR Z,decode_intcache
	ld (cpu_halted_smc),hl
	; Get the address of the HALT
	pop hl
	inc hl
	inc hl
	inc hl
	; Get the new number of cycles for RET (removing the HALT itself)
	ld c,(hl)
	inc bc \ inc bc \ inc bc  ; Preserve Z flag
	; Advance JIT address past HALT
	inc hl
	inc hl
	inc hl
	push hl
	; Advance GB address past HALT
	inc de
	; Return to interrupt dispatch if cycles to take were non-zero
	jr nz,dispatch_int_decoded
decode_intcache:
	; A = 0
	call.il decode_intcache_helper
	ld (ix+1),hl
	ld (ix+3),a
	jr dispatch_int_decoded
	
lyc_counter_checker:
lyc_counter = $+1
	ld hl,0
	or a
	sbc hl,bc
	jr z,lyc_expired_handler
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret
	
lyc_expired_handler:
	ld hl,LCDC
	bit 7,(hl)
	jr z,_
	ld l,IF & $FF
	set 1,(hl)
_
	ld hl,CYCLES_PER_FRAME
	add hl,bc
	ld (lyc_counter),hl
	; Special case, DE cannot exceed -CYCLES_PER_FRAME so this cannot replace it
	ret
	
stat_counter_checker_single:
stat_counter = $+1
	ld hl,0
	or a
	sbc hl,bc
	jr nz,stat_not_expired_single
	ld hl,IF
	set 1,(hl)
	ld hl,stat_line_count
	dec (hl)
	jr z,stat_counter_single_skip_vblank
	ld hl,CYCLES_PER_SCANLINE
	add hl,bc
	ld (stat_counter),hl
	or a
	sbc hl,bc
	add hl,de
	ret c
	ld de,-CYCLES_PER_SCANLINE
	ret
	
stat_counter_single_skip_vblank:
	ld (hl),144
	ld hl,CYCLES_PER_SCANLINE * 11
	add hl,bc
	ld (stat_counter),hl
	ld hl,CYCLES_PER_SCANLINE * 11
	add hl,de
	ret c
	ld de,-(CYCLES_PER_SCANLINE * 11)
	ret
	
stat_not_expired_single:
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret
	
stat_line_count:
	.db 144
	
stat_mode0_expired_handler:
	ld hl,IF
	set 1,(hl)
	ld hl,stat_line_count
	dec (hl)
	jr z,stat_counter_double_skip_vblank
	ld hl,MODE_0_CYCLES
stat_update_line_counter_double:
	call stat_double_swap_modes
stat_counter_checker_mode2:
	ld hl,(stat_counter)
	or a
	sbc hl,bc
	jr nz,stat_not_expired_double
	ld hl,IF
	set 1,(hl)
	ld hl,MODE_2_CYCLES + MODE_3_CYCLES
	call stat_double_swap_modes
stat_counter_checker_mode0:
	ld hl,(stat_counter)
	or a
	sbc hl,bc
	jr z,stat_mode0_expired_handler
stat_not_expired_double:
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret

stat_double_swap_modes:
	inc sp
	inc sp
	add hl,bc
	ld (stat_counter),hl
	or a
	sbc hl,bc
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret
	
stat_counter_double_skip_vblank:
	ld (hl),144
	ld hl,CYCLES_PER_SCANLINE * 10 + MODE_0_CYCLES
	jr stat_update_line_counter_double

timer_counter_checker:
timer_counter = $+1
	ld hl,0
	or a
	sbc hl,bc
	jr z,timer_expired_handler
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret
	
timer_expired_handler:
	ld hl,IF
	set 2,(hl)
	ld l,TMA & $FF
	xor a
	sub (hl)
timer_cycles_reset_factor_smc = $+1
	ld h,0
	ld l,a
	jr z,_
	mlt hl
_
	add hl,hl
	; If scheduled for 65536 cycles in the future, no need to reschedule
	; Returning here prevents the delay from being interpreted as 0
	ret c
	add hl,bc
	ld (timer_counter),hl
	or a
	sbc hl,bc
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret

audio_counter_checker:
audio_counter = $+1
	ld hl,0
	or a
	sbc hl,bc
	jr z,audio_expired_handler
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret
	
audio_expired_handler:
	ld.lil a,(mpLcdMis)
	or a
	jr nz,do_frame_interrupt
frame_interrupt_return:
	ld a,(NR52)
	tst a,$0F
	jr z,audio_expired_disabled
	push.l bc
	 ld h,audio_port_value_base >> 8
	 ld b,$3F
	 ld c,a
	 rra
	 jr nc,++_
	 ld l,NR14-ioregs
	 bit 6,(hl)
	 jr z,++_
	 ld l,NR11-ioregs
	 ld a,(hl)
	 inc a
	 tst a,b
	 jr nz,_
	 dec c
	 sub $40
_
	 ld (hl),a
_
	 bit 1,c
	 jr z,++_
	 ld l,NR24-ioregs
	 bit 6,(hl)
	 jr z,++_
	 ld l,NR21-ioregs
	 ld a,(hl)
	 inc a
	 tst a,b
	 jr nz,_
	 res 1,c
	 sub $40
_
	 ld (hl),a
_
	 bit 2,c
	 jr z,_
	 ld l,NR34-ioregs
	 bit 6,(hl)
	 jr z,_
	 ld l,NR31-ioregs
	 inc (hl)
	 jr nz,_
	 res 2,c
_
	 bit 3,c
	 jr z,_
	 ld l,NR44-ioregs
	 bit 6,(hl)
	 jr z,_
	 ld l,NR41-ioregs
	 ld a,(hl)
	 inc a
	 and b
	 ld (hl),a
	 jr nz,_
	 res 3,c
_
	 ld a,c
	 ld (NR52),a
	pop.l bc
audio_expired_disabled:
	ld a,b
	add a,4096 >> 8	; Double this in double-speed mode
	ld (audio_counter+1),a
	sub b
	add a,d
	ret c
	ld de,-4096	; Double this in double-speed mode
	ret

do_frame_interrupt:
	jp.lil frame_interrupt

serial_counter_checker:
serial_counter = $+1
	ld hl,0
	or a
	sbc hl,bc
	jr z,serial_expired_handler
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret
	
serial_expired_handler:
	ld hl,SC
	res 7,(hl)
	dec hl
	ld (hl),h
	ld l,IF & $FF
	set 3,(hl)
	call disabled_counter_checker
disabled_counter_checker:
	ret
	
decode_mem:
	ld a,(memroutine_next)
	sub ixl
	ld a,(memroutine_next+1)
	sbc a,ixh
	jr nc,_
	pop af
	ex af,af'
	ld a,(ix)
	pop ix
	lea ix,ix-2
	ld (ix),a
_
	push hl
	 push de
	  call.il decode_mem_helper
	  ld (ix+1),de
	  ld (ix),$CD
	  ; Load the previous byte into IYL just in case this was LD (HL),n
	  ld e,(ix-1)
	  ld a,iyl
	  ld iyl,e
	 pop de
	pop hl
	ex af,af'
	jp (ix)
	
decode_jump:
	ex af,af'
	exx
	push.l hl
	pop hl
	ld c,a
	push bc
	 inc hl
	 inc hl
	 inc hl
	 ld c,(hl)
	 inc hl
	 ld ix,(hl)
	 push hl
	  inc hl
	  inc hl
	  ld de,(hl)
	  jp.lil decode_jump_helper
decode_jump_return:
	 pop hl
	 ld (hl),ix
	 ld de,-5
	 add hl,de
	 neg
	 ld (hl),a
	 dec hl
	 ld (hl),$D6	;SUB -cycles
	 dec hl
	 ld (hl),$08	;EX AF,AF'
	 jr nz,decode_jump_waitloop_return
	 ; Special case for zero cycles, because carry would be inverted
	 ; Just do a direct jump, because cycles cannot overflow
	 inc hl
	 inc hl
	 ld (hl),ix
	 dec hl
	 ld (hl),$C3	;JP target
	 dec hl
	 ld (hl),a  ;NOP (instructions are not allowed to begin with JP)
decode_jump_waitloop_return:
	pop bc
	ld a,c
	push hl
	pop.l hl
	exx
	ex af,af'
	ret
	
decode_call:
	ex (sp),hl
	push af
	 push bc
	  push de
	   inc hl
	   push hl
	    inc hl
	    inc hl
	    ld de,(hl)
	    dec de
	    call.il decode_call_helper
	   pop hl
	  pop de
	  ld (hl),a  ;taken cycles
	  dec hl
	  jr c,++_
	  dec hl
	  ld (hl),ix
	  dec hl
	  ld (hl),$C3  ;JP jit_target
	  dec hl
	  ld (hl),RST_CALL
_
	 pop bc
	pop af
	ex (sp),hl
	ret
	
_
	  ld (hl),ix
	  dec hl
	  dec hl
	  ld (hl),bc
	  dec hl
	  ld (hl),$CD  ;CALL do_rom_bank_call
	  jr --_
	
decode_call_cond:
	ex (sp),hl
	push af
	 push bc
	  push de
	   push hl
	    inc hl
	    inc hl
	    ld de,(hl)
	    dec de
	    call.il decode_call_helper
	   pop hl
	  pop de
	  ld (hl),a
	  dec hl
	  jr c,++_
	  dec hl
	  ld (hl),ix
	  dec hl
	  ld (hl),$C3
	  dec hl
	  dec hl
_
	  dec hl
	  ld (hl),$CD
	 pop bc
	pop af
	ex (sp),hl
	ret
	
_
	  ld (hl),ix
	  dec hl
	  dec hl
	  ld (hl),bc
	  dec hl
	  dec hl
	  ; Modify the conditional entry point to use the banked call
	  ld bc,(hl)
	  dec bc
	  dec bc
	  ld (hl),bc
	  jr --_
	
do_rst_00:
	ld ix,dispatch_rst_00
	jr decode_rst
do_rst_08:
	ld ix,dispatch_rst_08
	jr decode_rst
do_rst_10:
	ld ix,dispatch_rst_10
	jr decode_rst
do_rst_18:
	ld ix,dispatch_rst_18
	jr decode_rst
do_rst_20:
	ld ix,dispatch_rst_20
	jr decode_rst
do_rst_28:
	ld ix,dispatch_rst_28
	jr decode_rst
do_rst_30:
	ld ix,dispatch_rst_30
	jr decode_rst
do_rst_38:
	ld ix,dispatch_rst_38
	jr decode_rst
	
decode_rst:
	jp.lil decode_rst_helper
	
do_rst:
	ex af,af'
	exx
do_rst_decoded:
	ex.l de,hl
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr nc,++_
_
	pop hl
	ld c,(hl)  ; Cycles for taken RET
	inc hl
	inc hl
	inc hl
	push hl  ; JIT return address
	dec hl
	dec hl
	ld hl,(hl)  ; Game Boy return address
	ex.l de,hl
	add a,(ix+3)  ; Count cycles
	jp nc,do_call_no_overflow
	inc iyh
	jp nz,do_call_no_overflow
	; Carry is set
	push.l hl
	call cycle_overflow_for_rst_or_int
	jp callstack_ret
	
_
	call.il callstack_overflow_helper
	jr --_
	
do_banked_call_cond:
	pop ix
	pea ix+2
	ld ix,(ix)
	jp (ix)
	
	jr nz,do_banked_call_cond
do_call_nz:
	jr z,skip_cond_call
	jp r_call
	
	jr z,do_banked_call_cond
do_call_z:
	jr nz,skip_cond_call
	jp r_call
	
	jr nc,do_banked_call_cond
do_call_nc:
	jr c,skip_cond_call
	jp r_call
	
	jr c,do_banked_call_cond
do_call_c:
	jp c,r_call
skip_cond_call:
	pop ix
	lea ix,ix+7
	ex af,af'
	add a,(ix-3)
	jr c,++_
_
	dec a
	ex af,af'
	jp (ix)
_
	jr z,--_
	inc iyh
	jr nz,--_
	dec a
	ld iyl,a
	exx
	ld a,(ix-3)
	sub 4
	ld c,a
	ld de,(ix-2)
	push ix
	push.l hl
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_helper
#else
	jp.lil schedule_event_helper
#endif
	
wait_for_interrupt_stub:
	ei
	halt
	ret.l
	
flush_handler:
	exx
flush_address = $+1
	ld de,0
	jp.lil flush_normal
	
dispatch_rst_00:
	jp 0
	.db 0
dispatch_rst_08:
	jp 0
	.db 0
dispatch_rst_10:
	jp 0
	.db 0
dispatch_rst_18:
	jp 0
	.db 0
dispatch_rst_20:
	jp 0
	.db 0
dispatch_rst_28:
	jp 0
	.db 0
dispatch_rst_30:
	jp 0
	.db 0
dispatch_rst_38:
	jp 0
	.db 0
dispatch_vblank:
	jp 0
	.db 0
dispatch_stat:
	jp 0
	.db 0
dispatch_timer:
	jp 0
	.db 0
dispatch_serial:
	jp 0
	.db 0
dispatch_joypad:
	jp 0
	.db 0
	
flush_mem_handler:
	exx
	ex af,af'
	ld iyl,a
	ld a,b
	pop bc
	jp.lil flush_mem
	
coherency_handler:
	pop ix
	pea ix+RAM_PREFIX_SIZE-3
	ld ix,(ix)
	jp.lil check_coherency_helper

coherency_return:
	pop.l hl
	exx
	ex af,af'
	ret
	   
do_swap:
	inc a
	jr nz,do_swap_generic
	ld a,iyl
	ex af,af'
	rrca
	rrca
	rrca
	rrca
	or a
	ret
do_swap_generic:
	inc a
	jr z,do_swap_hl
	add a,$7E	;LD A,r
	ld (_),a
	add a,a
	add a,a
	add a,a
	sub $79		;LD r,A
	ld (++_),a
	ld a,iyl
	ex af,af'
	push af
_
	 ld a,b
	 rrca
	 rrca
	 rrca
	 rrca
	 or a
_
	 ld b,a
	pop ix
	ld a,ixh
	ret
do_swap_hl:
	call mem_read_any_before_write
	rrca
	rrca
	rrca
	rrca
	or a
	ex af,af'
	push af
	 ex af,af'
	 call mem_write_any_after_read
	pop ix
	ld a,ixh
	ret
	
do_bits:
	inc hl
	ex (sp),hl
	sub $30
	sub 8
	jr c,do_swap
	add a,$38-1	;Use L instead of (HL)
	cp $C0
	jp pe,do_bits_readonly
	ld (do_bits_smc),a
	call mem_read_any_before_write
	; Use L because we have to affect flags, bleh
	push hl
	 ld l,a
	 ex af,af'
	 ld h,a
do_bits_smc = $+1
	 rlc l
	 ld a,l
	 ex (sp),hl
	 call mem_write_any_after_read
	pop ix
	ld a,ixh
	ret
do_bits_readonly:
	ld (do_bits_readonly_smc),a
	call mem_read_any
	; Use L because we have to affect flags, bleh
	push hl
	 ld l,a
	 ld a,iyl
	 ex af,af'
do_bits_readonly_smc = $+1
	 bit 0,l
	pop hl
	ret
	
ophandler08:
	push af
	 push de
	  exx
	  push hl
	   exx
	   ex af,af'
	   ld iyl,a
	   ex (sp),hl
	   ld de,(sp_base_address_neg)
	   add hl,de
	   ld de,-1
	   add iy,de
	   ex de,hl
	   ; We need special handling if an event is triggered by the first write
	   ld hl,event_cycle_count
	   dec (hl)
	   push ix
	    ex (sp),hl
	    jr nc,ophandler08_underflow
	    ld a,e
	    call mem_write_any
ophandler08_continue:
	    ex (sp),hl
	    ; Restore the event cycle count, or increment if an event was triggered
	    inc (hl)
	   pop hl
	   inc hl
	   ld a,d
	   call mem_write_any_after_read
	  pop hl
	 pop de
	pop af
	ret
	   ; If the cycle count underflowed, modify event trigger logic
	   ; to force it to recognize the count as an overflow
ophandler08_underflow:
	   xor a
	   ld (trigger_event_remove_smc),a
	   ld a,e
	   call mem_write_any
	   ld a,trigger_event_no_remove - (trigger_event_remove_smc+1)
	   ld (trigger_event_remove_smc),a
	   jr ophandler08_continue
	
ophandler27:
	exx
	ld c,a
	; Get the DAA adjustment according to the N, H, C flags only
	ld a,0
	daa
	; If the adjustment is 0, special-case to determine N flag
	jr z,ophandler27_preserve_n
	; If the adjustment is greater than 0, do addition
	adc a,a
	jr nc,ophandler27_add
	; Restore adjustment value and set carry flag
	rra
	; If adjustment was $FA (not $A0 or $9A), output carry reset
	jp m,ophandler27_subtract_reset_carry
	; Otherwise, output carry set
	; Add the adjustment to the original value (plus 1) and decrement
	adc a,c
	dec a
	; If result is 0, N and Z are set, H is reset, C was set by the addition
	jr z,_
	; Set N and C, reset H and Z
	scf
	ld c,2
	dec c
_
	exx
	ret
	
ophandler27_preserve_n:
	; Determine whether N was set
	ld a,$FF
	daa
	rlca	; Resets H and N flags
	jr nc,ophandler27_natural_daa
	; N was set, use adjustment of 0
	xor a
ophandler27_subtract_reset_carry:
	; Add the adjustment to the original value
	add a,c
	; Set N, reset H and C, update Z
	sub 0
	exx
	ret
	
ophandler27_add:
	; Restore the original H and C flags
	add a,a
ophandler27_natural_daa:
	; Do the natural DAA operation for addition, which is equivalent to GB
	ld a,c
	daa
	; Reset H and N, preserve C and Z
	rla
	rra
	exx
	ret
	
	
ophandler31:
	pop ix
	pea ix+2
	exx
	ld hl,(ix)
	jp set_gb_stack
	
ophandler33:
	ex af,af'
	exx
	inc.l hl
	bit 0,l
ophandler33_jr_smc = $
	jr nz,++_
	inc b
	djnz _
	ld c,a
	ld a,h
ophandler33_bound_smc = $+1
	cp 0
	ld a,c
	jp m,_
ophandler33_3B_overflow:
	ld de,(sp_base_address_neg)
	add hl,de
	ex af,af'
	jp set_gb_stack
_
	inc b
_
	exx
	ex af,af'
	ret
	
ophandler3B:
	ex af,af'
	exx
	bit 0,l
	dec.l hl
ophandler3B_jr_smc = $
	jr nz,-_
	djnz -_
	ld c,a
	ld a,h
ophandler3B_bound_smc = $+1
	cp 0
	ld a,c
	jp m,ophandler33_3B_overflow
	exx
	ex af,af'
	ret
	
ophandler34:
	ex af,af'
	ld iyl,a
	call mem_read_any_before_write
	ld ixl,a
	ex af,af'
	inc ixl
	jr _
	
ophandler35:
	ex af,af'
	ld iyl,a
	call mem_read_any_before_write
	ld ixl,a
	ex af,af'
	dec ixl
_
	push af
	 call mem_write_any_after_read_ixl
	pop af
	ret
	
ophandler39:
	push de
	 exx
	 push hl
	  exx
	  ex (sp),hl
sp_base_address_neg = $+1
	  ld de,0
	  add hl,de
	  ex de,hl
	 pop hl
	 add hl,de
	pop de
	ret
	
handle_waitloop_stat:
	jr handle_waitloop_stat
	
handle_waitloop_variable:
	pop ix
	ex af,af'
	exx
	; Skip straight to the counter expiration
	ld iy,(ix+2)
	ld a,iyh
	ld c,iyl
	ld iyh,0
handle_waitloop_common:
	ld de,(ix+6)
	ld ix,(ix+4)
	push ix
#ifdef VALIDATE_SCHEDULE
	call.il schedule_jump_event_helper
#else
	jp.lil schedule_jump_event_helper
#endif
	
_
	inc iyh
	jr nz,_
	ld a,(ix+3)
	jr handle_waitloop_common
	
handle_waitloop_ly:
	pop ix
	ex af,af'
	exx
	; Add the next jump cycles, but preserve the original count
	ld c,(ix+2)
	ld d,iyh
	ld e,a
	add a,c
	ld iyl,a
	jr c,-_
_
	push.l hl
	; Get the current scanline cycle count
	call get_scanline_from_cycle_offset
	ld d,a
	; Adjust the cycle offset for the start of LY=0,
	; to avoid skipping from LY=153 to the middle of LY=0
	ld a,e
	cp 9
	jr nz,_
	ld a,d
	sub 1<<1
	jr c,_
	ld d,a
_
	; Get the cached cycle offset of the LY load
	ld hl,(ix+4)
	inc hl
	ld hl,(hl)
	dec hl
	ld a,(hl)
	; Convert this to the (negative) cycles passed since the LY read
	sub c
	ld h,(ix)
	add a,h
	cpl
	add a,a ; NOP this out in double-speed mode
	; Apply the negative count to the scanline offset
	add a,d
	; If the result is negative, the scanline already changed
	; since the last read, so don't skip any loops
	jr nc,handle_waitloop_ly_finish
	sub CYCLES_PER_SCANLINE<<1
	rra ; NOP this out in double-speed mode
	; Choose the smaller absolute value of the cycle counter
	; and the remaining scanline cycles
	inc iyh
	jr nz,_
	cp iyl
	jr nc,_
	ld a,iyl
_
	ld l,a
	; Skip as many full loops as possible without exceeding the cycle count
_
	add a,h
	jr nc,-_
	sub h
	sub l
	; Add in the cycles (this cannot overflow because of the counter limit)
	add a,iyl
	jr c,_
	dec iyh
	.db $DA ;JP C,
handle_waitloop_ly_finish:
	ld a,iyl
_
	pop.l hl
	exx
	ex af,af'
	ld ix,(ix+4)
	jp (ix)

ophandlerEI_delay_expired:
	; An event is scheduled for the current cycle, so we have to delay the
	; actual enabling of the IME flag.
	ld hl,event_counter_checkers_ei_delay
	ld (ei_delay_event_smc),hl
	ld de,schedule_ei_delay
	ld (hl),de
ophandlerEI_no_interrupt:
	pop.l hl
	exx
ophandlerEI_no_change:
	; IME did not change, which also means no need to check for interrupts
	ld a,iyl
	ex af,af'
	ret
	
ophandlerEI:
intstate_smc_1 = $
	ret	; SMC overrides with EX AF,AF' when IME=0
	ld iyl,a
	
	; Always disable just the EI handler.
	; This prevents consecutive EIs from causing multiple enable delays,
	; while still preventing interrupts from happening if an event happens
	; during the delay.
	ld a,$C9
	ld (intstate_smc_1),a

	; Check if an event is scheduled at the current cycle
	exx
	push.l hl
	pop hl
	push hl
	ld a,(hl)
	cp RST_EVENT
	jr z,ophandlerEI_delay_expired
	
	; No event is scheduled for the current cycle, so fully set IME=1 now
	; and schedule an event at the following instruction, only if an
	; interrupt is currently requested
	ld a,trigger_interrupt - (intstate_smc_2 + 1)
	ld (intstate_smc_2),a
	ld hl,IF
	ld a,(hl)
	ld l,h
	and (hl)
	jr z,ophandlerEI_no_interrupt

	call get_mem_cycle_offset
	inc de	; Advance to cycle after EI
	inc de  ; Advance after EI delay cycle
	ld a,(hl)
	or a	; Check if cycles remain in the block
	jr z,++_
	; Interrupt check will happen in this block
	push hl
	 ld hl,i
	 add hl,de
	 ld i,hl
	pop hl
	dec hl
	dec hl
	ld de,(hl)
	ld c,a
	dec a
	ld iyl,a
	xor a
	cp iyh
	ld iyh,a
	jr c,_
	ld hl,(event_address)
#ifdef DEBUG
	ld a,h
	cp event_value >> 8
	jr z,$
#endif
	ld a,(event_value)
	ld (hl),a
#ifdef DEBUG
	ld hl,event_value
	ld (event_address),hl
#endif
	scf
_
	sbc a,a  ; Same as ld a,iyl \ sub c
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_later
#else
	jp.lil schedule_event_later
#endif
_
	; Interrupt check is delayed until the next block
	; Set cycle counter to -1 and let the next block schedule the event
	ld hl,i
	add hl,de
	ld i,hl
	pop.l hl
	exx
	dec a
	ld iyh,a
	ex af,af'
	ret
	
	
schedule_ei_delay:
	; Force an event after one GB cycle
	ld de,-1
	; Overwrite the function pointer with the following code,
	; which will run after the one GB cycle elapses
	call _
schedule_ei_delay_startup:
	; Restore the default counter checker pointer
	ld (ei_delay_event_smc),sp
	; Enable interrupts
	ld a,trigger_interrupt - (intstate_smc_2 + 1)
	ld (intstate_smc_2),a
	ret
_
	; Return to the next counter checker
	inc sp
	inc sp
	ret
	
ophandler76:
	pop ix
	ex af,af'
	exx
	push.l hl
	ld c,a
	ld hl,IF
	ld a,(hl)
	ld l,h
	and (hl)
	jr z,haltspin
	; Halt is being exited naturally
	; Reset halted state
	ld hl,((decode_intcache - (cpu_halted_smc+2)) << 8) | $28 ;JR Z,decode_intcache
	ld (cpu_halted_smc),hl
	; Advance to after the HALT
	lea ix,ix+3
	; Count cycles for the next sub-block
	ld a,(ix-3)
	add a,c
	jr c,ophandler76_maybe_overflow
ophandler76_no_overflow:
	pop.l hl
	exx
	ex af,af'
	jp (ix)
haltspin:
	; Set instruction cycle offset of 0
	ld c,a
	; Set halted state
	ld hl,((exit_halt - (cpu_halted_smc+2)) << 8) | $18 ;JR exit_halt
	ld (cpu_halted_smc),hl
	; Set GB address to HALT itself
	ld hl,(ix+1)
	dec hl
	ld (event_gb_address),hl
	; Set remaining cycles to 0
	ld iy,0
	; Push JIT address of HALT itself
	pea ix-3
	jp do_event_pushed
	
ophandler76_maybe_overflow:
	inc iyh
	jr nz,ophandler76_no_overflow
	ld iyl,a
	; Carry is set, we subtract out the cycle for the HALT itself
	sbc a,c
	ld c,a
	ld de,(ix-2)
	push ix
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_helper
#else
	jp.lil schedule_event_helper
#endif
	
trigger_event:
	exx
	push.l hl
trigger_event_pushed:
	 ; Get the cycle offset, GB address, and JIT address after the current opcode
	 call get_mem_cycle_offset
	 ; If the end of this instruction is already past the target, no reschedule
	 xor a
	 cp d
	 jr z,trigger_event_already_triggered
	 ; If the counter already overflowed, remove any already-scheduled event
	 cp iyh
	 ld iyh,a
trigger_event_remove_smc = $+1
	 jr nz,trigger_event_no_remove
#ifdef DEBUG
	 ld a,(event_address+1)
	 cp event_value >> 8
	 jr z,$
#endif
	 ld a,(event_value)
event_address = $+1
	 ld (event_value),a
	 xor a
#ifdef DEBUG
	 jr _
#endif
trigger_event_no_remove:
#ifdef DEBUG
	 ld a,(event_address+1)
	 sub event_value >> 8
	 jr nz,$
_
#endif
	 inc a  ; Cycle count at event is relative to the memory access
	 ld (event_cycle_count),a
	 add a,(hl)
	 ld iyl,a
	 dec hl
	 dec hl
	 ld hl,(hl)
	 ld (event_gb_address),hl
	 lea hl,ix
	 ld (event_address),hl
	 ld a,(hl)
	 ld (event_value),a
	 ld (hl),RST_EVENT
	 ld hl,i
	 add hl,de	; Reset div counter to the time of memory access
	 ld i,hl
trigger_event_already_triggered:
	pop.l hl
z80_restore_swap_ret:
	ld a,iyl
	ex af,af'
z80_swap_ret:
	exx
z80_ret:
	ret
		
ophandlerE8:
	exx
	ld c,a
	pop de
	ld a,(de)
	inc de
	push de
	ld de,(sp_base_address_neg)
	add hl,de
	ld e,a
	rla
	sbc a,a
	ld d,a
	ld a,l
	add hl,de
	add a,e
	; Reset Z flag but preserve H/C flags and keep N flag reset.
	ld a,$04
	daa  ; Resets H but increases low nibble to $A if H was set.
	daa  ; Iff low nibble is $A, sets H. Z is reset always.
	ld a,c
	jp set_gb_stack
	
ophandlerE9:
	push hl
	 exx
	pop de
	ld c,a
	push bc
     call.il lookup_code_cached
	pop bc
	scf
	adc a,c
	jr c,++_
_
	exx
	ex af,af'
	jp (ix)
_
	inc iyh
	jr nz,--_
	ld iyl,a
	sbc a,c
	ld c,a
	inc de
	dec de
	push ix
	push.l hl
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_helper
#else
	jp.lil schedule_event_helper
#endif
	
ophandlerF1:
	exx
	ld d,flags_lut >> 8
	inc b
ophandlerF1_jump_smc_1 = $+1
	djnz ophandlerF1_pop_z80
	ld a,h
do_pop_bound_smc_3 = $+1
	cp 0
ophandlerF1_jump_smc_2 = $+1
	jp m,ophandlerF1_pop_z80
ophandlerF1_overflow:
	ex af,af'
	ld iyl,a
	call pop_overflow
	exx
	pop de
ophandlerF1_continue:
	ld c,d
ophandlerF1_rtc_continue:
	ld d,flags_lut >> 8
	res 3,e
	ld a,(de)
	ld e,a
	ld d,c
	push de
	 exx
	pop af
	ret
	
ophandlerF1_pop_hmem:
	ex af,af'
	ld iyl,a
	call pop_hmem
	jr ophandlerF1_continue
	
ophandlerF1_pop_rtc:
	inc b
	ld ix,(sp_base_address)
	ld e,(ix)
	ld c,e
	inc.l hl
	inc.l hl
	jr ophandlerF1_rtc_continue
	
ophandlerF1_pop_adl:
	inc b
	ld.l e,(hl)
	inc.l hl
	res 3,e
	ld a,(de)
	ld e,a
	ld.l d,(hl)
	inc.l hl
	push de
	 exx
	pop af
	ret
	
ophandlerF1_pop_z80:
	bit 6,b
	jr nz,ophandlerF1_overflow
	inc b
	ld e,(hl)
	inc hl
	res 3,e
	ld a,(de)
	ld e,a
	ld d,(hl)
	inc hl
	push de
	 exx
	pop af
	ret
	
ophandlerF2:
	ld ixh,$FF
	ld ixl,c
	ex af,af'
	ld iyl,a
	call mem_read_ports_always
	ld a,ixl
	ret
	
ophandlerF3:
	push hl
	 ld hl,intstate_smc_1
	 ld (hl),$08 ;EX AF,AF'
	 ld hl,intstate_smc_2
	 ld (hl),0
	 ; Disable any delayed EI
	 ld hl,event_counter_checkers
	 ld (ei_delay_event_smc),hl
	pop hl
	ret
	
ophandlerF5:
	exx
	ld c,a
	push af
	pop de
	ld d,flags_lut >> 8
	set 3,e
	ld a,(de)
	ld d,c
	ld e,a
	ld a,c
do_push_jump_smc_1 = $+1
	djnz do_push_hmem
	jr do_push_check_overflow
	
ophandlerE5:
	push hl
	 exx
	pop de
do_push_jump_smc_2 = $+1
	djnz do_push_hmem
do_push_check_overflow:
	ex af,af'
	ld c,a
	ld a,h
do_push_bound_smc_1 = $+1
	cp 0
	jp m,do_push_overflow
	ld a,c
	ex af,af'
do_push_jump_smc_3 = $+1
	jr do_push_hmem
	
ophandlerD5:
	push de
	 exx
	pop de
do_push_jump_smc_4 = $+1
	djnz do_push_hmem
	jr do_push_check_overflow
	
ophandlerC5:
	push bc
	 exx
	pop de
do_push_jump_smc_5 = $+1
	djnz do_push_hmem
	jr do_push_check_overflow
	
do_push_adl:
	dec.l hl
	ld.l (hl),d
	dec.l hl
	ld.l (hl),e
	exx
	ret
	
do_push_z80:
	dec hl
	dec hl
	ld (hl),de
	exx
	ret

schedule_event_finish_for_call_now:
	ex de,hl
schedule_event_finish_for_call:
	ld (event_cycle_count),a
	ld (event_gb_address),hl
;#ifdef DEBUG
;	ld a,(event_address+1)
;	cp event_value >> 8
;	jr nz,$
;#endif
	lea hl,ix
	ld (event_address),hl
	ld a,(hl)
	ld (event_value),a
	ld (hl),RST_EVENT
schedule_event_finish_for_call_no_schedule:
	pop.l hl
	ld a,iyl
	ex af,af'
	pop de
	push.l de  ; Cache Game Boy return address
do_push_and_return_jump_smc = $+1
	djnz do_push_adl
	pop ix
	jr do_push_for_call_check_overflow

do_push_for_call_rtc:
	push bc
	push ix
do_push_rtc:
	ld ix,(sp_base_address)
	ld (ix+5),e
	dec.l hl
	dec.l hl
	exx
	ret
	
do_push_for_call_cart:
	push bc
	push ix
do_push_cart:
	ld ix,mem_write_cart_always
	push af
	 call do_push_generic
	pop af
	ret
	
do_push_for_call_vram:
	push bc
	push ix
do_push_vram:
	ld ix,mem_write_vram_always
	push af
	 call do_push_generic
	pop af
	ret
	
do_push_for_call_hmem:
	push bc
	push ix
do_push_hmem:
	push af
	 ; We need special handling if an event is triggered by the first write
	 ld ix,event_cycle_count
	 dec (ix)
	 dec hl
	 push hl
	 pop ix
	 ex af,af'
	 ld iyl,a
	 ld a,e
	 push af
	  ld a,d
	  ex af,af'
	  dec hl
	  push hl
	   ld de,-1
	   add iy,de
	   exx
	   jr nc,push_hmem_underflow
	   call mem_write_hmem_swapped
push_hmem_continue:
	   ; Restore the event cycle count, or increment if an event was triggered
	   ld ix,event_cycle_count
	   inc (ix)
	  pop ix
	 pop af
	 ex af,af'
	 inc iy
	 call mem_write_hmem_swapped
	pop af
	ret
	
do_push_for_call:
	ex af,af'
	push.l de  ; Cache Game Boy return address
do_push_for_call_jump_smc_1 = $+1
	djnz do_push_for_call_adl
	push bc  ; Push decremented stack offset and RET cycle count
do_push_for_call_check_overflow:
	ex af,af'
	ld c,a
	ld a,h
do_push_bound_smc_2 = $+1
	cp 0
	jp m,do_push_for_call_overflow
	ld a,c
	ex af,af'
do_push_for_call_jump_smc_2 = $+1
	jr do_push_for_call_rtc
	
	   ; If the cycle count underflowed, modify event trigger logic
	   ; to force it to recognize the count as an overflow
push_hmem_underflow:
	   xor a
	   ld (trigger_event_remove_smc),a
	   call mem_write_hmem_swapped
	   ld a,trigger_event_no_remove - (trigger_event_remove_smc+1)
	   ld (trigger_event_remove_smc),a
	   jr push_hmem_continue
	
do_push_for_call_z80:
	push bc
	dec hl
	dec hl
	ld (hl),de
	exx
	jp (ix)
	
do_push_for_call_adl:
	push bc
	dec.l hl
	ld.l (hl),d
	dec.l hl
	ld.l (hl),e
	exx
	jp (ix)
	
	; Pushes using the memory write routine passed in IX
	; Currently, the routine must not require cycle info
	; Destroys AF and unswaps BCDEHL'
do_push_generic:
	dec.l hl
	push hl
	 ex (sp),ix
	 dec.l hl
	 ex af,af'
	 ld iyl,a
	 ld a,e
	 ld c,d
	 ld de,(sp_base_address_neg)
	 add ix,de
	pop de
	push de
	 push af
	  pea ix-1
	   call _
	  pop ix
	 pop af
	 ex af,af'
	ret
_
	push de
	 ld a,c
	 exx
	 ex af,af'
	ret
	
ophandlerF8:
	ld ixl,a
	pop hl
	ld a,(hl)
	inc hl
	push hl
	exx
	push hl
	 exx
	pop hl
	push de
	 ld de,(sp_base_address_neg)
	 add hl,de
	 ld e,a
	 rla
	 sbc a,a
	 ld d,a
	 ld a,l
	 add hl,de
	 add a,e
	 ; Reset Z flag but preserve H/C flags and keep N flag reset.
	 ld a,$04
	 daa  ; Resets H but increases low nibble to $A if H was set.
	 daa  ; Iff low nibble is $A, sets H. Z is reset always.
	pop de
	ld a,ixl
	ret
	
reset_z_flag:
	; IYH is guaranteed to be 0 or at least 118 (for double speed frame)
	ld iyl,iyh
	dec iyl
	ret
	
ophandlerF9:
	push hl
	 exx
	pop hl
	jp set_gb_stack
	
ophandlerRETcond:
	; Increment the taken cycle count by 1 before returning
	exx
	pop de
	inc de ; Make sure not to destroy flags
	ret
	
ophandlerRETI:
	exx
	ld c,a
	; Enable interrupts
	ld a,$C9 ;RET
	ld (intstate_smc_1),a
	ld a,trigger_interrupt - (intstate_smc_2 + 1)
	ld (intstate_smc_2),a
	; Check if an interrupt is pending, if not then return normally
	ex.l de,hl
	ld hl,IF
	ld a,(hl)
	ld l,h  ;ld hl,IE
	and (hl)
	jr nz,_
	ld a,c
	ex.l de,hl
	ex af,af'
	pop de
	ret
_
	ld iyl,c
	ld a,b
	; Schedule an event after the return
	lea bc,iy+4
	inc b
	djnz _
	; If an event will already be scheduled on return, just return
	ld b,a
	ld a,iyl
	ex.l de,hl
	ex af,af'
	pop de
	ret
_
	; Update the cycle counter
	ld hl,i
	add hl,bc
	ld i,hl
	ld b,a
	ld iyh,-1
	ld a,-4
	ex.l de,hl
	ex af,af'
	pop de
	ret
	
pop_hmem:
	inc b
	push hl
	pop ix
	inc hl
	push hl
	 inc hl
	 exx
	 dec iy
	 call mem_read_hmem_swapped
	 ex (sp),ix
	 inc iy
	 ex af,af'
	 call mem_read_hmem_swapped
	 exx
	pop de
	ld d,ixl
	ret
	
write_vram_handler:
	pop ix
	pea ix+2
	ex af,af'
	ld iyl,a
	exx
	ld de,(ix)
	jp.lil write_vram_and_expand_swapped
	
mem_write_vram_always:
	jp.lil write_vram_and_expand
	
mem_write_any_vram:
	push hl
	 exx
	pop de
	jp.lil write_vram_and_expand_swapped
	
write_cart_handler:
	pop ix
	pea ix+2
	ld ix,(ix)
	ex af,af'
	ld iyl,a
	jp mem_write_cart_always
	
write_cram_bank_handler:
	pop ix
	pea ix+2
	exx
	ld de,(ix)
	ld.lil ix,(cram_bank_base)
	ex af,af'
write_cram_bank_handler_smc_1 = $+1
	add.l ix,de
	ex af,af'
write_cram_bank_handler_smc_2 = $+3
	ld.l (ix),a
	exx
	ret
	
read_rom_bank_handler:
	pop ix
	pea ix+2
	exx
	ld de,(ix)
	ld.lil ix,(rom_bank_base)
	ex af,af'
	add.l ix,de
	ex af,af'
	ld.l a,(ix)
	exx
	ret
	
read_cram_bank_handler:
	pop ix
	pea ix+2
	exx
	ld de,(ix)
	ld.lil ix,(cram_bank_base)
	ex af,af'
read_cram_bank_handler_smc = $+1
	add.l ix,de
	ex af,af'
	ld.l a,(ix)
	exx
	ret
	
readP1handler:
	ex af,af'
	ld iyl,a
	call readP1
	ld a,ixl
	ret
	
readDIVhandler:
	ex af,af'
	ld iyl,a
	call readDIV
	ld a,ixl
	ret
	
readTIMAhandler:
	ex af,af'
	ld iyl,a
	call readTIMA
	ld a,ixl
	ret
	
readLYhandler:
	ex af,af'
	ld iyl,a
	call readLY
	ld a,ixl
	ret
	
readSTAThandler:
	ex af,af'
	ld iyl,a
	call readSTAT
	ld a,ixl
	ret
	
readSTAT_vblank:
	 daa
	 cp (hl)
	 jr z,readSTAT_mode1
	 res 2,c
	 jr readSTAT_mode1
	
readLY_maybeforce0:
	ld a,d
	rra
	cp 1
	jr c,readLY_noforce0
readLY_force0:
	xor a
	jr readLY_continue
	
readP1:
	ld a,(P1)
	or $CF
	ld ix,(keys)
	bit 4,a
	jr nz,_
	and ixl 
_
	bit 5,a
	jr nz,readP1_finish
	and ixh
	jr readP1_finish
	
readSTAT:
	call get_mem_cycle_offset_swap_push
	 call get_scanline_from_cycle_offset
	 ld d,a
	 ld hl,STAT
	 ld a,(hl)
	 or $87
	 ld c,a
	 dec hl
	 bit 7,(hl)
	 jr z,readSTAT_mode0
	 ld l,LYC & $FF
	 ld a,e
	 sub 10
	 jr c,readSTAT_vblank
	 cp (hl)
	 jr z,_
	 res 2,c
_
	 ld a,d
	 sub MODE_2_CYCLES<<1
	 jr c,readSTAT_mode2
	 sub MODE_3_CYCLES<<1
	 jr c,readSTAT_mode3
readSTAT_mode0:
	 dec c
readSTAT_mode1:
	 dec c
readSTAT_mode2:
	 dec c
readSTAT_mode3:
	 ld ixl,c
	pop.l hl
	exx
	ld a,iyl
	ex af,af'
	ret
	
readLY:
	exx
	ld a,(LCDC)
	add a,a
	jr nc,readLY_force0
	call get_mem_cycle_offset_push
	 call get_scanline_from_cycle_offset
	pop.l hl
	ld d,a
	ld a,e
	sub 9
	jr z,readLY_maybeforce0
	jr nc,_
readLY_noforce0:
	daa
_
	dec a
readLY_continue:
	exx
readP1_finish:
	ld ixl,a
	ld a,iyl
	ex af,af'
	ret

	
	;IX=GB address, reads into IXL
mem_read_hmem:
	ex af,af'
	ld iyl,a
mem_read_hmem_swapped:
	ld a,ixh
	cp $FE
	jr c,mem_read_bail
	jr z,mem_read_oam
	;IX=GB address, reads into IXL, AF'=GB AF
mem_read_ports_always:
	ld a,ixl
	add a,a
	jr c,mem_read_hram
	jr z,readP1
	cp TIMA*2 & $FF
	jr z,readTIMA
	cp LY*2 & $FF
	jr z,readLY
	cp DIV*2 & $FF
	jr z,readDIV
	cp STAT*2 & $FF
	jr z,readSTAT
mem_read_hram:
mem_read_oam:
	ld ix,(ix)
	ld a,iyl
	ex af,af'
	ret
	
readTIMA:
	call updateTIMA
	 ld ixl,a
	pop.l hl
	exx
	ld a,iyl
	ex af,af'
	ret
	
mem_read_bail:
	pop ix
	ld a,iyl
	ex af,af'
	pea ix-8
	ret
	
mem_read_any_before_write:
	dec iy
	;HL=GB address, reads into A, AF'=GB AF
mem_read_any:
	ld a,h
	cp $FE
	jr nc,mem_read_any_hmem
	ex de,hl
	add a,a
	jr c,++_
	add a,a
	jr c,_
rom_start_smc_2 = $+3
	ld.lil ix,0
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
_
	ld.lil ix,(rom_bank_base)
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
_
	add a,a
	jr nc,_
	ld.lil ix,wram_base
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret p
	ex de,hl
	ld.lil ix,wram_base-$2000
	jr mem_read_any_finish
	
readDIV:
	call get_mem_cycle_offset_swap_push
	 ld hl,i
	 add hl,de
	 add hl,hl
	 add hl,hl
	 ex de,hl
	 ld ixl,d
	pop.l hl
	exx
	ld a,iyl
	ex af,af'
	ret
	
_
	jp m,_
	ld.lil ix,vram_base
mem_read_any_finish:
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
_
	ld.lil ix,(cram_bank_base)
mem_read_any_rtc_smc = $+1
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
	
mem_read_any_hmem:
	jr z,mem_read_any_oam
	ld a,l
	add a,a
	jr c,mem_read_any_hram
	push hl
	pop ix
	call mem_read_ports_always
	ex af,af'
	ld a,ixl
	ret
mem_read_any_hram:
mem_read_any_oam:
	ld a,(hl)
	ret
	
	;HL=GB address, IXL=data, destroys A,AF'
mem_write_any_after_read_ixl:
	ld a,ixl
mem_write_any_after_read:
	inc iy
	;HL=GB address, A=data, preserves AF, destroys AF'
mem_write_any:
	ex af,af'
mem_write_any_swapped:
	ld a,h
	cp $FE
	jr nc,mem_write_any_hmem
	add a,a
	jr nc,mem_write_any_cart
	add a,a
	jr nc,_
	ex de,hl
	add a,a
	jr c,mem_write_any_wram_mirror
mem_write_any_wram:
	ld.lil ix,wram_base
mem_write_any_finish:
	add.l ix,de
	ex de,hl
	ld a,iyl
	ex af,af'
	ld.l (ix),a
	ret
	
_
	jp p,mem_write_any_vram
mem_write_any_cram:
	ex de,hl
	ld.lil ix,(cram_bank_base)
mem_write_any_cram_smc_1 = $+1
	add.l ix,de
	ex de,hl
	ld a,iyl
	ex af,af'
mem_write_any_cram_smc_2 = $+3
	ld.l (ix),a
	ret
	
mem_write_any_cart:
	push hl
	pop ix
	jr mem_write_cart_always
	
mem_write_any_wram_mirror:
	ld.lil ix,wram_base-$2000
	jr mem_write_any_finish
	
mem_write_any_hmem:
	push hl
	pop ix
	jr nz,mem_write_ports_swapped
	jr mem_write_oam_swapped
	
	; Inputs: IX = GB address
	;         IY = cycle counter,
	;         A = data to wrote
	; Outputs: A' = low cycle counter
	; Destroys: IX, F', C', DE'
mem_write_hmem:
	ex af,af'
	ld iyl,a
mem_write_hmem_swapped:
	ld a,ixh
	inc a
	jr z,mem_write_ports_swapped
	inc a
	jr nz,mem_write_bail
mem_write_hram_swapped:
mem_write_oam_swapped:
	ld a,iyl
	ex af,af'
	ld (ix),a
	ret

ophandlerE2:
	ld ixh,$FF
	ld ixl,c
	ex af,af'
	ld iyl,a
; Inputs: IX = GB address
;         IY = cycle counter,
;         A' = data to write
; Outputs: AF = input AF'
;          A' = low cycle counter
; Destroys: IX, F', C', DE'
mem_write_ports_swapped:
	ld a,ixl
	cp $7F
	jp pe,mem_write_hram_swapped
	push hl
	 sub WX+1-ioregs
	 ld l,a
	 ld h,mem_write_port_routines >> 8
	 ld l,(hl)
	 ex (sp),hl
	 ret m
	pop af
	ld a,iyl
	ex af,af'
	ret
	
	;IX=GB address, A=data, preserves AF, destroys F'
mem_write_vram:
	ex af,af'
	ld iyl,a
	ld a,ixh
	sub $20
	jp.lil pe,write_vram_and_expand
mem_write_bail:
	pop ix
	ld a,RST_MEM
	cp (ix-8)
	jr z,mem_write_bail_a
	cp (ix-10)
	jr z,mem_write_bail_r
	dec ix
mem_write_bail_r:
	ld a,iyl
	ex af,af'
	pea ix-10
	ret
mem_write_bail_a:
	ld a,iyl
	ex af,af'
	push af
	pea ix-8
	ret
	
	;IX=GB address, A=data, preserves AF, destroys F'
mem_write_cart:
	ex af,af'
	ld iyl,a
	ld a,ixh
	rla
	jr c,mem_write_bail
	;IX=GB address, A'=data, preserves AF', destroys AF
mem_write_cart_always:
	ld a,ixh
	sub $20
	jr c,mbc_0000
	sub $20
	jr c,mbc_2000
	sub $20
	jr c,mbc_4000
	
mbc_6000:
	ld a,(mbc_z80)
	cp 4 ;MBC3+RTC
	jr z,++_
	dec a
	jr nz,mbc_6000_denied
	ex af,af'
	push af
	 ex af,af'
	pop af
	rra
	ld a,$28
	jr nc,_
	ld a,$20
_
	ld (mbc1_ram_smc),a
mbc_6000_denied:
mbc_0000:
	ld a,iyl
	ex af,af'
	ret
	
_
	jp.lil mbc_rtc_latch_helper
	
mbc_4000:
	push bc
	 ld b,$60
	 ex af,af'
	 ld c,a
	 ex af,af'
mbc_z80 = $+1
	 ld a,0
	 dec a	; Check for MBC1
	 jr nz,_
mbc1_ram_smc = $
	 jr z,mbc_ram
	 ld a,c
	 rrca
	 rrca
	 rrca
	 jr mbc_4000_continue
_
	 srl a
	 jr nc,mbc_ram ; MBC3 or MBC5
	 jp.lil nz,mbc_rtc_helper ; MBC3+RTC
	 jr mbc_4000_denied ; MBC2
mbc_ram:
cram_size_smc = $
	 or a
	 sbc a,a
	 and c
	 rrca
	 rrca
	 rrca
	 and b
	 ld b,a
	 ld c,0
cram_base_0 = $+3
	 ld.lil ix,0
mbc_ram_any:
	 add.l ix,bc
	 ld.lil (cram_bank_base),ix
	 ; See if SP is pointing into the swapped bank
	 ld a,(curr_gb_stack_bank)
	 cp 5 << 5
	 jr nz,mbc_no_fix_sp
mbc_fix_sp:
	 ; If so, update it
	 exx
	 push bc
	  ld bc,(sp_base_address_neg)
	  add hl,bc
	  call.il set_gb_stack_bounds_helper
	  ex de,hl
	  add.l hl,bc
	 pop bc
	 exx
mbc_no_fix_sp:
mbc_4000_denied:
	pop bc
	ld a,iyl
	ex af,af'
	ret
	
mbc_2000:
	push bc
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,c
rom_bank_mbc_mask_smc = $+1
	 ld b,$FF
mbc_4000_continue:
curr_rom_bank = $+1
	 ld c,1
	 ; Mask the new value and check if 0-page should be overridden
	 and b
mbc5_rom_bank_smc = $
	 jr z,mbc_zero_page_override
mbc_zero_page_continue:
	 ; Set only the given mask of the page
	 xor c
	 and b
	 xor c
	 ; Adjust value to physical page based on ROM size
rom_bank_mask_smc = $+1
	 and 0
mbc5_rom_bank_continue:
	 ld (curr_rom_bank),a
	 ld (rom_bank_check_smc_1),a
	 ld (rom_bank_check_smc_2),a
	 ld b,a
	 xor c
	 push hl
	  jp.lil nz,mbc_change_rom_bank_helper
mbc_2000_finish:
	 pop hl
	pop bc
	ld a,iyl
	ex af,af'
	ret
	
mbc_zero_page_override:
	; If the masked value is 0, increase the result (except MBC5)
	; When setting the high bits, this is ignored by the masking below
	inc a
	jr mbc_zero_page_continue
	
	
write_audio_enable:
	 or c
	 ld (de),a
	 ; Set the appropriate bit in NR52
	 ld a,e
	 and 3
	 cp 2
	 inc a
	 jr c,_
	 sub 2
	 add a,a
	 add a,a
_
	 ld c,a
	 ld e,NR52-ioregs
	 ld a,(de)
	 or c
	 ld (de),a
	 exx
	pop af
	ret
	
get_mem_cycle_offset_swap_push:
	exx
get_mem_cycle_offset_push:
	push.l hl

; Inputs: IY = current block cycle base
;         I = number of empty call stack entries
;         (bottom of stack) = JIT return address
;         AFBCDEHL' have been swapped
; Outputs: DE = (negative) cycle offset
;          May be positive if target lies within an instruction
;          (HL) = cycles until block end
;          (HL-2) = Game Boy address
;          IX = current JIT address
; Destroys HL,IX
get_mem_cycle_offset:
	; Get the address of the recompiled code: the bottom stack entry
	ld hl,(((myz80stack - 4 - 2) / 2) - (myADLstack - 3)) & $FFFF
	add.l hl,sp
	add hl,hl
	ld ix,(hl)
	; Check if the JIT target address is an absolute jump instruction,
	; which indicates that the memory access is a branch dispatch
	ld a,$C3
	cp (ix)
	jr z,get_mem_cycle_offset_for_branch
	; Assume the JIT code was a routine call; get its target address
	ld hl,(ix-2)
	; Check if the target begins with a JP, RST 00h, or RST 10h instruction.
	; This should only be the case when the target is a cycle cache trampoline.
	; Note that to avoid false positives, no routine called directly from JIT
	; should start with JP nnnn; OUT (nn),A; RST 00h; or RST 10h.
	xor (hl)
	and $EB
	jr nz,resolve_get_mem_cycle_offset_call
	; We probably have a trampoline target; however, we must check whether our
	; assumption that the JIT code was a routine call is accurate. If the first
	; byte of the JIT code is a NOP, then we actually have a bitwise prefix op
	; or a POP.
	cp (ix-3)
	jr z,resolve_mem_cycle_offset_prefix
	; Get a pointer to the cached cycle offset, which is right before the target
	dec hl
get_mem_cycle_offset_continue:
	; Carry is set, calculate the cycle count in DE.
	; The extra one cycle is subtracted because the memory access occurs during
	; the last cycle of the instruction.
	ld a,iyl
	sbc a,(hl)
	ld e,a
	ld d,iyh
	ret nc
	dec d
	ret
	
	
resolve_get_mem_cycle_offset_call:
	; If the code is not in the JIT area, the routine call was actually for a RET
	ld a,ixh
	cp jit_start >> 8
	jr c,get_mem_cycle_offset_for_ret
	; Check if the JIT code corresponds to a prefixed bitwise operation, which begins with NOP
	xor a
	cp (ix-3)
	jr z,resolve_mem_cycle_offset_prefix
	; If not, the JIT code is a routine call, so we emit a jump to the original call target
	ld c,h
	ld h,l
	ld l,$C3
	.db $CA	;JP Z,
resolve_mem_cycle_offset_prefix:
	; For a prefixed bitwise operation, we emit the two-byte operation followed by a RET
	ld c,$C9
	jp.lil resolve_mem_cycle_offset_helper

get_mem_cycle_offset_for_branch:
	; This is a push related to an RST, CALL, or interrupt
	ld a,ixh
	cp jit_start >> 8
	jp.lil nc,get_mem_cycle_offset_for_call_helper
	
	; Retrieve the cycle info and actual target address,
	; and infer the Game Boy address
	ld hl,mem_cycle_scratch
	ld a,ixl
	sub dispatch_rst_00 & $FF
	add a,a
	ld (hl),a
	inc hl
	ld (hl),0
	; Subtract 4 cycles for RST, or 5 for interrupt
	cp $40
	ld a,-5
get_mem_cycle_offset_for_call_finish:
	adc a,(ix+3)
	inc hl
	ld (hl),a
	ld ix,(ix+1)  ; Get the actual target from the dispatch
	; Check if it's possibly the flush handler
	ld a,(jit_start >> 8) - 1
	cp ixh
	jr c,get_mem_cycle_offset_continue
	; The target should be the flush handler.
#ifdef DEBUG
	ld a,ixh
	cp flush_handler >> 8
	jr nz,$
	ld a,ixl
	cp flush_handler & $FF
	jr nz,$
#endif
	; Set the JIT address to a harmless location in case an event is scheduled.
	ld ix,event_value
	scf
	jr get_mem_cycle_offset_continue

get_mem_cycle_offset_for_ret:
	; The second read of an unconditional RET is always two cycles after
	; the end of a JIT sub-block. A conditional RET adds an extra cycle,
	; so we must retrieve this information which is saved on the stack.
	ld a,l
	cp pop_overflow & $FF
	; Get the address of the cycle count stored on the stack,
	; this is just above the return address of the call to pop_overflow
	ld hl,(((myz80stack - 4 - 4) / 2) - (myADLstack - 3)) & $FFFF
	add.l hl,sp
	add hl,hl
	ld a,(hl)
	jr z,_
	; For a callstack-based return, we must subtract the original count
	; which is located underneath the return address
	inc hl
	inc hl
	inc hl
	inc hl
	sub (hl)
_
	; Check the low bit to see if it was conditional
	rra
	; IX and HL returns are never used by reads; just get the cycle count
	lea de,iy+2
	ret nc
	inc de
	ret

mem_cycle_scratch:
	.dw 0
	.db 0
	
get_scanline_overflow:
	sbc hl,de
	jr get_scanline_from_cycle_count
	
	
get_mem_scanline_offset:
	call get_mem_cycle_offset_swap_push
	
; Inputs: DE = (negative) cycles until target
;         May be non-negative if target falls within an instruction
; Outputs: A = cycle count within scanline (as if running in double-speed mode)
;          E = scanline index relative to vblank
;              (0-9 during vblank, 10-153 during frame)
; Destroys: D, HL
get_scanline_from_cycle_offset:
	ld hl,i
	add hl,de
	ld de,(vblank_counter)
	xor a
	sbc hl,de
	ld de,CYCLES_PER_FRAME
	add hl,de
	jr nc,get_scanline_overflow
	
; Inputs: HL = cycle count within frame
; Outputs: A = cycle count within scanline (as if running in double-speed mode)
;          E = scanline index relative to vblank
;              (0-9 during vblank, 10-153 during frame)
; Destroys: D, HL
get_scanline_from_cycle_count:
	add hl,hl
scanline_cycle_count_cache = $+1
	ld de,0
	xor a
	sbc hl,de
	cp h
	jr nz,++_
	ld a,l
scanline_index_cache = $+1
	ld de,(CYCLES_PER_SCANLINE<<1) * 256 + 0
	cp d
	ret c
	sub d
	cp d
	jr nc,_
	inc e
	ex de,hl
get_scanline_from_cycle_count_finish:
	ld e,l
	ld (scanline_index_cache),hl
	mlt hl
	ld (scanline_cycle_count_cache),hl
	ret
_
	mlt de
_
	add hl,de
	
	; Algorithm adapted from Improved division by invariant integers
	; To make things simpler, a pre-normalized divisor is used, and the dividend
	; and remainder are scaled and descaled according to the normalization factor
	; This should also make it trivial to support GBC double-speed mode in the
	; future where the normalized divisor will be the actual divisor
	ld d,65535 / (CYCLES_PER_SCANLINE<<1) - 256
	ld e,h
	mlt de
	ld a,l
	add hl,de
	inc h
	ld e,h
	ld d,CYCLES_PER_SCANLINE<<1
	mlt de
	sub e
	cp l
	ld l,h
	ld h,CYCLES_PER_SCANLINE<<1
	jr c,_
	jr z,_
	dec l
	add a,h
	jr c,get_scanline_from_cycle_count_finish
	; Unlikely condition (allow redundant compare)
_
	cp h
	jr c,get_scanline_from_cycle_count_finish
	; Unlikely condition
	inc l
	sub h
	jr get_scanline_from_cycle_count_finish
	
	
; Output: BCDEHL' are swapped
;         (SPL) = saved HL'
;         DE = current cycle offset
;         A = current TIMA value
;         (TIMA) updated to current value
;         Interrupts are disabled
updateTIMA:
	call get_mem_cycle_offset_swap_push
	 ld a,(TAC)
	 and 4
	 ld a,(TIMA)
	 ret z
	 ld hl,i
	 ld a,b
	 ld bc,(timer_counter)
	 sbc hl,bc
	 ld b,a
	 ; Handle special case if cycle offset is non-negative
	 xor a
	 cp d
	 add hl,de
	 jr z,updateTIMAoverflow
updateTIMAcontinue:
updateTIMA_smc = $+1
	 jr $+8
	 add hl,hl
	 add hl,hl
	 add hl,hl
	 add hl,hl
	 add hl,hl
	 add hl,hl
	 add a,h
	 ld (TIMA),a
	 ret
	
updateTIMAoverflow:
	 ; Check if adding the cycle offset created a non-negative result
	 jr c,_
	 sbc hl,de
	 add hl,de
	 jr nz,updateTIMAcontinue
_
	 ; If so, offset the TIMA value by TMA
	 ld a,(TMA)
	 jr updateTIMAcontinue
	
	.block (-$-189)&$FF
	
_writeSC:
	ld a,iyl
	ex af,af'
_writeSChandler:
	push af
	 push hl
	  or $7E
	  ld (SC),a
	  inc a
	  ld hl,disabled_counter_checker
	  jr nz,_
	  ex af,af'
	  ld iyl,a
	  call trigger_event
	  ; Set this cycle count after setting up the trigger
	  ld hl,i
	  ld a,h
	  add a,1024 >> 8
	  ld h,a
	  ld (serial_counter),hl
	  ld hl,serial_counter_checker
_
	  ld (event_counter_checker_slot_serial),hl
	 pop hl
	pop af
	ret
	
_writeNR52handler:
	ex af,af'
	ld iyl,a
_writeNR52:
	jp.lil NR52_write_helper
	
mem_write_port_handler_base = $-2
writeNR52handler:
	jr _writeNR52handler
writeSChandler:
	jr _writeSChandler
writeNR10handler:
	call write_audio_handler
	.db NR10 - ioregs
writeNR11handler:
	call write_audio_handler
	.db NR11 - ioregs
writeNR12handler:
	call write_audio_handler
	.db NR12 - ioregs
writeNR13handler:
	call write_audio_handler
	.db NR13 - ioregs
writeNR14handler:
	call write_audio_handler
	.db NR14 - ioregs
writeNR21handler:
	call write_audio_handler
	.db NR21 - ioregs
writeNR22handler:
	call write_audio_handler
	.db NR22 - ioregs
writeNR23handler:
	call write_audio_handler
	.db NR23 - ioregs
writeNR24handler:
	call write_audio_handler
	.db NR24 - ioregs
writeNR30handler:
	call write_audio_handler
	.db NR30 - ioregs
writeNR31handler:
	call write_audio_handler
	.db NR31 - ioregs
writeNR32handler:
	call write_audio_handler
	.db NR32 - ioregs
writeNR33handler:
	call write_audio_handler
	.db NR33 - ioregs
writeNR34handler:
	call write_audio_handler
	.db NR34 - ioregs
writeNR41handler:
	call write_audio_handler
	.db NR41 - ioregs
writeNR42handler:
	call write_audio_handler
	.db NR42 - ioregs
writeNR43handler:
	call write_audio_handler
	.db NR43 - ioregs
writeNR44handler:
	call write_audio_handler
	.db NR44 - ioregs
writeNR50handler:
	call write_audio_handler
	.db NR50 - ioregs
writeNR51handler:
	call write_audio_handler
	.db NR51 - ioregs
	
writeSCYhandler:
	ld ix,SCY
	jr write_scroll_swap
	
writeSCXhandler:
	ld ix,SCX
	jr write_scroll_swap
	
writeWYhandler:
	ld ix,WY
	jr write_scroll_swap
	
writeWXhandler:
	ld ix,WX
	jr write_scroll_swap
	
writeDMAhandler:
	ld ix,DMA
	jr write_scroll_swap
	
writeBGPhandler:
	ld ix,BGP
	jr write_scroll_swap

writeIEhandler:
	push af
	 ex af,af'
	 ld iyl,a
	 jp writeIEhandler_continue

writeIFhandler:
	push af
	 ex af,af'
	 ld iyl,a
	 jp writeIFhandler_continue

write_audio_handler:
	ex af,af'
	ld iyl,a
	ex (sp),hl
	ld ix,(hl)
	pop hl
	
#if $ & 255
	.error "mem_write_port_routines must be aligned: ", $ & 255
#endif
	
mem_write_port_routines:
write_audio:
	ld a,iyl
	ex af,af'
write_audio_disable_smc = $
	push af
	 exx
	 ld c,a
	 ld e,ixl
	 ld d,$FF
	 ld ixh,audio_port_value_base >> 8
	 ld (ix),c
	 ld a,(ix + audio_port_masks - audio_port_values)
	 ; Handle writes to the enable bit specially
	 cp $BF
	 jr nz,_
	 bit 7,c
	 jp nz,write_audio_enable
_
	 or c
	 ld (de),a
	 exx
	pop af
	ret
	
write_scroll_swap:
	ex af,af'
	ld iyl,a
write_scroll:
	push ix
	 call get_mem_scanline_offset
	pop hl
	jp.lil scroll_write_helper
	
writeLCDChandler:
	ex af,af'
	ld iyl,a
writeLCDC:
	call get_mem_cycle_offset_swap_push
	push de
	 call get_scanline_from_cycle_offset
	pop hl
	jp.lil lcdc_write_helper
	
writeTAChandler:
	ex af,af'
	ld iyl,a
writeTAC:
	call updateTIMA
	jp.lil tac_write_helper
	
writeDIVhandler:
	ex af,af'
	ld iyl,a
writeDIV:
	call updateTIMA
	jp.lil div_write_helper
	
writeSTAThandler:
	ex af,af'
	ld iyl,a
writeSTAT:
	call get_mem_scanline_offset
	jp.lil stat_write_helper
	
writeLYChandler:
	ex af,af'
	ld iyl,a
writeLYC:
	call get_mem_scanline_offset
	jp.lil lyc_write_helper
	
writeTIMAhandler:
	ex af,af'
	ld iyl,a
writeTIMA:
	call updateTIMA
	ex af,af'
	ld (TIMA),a
	ex af,af'
	jp.lil tima_write_helper
	
writeIE:
	ex af,af'
	push af
	 ex af,af'
writeIEhandler_continue:
	pop af
	and $1F
	ld (IE),a
	jr checkInt
	
writeIF:
	ex af,af'
	push af
	 ex af,af'
writeIFhandler_continue:
	pop af
	or $E0
	ld (IF),a
checkInt:
	; Check the pre-delay interrupt state, since if the interrupt enable
	; delay is active then an interrupt check is already scheduled
	ld a,(intstate_smc_2)
	or a
	jr z,checkIntDisabled
	push hl
	 ld hl,IF
	 ld a,(hl)
	 ld l,h
	 and (hl)
	pop hl
	jp nz,trigger_event
checkIntDisabled:
write_port_ignore:
	ld a,iyl
	ex af,af'
	ret
	
writeSC:
	jp _writeSC
	
writeNR52:
	jp _writeNR52
	
write_port_direct:
	ld a,iyl
	ex af,af'
	ld (ix),a
	ret
	
	.echo (mem_write_port_routines+256-(WX+2-ioregs))-$, " bytes remaining for port writes"
	.block (mem_write_port_routines+256-(WX+2-ioregs))-$

	.db writeIE - mem_write_port_routines
;00
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db writeSC - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db writeDIV - mem_write_port_routines
	.db writeTIMA - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db writeTAC - mem_write_port_routines
;08
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db writeIF - mem_write_port_routines
;10
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
;18
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
;20
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db writeNR52 - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
;28
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
;30
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
;38
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
;40
	.db writeLCDC - mem_write_port_routines
	.db writeSTAT - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db writeLYC - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
;48
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	
audio_port_value_base:
	.block 1
	
rtc_latched:
	.db 0	;seconds
	.db 0	;minutes
	.db 0	;hours
	.dw 0	;days
rtc_current:
	.db 0	;seconds
	.db 0	;minutes
	.db 0	;hours
	.dw 0	;days
rtc_last:
	.db 0   ;seconds
	.db 0   ;minutes
	.db 0   ;hours
	.dw 0   ;days
	
audio_port_values:
	.block NR52 - NR10
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
	
keys:
	.dw $FFFF
	
memroutine_next:
	.dl 0
render_save_sps:
	.dw 0
	
	; One word of stack space for sprite rendering during vblank
	.dw 0
event_counter_checkers_ei_delay:
	.dw schedule_ei_delay_startup
event_counter_checkers:
event_counter_checker_slot_LYC:
	.dw disabled_counter_checker
event_counter_checker_slot_STAT:
	.dw disabled_counter_checker
event_counter_checker_slot_timer:
	.dw disabled_counter_checker
event_counter_checker_slot_serial:
	.dw disabled_counter_checker
event_counter_checker_slot_audio:
	.dw audio_counter_checker
	.dw event_counter_checkers_done
	
	.assume adl=1
z80codesize = $-0
	.org z80code+z80codesize
	
	.echo "Z80 mode code size: ", z80codesize
	
jit_start = z80codesize
