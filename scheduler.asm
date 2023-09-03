	.assume adl=0

	; Check if an event was scheduled at or before the current memory cycle
	; Inputs: DE = cycle count at end of block (only call when D=0)
	;         C = block-relative cycle offset (negative)
	;         I = time of next event
	; Outputs: DE = updated cycle count at end of block
	;          I = updated time of next event
	; Destroys: AF, B, HL
handle_events_for_mem_access:
	ld a,e
	add a,c
	ret nc
	; Save and override the terminating event counter checker, preventing interrupt dispatch
	ld hl,(event_counter_checkers_ei_delay)
	push hl
	 ld hl,event_expired_for_mem_access_loop
	 ld (event_counter_checkers_ei_delay),hl
	 push ix
	  ; Save the cycle remainder in IX
	  ld ixl,a
	  ld ixh,d
	  ; Advance the cycle offsets to after the current cycle
	  cpl
	  ld hl,event_cycle_count
	  add a,(hl)
	  ld (hl),a
	  ASSERT_C
	  ld a,c
	  cpl
	  ld e,a
	  jr do_event_pushed

event_expired_for_mem_access_loop:
	   ld sp,(event_save_sp)
	   ld h,b
	   ld l,c
	   ; Check if there are more events before the memory access
	   add ix,de
	   jr c,event_expired_more_events
	   ; Advance the next event time to after the current cycle
	   sbc hl,de
	   lea de,ix+1
	   add hl,de
	   ld i,hl
	  pop de
	  ; Restore the memory cycle offset
	  ld a,e
	  cpl
	  ld c,a
	 pop ix
	pop hl
	; Restore the terminating event counter checker
	ld (event_counter_checkers_ei_delay),hl
	ret

start_emulation:
	call set_gb_stack
	pop hl
	exx
	ex af,af'
	jr event_not_expired_start_emulation

#ifdef DEBUG
event_debug_address:
	.db 0
#endif

do_event:
	exx
#ifdef DEBUG
	ld hl,event_debug_address
	ld (event_address),hl
#endif
	ex af,af'
event_cycle_count = $+1
	ld l,0
	sub l
	ASSERT_NC
	ld e,a
	ld h,d
do_event_any:
#ifdef DEBUG
	inc h
	dec h
	jr nz,$
#endif
do_event_any_noassert:
	push hl
	ex (sp),ix
#ifdef VALIDATE_STACK
	ld hl,($AABD - myADLstack) & $FFFF
	add.l hl,sp
	mlt hl
	ld l,h
	add hl,hl
	ld h,(myz80stack - 4) >> 8
	sbc hl,sp
	jr nz,$
#endif
do_event_pushed:
	push de
#ifdef SCHEDULER_LOG
	 call.il scheduler_log
#endif
#ifdef 0
	 push iy
	 ex af,af'
	 exx
	 push af
	 push bc
	 push de
	 push hl
	 ex af,af'
	 exx
	 ld hl,(event_gb_address)
	 push hl
	 ld hl,i
	 push hl
	 FASTLOG_EVENT_Z80(TRIGGER_EVENT, 18)
	 dec sp \ dec sp \ dec sp \ dec sp
#endif
event_expired_interrupt_loop:
	 ; Check scheduled events
	 ld (event_save_sp),sp
event_expired_halt_loop:
	 ld hl,i ; This clears the carry flag
event_expired_loop:
	 ld sp,event_counter_checkers

	 ld b,h
	 ld c,l
ppu_counter = $+1
	 ld de,0
	 adc hl,de
	 ret z
	 ex de,hl
ppu_scheduled:
	 inc sp
	 inc sp
audio_counter_checker:
audio_counter = $+1
	 ld hl,0
	 or a
	 sbc hl,bc
	 jp z,audio_expired_handler
	 add hl,de
	 ret c
	 ex de,hl
	 sbc hl,de
	 ex de,hl
	 ret

event_expired_more_events:
	 or a
	 sbc hl,de
	 or a
	 jr event_expired_loop

cpu_continue_halt:
	 ld ix,0
	 jr event_expired_halt_loop

schedule_ei_delay:
	 ; Force an event after one GB cycle
	 ld de,-1
	 ; Overwrite the function pointer with the following code,
	 ; which will run after the one GB cycle elapses
	 call event_counter_checkers_done
schedule_ei_delay_startup:
	 ; Enable interrupts, but check for halt spin
	 ; Interrupts may have been enabled by RETI so check specifically for halt mode
	 ld hl,intstate_smc_2
	 ld a,(hl)
	 cp cpu_exit_halt_no_interrupt - (intstate_smc_2 + 1)
	 ld (hl),trigger_interrupt - (intstate_smc_2 + 1)
	 jr nz,_
	 ld (hl),cpu_exit_halt_trigger_interrupt - (intstate_smc_2 + 1)
_
	 ; Restore the default counter checker end pointer
	 call event_counter_checkers_done
event_counter_checkers_done:
#ifdef VALIDATE_SCHEDULE
	 ;ld de,-1
#endif
	 ld h,b
	 ld l,c
	 add ix,de
	 jr c,event_expired_more_events
	 sbc hl,de
	 ld i,hl
event_save_sp = $+1
	 ; Use this initial value in case the CPU is halted when loading a save state
	 ld sp,myz80stack-4-4
event_not_expired_start_emulation:
	pop de
event_not_expired:
	ld hl,(IE)
	ld a,l
	and h
intstate_smc_2 = $+1
	jr nz,trigger_interrupt
cpu_halted_smc = $
	add ix,de
	ld a,ixl
	jr c,event_reschedule
event_no_reschedule:
	ld d,ixh
	pop ix
	exx
	ex af,af'
	jp (hl)

cpu_exit_halt_no_interrupt:
	xor a
	ld (intstate_smc_2),a
	ld hl,$19DD ; ADD IX,DE
	ld (cpu_halted_smc),hl
	jr cpu_halted_smc

event_reschedule:
	inc bc ;BCU=0
	ld b,e
	ld c,a
	ld de,(event_gb_address)
	exx
	push hl
	pop ix
	exx
	; This is guaranteed to carry, so the event cannot be now
	sub b
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_later
#else
	jp.lil schedule_event_later
#endif

#ifdef NO_PORTS
trigger_int_callstack_overflow:
	call callstack_overflow_helper
	jr trigger_int_callstack_overflow_continue
#endif

trigger_interrupt_retry_dispatch:
	; Count the full dispatch cycles again, without causing another retry
	lea de,ix-4
	; Skip the push and the first SMC for disabling interrupts, which have
	; already been done
	ld l,a
	; Restore Game Boy BC
	pop ix
	jr trigger_interrupt_pushed

#ifdef SHADOW_STACK
do_push_for_interrupt_set_shadow_stack:
	call set_shadow_stack
	jr do_push_for_interrupt_continue
#endif

cpu_exit_halt_trigger_interrupt:
	ld bc,$19DD ; ADD IX,DE
	ld (cpu_halted_smc),bc
trigger_interrupt:
	ld l,a
	; Disable interrupts
	ld a,$08 ;EX AF,AF'
	ld (intstate_smc_1),a
	; Get the number of cycles to be taken by RET
	rrca	;ld a,4
	add a,e
	dec iyl
	jp p,do_push_overflow_for_interrupt
do_push_for_interrupt_continue:
event_gb_address = $+1
	ld bc,event_gb_address
do_push_for_interrupt_shadow_stack_smc = $
trigger_interrupt_push_offset_smc_1 = $+3
	ld.l (iy),b
	dec iyl
trigger_interrupt_push_offset_smc_2 = $+3
	ld.l (iy),c
	push.l bc  ; Cache Game Boy return address on callstack
	; Restore cycle count into DE
	lea de,ix
	; Restore Game Boy BC
	pop ix
	exx
	; Push JIT return address
	push hl
#ifdef NO_PORTS
	; Check for callstack overflow
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr nc,trigger_int_callstack_overflow
#endif
trigger_int_callstack_overflow_continue:
	exx
	; Push stack offset and RET cycle count
	ld b,a
	ld c,iyl
	push bc
trigger_interrupt_pushed:
	; More disabling interrupts
	xor a
	ld (intstate_smc_2),a
	; Get the lowest set bit of the active interrupts
	sub l
	and l
	; Clear the IF bit
	xor h
	ld b,a
	; Index the dispatch routines by the interrupt bit times 4
	xor h
	add a,a
	add a,a
	; Save the new IF value
	sbc hl,hl ;active_ints
	ld (hl),b
	exx
	ld l,a
	ld h,dispatch_vblank >> 8
	; Get number of cycles to be taken
	ld a,(hl)
	inc hl
	exx
	add a,e
	jr c,dispatch_int_maybe_overflow
dispatch_int_no_overflow:
	exx
	ex af,af'
	jp (hl)

do_push_overflow_for_interrupt:
	push hl
	 ld d,a ; Preserve the RET cycle count
	 call shift_stack_window_lower_preserved_a_swapped
	 exx
	 ld a,d
	pop hl
	jr nc,do_push_for_interrupt_continue
do_push_for_interrupt_no_shadow_stack:
	; Restore cycle count into DE and restore Game Boy BC
	ex (sp),ix
	pop de
	ld a,e
	ld hl,(event_gb_address)
	ld e,5 ; Cycles for interrupt dispatch
	call do_push_for_call_slow
	ex af,af'
	exx
	ld e,a
	; Re-check the active interrupts
	ld hl,(IE)
	ld a,l
	and h
	ld l,a
	jr trigger_interrupt_pushed

dispatch_int_maybe_overflow:
	inc d
	jr nz,dispatch_int_no_overflow
	ld c,a
	; Check if an event was scheduled during the first 4 cycles of dispatch
	ld a,e
	sub -4
	jr nc,dispatch_int_handle_events
	cpl
	add a,c
	ld b,a
	inc bc \ dec bc ;BCU=0
	exx
	ld a,l
	inc hl
	ld hl,(hl)
	push hl
	 exx
	 ex (sp),ix
	 add a,10*4-1
	 rra
	 ld l,a
	 ld h,dispatch_vblank >> 8
	 ld de,(hl)
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_event_helper
#else
	 jp.lil schedule_event_helper
#endif

dispatch_int_handle_events:
	; Set IX to the 4-cycle-added value
	push ix
	 ld ixl,a
	 ld ixh,0
	 ; Restore the original value of IF
	 exx
	 ld a,l
	 exx
	 dec a
	 rrca
	 rrca
	 xor b
	 ld (active_ints),a
	 ; Set the restoring interrupt trigger
	 ld a,trigger_interrupt_retry_dispatch - (intstate_smc_2 + 1)
	 ld (intstate_smc_2),a
	 ; SP may have been adjusted by the callstack push, so set the new SP restore value
	 push de
	  jp event_expired_interrupt_loop


; Writes to the GB timer count (TIMA).
; Does not use a traditional call/return, must be jumped to directly.
;
; Updates the GB timer based on the new value, if enabled.
;
; Inputs:  BC = current cycle offset
;          L' = value to write
;          BCDEHL' are swapped
; Outputs: TIMA and GB timer updated
;          Event triggered
tima_write_helper:
	ld hl,TAC
	bit 2,(hl)
	exx
	ld a,l
	ld (TIMA),a
	exx
#ifdef DEBUG
	jp z,z80_restore_swap_ret
#else
	jp z,z80_restore_swap_ret
#endif
tima_reschedule_helper:
	ld hl,i
	add hl,bc
	push bc
	 cpl
	 ld c,a
	 ld a,(timer_cycles_reset_factor_smc)
	 ld b,a
	 add a,a
	 dec a
	 or l
	 ld l,a
	 mlt bc
	 inc bc
	 add hl,bc
	 add hl,bc
	 ld (timer_counter),hl
	 ; Get the relative time of the event from the currently scheduled event
	 ld b,h
	 ld c,l
	 ld hl,i ; Resets carry
	 sbc hl,bc
	pop bc ; Restore current cycle count
	jr reschedule_event_resolved

enableHDMA:
	rrca
	ld (hl),a
	push bc
	 call updateSTAT
	 call schedule_hdma
	 ; Get the relative time of the event from the currently scheduled event
	 ld b,h
	 ld c,l
	 ld hl,i ; Resets carry
	 sbc hl,bc
	pop bc
	ld a,(STAT)
	and 3
	jr nz,reschedule_event
	ld hl,hdma_immediate_handler
	ld (event_counter_checker_slot_HDMA),hl
	jr trigger_event

reschedule_event_PPU:
	 pop bc
	 ; Get the relative time of the event from the currently scheduled event
	 ex de,hl
	 ld hl,i
	 add hl,de
	pop de
reschedule_event:
	; Calculate current cycle count from block offset
	ld a,e
	add a,c
	ld c,a
	ld b,d
	jr c,reschedule_event_resolved
	dec b
reschedule_event_resolved:
	; If the current event is scheduled before or at the current cycle, do nothing
	xor a
	cp b
	jr z,reschedule_event_no_reschedule
	; If the new event is after or at the currently scheduled event, do nothing
	dec hl
	add hl,bc
	jr c,reschedule_event_no_reschedule
	sbc hl,bc
	inc hl
	; Add to the cycle counter
	add hl,de
	; If the cycle counter overflowed, trigger an event without removing
	jr c,trigger_event_from_reschedule_no_remove
	; If the counter already overflowed, trigger an event and remove existing
	or d
	jr z,trigger_event_from_reschedule_remove
	; Update the cycle counter
	ex de,hl
	; Update the schedule time
	sbc hl,de
	ld b,h
	ld c,l
	ld hl,i
	add hl,bc
	ld i,hl
reschedule_event_no_reschedule:
trigger_event_already_triggered:
	ld a,e
	ex af,af'
	exx
	ret

trigger_event_pop:
	 pop bc
	pop de
	; Input: DE=block cycle count, C=block cycle offset of memory access
trigger_event:
	ld h,$FF
	ld l,c
	add hl,de
	ld b,h
	ld c,l
	; Input: DE=block cycle count, BC=cycle count of memory access
trigger_event_resolved:
	; If the end of this instruction is already past the target, no reschedule
	xor a
	cp b
	jr z,trigger_event_already_triggered
	; If the counter already overflowed, remove any already-scheduled event
	cp d
trigger_event_from_reschedule_no_remove:
	ld d,a
	jr c,trigger_event_no_remove
trigger_event_from_reschedule_remove:
#ifdef DEBUG
	ld a,(event_address+1)
	cp (event_debug_address >> 8) + 1
	jr c,$
#endif
	ld a,(event_value)
event_address = $+1
	ld (event_value),a
#ifdef DEBUG
	ld hl,event_debug_address
	ld (event_address),hl
#endif
	scf
trigger_event_no_remove:
	; Adjust cycle count to the end of the instruction,
	; assuming the memory access is on the last cycle
	ASSERT_C
	ld a,e
	sbc a,c
	ld e,a
#ifdef DEBUG
	ld a,(event_address+1)
	cp (event_debug_address >> 8) + 1
	jr nc,$
#endif
	ld hl,i
	add hl,bc	; Reset div counter to the time of memory access
	ld i,hl
	; Get the address after the CALL for this memory access (likely JIT)
	ex (sp),ix
	 exx
	 lea hl,ix
	 exx
	 ; Get the CALL target (likely trampoline)
	 ld ix,(ix-2)
	 ; Get the GB address before the trampoline/routine
	 ld hl,(ix-2)
	 ld (event_gb_address),hl
	 ; Cycle count after instruction is 1 after the event time
	 lea bc,ix
	 ld ix,1
	 ; Check whether the trampoline is in the low or high pool
	 ; No need to set or reset the carry because the Game Boy address buffers it
	 ld hl,(trampoline_next)
	 sbc hl,bc
	 ; If in the high pool, simply invoke event handlers
	 jp c,do_event_pushed
	 ; Check if this was low pool or a generic write
	 ld a,b
	 cp jit_start >> 8
	 jr c,trigger_event_for_generic_write
	 ; Execute INC DE, DEC DE, POP AF, or NOP
	 exx
	 ld a,(hl)
	 ld (trigger_event_low_pool_smc),a
	 inc hl
	 ; Non-destructively pop the old IX from the stack and re-push it,
	 ; to ensure safe operation for a potential POP AF
	 pop af
	 ex af,af'
trigger_event_low_pool_smc = $
	 nop
	 ex af,af'
	 push af
	 exx
	 jp do_event_pushed

trigger_event_for_generic_write:
	 ; Get the JIT address for the event
generic_write_jit_address = $+1
	 ld hl,0
	 ; Adjust the cycle counter back from the last-cycle access adjustment
	 inc e
	 ; Calculate the event cycle offset based on the end of instruction
	 ld a,e
generic_write_instr_cycle_offset = $+1
	 sub 0
	 jr trigger_event_for_generic_write_finish

schedule_event_finish:
	 ld (event_gb_address),hl
	 lea hl,ix
trigger_event_for_generic_write_finish:
	 ld (event_cycle_count),a
#ifdef DEBUG
	 ld a,(event_address+1)
	 cp (event_debug_address >> 8) + 1
	 jr nc,$
#endif
	 ld (event_address),hl
	 ld a,(hl)
	 ld (event_value),a
	 ld (hl),RST_EVENT
	 ld a,e
_
	pop ix
	ex af,af'
	exx
	jp (hl)

schedule_event_finish_no_schedule:
	 ld a,c
	 jr -_
