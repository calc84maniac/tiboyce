z80code:
	.assume adl=0
	.org 0
active_ints:
	.db 0
waitloop_sentinel:
	.db 0
	
	.block $08-$
	; Input: DE=Game Boy HL
	; Output: UHL=direct read pointer, or implementation is patched
	; Destroys: F'
r_get_hl_read_ptr:
	ex af,af'
get_hl_read_ptr_swapped:
	ld h,mem_read_lut >> 8
	ld l,d
	ld l,(hl)
	; Implicitly reset Z flag to indicate a patchable caller
	inc h ;mem_get_ptr_routines
	jp (hl)
	
	.block $10-$
	; Input: DE=Game Boy HL
	; Output: UHL=direct read/write pointer, or implementation is patched
	; Destroys: F'
r_get_hl_readwrite_ptr:
	ex af,af'
get_hl_readwrite_ptr_swapped:
	ld h,mem_write_lut >> 8
	ld l,d
	ld l,(hl)
	; Implicitly reset Z flag to indicate a patchable caller
	dec h ;mem_get_ptr_routines
	jp (hl)
	
	.block $18-$
r_event:
	pop hl
	dec hl
event_value = $+1
	ld (hl),0
	jp do_event

	.block $20-$
r_get_hl_hram_ptr:
	ex af,af'
	inc de
	ld hl,$007F
	add hl,de
	dec de
	jr nc,_
	ld h,d
	ld l,e
	ex af,af'
	ret
_
	jp unpatch_hl_hram_access
	
	;.block $28-$
	; Taken by previous routine
	
	.block $30-$
r_call:
	ex af,af'
	dec iyl
	jp m,do_call
	jr do_push_overflow_for_call
	
	.block $38-$
r_cycle_check:
rst38h:
	exx
	inc d
	exx
	ret nz
	ex (sp),ix
	jr nc,cycle_overflow_for_jump
	inc ix
	lea hl,ix
	exx
	inc bc ;BCU=0
	ld c,a
	ld b,(ix-4)
	ld de,(ix-8)
#ifdef VALIDATE_SCHEDULE
	call.il schedule_subblock_event_helper
#else
	jp.lil schedule_subblock_event_helper
#endif
	
cycle_overflow_for_jump:
	ld hl,(ix+2)
	push hl
	 exx
	 inc bc ;BCU=0
	 ld c,a
	 ld a,-3
	 sub (ix-3)
	 ld b,a
	 ld de,(ix+4)
	pop ix
#ifdef VALIDATE_SCHEDULE
	call.il schedule_jump_event_helper_adjusted
#else
	jp.lil schedule_jump_event_helper_adjusted
#endif
	
do_call_no_shadow_stack:
	push bc
do_push_overflow_for_call_slow:
	 push hl
	  call do_push_any_slow_for_call
	  exx
	 pop hl
	pop bc
	; Remove JIT return address from callstack cache, while preserving
	; the Game Boy return address in HL
	inc sp
	inc sp
	; Count cycles for taken CALL
	add a,c
	jr nc,do_call_dispatch
	inc d
	jr nz,do_call_dispatch
	ex de,hl
	ld l,a
	ld a,c
	jr cycle_overflow_for_call_pushed
	
do_call_callstack_overflow:
	call callstack_overflow_helper
	scf
	jr do_call_dispatch
	
#ifdef SHADOW_STACK
do_call_set_shadow_stack:
	call set_shadow_stack
	jr do_call_shadow_stack_smc
#endif

do_push_overflow_for_call:
	push bc
	 push hl
	  exx
	  push hl
	   call shift_stack_window_lower
	  pop hl
	  exx
	 pop hl
	 jr c,do_push_overflow_for_call_slow
	pop bc
do_call:
do_call_shadow_stack_smc = $
	; Push Game Boy return address to the stack
do_call_push_offset_smc_1 = $+3
	ld.l (iy),h
	dec iyl
do_call_push_offset_smc_2 = $+3
	ld.l (iy),l
	push.l hl  ; Cache Game Boy return address on callstack
	; Count cycles for taken CALL
	add a,c
	jr c,do_call_maybe_cycle_overflow
do_call_no_cycle_overflow:
	; Check for callstack overflow
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr nc,do_call_callstack_overflow
	; Push RET cycle count and stack offset
	ld c,iyl
	push bc
do_call_dispatch:
	; Dispatch to JIT target
	; Carry is set to indicate to the decoder that the callstack cache was used,
	; or reset if not. This indicates how to retrieve the JIT address.
	exx
	ex af,af'
	jp (hl)
	
do_rom_bank_call:
	exx
	ex af,af'
	pop hl
	ld e,a
	ld b,(hl)  ; Cycles for taken RET
	inc hl
banked_call_mismatch_continue:
	ld c,(hl)  ; Cycles for taken CALL
	inc hl
	push de
	ld de,(hl)  ; Game Boy return address
	inc hl
	inc hl
curr_rom_bank = $+1
	ld a,0  ; Get current bank
	cp (hl)
	jr nz,banked_call_mismatch
	inc hl
	ex (sp),hl
	ex de,hl
	ld a,e
	dec iyl
	jp m,do_call
	jp do_push_overflow_for_call
	
banked_call_mismatch:
	jp.lil banked_call_mismatch_helper
	
do_unbanked_call:
	exx
	ex af,af'
	pop hl
	ld b,(hl)  ; Cycles for taken RET
	inc hl
	ld c,(hl)  ; Cycles for taken CALL
	inc hl
	push de
	ld de,(hl)  ; Game Boy return address
	inc hl
	inc hl
	inc hl  ; Skip RST_CALL
	ex (sp),hl
	ex de,hl
	dec iyl
	jp m,do_call
	jp do_push_overflow_for_call
	
do_call_maybe_cycle_overflow:
	inc d
	jr nz,do_call_no_cycle_overflow
cycle_overflow_for_call:
	ex de,hl
	; Check for callstack overflow
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	call nc,callstack_overflow_helper
	ld l,a
	ld a,c
	; Push RET cycle count and stack offset
	ld c,iyl
	push bc
cycle_overflow_for_call_pushed:
	exx
	push hl
	 exx
	 ex (sp),ix
	 inc bc ;BCU=0
	 ld c,l
	 sub 6
	 ld b,a
	 dec de
	 dec de
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_call_event_helper
#else
	 jp.lil schedule_call_event_helper
#endif
	
	jr nz,do_rom_bank_call
do_call_nz:
	jr z,skip_cond_call
	jr do_unbanked_call
	
	jr z,do_rom_bank_call
do_call_z:
	jr nz,skip_cond_call
	jr do_unbanked_call
	
	jr nc,do_rom_bank_call
do_call_nc:
	jr c,skip_cond_call
	jr do_unbanked_call
	
	jr c,do_rom_bank_call
do_call_c:
	jr c,do_unbanked_call
skip_cond_call:
	pop hl
	ex af,af'
	add a,(hl) ; Count cycles for taken RET (1 too many)
	inc hl \ inc hl \ inc hl \ inc hl \ inc hl
	jr c,++_
_
	dec a
	ex af,af'
	jp (hl)
_
	jr z,--_
	exx
	inc d
	exx
	jr nz,--_
	dec a
	push hl
	 exx
	 ex (sp),ix
	 inc bc ;BCU=0
	 ld c,a
	 ld a,(ix-5)
	 sub 4
	 ld b,a
	 ld de,(ix-3)
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_event_helper
#else
	 jp.lil schedule_event_helper
#endif
	
cycle_overflow_for_bridge:
	exx
	inc d
	exx
	ret nz
	exx
	ex (sp),ix
	ld bc,(ix-4-1) ; BCU=0
	ld c,a
	ld de,(ix+4)
	ld ix,(ix+2)
	exx
	lea hl,ix
	exx
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_helper_a
#else
	jp.lil schedule_event_helper_a
#endif
	
do_overlapped_jump:
	ex af,af'
	pop hl
	inc hl
	add a,(hl)
	inc hl
	inc hl
	jr c,do_overlapped_jump_maybe_overflow
	ex af,af'
	jp (hl)
	
do_rom_bank_jump:
	ex af,af'
	exx
	ld e,a
	exx
	pop hl
rom_bank_check_smc_1 = $+1
	ld a,0
	cp (hl)
	jr nz,banked_jump_mismatch
	inc hl
	ld a,(hl)
	inc hl
	inc hl
	exx
banked_jump_mismatch_continue:
	add a,e
	jr c,++_
_
	exx
	ex af,af'
	jp (hl)
do_overlapped_jump_maybe_overflow:
	exx
_
	inc d
	jr nz,--_
do_slow_jump_overflow_common:
	ld e,a
	exx
	inc hl
	push hl
	 ld hl,(hl)
	 exx
	 ex (sp),ix
	 ld bc,(ix-3) ;BCU=0
	 ld a,c
	 add a,b
	 ld b,a
	 sub c
	 ld c,e
	 ld de,(ix+2)
	 ld ix,(ix)
#ifdef VALIDATE_SCHEDULE
	 call.il c,schedule_slow_jump_event_helper
	 call.il schedule_event_helper
#else
	 jp.lil c,schedule_slow_jump_event_helper
	 jp.lil schedule_event_helper
#endif
	
banked_jump_mismatch:
	jp.lil banked_jump_mismatch_helper
	
schedule_event_finish:
	 ld (event_cycle_count),a
	 ld (event_gb_address),hl
#ifdef DEBUG
	 ld a,(event_address+1)
	 cp (event_value >> 8) + 1
	 jr nc,$
#endif
	 lea hl,ix
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
	
	; Check if an event was scheduled at or before the current memory cycle
	; Inputs: DE = cycle count at end of block (only call when D=0)
	;         C = block-relative cycle offset (negative)
	;         A = D = 0
	; Destroys: AF, BC, HL
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
	
do_event:
	exx
#ifdef DEBUG
	ld hl,event_value
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
	 ld hl,intstate_smc_2
	 ld a,(hl)
	 or a
	 ld (hl),trigger_interrupt - (intstate_smc_2 + 1)
	 jr z,_
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
	
trigger_int_callstack_overflow:
	call callstack_overflow_helper
	jr trigger_int_callstack_overflow_continue
	
trigger_interrupt_retry_dispatch:
	; Count the full dispatch cycles again, without causing another retry
	lea de,ix-4
	; Skip the push and the first SMC for disabling interrupts, which have
	; already been done
	ld l,a
	; Restore Game Boy BC
	pop ix
	jr trigger_interrupt_pushed
	
do_push_for_interrupt_no_shadow_stack:
	; Restore Game Boy BC
	ex (sp),ix
	; Restore cycle count into DE
	pop de
	ld h,b
	ld l,c
	call do_push_any_slow_for_call
	exx
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
	dec iyl
	jp p,do_push_overflow_for_interrupt
	rrca	;ld a,4
event_gb_address = $+1
	ld bc,event_gb_address
do_push_for_interrupt_continue:
do_push_for_interrupt_shadow_stack_smc = $
trigger_interrupt_push_offset_smc_1 = $+3
	ld.l (iy),b
	dec iyl
trigger_interrupt_push_offset_smc_2 = $+3
	ld.l (iy),c
	push.l bc  ; Cache Game Boy return address on callstack
	; Get the number of cycles to be taken by RET
	add a,e
	ld b,a
	; Restore cycle count into DE
	lea de,ix
	; Restore Game Boy BC
	pop ix
	exx
	; Push JIT return address
	push hl
	; Check for callstack overflow
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	jr nc,trigger_int_callstack_overflow
trigger_int_callstack_overflow_continue:
	exx
	; Push stack offset and RET cycle count
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
	ld c,a
	; Index the dispatch routines by the interrupt bit times 4
	xor h
	add a,a
	add a,a
	; Save the new IF value
	sbc hl,hl ;active_ints
	ld (hl),c
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
	 call shift_stack_window_lower_preserved_a_swapped
	 exx
	pop hl
	ld a,4
	jr nc,do_push_for_interrupt_continue
	; Restore cycle count into DE and restore Game Boy BC
	ex (sp),ix
	pop de
	exx
	; Push JIT return address
	push hl
	ld hl,(event_gb_address)
	exx
	push hl
	 ld a,e
	 exx
	 call do_push_any_slow_swapped
	 exx
	 ld e,a
	pop hl
	jr trigger_interrupt_pushed
	
dispatch_int_maybe_overflow:
	inc d
	jr nz,dispatch_int_no_overflow
	inc bc ;BCU=0
	; Check if an event was scheduled during the first 4 cycles of dispatch
	ld b,a
	ld a,e
	sub -4
	jr nc,dispatch_int_handle_events
cycle_overflow_for_int:
	ld c,e
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
	 dec c
	 xor c
	 ld (active_ints),a
	 ; Set the restoring interrupt trigger
	 ld a,trigger_interrupt_retry_dispatch - (intstate_smc_2 + 1)
	 ld (intstate_smc_2),a
	 ; The correct SP restore value is already saved, so enter the loop directly
	 push de
	  jp event_expired_interrupt_loop
	
	; This is called when a CALL, RST, or interrupt occurs
	; which exceeds the defined callstack limit.
	; Inputs: SPL = myADLstack - (CALL_STACK_DEPTH * CALL_STACK_ENTRY_SIZE_ADL) - 3
	;         (SPL) = value to preserve on ADL callstack
	;         SPS = myz80stack - 4 - (CALL_STACK_DEPTH * CALL_STACK_ENTRY_SIZE_Z80) - 4
	;         (SPS) = return value
	;         (SPS+2) = value to preserve on Z80 callstack
	; Outputs: SPL = myADLstack
	;          SPS = myz80stack - 4 - 2
	;          (SPS) = preserved Z80 callstack value
	; Destroys: HL
callstack_overflow_helper:
	pop.l hl
	ld.lil sp,myADLstack
	push.l hl
	pop hl
	ld (callstack_overflow_helper_smc),hl
	pop hl
	ld sp,myz80stack - 4
	push hl
callstack_overflow_helper_smc = $+1
	jp 0
	
ppu_mode2_line_0_lyc_match:
	; The LYC match bit was already set by the scheduled LYC event
	; Just transition from mode 1 to mode 2
	inc a
	ld (hl),a
	; Check for mode 1 or LYC blocking
	tst a,$50
	jr nz,ppu_mode2_continue
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	dec h
	jr ppu_mode2_continue
	
ppu_expired_mode2_line_0:
	ld hl,ppu_expired_mode2
	push hl
	inc sp
	inc sp
	; Check if LYC is 0
	ld hl,LYC
	ld a,h ;$FF
	ld (ppu_mode2_LY),a
	and (hl)
	ld l,STAT & $FF
	ld a,(hl)
	jr z,ppu_mode2_line_0_lyc_match
	; Check for mode 1 blocking
	bit 4,a
	jr nz,ppu_mode2_blocked_fast
	sbc hl,hl ;ld hl,active_ints
ppu_expired_mode2:
	; Request STAT interrupt
	set 1,(hl) ;active_ints
ppu_mode2_blocked:
	; Set mode 2
	ld hl,STAT
	ld a,(hl)
ppu_mode2_blocked_fast:
	and $F8
	or 2
	ld (hl),a
ppu_mode2_continue:
	; Allow catch-up rendering if this frame is not skipped
ppu_mode2_enable_catchup_smc = $+1
	ld r,a
	ld l,-MODE_2_CYCLES
	add hl,de
	ld (nextupdatecycle_STAT),hl
	ld hl,-CYCLES_PER_SCANLINE
	ex de,hl
	add hl,de
	ld (nextupdatecycle_LY),hl
	ld (ppu_counter),hl
ppu_mode2_LY = $+1
	ld a,0
	inc a
	ld (ppu_mode2_LY),a
	ld (LY),a
ppu_mode2_event_line = $+1
	cp 0
	jp nz,audio_counter_checker
	ld hl,STAT
	; Check whether vblank should be scheduled immediately
	cp 143
	jr z,ppu_mode2_prepare_vblank
	; Set next line event to vblank
	ld a,143
	ld (ppu_mode2_event_line),a
	; Set LYC coincidence bit
	set 2,(hl)
	; Block mode 2 interrupt after LYC coincidence, if enabled
	bit 6,(hl)
	jp z,audio_counter_checker
	call ppu_scheduled
	
ppu_expired_mode2_lyc_blocking:
	ld hl,ppu_expired_mode2
	push hl
	inc sp
	inc sp
	jr ppu_mode2_blocked
	
ppu_expired_mode0_line_0:
	xor a
	ld (ppu_mode0_LY),a
	ld hl,LYC
	or (hl)
	jr z,ppu_expired_mode0_lyc_match
	ld hl,ppu_expired_mode0
	push hl
	inc sp
	inc sp
	sbc hl,hl ;ld hl,active_ints
ppu_expired_mode0:
	; Request STAT interrupt
	set 1,(hl) ;active_ints
	; Set mode 0
	ld hl,STAT
	ld a,(hl)
	and $F8
	ld (hl),a
ppu_mode0_blocked:
	; Allow catch-up rendering if this frame is not skipped
ppu_mode0_enable_catchup_smc = $+1
	ld r,a
	ld l,-MODE_0_CYCLES
	add hl,de
	ld (nextupdatecycle_STAT),hl
	ld (nextupdatecycle_LY),hl
	ld hl,-CYCLES_PER_SCANLINE
	ex de,hl
	add hl,de
	ld (ppu_counter),hl
ppu_mode0_LY = $+1
	ld a,0
	ld (LY),a
	inc a
	ld (ppu_mode0_LY),a
ppu_mode0_event_line = $+1
	cp 0
	jp nz,audio_counter_checker
	; Check whether vblank should be scheduled immediately
	cp 144
	jr z,ppu_mode0_prepare_vblank
	call ppu_scheduled
	
ppu_expired_mode0_lyc_match:
	ld hl,ppu_expired_mode0
	push hl
	inc sp
	inc sp
	; Set next line event to vblank
	ld a,144
	ld (ppu_mode0_event_line),a
	; Set mode 0 and LYC coincidence bit
	ld hl,STAT
	ld a,(hl)
	and $F8
	or 4
	ld (hl),a
	; Block mode 0 interrupt during LYC coincidence, if enabled
	bit 6,a
	jr nz,ppu_mode0_blocked
	; Request STAT interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	dec h
	jr ppu_mode0_blocked
	
ppu_mode2_prepare_vblank:
	; Check if LYC matches during active video
	ld a,(LYC)
	cp 143
	jr c,_
	jr nz,ppu_expired_pre_vblank
	; If LYC is on line 143, set coincidence bit
	set 2,(hl)
_
	; Set next line event to LYC
	ld (ppu_mode2_event_line),a
	jr ppu_expired_pre_vblank
	
ppu_mode0_prepare_vblank:
	; Reset scheduled time and offset
	sbc hl,de
	ld e,-MODE_0_CYCLES
	add hl,de
	ld (ppu_counter),hl
	; Check if LYC matches during active video
	ld a,(LYC)
	cp 144
	jr nc,ppu_expired_pre_vblank
	; Set next line event to LYC
	ld (ppu_mode0_event_line),a
	jr ppu_expired_pre_vblank
	
ppu_expired_lyc_mode2:
	; Set LY to LYC
	ld hl,LYC
	ld a,(hl)
	dec hl
	ld (hl),a
	; Set STAT to mode 2 with LY=LYC bit set
	ld l,STAT & $FF
	ld a,(hl)
	or $07
	dec a
	ld (hl),a
	; Allow catch-up rendering if this frame is not skipped
ppu_lyc_enable_catchup_smc = $+1
	ld r,a
	; Set interrupt bit, if LYC interrupt is enabled
	bit 6,a
	jr z,_
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	dec h
_
	; Set LY/STAT caches
	ld l,-MODE_2_CYCLES
	add hl,de
	ld (nextupdatecycle_STAT),hl
	ld hl,-CYCLES_PER_SCANLINE
	add hl,de
	ld (nextupdatecycle_LY),hl
	; Set next scheduled time to vblank
	ld hl,(vblank_counter)
	add hl,de
	ex de,hl
	or a
	sbc hl,de
	ld (ppu_counter),hl
	add hl,bc
	ex de,hl
ppu_expired_pre_vblank:
	call ppu_scheduled
	
ppu_expired_vblank:
	; Always trigger vblank interrupt
	set 0,(hl) ;active_ints
	; Set LY to 144
	ld hl,LY
	ld a,144
	ld (hl),a
	; Check for either a LYC match or an LYC block
	inc hl
	sub (hl)
	sub 2
	inc a
	ld l,STAT & $FF
	ld a,(hl)
	jr c,ppu_vblank_lyc_close_match
	; Set mode 1
	and $F8
	inc a
	ld (hl),a
	; Check for mode 1 or mode 2 interrupt enable
	tst a,$30
	jr nz,ppu_vblank_mode1_int
ppu_vblank_stat_int_continue:
	; Set next LY/STAT update to scanline 145
	ld l,-CYCLES_PER_SCANLINE
	add hl,de
	ld (nextupdatecycle_LY),hl
	ld (nextupdatecycle_STAT),hl
ppu_expired_lcd_off:
	; Set the next vblank start time
	ld hl,CYCLES_PER_FRAME
	add hl,bc
	ld (vblank_counter),hl
	; Save a persistent time by which the next vblank must occur,
	; in case the LCD is toggled on and off
	ld (persistent_vblank_counter),hl
	; Set the next event time and handler
ppu_post_vblank_event_offset = $+1
	ld hl,-CYCLES_PER_FRAME
	ex de,hl
	add hl,de
	ld (ppu_counter),hl
ppu_post_vblank_event_handler = $+1
	ld hl,ppu_expired_vblank
	push hl
	jp.lil vblank_helper
	
ppu_vblank_lyc_close_match:
	jr nz,ppu_vblank_lyc_match
	; LYC=143 case
	; Set mode 1
	and $F8
	inc a
	ld (hl),a
	; Check for mode 1 or mode 2 interrupt enable
	tst a,$30
	jr z,ppu_vblank_stat_int_continue
	; Check for STAT block
	bit 6,a
	jr nz,ppu_vblank_stat_int_continue
ppu_vblank_mode1_int:
	; Check for mode 0 block
	bit 3,a
	jr nz,ppu_vblank_stat_int_continue
	; Trigger STAT interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	dec h
	jr ppu_vblank_stat_int_continue
	
ppu_vblank_lyc_match:
	; LYC=144 case
	; Set mode 1 with LY=LYC bit
	and $F8
	or 5
	ld (hl),a
	; Check for mode 0 block
	bit 3,a
	jr nz,ppu_vblank_stat_int_continue
	; Check for either mode 1, mode 2, or LY=LYC interrupt enable
	and $70
	jr z,ppu_vblank_stat_int_continue
	; Trigger STAT interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	dec h
	jr ppu_vblank_stat_int_continue
	
ppu_expired_lyc_mode1:
	; Set LY to LYC
	ld hl,LYC
	ld a,(hl)
	dec hl
	ld (hl),a
	; Set STAT to mode 1 with LY=LYC bit set
	ld l,STAT & $FF
	ld a,(hl)
	and $F8
	or $05
	ld (hl),a
	; Set LY/STAT caches
ppu_lyc_scanline_length_smc = $+1
	ld l,-CYCLES_PER_SCANLINE
	add hl,de
	ld (nextupdatecycle_LY),hl
	ld (nextupdatecycle_STAT),hl
	; Prepare next event
ppu_post_mode1_lyc_event_handler = $+1
	ld hl,0
	push hl
	inc sp
	inc sp
ppu_post_mode1_lyc_event_offset = $+1
	ld hl,0
	ex de,hl
	add hl,de
	ld (ppu_counter),hl
	; Check if LY=LYC interrupt is enabled and not blocked by mode 1 interrupt
	xor $40
	and $50
	jp nz,audio_counter_checker
	; If so, trigger LYC interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	jp audio_counter_checker
	
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
	set 2,(hl) ;active_ints
timer_expired_handler_already_set:
timer_period = $+1
	ld hl,0
	; If scheduled for 65536 cycles in the future, no need to reschedule
	; Returning here prevents the delay from being interpreted as 0
	add hl,hl
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
	set 3,(hl) ;active_ints
	dec h
	inc hl ;SB
	ld (hl),h ;$FF
	inc hl ;SC
	res 7,(hl)
	call disabled_counter_checker
disabled_counter_checker:
	ret
	
decode_block_bridge:
	ex af,af'
	scf
	.db $D2 ;JP NC,
decode_jump:
	ex af,af'
	or a
	exx
	pop hl
	ld e,a
	push de
	 push ix
	  inc hl
	  inc hl
	  inc hl
	  ld ix,(hl)
	  ld (hl),$C3 ;JP
	  inc hl
	  push hl
	   inc hl
	   ld a,(hl)
	   inc hl
	   ld de,(hl)
	   jp.lil decode_jump_helper
decode_jump_return:
	  pop hl
	  ld (hl),ix
	  ld de,-5
	  add hl,de
	  sbc a,b ; Carry is set
	  cpl
	  ld (hl),a
	  dec hl
	  ld (hl),$D6	;SUB -cycles
decode_block_bridge_finish:
	  dec hl
	  ld (hl),$08	;EX AF,AF'
decode_jump_waitloop_return:
	 pop ix
	pop de
	ld a,e
	push hl
	exx
	ex af,af'
	ret
	
decode_block_bridge_return:
	  pop hl
	  ld (hl),ix
	  ld de,-5
	  add hl,de
	  ld (hl),$DC	;CALL C,cycle_overflow_for_bridge
	  dec hl
	  ld (hl),a
	  dec hl
	  ld (hl),$C6	;ADD A,cycles
	  jr decode_block_bridge_finish
	
decode_bank_switch_return:
	  pop hl
	  ld (hl),ix
	  dec hl
	  ld (hl),$C3	;JP target
	  dec hl
	  ld (hl),b	;negative jump cycles
	  dec hl
	  ld (hl),a  ;taken cycle count
	  dec hl
	  ld (hl),c	;bank id
	  dec hl
	  dec hl
	  ld (hl),de
	  dec hl
	  ld (hl),$CD	;CALL do_xxxx_jump
	  jr decode_jump_waitloop_return
	
decode_call:
	exx
	ex af,af'
	; Grab the JIT return address from above or below the stack pointer,
	; depending on whether the callstack cache was used during dispatch
	ccf
	sbc hl,hl
	add hl,hl
	inc hl
	add hl,hl
	add hl,sp
	ld hl,(hl)
	ld e,a
	push de
	 push ix
	  ; Grab the Game Boy return address
	  dec hl
	  dec hl
	  dec hl
	  ld de,(hl)
	  push de
	   dec de
	   jp.lil decode_call_helper

decode_call_return:
	   ld (ix+3-10),hl ; Set new JIT target
	   exx
	   ; Check whether this was a conditional call or not
	   ; This holds $CD (CALL) for conditional or $D9 (EXX) for unconditional
	   bit 4,(ix+3-8)
	   jr c,decode_call_banked
	   jr z,decode_call_cond_finish
	   ld (ix+3-6),c  ; Taken call cycles
decode_call_finish:
	  pop de
	 pop ix
	pop hl
	ld b,0
	add hl,bc
	ld a,c
	jp c,cycle_overflow_for_call_pushed
	ex de,hl
	ld a,e
	ex af,af'
	exx
	jp (hl)
	
decode_call_banked:
	   ld a,(curr_rom_bank)
	   ld (ix+3-1),a
	   ld (ix+3-8),$CD  ;CALL do_rom_bank_call
	   ld hl,do_rom_bank_call
	   jr nz,decode_call_cond_banked_finish
	   ; Modify the conditional entry point to use the banked call
	   ld hl,(ix+3-7)
	   dec hl
	   dec hl
decode_call_cond_banked_finish:
	   ld (ix+3-7),hl
decode_call_cond_finish:
	   ld (ix+3-4),c  ; Taken call cycles
	   jr decode_call_finish
	
do_rst_00:
	exx
	ld hl,dispatch_rst_00
	ex af,af'
	dec iyl
	jp m,do_rst
	jr do_push_overflow_for_rst
do_rst_08:
	exx
	ld hl,dispatch_rst_08
	ex af,af'
	dec iyl
	jp m,do_rst
	jr do_push_overflow_for_rst
do_rst_10:
	exx
	ld hl,dispatch_rst_10
	ex af,af'
	dec iyl
	jp m,do_rst
	jr do_push_overflow_for_rst
do_rst_18:
	exx
	ld hl,dispatch_rst_18
	ex af,af'
	dec iyl
	jp m,do_rst
	jr do_push_overflow_for_rst
do_rst_20:
	exx
	ld hl,dispatch_rst_20
	ex af,af'
	dec iyl
	jp m,do_rst
	jr do_push_overflow_for_rst
do_rst_28:
	exx
	ld hl,dispatch_rst_28
	ex af,af'
	dec iyl
	jp m,do_rst
	jr do_push_overflow_for_rst
do_rst_30:
	exx
	ld hl,dispatch_rst_30
	ex af,af'
	dec iyl
	jp m,do_rst
	jr do_push_overflow_for_rst
do_rst_38:
	exx
	ld hl,dispatch_rst_38
	ex af,af'
	dec iyl
	jp m,do_rst
do_push_overflow_for_rst:
	push hl
	 ld h,a
	 exx
	 push bc
	  push hl
	   call shift_stack_window_lower_preserved_a_swapped
	   exx
	  pop hl
	 pop bc
	 exx
	pop hl
	jr nc,do_rst
	add a,(hl)
	inc hl
	exx
do_rst_no_shadow_stack:
	call do_push_any_slow_for_call
decode_rst_return:
	jr nc,_
	inc d
	jr z,cycle_overflow_for_rst_pushed
_
	exx
	ex af,af'
	jp (hl)
	
#ifdef SHADOW_STACK
do_rst_set_shadow_stack:
	call set_shadow_stack
	jr do_rst_shadow_stack_smc
#endif
	
do_rst:
	; Count cycles and advance to JP
	add a,(hl)
	inc hl
	exx
do_rst_shadow_stack_smc = $
	; Push Game Boy return address to the stack
do_rst_push_offset_smc_1 = $+3
	ld.l (iy),h
	dec iyl
do_rst_push_offset_smc_2 = $+3
	ld.l (iy),l
	push.l hl  ; Cache Game Boy return address on callstack
do_rst_no_shadow_stack_continue:
	jp nc,do_call_no_cycle_overflow
	inc d
	jp nz,do_call_no_cycle_overflow
cycle_overflow_for_rst:
	; Check for callstack overflow
	ld hl,(-call_stack_lower_bound) & $FFFF
	add hl,sp
	call nc,callstack_overflow_helper
	; Push RET cycle count and stack offset
	ld c,iyl
	push bc
cycle_overflow_for_rst_pushed:
	inc bc ;BCU=0
	ld c,a
	exx
	dec hl
	ld a,(hl)
	push hl
	 exx
	 ex (sp),ix
	 sub 4
	 ld b,a
	 lea hl,ix+10*4
	 srl l
	 ld de,(hl)
	 ld ix,(ix+2)
	 exx
	 lea hl,ix
	 exx
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_event_helper
#else
	 jp.lil schedule_event_helper
#endif
	
decode_rst:
	jp.lil decode_rst_helper
	
do_swap_b:
	ld h,a
	ld a,ixh
	rrca
	rrca
	rrca
	rrca
	or a
	ld ixh,a
	ld a,h
	ret
	
do_swap_c:
	ld h,a
	ld a,ixl
	rrca
	rrca
	rrca
	rrca
	or a
	ld ixl,a
	ld a,h
	ret
	
do_swap_d:
	ld h,a
	ld a,b
	rrca
	rrca
	rrca
	rrca
	or a
	ld b,a
	ld a,h
	ret
	.block 2
	
do_swap_e:
	ld h,a
	ld a,c
	rrca
	rrca
	rrca
	rrca
	or a
	ld c,a
	ld a,h
	ret
	.block 2
	
do_swap_h:
	ld h,a
	ld a,d
	rrca
	rrca
	rrca
	rrca
	or a
	ld d,a
	ld a,h
	ret
	.block 2
	
do_swap_l:
	ld h,a
	ld a,e
	rrca
	rrca
	rrca
	rrca
	or a
	ld e,a
	ld a,h
	ret
	.block 2
	
do_swap_hl_normal:
	call get_hl_readwrite_ptr_swapped
	; Shadow carry is reset on successful pointer get
	ex af,af'
	push af
	 ; Sets Z,N,H flags appropriately
	 ld.l a,(hl)
	 rrd.l
	pop hl
	ld a,h
	ret
	
do_swap_hl_hram:
	ld h,a
	; Check for HRAM
	; Technically $FF7F is included here, but SWAP is non-destructive on $FF
	ld a,e
	inc a
	rlca
	sbc a,a
	and d ; Resets carry
	inc a
	jr nz,_
do_swap_hl_hram_finish:
	ex de,hl
	; Sets Z,N,H flags appropriately
	ld a,(hl)
	rrd
	ex de,hl
	ld a,h
	ret
_
	ld a,h
	pop hl
	dec hl
	ld (hl),do_swap_hl_normal >> 8
	dec hl
	ld (hl),do_swap_hl_normal & $FF
	dec hl
	jp (hl)
	
ophandler27:
	; Save input A value
	ld l,a
	; Split execution path on input carry, to extract both H and N
	jr nc,ophandler27_no_carry
	; Map a value based on input H and N flags:
	;   N=0, H=0 -> $A6
	;   N=0, H=1 -> $AA
	;   N=1, H=0 -> $06
	;   N=1, H=1 -> $00
	ld a,$66
	daa
	; Invert N flag into C, and if N=0, put the H flag in H, else put it in Z
	add a,a
	; Restore input A value
	ld a,l
	jr c,ophandler27_add
	; Case for N=1, C=1
	jr nz,_
	; Subtract the adjustment for H=1
	sub 6
_
	; Subtract the adjustment for C=1, set N flag, reset H flag, update Z flag
	sub $60
	ret c
	jr z,_
	; If Z flag need not be set, set C/N flags, reset H/Z flags
	cp $A0
	ret
_
	; If Z flag must be set, set C/N/Z flags, reset H flag
	sub $A0
	daa
	ret
	
ophandler27_no_carry:
	; Map a value based on input H and N flags:
	;   N=0, H=0 -> $46
	;   N=0, H=1 -> $4A
	;   N=1, H=0 -> $86
	;   N=1, H=1 -> $80
	ld a,$E6
	daa
	; Put the N flag in C, and if N=0, put the H flag in H, else put it in Z
	add a,a
	; Restore input A value
	ld a,l
	jr c,ophandler27_sub_no_carry
ophandler27_add:
	; N=0, C and H were restored
	daa
	; Reset H and N, preserve C and Z
	rla
	rra
	ret
	
ophandler27_sub_no_carry:
	; Case for N=1, C=0
	jr nz,_
	; Subtract the adjustment for H=1
	sub 6
_
	; Set N, reset H and C, update Z
	sub 0
	ret
	
handle_waitloop_stat:
	ex (sp),ix
	ex af,af'
	exx
	ld bc,(nextupdatecycle_STAT)
	; Add the next jump cycles, and don't skip anything if expired
	add a,(ix+2)
	jr nc,handle_waitloop_common
_
	inc d
	jr nz,handle_waitloop_common
	ld b,(ix+2)
	jr handle_waitloop_overflow
	
handle_waitloop_ly:
	ex (sp),ix
	ex af,af'
	exx
	ld bc,(nextupdatecycle_LY)
	; Add the next jump cycles, and don't skip anything if expired
	add a,(ix+2)
	jr c,-_
handle_waitloop_common:
	ld e,a
	; Check if the waitloop sentinel is set
	ld a,(event_address+1)
	or a
	jr nz,handle_waitloop_set_sentinel
	; Get the current number of cycles until the next register update
	ld hl,i
	add hl,bc
	add hl,de
	; Offset to the read time to allow extra skips as needed
	ld a,l
	add a,(ix)
	ld l,a
	; If the update has already passed or is not cached, don't skip
	sbc a,a
	cp h
	ld a,e
	jr nz,handle_waitloop_finish
	ld bc,(ix+1)
	; Choose the smaller absolute value of the cycle counter
	; and the remaining cycles until register change
	inc d
	jr nz,_
	cp l
	jr nc,handle_waitloop_skip_to_expiration
_
	dec d
	; Skip as many full loops as possible until the update time is reached
	ld a,l
_
	add a,c
	jr nc,-_
	sub l
	; Add in the cycles, which may overflow if the update time and
	; cycle expiration time are in the same block
	add a,e
	jr nc,handle_waitloop_finish
	inc d
	jr z,handle_waitloop_overflow
handle_waitloop_finish:
	exx
	ex af,af'
	pop ix
	jp (hl)
	
handle_waitloop_set_sentinel:
	ld a,e
handle_waitloop_set_sentinel_2:
	ld hl,waitloop_sentinel
	ld (event_address),hl
	jr handle_waitloop_finish
	
	; Skip as many full loops as possible until the cycle count expires
handle_waitloop_skip_to_expiration:
	add a,c
	jr nc,handle_waitloop_skip_to_expiration
handle_waitloop_overflow:
	ld c,a
handle_waitloop_variable_finish:
	ld de,(ix+3)
	exx
	push hl
	pop ix
	exx
#ifdef VALIDATE_SCHEDULE
	call.il schedule_jump_event_helper
#else
	jp.lil schedule_jump_event_helper
#endif
	
handle_waitloop_variable:
	ex (sp),ix
	ex af,af'
	exx
	; Add the next jump cycles, and don't skip anything if expired
	ld bc,(ix+2-1) ;BCU=0
	add a,b
	jr nc,_
	inc d
	jr z,handle_waitloop_overflow
_
	ld c,a
	; Check if the waitloop sentinel is set
	ld a,(event_address+1)
	or a
	; Skip straight to the counter expiration
	jr z,handle_waitloop_variable_finish
	ld a,c
	jr handle_waitloop_set_sentinel_2

ophandlerEI_delay_expired:
	; An event is scheduled for the current cycle, so we have to delay the
	; actual enabling of the IME flag.
	ld hl,schedule_ei_delay
	ld (event_counter_checkers_ei_delay),hl
ophandlerEI_no_interrupt:
	; IME did not change, which also means no need to check for interrupts
	ld a,e
	exx
	ex af,af'
	ret
	
ophandlerEI:
intstate_smc_1 = $
	ret	; SMC overrides with EX AF,AF' when IME=0
	exx
	ld e,a
	; Check if an event is scheduled at the current cycle.
	; Note that EI always ends a JIT block, so the cycle count cannot exceed 0.
	or d
	
	; Always disable just the EI handler.
	; This prevents consecutive EIs from causing multiple enable delays,
	; while still preventing interrupts from happening if an event happens
	; during the delay.
	ld a,$C9
	ld (intstate_smc_1),a

	jr z,ophandlerEI_delay_expired
	
	; No event is scheduled for the current cycle, so fully set IME=1 now
	; and schedule an event at the following instruction, only if an
	; interrupt is currently requested
	ld a,trigger_interrupt - (intstate_smc_2 + 1)
	ld (intstate_smc_2),a
	ld hl,(IE)
	ld a,h
	and l
	jr z,ophandlerEI_no_interrupt

	; Interrupt check is delayed until the next block
	; Set cycle counter to -1 and let the next block schedule the event
	ld hl,i
	add hl,de
	ld i,hl
	ld a,$FF
	ld d,a
	exx
	ex af,af'
	ret
	
decode_halt:
	exx
	pop hl
	inc hl
	push ix
	push hl
	inc hl
	inc hl
	ld de,(hl)
	jp.lil decode_halt_helper
decode_halt_continue:
	pop hl
	ld (hl),ix
	dec hl
	ld (hl),a
	ld a,ixh
	cp flush_handler >> 8
	jr nz,_
	ld a,ixl
	cp flush_handler & $FF
_
	pop ix
	push hl
	dec hl
	ld (hl),ophandler_halt >> 8
	dec hl
	ld (hl),ophandler_halt & $FF
	jr nz,_
	; If the JIT needs to be flushed, flush at the HALT address itself
	pop hl
	ld a,d
	ld de,(flush_address)
	dec de
	jp.lil flush_normal
ophandler_halt:
	ex af,af'
	exx
	ld e,a
_
	ld hl,(IE)
	ld a,h
	and l
	ld hl,intstate_smc_2
	jr z,haltspin
	; If interrupts are enabled, go straight to the event handler
	; without setting up the SMC for halt spinning
	; This can only happen as the result of an EI delay slot,
	; so the cycle counter is guaranteed to go from -1 to 0
	ld a,(hl)
	or a
	jr nz,haltnospin
	; Emulate HALT bug in this case
	exx
	pop hl
	; Advance to after the HALT
	inc hl \ inc hl \ inc hl \ inc hl \ inc hl
	; Count cycles for the bugged instruction
	ld a,(hl) \ inc hl
	exx
	add a,e
	jr c,ophandler_halt_maybe_overflow
ophandler_halt_no_overflow:
	exx
	ex af,af'
	jp (hl)
haltspin:
	; Set halted state
	ld a,(hl)
	sub trigger_interrupt - cpu_exit_halt_trigger_interrupt
	jr nc,_
	xor (cpu_exit_halt_trigger_interrupt - trigger_interrupt) ^ (cpu_exit_halt_no_interrupt - cpu_halted_smc)
_
	ld (hl),a
	inc hl
	ld (hl),$18 ;JR cpu_continue_halt
	inc hl
	ld (hl),cpu_continue_halt - (cpu_halted_smc + 2)
haltnospin:
	pop hl
	; Set instruction cycle offset
	ld e,(hl)
	inc hl
	; Push JIT address after HALT
	ld bc,(hl)
	push bc
	 inc hl
	 inc hl
	 ; Set GB address to after HALT
	 ld hl,(hl)
	 ld (event_gb_address),hl
	 ; Put return JIT address in HL'
	 exx
	pop hl
	exx
	; Set remaining cycles to 0
	sbc hl,hl
	ld d,h
	jp do_event_any
	
ophandler_halt_maybe_overflow:
	inc d
	jr nz,ophandler_halt_no_overflow
	inc bc ;BCU=0
	ld c,a
	; Carry is set, subtract out the cycle for the HALT itself
	sbc a,e
	ld b,a
	exx
	push hl
	 ex (sp),ix
	 ld de,(ix-3)
	 dec de
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_event_helper
#else
	 jp.lil schedule_event_helper
#endif
	
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
	jr _
	
reschedule_event_PPU:
	 pop bc
	 ; Get the relative time of the event from the currently scheduled event
	 ex de,hl
	 ld hl,i
	 add hl,de
	pop de
reschedule_event_serial:
	; Calculate current cycle count from block offset
	ld a,e
	add a,c
	ld c,a
	ld b,d
	jr c,_
	dec b
_
	; If the current event is scheduled before or at the current cycle, do nothing
	xor a
	cp b
	jr z,reschedule_event_no_reschedule
	; If the new event is after or at the currently scheduled event, do nothing
	dec bc
	add hl,bc
	jr c,reschedule_event_no_reschedule
	; If the counter already overflowed, trigger an event now to reschedule
	or d
	jr z,trigger_event
	; Update the schedule time
	sbc hl,bc
	ld b,h
	ld c,l
	ld hl,i ; Resets carry
	sbc hl,bc
	ld i,hl
	; Update the cycle counter
	ex de,hl
	add hl,bc
	ex de,hl
	; If the cycle counter didn't overflow, just continue execution
	jr nc,reschedule_event_no_reschedule
	; Trigger an event without attempting to remove an event trigger
	pop hl
	push hl
	push ix
	 call get_mem_write_info
	 ld (event_cycle_count),a
	 jr trigger_event_no_remove
	
trigger_event_already_triggered:
	pop ix
reschedule_event_no_reschedule:
	ld a,e
	ex af,af'
	exx
	ret
	
trigger_event_pop:
	 pop bc
	pop de
trigger_event:
	; Get the cycle offset, GB address, and JIT address after the current opcode
	pop hl
	push hl
	push ix
	 call get_mem_write_info
	 ; If the end of this instruction is already past the target, no reschedule
	 inc b
	 dec b
	 jr z,trigger_event_already_triggered
	 ld (event_cycle_count),a
	 ; If the counter already overflowed, remove any already-scheduled event
	 xor a
	 cp d
	 ld d,a
	 jr nz,trigger_event_no_remove
#ifdef DEBUG
	 ld a,(event_address+1)
	 cp (event_value >> 8) + 1
	 jr c,$
#endif
	 ld a,(event_value)
event_address = $+1
	 ld (event_value),a
#ifdef DEBUG
	 jr _
#endif
trigger_event_no_remove:
#ifdef DEBUG
	 ld a,(event_address+1)
	 cp (event_value >> 8) + 1
	 jr nc,$
_
#endif
	 ld (event_gb_address),ix
	pop ix
	ld a,(hl)
	ld (event_value),a
	ld (event_address),hl
	ld (hl),RST_EVENT
	ld hl,i
	add hl,bc	; Reset div counter to the time of memory access
	ld i,hl
	; Cycle count at event is relative to the memory access
	ld a,e
	sub c
	ex af,af'
	exx
	ret
	
z80_pop_restore_swap_ret:
	 pop bc
write_vram_and_expand_finish:
	pop de
z80_restore_swap_ret:
	ld a,e
	exx
z80_swap_af_ret:
	ex af,af'
z80_ret:
	ret
	
	; JP HL
ophandlerE9:
	ex af,af'
	push ix
	 exx
	 ld e,a
	 exx
	 push bc
	  call.il lookup_code_cached
	  inc de \ dec de ; DEU=0
	 pop bc
	 exx
	 scf
	 adc a,e
	 jr c,++_
_
	 exx
	 ex af,af'
	 ex (sp),ix
	 ret
_
	 inc d
	 jr nz,--_
	 exx
	 lea hl,ix
	 push de
	  exx
	  inc bc ;BCU=0
	  ld c,a
	  sbc a,e
	  ld b,a
	 pop de
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_event_helper
#else
	 jp.lil schedule_event_helper
#endif
	
ophandlerF3:
	ld hl,intstate_smc_1
	ld (hl),$08 ;EX AF,AF'
	ld hl,intstate_smc_2
	ld (hl),0
	; Disable any delayed EI
	ld hl,event_counter_checkers_done
	ld (event_counter_checkers_ei_delay),hl
	ret
	
ophandlerRETI:
	ex af,af'
	exx
	ld e,a
	; Enable interrupts
	ld a,$C9 ;RET
	ld (intstate_smc_1),a
	ld a,trigger_interrupt - (intstate_smc_2 + 1)
	ld (intstate_smc_2),a
	; Check if an interrupt is pending, if not then return normally
	ld hl,(IE)
	ld a,h
	and l
	ld a,e
	jr z,ophandlerRET_swapped_nc
	; Schedule an event after the return
	; If an event will already be scheduled on return, just return
	ld hl,4
	add hl,de
	jr c,ophandlerRET_swapped
	; Update the cycle counter
	ex de,hl
	ld hl,i
	add hl,de
	ld i,hl
	ld d,-1
	ld a,-4
	.db $21 ;LD HL,
	
	; (SPS) = cached RET cycles, cached stack offset
	; (SPS+2) = cached JIT address
	; (SPL) = cached bank delta, cached GB address
ophandlerRET:
	ex af,af'
	exx
ophandlerRET_swapped:
	or a
ophandlerRET_swapped_nc:
callstack_ret_retry_pop:
	; Pop the return address into BC
callstack_ret_pop_offset_smc = $+3
	ld.l bc,(iy)
	inc bc \ dec bc
	; Check if the stack may be overflowing its bounds
	inc iyl
	inc iyl
	jp p,callstack_ret_bound
callstack_ret_do_compare:
	; Get the cached return address in UHL.
	; The high byte of this address is non-zero if and only if
	; the mapped bank is different than when the call occurred.
callstack_ret_shadow_stack_smc = $
	pop.l hl
	; Both compare the return addresses and ensure the bank has not changed.
	sbc.l hl,bc
	pop hl
	jr nz,callstack_ret_target_mismatch
	; Count cycles
	add a,h
	jr c,callstack_ret_maybe_overflow
callstack_ret_no_overflow:
	exx
	ex af,af'
	ret
	
#ifdef SHADOW_STACK
callstack_ret_set_shadow_stack:
	call set_shadow_stack_rollback
	jr callstack_ret_retry_pop
#endif
	
callstack_ret_bound:
	push bc
	 exx
	 call shift_stack_window_higher
	 exx
	pop bc
	jr nc,callstack_ret_do_compare
	or a
callstack_ret_no_shadow_stack:
	jp do_pop_for_ret_slow
	
callstack_ret_maybe_overflow:
	inc d
	jr z,callstack_ret_overflow
	exx
	ex af,af'
	ret

callstack_ret_maybe_bank_mismatch:
	sub l
	and $20
	jr nz,callstack_ret_preserve_entry
	
	; Check if the bank difference is non-zero
	dec.l sp
	dec.l sp
	pop.l hl
	inc h
	dec h
	call nz,callstack_ret_bank_mismatch
	dec.l sp
	pop hl
	ld a,e
	or a
	jr callstack_ret_do_compare

callstack_ret_cond_maybe_overflow:
	inc d
	jr nz,callstack_ret_cond_no_overflow
	dec h
	; Make sure this wasn't the bottom of the callstack
	jr z,callstack_ret_preserve_entry
callstack_ret_overflow:
	; Subtract taken RET cycles to get the block cycle offset
	ld de,$FC00 ;DEU=0
	add hl,de
	; Put return address in DE
	ld d,b
	ld e,c
	ld b,h
	ld c,a
	; Get the cached JIT target address
	ex (sp),ix
	exx
	lea hl,ix
	exx
#ifdef VALIDATE_SCHEDULE
	call.il schedule_event_helper_a
#else
	jp.lil schedule_event_helper_a
#endif

callstack_ret_target_mismatch:
	ld e,a
	; Check for the bottom of the callstack
	dec h
	jp m,callstack_ret_preserve_entry
	ld a,iyl
	; If the target comparison carried, the bank delta was definitely zero
	jr nc,callstack_ret_maybe_bank_mismatch
	sub l
	add a,a
	add a,a
	; Pop the JIT return address
	pop hl
	jr z,do_ret_full ; If this was the exact stack depth, avoid popping the next entry
	add a,a
	ld a,e
	jr nc,callstack_ret_do_compare
	or a ; Unconditional RET
	push hl
callstack_ret_preserve_entry:
	dec sp
	dec sp
	dec.l sp
	dec.l sp
	dec.l sp
do_ret_full:
	push ix
	 push af
	  push de
	   inc de ;DEU=0
	   ld d,b
	   ld e,c
do_pop_for_ret_slow_finish:
	   call.il lookup_code_cached
	   ex de,hl
	  pop de
	  ld b,a
	  add a,4 ; Add the taken cycles for RET
	  ld c,a
	 pop af
	 ld a,e
	 adc a,c  ; Count cycles (possibly conditional)
	 jr c,do_ret_full_maybe_overflow
do_ret_full_no_overflow:
	 ex (sp),ix
	 exx
	 ex af,af'
	 ret
	
ophandlerRETcond:
	ex af,af'
	exx
callstack_ret_cond_retry_pop:
	; Pop the return address into BC
callstack_ret_cond_pop_offset_smc = $+3
	ld.l bc,(iy)
	inc bc \ dec bc
	; Check if the stack may be overflowing its bounds
	inc iyl
	inc iyl
	jp p,callstack_ret_cond_bound
callstack_ret_cond_do_compare:
	or a
callstack_ret_cond_do_compare_nc:
	; Get the cached return address in UHL.
	; The high byte of this address is non-zero if and only if
	; the mapped bank is different than when the call occurred.
callstack_ret_cond_shadow_stack_smc = $
	pop.l hl
	; Both compare the return addresses and ensure the bank has not changed.
	sbc.l hl,bc
	pop hl
	jr nz,callstack_ret_cond_target_mismatch
	; Count cycles
	inc h
	add a,h
	jr c,callstack_ret_cond_maybe_overflow
callstack_ret_cond_no_overflow:
	exx
	ex af,af'
	ret
	
callstack_ret_dummy_target:
	ex af,af'
	exx
	ld e,a
	dec sp
	dec sp
	; Check for conditional RET
	srl h
	jr nc,callstack_ret_preserve_entry
	; Remove the counted cycle
	dec de
	jr callstack_ret_preserve_entry
	
do_ret_full_maybe_overflow:
	 inc d
	 jr nz,do_ret_full_no_overflow
	 exx
	 lea hl,ix
	 exx
	 ; Get the target cycle offset
	 ld c,a
	 ex de,hl
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_event_helper_a
#else
	 jp.lil schedule_event_helper_a
#endif

callstack_ret_cond_target_mismatch:
	ld e,a
	; Check for the bottom of the callstack
	dec h
	jp m,callstack_ret_cond_preserve_entry
	ld a,iyl
	; If the target comparison carried, the bank delta was definitely zero
	jr nc,callstack_ret_cond_maybe_bank_mismatch
	sub l
	and $20
	; Pop the JIT return address
	pop hl
	ld a,e
	jr z,callstack_ret_cond_do_compare
	push hl
callstack_ret_cond_preserve_entry:
	scf
	jr callstack_ret_preserve_entry
	
callstack_ret_cond_maybe_bank_mismatch:
	sub l
	and $20
	scf
	jr nz,callstack_ret_preserve_entry
	
	; Check if the bank difference is non-zero
	dec.l sp
	dec.l sp
	pop.l hl
	inc h
	dec h
	call nz,callstack_ret_bank_mismatch
	dec.l sp
	pop hl
	ld a,e
	jr callstack_ret_cond_do_compare
	
#ifdef SHADOW_STACK
callstack_ret_cond_set_shadow_stack:
	call set_shadow_stack_rollback
	jr callstack_ret_cond_retry_pop
#endif
	
callstack_ret_cond_bound:
	push bc
	 exx
	 call shift_stack_window_higher
	 exx
	pop bc
	jr nc,callstack_ret_cond_do_compare_nc
callstack_ret_cond_no_shadow_stack:
	scf
	jp do_pop_for_ret_slow
	
callstack_ret_bank_mismatch:
	push bc
	 jp.lil callstack_ret_bank_mismatch_helper
	
	#include "mbc.asm"
	#include "stack.asm"
	#include "memory.asm"
	
	; Cached RST and interrupt handlers are combined in this space
	; Handlers consist of a jump followed by a cycle count
	; Interrupt handlers are indexed by 1, 2, 4, 8, 16
	; Address info is stored in halves of empty handler slots
	; Handler to address info mapping: add 10 slots and divide by 2
	; Unused slot halves: 7.5, 9.5, 10.0, 12.0, 12.5
	.block (-$)&$FF
dispatch_rst_00: ;0 -> 5.0
	.db 0 \ jp 0
dispatch_vblank: ;1 -> 5.5
	.db 0 \ jp 0
dispatch_stat:   ;2 -> 6.0
	.db 0 \ jp 0
dispatch_rst_08: ;3 -> 6.5
	.db 0 \ jp 0
dispatch_timer:  ;4 -> 7.0
	.db 0 \ jp 0
	; Address info for RST 00h, VBLANK, STAT, RST 08h, TIMER
	.dw $0000, $0040, $0048, $0008, $0050, 0
dispatch_serial: ;8 -> 9.0
	.db 0 \ jp 0
	; Address info for SERIAL, RST 10h
	.dw $0058, 0, 0, $0010
dispatch_rst_10: ;11 -> 10.5
	.db 0 \ jp 0
	; Address info for JOYPAD, RST 18h - 38h
	.dw 0, 0, $0060, $0018, $0020, $0028, $0030, $0038
dispatch_joypad: ;16 -> 13.0
	.db 0 \ jp 0
dispatch_rst_18: ;17 -> 13.5
	.db 0 \ jp 0
dispatch_rst_20: ;18 -> 14.0
	.db 0 \ jp 0
dispatch_rst_28: ;19 -> 14.5
	.db 0 \ jp 0
dispatch_rst_30: ;20 -> 15.0
	.db 0 \ jp 0
dispatch_rst_38: ;21 -> 15.5
	.db 0 \ jp 0
	
wait_for_interrupt_stub:
	ei
	halt
	ret.l
	
flush_handler:
	exx
	ld b,d
flush_address = $+1
	ld de,0
	ex af,af'
	jp.lil flush_normal
	
coherency_handler:
	ex (sp),ix
	 lea hl,ix+RAM_PREFIX_SIZE-3
	 exx
	 ld bc,(ix+2) ;BCU=0
	 ld ix,(ix)
	 ex af,af'
	 ld e,a
	 push de
	  jp.lil check_coherency_helper
	
handle_overlapped_op_1_1:
	ex (sp),ix
	jp.lil handle_overlapped_op_1_1_helper
	
handle_overlapped_op_1_2:
	ex (sp),ix
	jp.lil handle_overlapped_op_1_2_helper
	
handle_overlapped_op_2_1:
	ex (sp),ix
	jp.lil handle_overlapped_op_2_1_helper
	
Z80InvalidOpcode:
	jp.lil Z80InvalidOpcode_helper
	
Z80Error:
	jp.lil runtime_error
	
keys:
	.dw $FFFF
	
trampoline_next:
	.dl 0
render_save_sps:
	.dw 0
	
	; One word of stack space for sprite rendering during vblank
lcd_on_ppu_event_checker:
	.dw 0
event_counter_checkers:
event_counter_checker_slot_PPU:
	.dw ppu_expired_vblank
event_counter_checker_slot_timer:
	.dw disabled_counter_checker
event_counter_checker_slot_serial:
	.dw disabled_counter_checker
event_counter_checkers_ei_delay:
	.dw event_counter_checkers_done
	
	.assume adl=1
z80codesize = $-0
	.org z80code+z80codesize
	
	.echo "Z80 mode code size: ", z80codesize
	
jit_start = z80codesize
