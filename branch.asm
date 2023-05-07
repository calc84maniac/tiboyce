	.assume adl=0

do_call_no_shadow_stack:
	push bc
do_push_overflow_for_call_slow:
	 push hl
	  ld e,6 ; Cycles for taken CALL
	  exx
	  push hl
	   call do_push_for_call_slow_swap
	   ex af,af'
	  pop hl
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
	jr do_call_dispatch_push

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
do_call_dispatch_push:
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
banked_jump_mismatch_continue:
	inc hl
	inc hl
	exx
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
	; Save the new bank index
	ld (hl),a
	jp.lil banked_jump_mismatch_helper

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
#ifdef FASTLOG
	push af
	push hl
	FASTLOG_EVENT_Z80(CALLSTACK_OVERFLOW, 2)
	pop af
#endif
	pop hl
	ld sp,myz80stack - 4
	push hl
callstack_overflow_helper_smc = $+1
	jp 0

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
#ifdef FASTLOG
	   push af
	   push de
	   push hl
	   push ix
	   FASTLOG_EVENT_Z80(DECODE_JUMP, 8)
	   dec sp \ dec sp
	   pop af
#endif
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
	 jr c,do_push_overflow_for_rst_slow
	 exx
	pop hl
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

do_rst_no_shadow_stack:
	; Undo cycle counting because writes might be cycle-sensitive
	exx
	dec hl
	sub (hl)
	push hl
	 exx
do_push_overflow_for_rst_slow:
	 ld e,4 ; Cycles for taken RST
	 call do_push_for_call_slow
	 ex af,af'
	pop hl
	add a,(hl)
	inc hl
	exx
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

decode_rst:
	jp.lil decode_rst_helper


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
	jr c,_
	add a,c
	jr nc,-_
_
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
	ld hl,waitloop_sentinel
	ld (event_address),hl
	jr handle_waitloop_finish

	; Skip as many full loops as possible until the cycle count expires
handle_waitloop_skip_to_expiration:
	add a,c
	jr c,handle_waitloop_overflow
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
	ld e,a
	; Check if the waitloop sentinel is set
	ld a,(event_address+1)
	or a
	jr nz,handle_waitloop_set_sentinel
	; Skip straight to the counter expiration
	ld c,b
	jr handle_waitloop_variable_finish


ophandlerSTOP:
	ex af,af'
	exx
	pop hl
	ld hl,(hl)
	push ix
	 ld e,a
	 ld bc,KEY1
	 ld a,(bc)
	 rra
	 jr nc,_
	 add a,a
	 xor $80
	 ld (bc),a
	 ld b,d
	 ld c,e
	 ld d,a
	 push hl
	  call.il reset_div
	  ld i,hl
	  ld a,d
	  call.il set_cpu_speed
	 pop hl
	 ld de,$FFFF
_
	 push de
	  ex de,hl
	  call.il lookup_code_cached
	  ex de,hl
	 pop de
	 scf
	 adc a,e
	 jr nc,_
	 inc d
	 jr nz,_
	 exx
	 lea hl,ix
	 exx
	 inc bc
	 ld c,a
	 sbc a,e
	 ld b,a
	 ex de,hl
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_event_helper
#else
	 jp.lil schedule_event_helper
#endif

ophandlerJPHL:
#ifdef DEBUG
	pop hl
#endif
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


ophandlerRETI:
#ifdef DEBUG
	pop hl
#endif
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
#ifdef DEBUG
	.db $5B ;.LIL
#endif
	.db $21 ;LD HL,

	; (SPS) = cached RET cycles, cached stack offset
	; (SPS+2) = cached JIT address
	; (SPL) = cached bank delta, cached GB address
ophandlerRET:
#ifdef DEBUG
	pop hl
#endif
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
	ld c,$C9 ;RET
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
	 dec bc ;Set BCU=0; prior C is at least 4 so B doesn't change
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
	ld c,$FF ;stale open bus because of internal cycle

	; Input: Carry reset for unconditional RET, set for conditional RET
	;        C = open bus value, DA = cycle counter
do_pop_for_ret_slow:
	ld e,a
	push ix
	 push af
	  push de
	   ; Advance the cycle count past the end of the RET
	   adc a,4
	   jr nc,_
	   inc d
_
	   ld e,-3 ; The first read is 3 cycles before the end of the instruction
	   call do_pop_any_slow
	   push hl
	    exx
	   pop de
	   jp do_pop_for_ret_slow_finish

callstack_ret_bank_mismatch:
	push bc
	 jp.lil callstack_ret_bank_mismatch_helper
