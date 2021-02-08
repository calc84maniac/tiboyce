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
	djnz do_pop_ports
	jr do_pop_check_overflow
	
	;.block $18-$
	; Currently unused
	
	.block $20-$
r_call:
	ex af,af'
	exx
	pop ix
	ld c,a
	ld de,(ix+5)
	ld a,i
	dec a
	jr nz,do_call
	jr do_call_reset_callstack
	
	;.block $28-$
	; Currently unused
	
	.block $30-$
r_event:
	ex af,af'
	pop ix
	dec ix
	jp do_event
	
	.block $38-$
r_cycle_check:
rst38h:
	inc iyh
	ret nz
	ld iyl,a
	pop ix
	push hl
	 push de
	  push bc
	   jp nc,cycle_overflow_for_jump
	   ld c,(ix-3)
	   ld de,(ix-7)
	   jp.lil schedule_subblock_event_helper
	
do_pop_check_overflow:
	ld c,a
	ld a,h
do_pop_bound_smc_1 = $+1
	cp 0
_
	jp p,do_pop_overflow
	ld a,c
do_pop_jump_smc_2 = $+1
	jr do_pop_ports
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
	
do_pop_ports:
	ld iyl,a
	; Advance the return address to the end of the instruction,
	; and execute the skipped pop opcode here in case it's overwritten
	pop de
	ld a,(de)
	ld (do_pop_ports_smc),a
	inc de
	push de
	call pop_ports
	push de
	 exx
do_pop_ports_smc = $
	pop bc
	ret
	
callstack_pop_skip:
	jp p,callstack_pop_save
	ld a,i
	inc a
	ld i,a
	exx
	ld a,iyl
	ret
	
do_call_maybe_overflow:
	inc iyh
	jr nz,do_call_no_overflow
	call cycle_overflow_for_call
	jr callstack_ret
	
do_call_reset_callstack:
	ld sp,myz80stack-2
	ld a,CALL_STACK_DEPTH
do_call:
	ld i,a
	pea ix+7
	ld a,(ix+2)
do_call_common:
	push bc
	ld ix,(ix)
	add a,c
	jr c,do_call_maybe_overflow
do_call_no_overflow:
	call do_push_for_call
callstack_ret:
	ld iyl,a
	exx
	pop af
	pop ix
	scf
	sbc a,b
callstack_pop_check_overflow_smc:
	jr nz,callstack_pop_skip
	cp b
	jr z,callstack_pop_bound
callstack_pop_nobound:
	xor a
	or (ix-3)
	jr z,callstack_pop_nobank
rom_bank_check_smc_3 = $+1
	xor 0
	jr nz,callstack_pop_nomatch_inc
callstack_pop_nobank:
	ld a,i
	inc a
	ld i,a
	ld c,a	;C is now at least 2
	ld de,(ix-2)
	ld a,e
callstack_pop_compare_smc_1:
	cpi.l
	jr nz,callstack_pop_nomatch_dec
	ld a,d
callstack_pop_compare_smc_2:
	cpi.l
	jr nz,callstack_pop_nomatch_dec2
	inc b
	ld a,(ix-4)
dispatch_cycles_for_ret:
	ld (dispatch_cycles_for_ret_smc),a
dispatch_cycles_for_ret_smc = $+2
	lea iy,iy+0
	ld a,iyh
	or a
	jr z,cycle_overflow_for_ret
	exx
	ld a,iyl
	ex af,af'
	jp (ix)
	
callstack_pop_bound:
	ld a,h
do_pop_bound_smc_4 = $+1
	cp 0
	jp m,callstack_pop_nobound
callstack_pop_overflow:
	ld a,i
	inc a
	ld i,a
do_pop_for_ret_overflow:
	call pop_overflow
	ex af,af'
	exx
	pop de	; Ensure top byte clear
	jr pop_and_lookup_code_cached_continue
	
do_rom_bank_call:
	ex af,af'
	exx
	ld c,a
	pop ix
	ex (sp),ix
rom_bank_check_smc_1 = $+1
	ld a,0
	cp (ix+2)
	jr nz,banked_call_mismatch
banked_call_mismatch_continue:
	ld a,i
	dec a
	ld i,a
	ld a,(ix)
	ld de,(ix+3)
	lea ix,ix+5
	ex (sp),ix
	jr nz,do_call_common
	ld iyl,a
	pop af
	ld sp,myz80stack-2
	push af
	ld a,CALL_STACK_DEPTH
	ld i,a
	ld a,iyl
	jp do_call_common
	
callstack_pop_nomatch_inc:
	ld a,i
	inc a
	ld i,a
	jr pop_and_lookup_code_cached
	
callstack_pop_nomatch_dec2:
	dec.l hl
callstack_pop_nomatch_dec:
	dec.l hl
pop_and_lookup_code_cached:
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
	inc de	; Ensure top byte clear
	ld.l e,(hl)
	inc.l hl
	ld.l d,(hl)
	inc.l hl
pop_and_lookup_code_cached_continue:
	push bc
	 call.il lookup_code_cached
	pop bc
	add a,4
dispatch_cycles_for_reti_smc = $+1
	jr dispatch_cycles_for_ret
	
do_pop_for_ret_z80:
	bit 6,b
	jr nz,do_pop_for_ret_overflow
	inc b
	ld de,(hl)	; Ensure top byte clear
	inc hl
	inc hl
	jr pop_and_lookup_code_cached_continue
	
cycle_overflow_for_ret:
	ld a,(dispatch_cycles_for_ret_smc)
	push de
	 exx
	 ex (sp),hl
	 push de
	  push bc
	   ex de,hl
	   sub 4
	   ld c,a
	   push ix
	    jp.lil schedule_event_helper
	
banked_call_mismatch:
	jp.lil banked_call_mismatch_helper
	
callstack_pop_save:
	dec sp
	dec sp
	dec sp
	dec sp
	dec sp
	dec sp
	jr pop_and_lookup_code_cached
	
cycle_overflow_for_call:
	ld iyl,a
	sub c
	sub 6
	push ix
	 push de
	  exx
	  ex (sp),hl
	  push de
	   push bc
	    ld de,do_push_and_return
	    push de
	     ex de,hl
	     dec de
	     ld c,a
	     jp.lil schedule_call_event_helper
	
dispatch_cycles_for_reti:
	; Count cycles before attempting to trigger an interrupt
	ld (_+2),a
_
	lea iy,iy+0
	cpl
	add a,5
	ld (event_cycle_count),a
	ld a,dispatch_cycles_for_ret - (dispatch_cycles_for_reti_smc+1)
	ld (dispatch_cycles_for_reti_smc),a
	ld (event_gb_address),de
	exx
	jp do_event_no_reset
	
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
	exx
	jr c,++_
_
	ex af,af'
	jp (ix)
_
	inc iyh
	jr nz,--_
	ld iyl,a
	push hl
	 push de
	  push bc
	   ld b,(ix)
	   ld c,(ix+4)
	   ld de,(ix+6)
	   ld ix,(ix+1)
	   jr schedule_jump_event_helper_trampoline
	
banked_jump_mismatch:
	jp.lil banked_jump_mismatch_helper
	
cycle_overflow_for_jump:
	   xor a
	   sub (ix-3)
	   ld c,a
	   ld b,(ix+1)
	   ld de,(ix+4)
	   ld ix,(ix+2)
schedule_jump_event_helper_trampoline:
	   push ix
	    jp.lil schedule_jump_event_helper
	
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
schedule_event_finish_no_schedule:
	   pop ix
	  pop bc
	 pop de
	pop hl
	ld a,iyl
	ex af,af'
	jp (ix)
	
Z80InvalidOpcode:
	jp.lil Z80InvalidOpcode_helper
	
Z80Error:
	jp.lil runtime_error
	
do_push_for_call_overflow:
	; Place the call target where the cycle count resolver can identify it
	ld (do_push_for_call_target),ix
	call do_push_overflow
do_push_for_call_target = $+1
	jp 0
	
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
	
do_push_overflow:
	ld iyl,c
	ex af,af'
	push af
	 ld b,e	; B' can be used for safe storage, since set_gb_stack restores it
	 ld a,d
	 ld de,(sp_base_address_neg)
	 add hl,de
	 push hl
	  ld de,-1
	  add iy,de
	  exx
	  ex (sp),hl
	  dec hl
	  jr nc,do_push_overflow_underflow
	  call mem_write_any
do_push_overflow_continue:
	  exx
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

pop_overflow:
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
	push ix
	 call.il set_gb_stack_bounds_helper
	 call.il set_gb_stack_bank_helper
	pop ix
	inc.s bc
	jr set_gb_stack_bank_done	
	
do_event:
	ld iyl,a
event_value = $+3
	ld (ix),0
do_event_no_reset:
	push hl
#ifdef DEBUG
	 ld hl,event_value
	 ld (event_address),hl
#endif
do_event_pushed:
	 xor a
event_cycle_count = $+2
	 lea hl,iy+0
	 cp h
	 jr z,event_expired
	 sbc hl,hl	;ld hl,IE
	 ld a,(hl)
	 ld l,IF - ioregs
intstate_smc_1:
	 and (hl)
	 jr nz,trigger_interrupt_push
	 cp iyh
	 jr z,event_reschedule_push
	pop hl
	ld a,iyl
	ex af,af'
	jp (ix)

event_expired:
	 ; At least one event expired
	 push de
	  push bc
div_counter = $+1
	   ld hl,0
	   ld (event_save_sp),sp
event_expired_loop:
	   ld sp,event_counter_checkers

	   ld b,h
	   ld c,l
vblank_counter = $+1
	   ld de,0
	   sbc hl,de
	   ex de,hl
	   ret nz
	   jp.lil vblank_helper

event_counter_checkers_done:
	   ld h,b
	   ld l,c
	   or a
	   sbc hl,de
	   add iy,de
	   jr nc,_
	   ld a,(event_cycle_count)
	   dec a
	   adc a,iyl
	   ccf
	   jr nc,event_expired_loop
_
	   ld (div_counter),hl
event_save_sp = $+1
	   ld sp,0
	   
event_not_expired:
	   ld hl,IE
	   ld a,(hl)
	   ld l,IF - ioregs
intstate_smc_2:
	   and (hl)
	   jr nz,trigger_interrupt
	   cp iyh
	   jr z,event_reschedule
	  pop bc
	 pop de
	pop hl
	ld a,iyl
	ex af,af'
	jp (ix)
	   
event_reschedule_push:
	 push de
	  push bc
event_reschedule:
	   ld de,(event_gb_address)
	   ld a,(event_cycle_count)
	   neg
	   ld c,a
	   push ix
	    jp.lil schedule_event_helper
	
trigger_interrupt_push:
	 push de
	  push bc
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
	   ld hl,dispatch_joypad+1
	   jr trigger_int_selected
trigger_serial:
	   res 3,(hl)
	   ld hl,dispatch_serial+1
	   jr trigger_int_selected
trigger_timer:
	   res 2,(hl)
	   ld hl,dispatch_timer+1
	   jr trigger_int_selected
trigger_stat:
	   res 1,(hl)
	   ld hl,dispatch_stat+1
	   jr trigger_int_selected
trigger_vblank:
	   res 0,(hl)
	   ld hl,dispatch_vblank+1
trigger_int_selected:
	
	   push hl
	    ld a,$AF ;XOR A
	    ld (intstate_smc_1),a
	    ld (intstate_smc_2),a
	    exx
event_gb_address = $+1
	    ld de,event_gb_address
	    ; If we're on a HALT, exit it
cpu_halted = $+1
	    and 0
	    ld a,(event_cycle_count)
	    ld c,a
	    jp.lil z,dispatch_int_helper
	    ld (cpu_halted),a
	    inc a
	    sub (ix+3)
	    lea ix,ix+7
	    inc de
	    jp.lil dispatch_int_helper

dispatch_int_continue:
	   pop ix
	   or (ix-1)
	   jr z,decode_intcache
dispatch_int_decoded:
	   add a,c
	   exx
	  pop bc
	 pop de
	pop hl
	ld (_+2),a
_
	lea iy,iy+0
	ld a,iyh
	or a
	jp nz,do_push_for_int
	; Carry is reset
cycle_overflow_for_rst_or_int:
	push ix
	 push hl
	  push de
	   push bc
	    ld hl,do_push_and_return
	    push hl
	     ld a,(ix-1)
	     ; Subtract 4 for RST, 5 for CALL
	     adc a,-5
	     ld c,a
	     lea de,ix+(-(dispatch_rst_00+1 + $80) & $FF) - $80
	     ld d,0
	     sla e
	     ld ix,(ix+1)
	     jp.lil schedule_event_helper

decode_intcache:
	   exx
	   lea de,ix+(-(dispatch_rst_00+1 + $80) & $FF) - $80
	   ld d,a
	   sla e
	   call.il decode_intcache_helper
	   ld (ix+1),hl
	   ld (ix-1),a
	   exx
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
	 ld (hl),ix
	 dec hl
	 ld (hl),$C3	;JP target
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
	   dec hl
	   ld (hl),a
	   dec hl
	   dec hl
	   ld (hl),ix
	   dec hl
	   ld (hl),b
	  pop de
	 pop bc
	pop af
	ex (sp),hl
	ret
	
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
	   dec hl
	   ld (hl),a
	   dec hl
	   dec hl
	   ld (hl),ix
	   dec hl
	   dec hl
	   ; If a CALL opcode was returned instead of RST, this is a banked call
	   bit 1,b
	   jr nz,_
	   ; Modify the conditional entry point to use the banked call
	   ld de,(hl)
	   dec de
	   dec de
	   ld (hl),de
_
	   dec hl
	   ld (hl),$CD
	  pop de
	 pop bc
	pop af
	ex (sp),hl
	ret
	
do_rst_00:
	exx
	ld de,dispatch_rst_00
	jr decode_rst
do_rst_08:
	exx
	ld de,dispatch_rst_08
	jr decode_rst
do_rst_10:
	exx
	ld de,dispatch_rst_10
	jr decode_rst
do_rst_18:
	exx
	ld de,dispatch_rst_18
	jr decode_rst
do_rst_20:
	exx
	ld de,dispatch_rst_20
	jr decode_rst
do_rst_28:
	exx
	ld de,dispatch_rst_28
	jr decode_rst
do_rst_30:
	exx
	ld de,dispatch_rst_30
	jr decode_rst
do_rst_38:
	exx
	ld de,dispatch_rst_38
	jr decode_rst
	
decode_rst:
	jp.lil decode_rst_helper
	
do_rst:
	pop ix
	ex af,af'
	ld c,a
	ld a,i
	dec a
	jr z,++_
_
	ld i,a
	pea ix+4
	push bc
	ld a,(de)
	inc de
	push de
	 ld de,(ix+2)
	pop ix
	add a,c
	jr c,do_rst_maybe_overflow
do_rst_no_overflow:
	call do_push_for_call
	jp callstack_ret
_
	ld sp,myz80stack-2
	ld a,CALL_STACK_DEPTH
	jr --_
	
do_rst_maybe_overflow:
	inc iyh
	jr nz,do_rst_no_overflow
	; Carry is set
	exx
	ld iyl,a
	call cycle_overflow_for_rst_or_int
	jp callstack_ret
	
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
	add a,(ix-4)
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
	push hl
	 push de
	  push bc
	   ld a,(ix-4)
	   sub 4
	   ld c,a
	   ld de,(ix-2)
	   push ix
	    jp.lil schedule_event_helper
	
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
	.db 0
	jp 0
dispatch_rst_08:
	.db 0
	jp 0
dispatch_rst_10:
	.db 0
	jp 0
dispatch_rst_18:
	.db 0
	jp 0
dispatch_rst_20:
	.db 0
	jp 0
dispatch_rst_28:
	.db 0
	jp 0
dispatch_rst_30:
	.db 0
	jp 0
dispatch_rst_38:
	.db 0
	jp 0
dispatch_vblank:
	.db 0
	jp 0
dispatch_stat:
	.db 0
	jp 0
dispatch_timer:
	.db 0
	jp 0
dispatch_serial:
	.db 0
	jp 0
dispatch_joypad:
	.db 0
	jp 0
	
flush_mem_handler:
	exx
	ex af,af'
	ld iyl,a
	ld a,b
	pop bc
	jp.lil flush_mem
	
coherency_handler:
	pop ix
	push hl
	 push de
	  push bc
	   pea ix+RAM_PREFIX_SIZE-3
	    ld ix,(ix)
	    jp.lil check_coherency_helper

coherency_return:
	    ex af,af'
	   pop ix
	  pop bc
	 pop de
	pop hl
	jp (ix)
	   
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
	   lea hl,ix
	   jr nc,ophandler08_underflow
	   ld a,e
	   call mem_write_any
ophandler08_continue:
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
	ex af,af'
	pop ix
	push hl
	 push de
	  push bc
	   ld bc,(ix+2)
	   ; Skip straight to the counter expiration
	   ld iyh,0
	   ld a,c
handle_waitloop_common:
	   ld iyl,a
	   ld de,(ix+6)
	   ld ix,(ix+4)
	   push ix
	    jp.lil schedule_jump_event_helper
	
_
	   inc iyh
	   jr nz,_
	   jr handle_waitloop_common
	
handle_waitloop_ly:
	ex af,af'
	pop ix
	push hl
	 push de
	  push bc
	   ; Add the next jump cycles, but preserve the original count
	   ld bc,(ix+2)
	   ld d,iyh
	   ld e,a
	   add a,c
	   jr c,-_
_
	   ld b,a
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
	   cp b
	   jr nc,_
	   ld a,b
_
	   ld l,a
	   ; Skip as many full loops as possible without exceeding the cycle count
_
	   add a,h
	   jr nc,-_
	   sub h
	   sub l
	   ; Add in the cycles (this cannot overflow because of the counter limit)
	   add a,b
	   jr c,_
	   dec iyh
	   ld b,a
handle_waitloop_ly_finish:
	   ld a,b
_
	  pop bc
	 pop de
	pop hl
	ex af,af'
	ld ix,(ix+4)
	jp (ix)

ophandlerEI:
	ex af,af'
	ld iyl,a
	ld a,$A6 ;AND (HL)
	ld (intstate_smc_1),a
	ld (intstate_smc_2),a
	pop ix
	push hl
	 ld a,(ix)
	 ld (_+2),a
_
	 lea iy,iy+0
	 neg
	 inc a
	 ld (event_cycle_count),a
	 ld hl,(ix+1)
	 ld (event_gb_address),hl
	 lea ix,ix+4
	 jp do_event_pushed
	
ophandler76:
	ex af,af'
	ld iyl,a
	pop ix
	push hl
	 ld hl,IF
	 ld a,(hl)
	 ld l,h
	 and (hl)
	 ld hl,(ix+1)
	 jr z,haltspin
	 ld a,(ix)
	 ld (_+2),a
_
	 lea iy,iy+0
	 neg	; A is non-zero, so this sets carry
	 inc a
	 lea ix,ix+4
	 jr haltdone
haltspin:
	 dec hl
	 lea ix,ix-3
	 ld iy,0
haltdone:
	 ld (event_cycle_count),a
	 ld (event_gb_address),hl
	 sbc a,a
	 inc a
	 ld (cpu_halted),a
	 jp do_event_pushed
	
	
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
	 ld c,(hl)
	 ld iyh,a
	 sub c
	 inc c	; New cycle count is relative to the memory access
	 ld iyl,c
	 ld (event_cycle_count),a
	 dec hl
	 dec hl
	 ld hl,(hl)
	 ld (event_gb_address),hl
	 lea hl,ix
	 ld (event_address),hl
	 ld a,(hl)
	 ld (event_value),a
	 ld (hl),RST_EVENT
	 ld hl,(div_counter)
	 add hl,de	; Reset div counter to the time of memory access
	 ld (div_counter),hl
trigger_event_already_triggered:
	pop.l hl
	exx
	ld a,iyl
	ex af,af'
	ret
	
ophandlerE2:
	ld ixh,$FF
	ld ixl,c
	ex af,af'
	ld iyl,a
	jp mem_write_ports_swapped
	
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
	call z,reset_z_flag_only
	ld a,c
	jp set_gb_stack
	
ophandlerE9:
	ex af,af'
	push hl
	 push de
	  push bc
	   ex de,hl
	   ld l,a
	   call.il lookup_code_cached
	   scf
	   adc a,l
	   jr c,++_
_
	  pop bc
	 pop de
	pop hl
	ex af,af'
	jp (ix)
_
	   inc iyh
	   jr nz,--_
	   ld iyl,a
	   sbc a,l
	   ld c,a
	   inc de
	   dec de
	   push ix
	    jp.lil schedule_event_helper
	
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
	
ophandlerF1_pop_ports:
	ex af,af'
	ld iyl,a
	call pop_ports
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
	push af
	 ld a,$AF ;XOR A
	 ld (intstate_smc_1),a
	 ld (intstate_smc_2),a
	pop af
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
	djnz do_push_ports
	jr do_push_check_overflow
	
ophandlerE5:
	push hl
	 exx
	pop de
do_push_jump_smc_2 = $+1
	djnz do_push_ports
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
	jr do_push_ports
	
ophandlerD5:
	push de
	 exx
	pop de
do_push_jump_smc_4 = $+1
	djnz do_push_ports
	jr do_push_check_overflow
	
ophandlerC5:
	push bc
	 exx
	pop de
do_push_jump_smc_5 = $+1
	djnz do_push_ports
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

do_push_for_call_rtc:
	push ix
do_push_rtc:
	ld ix,(sp_base_address)
	ld (ix+5),e
	dec.l hl
	dec.l hl
	exx
	ret
	
do_push_for_call_cart:
	push ix
do_push_cart:
	ld ix,mem_write_cart_always
	push af
	 call do_push_generic
	pop af
	ret
	
do_push_for_call_vram:
	push ix
do_push_vram:
	ld ix,mem_write_vram_always
	push af
	 call do_push_generic
	pop af
	ret
	
do_push_ports:
	dec hl
	push hl
	pop ix
	push af
	 ex af,af'
	 ld iyl,a
	 ld a,e
	 push af
	  ld a,d
	  dec hl
	  push hl
	   ld de,-1
	   add iy,de
	   exx
	   jr nc,push_ports_underflow
	   call mem_write_ports
push_ports_continue:
	  pop ix
	 pop af
	 inc iy
	 call mem_write_ports
	pop af
	ret
	
do_push_and_return:
	exx
do_push_and_return_jump_smc = $+1
	djnz do_push_adl
	pop ix
	jr do_push_for_call_check_overflow
	
do_push_for_int:
	ld a,iyl
	exx
do_push_for_call:
	ex af,af'
do_push_for_call_jump_smc_1 = $+1
	djnz do_push_for_call_adl
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
	
do_push_and_return_ports:
	pop ix
do_push_for_call_ports:
	; Place the call target where the cycle count resolver can identify it
	ld (do_push_for_call_ports_target),ix
	call do_push_ports
do_push_for_call_ports_target = $+1
	jp 0
	
	   ; If the cycle count underflowed, modify event trigger logic
	   ; to force it to recognize the count as an overflow
push_ports_underflow:
	   ex af,af'
	   xor a
	   ld (trigger_event_remove_smc),a
	   ex af,af'
	   call mem_write_ports
	   ld a,trigger_event_no_remove - (trigger_event_remove_smc+1)
	   ld (trigger_event_remove_smc),a
	   jr push_ports_continue
	
do_push_for_call_z80:
	dec hl
	dec hl
	ld (hl),de
	exx
	jp (ix)
	
do_push_for_call_adl:
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
	 call z,reset_z_flag_only
	pop de
	ld a,ixl
	ret
	
reset_z_flag_only:
	push af
	dec sp
	pop af
	res 6,a
	push af
	inc sp
	pop af
	ret
	
clear_zhn_flags:
	ld ixl,a
	ld a,i
	ld a,ixl
	ret
	
ophandlerF9:
	push hl
	 exx
	pop hl
	jp set_gb_stack
	
ophandlerRETcond:
	; Increment the cycle count by 1 before returning
	ex af,af'
	inc a
	ret nz
	inc iyh
	ret
	
ophandlerRETI:
	ex af,af'
	ld iyl,a
	ld a,$A6 ;AND (HL)
	ld (intstate_smc_1),a
	ld (intstate_smc_2),a
	ld a,dispatch_cycles_for_reti - (dispatch_cycles_for_reti_smc+1)
	ld (dispatch_cycles_for_reti_smc),a
	exx
	jp pop_and_lookup_code_cached
	
ophandlerRET:
	dec sp
	dec sp
	ld iyl,a
	exx
	jp pop_and_lookup_code_cached
	
pop_ports:
	inc b
	push hl
	pop ix
	inc hl
	push hl
	 inc hl
	 exx
	 dec iy
	 call mem_read_ports_swapped
	 ex (sp),ix
	 inc iy
	 ex af,af'
	 call mem_read_ports_swapped
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
	
	ld ixl,a
	ld a,iyl
	ex af,af'
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
mem_read_ports:
	ex af,af'
	ld iyl,a
mem_read_ports_swapped:
	ld a,ixh
	cp $FE
	jr c,mem_read_bail
	jr z,mem_read_oam
	;IX=GB address, reads into IXL, AF'=GB AF
mem_read_ports_always:
	ld a,ixl
	add a,a
	jr c,mem_read_oam
	jr z,readP1
	cp TIMA*2 & $FF
	jr z,readTIMA
	cp LY*2 & $FF
	jr z,readLY
	cp DIV*2 & $FF
	jr z,readDIV
	cp STAT*2 & $FF
	jr z,readSTAT
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
	jr nc,mem_read_any_ports
	ex de,hl
	add a,a
	jr c,++_
	add a,a
	jr c,_
	ld.lil ix,(rom_start)
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
	exx
	push.l hl
	 call get_mem_cycle_offset
	 ld hl,(div_counter)
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
	
mem_read_any_ports:
	jr z,_
	ld a,l
	add a,a
	jr c,_
	push hl
	pop ix
	call mem_read_ports_always
	ex af,af'
	ld a,ixl
	ret
_
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
	jr nc,mem_write_any_ports
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
	
mem_write_any_ports:
	push hl
	pop ix
	jr nz,mem_write_ports_swapped
	jr mem_write_oam_swapped
	
	; Inputs: IX = GB address
	;         IY = cycle counter,
	;         A = data to wrote
	; Outputs: A' = low cycle counter
	; Destroys: IX, F', C', DE'
mem_write_ports:
	ex af,af'
	ld iyl,a
	ld a,ixh
	inc a
	jr z,mem_write_ports_swapped
	inc a
	jr nz,mem_write_bail
mem_write_oam_swapped:
	ld a,iyl
	ex af,af'
	ld (ix),a
	ret

; Inputs: IX = GB address
;         IY = cycle counter,
;         A' = data to write
; Outputs: AF = input AF'
;          A' = low cycle counter
; Destroys: IX, F', C', DE'
mem_write_ports_swapped:
	ld a,ixl
	cp $7F
	jp pe,mem_write_oam_swapped
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
	 jr mbc_2000_finish
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
	 jr z,mbc_fix_sp
mbc_4000_denied:
	pop bc
	ld a,iyl
	ex af,af'
	ret
	
mbc_zero_page_override:
	; If the masked value is 0, increase the result (except MBC5)
	; When setting the high bits, this is ignored by the masking below
	inc a
	jr mbc_zero_page_continue
	
mbc_2000:
	push bc
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,c
mbc5_rom_bank_smc = $
rom_bank_mbc_mask_smc = $+1
	 ld b,$FF
mbc_2000_finish:
curr_rom_bank = $+1
	 ld c,1
	 ; Mask the new value and check if 0-page should be overridden
	 and b
	 jr z,mbc_zero_page_override
mbc_zero_page_continue:
	 ; Set only the given mask of the page
	 xor c
	 and b
	 xor c
mbc5_rom_bank_continue:
	 ; Adjust value to physical page based on ROM size
rom_bank_mask_smc = $+1
	 and 0
	 ld c,a
	 ld (curr_rom_bank),a
	 ld (rom_bank_check_smc_1),a
	 ld (rom_bank_check_smc_2),a
	 ld (rom_bank_check_smc_3),a
	 ld b,3
	 mlt bc
	 ld.lil ix,rombankLUT
	 add.l ix,bc
	 ld.l ix,(ix)
	 ld.lil (rom_bank_base),ix
	 ; See if SP is pointing into the swapped bank
	 ld a,(curr_gb_stack_bank)
	 cp 3 << 5
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
mbc_2000_denied:
	pop bc
	ld a,iyl
	ex af,af'
	ret
	
	
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
	ld hl,i
	ld h,CALL_STACK_ENTRY_SIZE
	mlt hl
	ld de,myz80stack - 2 - ((CALL_STACK_DEPTH + 1) * CALL_STACK_ENTRY_SIZE) - 2
	add hl,de
	ld ix,(hl)
	; Assume the JIT code was a routine call; get its target address
	ld hl,(ix-2)
	; Check if the target begins with a JP, RST 00h, or RST 10h instruction.
	; This should only be the case when the target is a cycle cache trampoline.
	; Note that to avoid false positives, no routine called directly from JIT
	; should start with JP nnnn; OUT (nn),A; RST 00h; or RST 10h.
	ld a,(hl)
	xor $C3
	and $EB
	jr nz,resolve_get_mem_cycle_offset_call
	; We probably have a trampoline target; however, we must check whether our
	; assumption that the JIT code was a routine call is accurate. If the first
	; byte of the JIT code is a NOP, then we actually have a bitwise prefix op.
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
	; Check if the call was made from a JIT address or not
	ld a,ixh
	cp jit_start >> 8
	jr c,resolve_mem_cycle_offset_special
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

resolve_mem_cycle_offset_special:
	; Check the opcode of the non-JIT target
	ld a,(ix)
	cp $C3
	jr nz,resolve_mem_cycle_offset_ret
	; This is a push related to an RST, interrupt, or CALL,
	; so retrieve the cycle info and actual target address,
	; and infer the Game Boy address
	ld ix,(ix+1)
	ld a,ixh
	cp jit_start >> 8
	jr nc,resolve_mem_cycle_offset_for_call
	ld hl,mem_cycle_scratch
	ld a,ixl
	sub (dispatch_rst_00 + 1) & $FF
	; If this carries, the target should be the flush handler.
	add a,a
	jr c,resolve_mem_cycle_offset_flush
	ld (hl),a
	inc hl
	ld (hl),0
	inc hl
	; Subtract 4 cycles for RST, or 5 for interrupt
	cp $40
	ld a,(ix-1)
	adc a,-5	; Sets carry flag
	ASSERT_C
	ld (hl),a
	ld ix,(ix+1)
	jr get_mem_cycle_offset_continue

resolve_mem_cycle_offset_ret:
	; The second read of an unconditional RET is always two cycles after
	; the end of a JIT sub-block. A conditional RET adds an extra cycle,
	; but this is already accounted by pre-incrementing IY.
	; Additionally, IX and HL returns are never used by reads.
	lea de,iy+2
	ret
	
resolve_mem_cycle_offset_flush:
	; The target should be the flush handler.
#ifdef DEBUG
	ld a,ixh
	cp flush_handler >> 8
	jr nz,$
	ld a,ixl
	cp flush_handler & $FF
	jr nz,$
#endif
	; Get the Game Boy address currently in the handler.
	ld de,(ix+2)
	ld (hl),de
	inc hl
	inc hl
	; Cycle count is always zero.
	ld (hl),0
	; Set the JIT address to a harmless location in case an event is scheduled.
	ld ix,event_value
	jr get_mem_cycle_offset_continue

resolve_mem_cycle_offset_for_call:
	; Get the end of the CALL instruction from the call stack
	ld hl,i
	ld h,CALL_STACK_ENTRY_SIZE
	inc l
	mlt hl
	add hl,de
	ld hl,(hl)
	; Get the Game Boy return address to use for extracting the CALL target
	dec hl
	dec hl
	ld de,(hl)
	; Get the cycle count
	dec hl
	dec hl
	dec hl
	ld a,(hl)
	jp.lil resolve_mem_cycle_offset_for_call_helper

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
	ld hl,(div_counter)
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
	 ld hl,(div_counter)
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
	
_writeSChandler:
	ex af,af'
	ld iyl,a
_writeSC:
	ex af,af'
	push af
	 push hl
	  or $7E
	  ld (SC),a
	  inc a
	  ld hl,disabled_counter_checker
	  jr nz,_
	  call trigger_event
	  ex af,af'
	  ; Set this cycle count after setting up the trigger
	  ld hl,(div_counter)
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
	ld a,(intstate_smc_1)
	rra
	jr c,checkIntDisabled
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
