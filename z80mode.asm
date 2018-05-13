z80code:
	.assume adl=0
	.org 0
r_bits:
	ex af,af'
	ex (sp),hl
	ld a,(hl)
	inc hl
	ex (sp),hl
	jp do_bits
	
	.block $08-$
r_mem:
	di
	pop ix
	push af
	 jp decode_mem
	
	.block $10-$
r_cycle_check:
	ex af,af'
	ld a,iyh
	or a
	ret nz
	jp cycle_overflow_for_jump
	
	.block $18-$
r_push:
	pop ix
	exx
	pop de
	jr do_push
	
	.block $20-$
r_pop:
	exx
	ld.l ix,(hl)
	ex (sp),ix
	jr do_pop
	
	.block $28-$
r_call:
	pop ix
	exx
	djnz do_call
	jr do_call_reset_callstack
	
	.block $30-$
r_event:
	ex af,af'
	pop ix
	dec ix
	jp do_event
	
	.block $38-$
r_error:
rst38h:
	push af
	 ld a,i
	 jp pe,Z80Error
	 ld a,pLcdMis >> 8
	 in a,(pLcdMis & $FF)
	 or a
	 jp.lil nz,frame_interrupt
	 ;ld a,pIntMaskedStatus >> 8
	 ;in a,(pIntMaskedStatus & $FF)
	 ;rra
	 ;jr nc,$
	
on_interrupt:
	 inc a
	 ld.lil (mpIntAcknowledge),a
	 inc a
	 ld.lil (exitReason),a
frame_interrupt_return:
	pop af
	ei
	ret
	 
Z80Error:
	di
	jp.lil Z80Error_helper
	
	
do_push_and_return:
	ex (sp),ix
do_push:
	dec.l hl
do_push_smc_1 = $+1
	ld.l (hl),d
	dec.l hl
do_push_smc_2 = $+1
	ld.l (hl),e
	exx
	jp (ix)
	
do_pop:
	inc.l hl
	inc.l hl
	exx
	jp (ix)
	
do_call_reset_callstack:
	ld b,CALL_STACK_DEPTH
	ld.lil sp,myADLstack
	ld sp,myz80stack-2
do_call:
	pea ix+6
	ex af,af'
	ld de,(ix+2)
	ld a,(ix+5)
	ld ix,(ix)
do_call_common:
	dec.l hl
do_push_smc_3 = $+1
	ld.l (hl),d
	dec.l hl
do_push_smc_4 = $+1
	ld.l (hl),e
	push.l hl
	call dispatch_cycles_exx
call_stack_ret:
	ex af,af'
	exx
	pop.l de
	or a
	sbc.l hl,de
	add.l hl,de
	jr nz,ophandlerRETskip
	pop ix
	inc b
	ld c,b	;C is now at least 2
	ld a,(ix-4)
	cpi.l
	jr nz,ophandlerRETnomatch_dec
	ld de,(ix-3)
	ld a,e
	cpi.l
	jr nz,ophandlerRETnomatch_dec2
	ld a,d
dispatch_cycles_exx:
	exx
dispatch_cycles:
	ld (_+2),a
_
	lea iy,iy+0
	ld a,iyh
	or a
	jr z,cycle_overflow
	ex af,af'
	jp (ix)
	
ophandlerRETskip:
	jr c,ophandlerRETsave
	pop de
	inc b
	exx
	ex af,af'
	ret
	
ophandlerRETsave:
	push.l de
	ld de,call_stack_ret
	push de
	jr ophandlerRETnomatch
	
ophandlerRETnomatch_dec2:
	dec.l hl
ophandlerRETnomatch_dec:
	dec.l hl
ophandlerRETnomatch:
	push bc
	 di
	 call.il pop_and_lookup_code_cached
	 ei
	pop bc
	jr dispatch_cycles_exx
	
do_rom_bank_call:
	ex af,af'
	pop ix
rom_bank_check_smc_1 = $+1
	ld a,0
	cp (ix)
	jr nz,banked_call_mismatch
	ld ix,(ix+1)
	exx
	ex (sp),ix
	ld a,(ix+3)
banked_call_mismatch_continue:
	ld de,(ix)
	lea ix,ix+4
	ex (sp),ix
	djnz do_call_common
	ld.lil sp,myADLstack
	pop bc
	ld sp,myz80stack-2
	push bc
	ld b,CALL_STACK_DEPTH
	jp do_call_common
	
banked_call_mismatch:
	di
	jp.lil banked_call_mismatch_helper
	
cycle_overflow_for_jump_alt:
	pea ix-1
cycle_overflow_for_jump:
	pop ix
	ld ix,(ix+2)
cycle_overflow:
cycle_overflow_flush_smc = $
	push ix
	push hl
	 push de
	  push bc
	   lea de,ix
	
schedule_event:
	   ld a,(event_value)
event_address = $+1
	   ld (event_value),a
	
	   di
	   jp.lil schedule_event_helper
	   
do_rom_bank_jump:
	ex af,af'
	pop ix
	ld a,(ix+4)
	ld (_+2),a
rom_bank_check_smc_2 = $+1
	ld a,0
	xor (ix+3)
	jr nz,banked_jump_mismatch
_
	lea iy,iy+0
	cp iyh
	jr z,cycle_overflow_for_jump_alt
	ex af,af'
	jp (ix)
	
banked_jump_mismatch:
	di
	jp.lil banked_jump_mismatch_helper
	
vblank_handler:
	   di
	   jp.lil vblank_helper
	
do_event:
event_value = $+3
	ld (ix),0
	push hl
do_event_pushed:
	 push de
	  push bc
	   ld hl,event_value
	   ld (event_address),hl
event_cycle_loop:
	   ld a,iyh
	   or a
	   jr nz,not_expired
	   ld a,iyl
event_cycle_count = $+1
	   add a,0
	   jr nc,not_expired
	   
	   ; Event expired
	   ld bc,(frame_cycle_target)
	   ld hl,CYCLES_PER_SCANLINE * 144 + 1
	   sbc hl,bc	; Carry is set
	   jr z,vblank_handler
vblank_handler_ret:

	   ld hl,CYCLES_PER_FRAME
	   add hl,bc
	   ex de,hl
	   
current_lyc_target_count = $+1
	   ld hl,CYCLES_PER_SCANLINE * 153 + 2
	   sbc hl,bc
	   call z,LYCmatch
	   
	   ld hl,STAT
	   ld a,(hl)
	   and $28
	   call nz,stat_mode_handler
	   
	   call update_cycle_target
	   ld hl,-CYCLES_PER_FRAME
	   add hl,de
	   jr c,_
	   ex de,hl
_
	   ld (frame_cycle_target),hl
	   ex de,hl
	   ld h,b
	   ld l,c
	   or a
	   sbc hl,de
	   jr c,_
	   ; Make sure carry gets set
	   ld de,CYCLES_PER_FRAME
	   sbc hl,de
_
	   ex de,hl
serial_enable_smc = $
	   jr nc,serial_cycle_handler
serial_cycle_continue:
	   add iy,de
div_cycle_count = $+1
	   ld hl,0
	   or a
	   sbc hl,de
	   ld (div_cycle_count),hl
	   ld a,(TAC)
	   and 4
	   jr z,event_cycle_loop
timer_cycles_reset_loop:
	   add hl,de
timer_cycle_target = $+1
	   ld bc,0
	   xor a
	   sbc hl,bc
	   jr z,timer_cycles_reset
	   or a
	   sbc hl,de
	   jr c,event_cycle_loop
	   ld (div_cycle_count),bc
	   ex de,hl
	   ld hl,(frame_cycle_target)
	   sbc hl,de
	   jr nc,_
	   ld bc,CYCLES_PER_FRAME
	   add hl,bc
_
	   ld (frame_cycle_target),hl
	   add iy,de
event_cycle_loop_shortcut:
	   jr event_cycle_loop
	   
not_expired:
	   ld hl,IE
	   ld a,(hl)
	   ld l,IF - ioregs
	   and (hl)
intstate = $+1
	   and $00
	   jr nz,trigger_interrupt
	  pop bc
	 pop de
	pop hl
	cp iyh
	jr z,event_reschedule
	ex af,af'
	jp (ix)
	
serial_cycle_handler:
serial_cycle_count = $+1
	   ld hl,0
	   add hl,de
	   ld (serial_cycle_count),hl
	   jr c,serial_cycle_continue
	   ex de,hl
	   sbc hl,de
	   jr z,serial_transmit_complete
	   ex de,hl
	   ld bc,(frame_cycle_target)
	   add hl,bc
	   jr c,_
	   ld bc,CYCLES_PER_FRAME
	   add hl,bc
_
	   ld (frame_cycle_target),hl
	   or a
	   sbc hl,hl
	   ld (serial_cycle_count),hl
serial_cycle_continue_shortcut:
	   jr serial_cycle_continue
	
timer_cycles_reset:
	   ld hl,IF
	   set 2,(hl)
	   ld l,TMA & $0F
	   sub (hl)
	   ld l,a
timer_cycles_reset_factor_smc = $+1
	   ld h,0
	   jr z,_
	   mlt hl
_
	   add hl,hl
	   or h
	   add hl,bc
	   ld (timer_cycle_target),hl
	   ld hl,(div_cycle_count)
	   jr nz,timer_cycles_reset_loop
	   jr event_cycle_loop_shortcut
	   
event_reschedule:
	push ix
	push hl
	 push de
	  push bc
	   lea de,ix
	   ld.lil ix,(event_gb_address)
	   ld a,(event_cycle_count)
	   di
	   jp.lil schedule_event_helper_post_lookup
	   
serial_transmit_complete:
	   ld hl,SC
	   res 7,(hl)
	   dec hl
	   ld (hl),h
	   ld l,IF & $FF
	   set 3,(hl)
	   ld a,$30	;JR NC
	   ld (serial_enable_smc),a
	   jr serial_cycle_continue_shortcut
	
trigger_interrupt:
	   rrca
	   jr c,dispatch_vblank
	   rrca
	   jr c,dispatch_stat
	   rrca
	   jr c,dispatch_timer
	   rrca
	   jr c,dispatch_serial
dispatch_joypad:
	   res 4,(hl)
	   ld c,$60
	   call decode_intcache
	   jr dispatch_int
dispatch_serial:
	   res 3,(hl)
	   ld c,$58
	   call decode_intcache
	   jr dispatch_int
dispatch_timer:
	   res 2,(hl)
	   ld c,$50
	   call decode_intcache
	   jr dispatch_int
dispatch_stat:
	   res 1,(hl)
	   ld c,$48
	   call decode_intcache
	   jr dispatch_int
dispatch_vblank:
	   res 0,(hl)
	   ld c,$40
	   call decode_intcache
dispatch_int:
	
	   push hl
	    push ix
	     di
	     call.il get_event_gb_address
	     ei
	    pop de
	 
	    ; If we're on a HALT, exit it
	    ld.l a,(ix)
	    xor $76
	    ld a,(event_cycle_count)
	    jr nz,_
	    inc.l ix
	    inc hl
	    inc de
	    inc de
	    inc de
	    inc a
_
	    push hl
int_return_sp = $+2
	     ld.lil hl,0
	     ld b,(hl)
	     djnz _
	     sbc hl,hl
	     ld.lil (int_cached_return),hl
	     ld.lil hl,z80codebase+int_return_stack
_
	     ld (hl),a
	     add a,c
	     ld.lil bc,(int_cached_return)
	     ld.lil (int_cached_return),ix
	     inc.l hl
	     ld.l (hl),bc
	     inc hl
	     inc hl
	     inc hl
	     ld (hl),de
	     inc hl
	     inc hl
	     ld (int_return_sp),hl
	    pop hl
	   pop ix
	  pop bc
	 pop de
	 ex (sp),hl
	 exx
	pop de
	call do_push_and_return
	ld (_+2),a
_
	lea iy,iy+0
	xor a
	ld (intstate),a
	cp iyh
	jr z,_
	ex af,af'
	ret
	
_
	pop ix
	jp cycle_overflow
	
decode_intcache:
	push ix
	 di
	 call.il decode_intcache_helper
	pop hl
	ex (sp),hl
	dec hl
	dec hl
	ld (hl),ix
	dec hl
	ld (hl),$21	;LD HL,addr
	dec hl
	ld (hl),a
	dec hl
	pop ix
	jp (hl)
	
LYCmatch:
	ld hl,LCDC
	bit 7,(hl)
	ret z
	inc hl
	bit 6,(hl)
	ret z
	ld l,IF - ioregs
	set 1,(hl)
	ret
	
	; Input: HL = STAT
	;        BC = current cycle count
	;        DE = BC + CYCLES_PER_FRAME
	;        A = bits 3 and 5 of (STAT)
	;        Carry is reset
	; Output: DE = cycle target upper bound
	;         Carry is reset
stat_mode_handler:
	dec hl
	bit 7,(hl)
	ret z
	
	ld l,a
	push hl
	 ld h,b
	 ld l,c
	 call get_scanline_from_cycle_count
	pop hl
	ld d,a
	ld a,e
	cp 144
	jr nc,stat_mode_1
	
	ld a,d
	or a
	jr nz,_
	set 6,l
_
	cp MODE_2_CYCLES + MODE_3_CYCLES
	jr nz,_
	set 4,l
_
	inc e
	ld d,CYCLES_PER_SCANLINE
	mlt de
	ld a,e
	jr c,_
	bit 5,l
	jr nz,+++_
	add a,MODE_2_CYCLES + MODE_3_CYCLES
	jr nc,++_
	inc d
	jr ++_
_
	bit 3,l
	jr z,++_
	sub MODE_0_CYCLES
	jr nc,_
	dec d
_
	ld e,a
_
	ld a,l
	rlca
	and l
	ret z
	ld l,IF - ioregs
	set 1,(hl)
	ret
	
stat_mode_1:
	ld de,CYCLES_PER_FRAME
	bit 5,l
	ret nz
	ld e,(CYCLES_PER_FRAME + MODE_2_CYCLES + MODE_3_CYCLES) & $FF
	ret
	
decode_mem:
	 ld a,(memroutine_next)
	 sub ixl
	 ld a,(memroutine_next+1)
	 sbc a,ixh
	 jr nc,_
	 ld a,(ix)
	pop ix
	pop ix
	lea ix,ix-2
	ld (ix),a
	ex af,af'
	push af
_
	 push hl
	  push de
	   call.il decode_mem_helper
	   ld (ix+1),de
	   ld (ix),$CD
	  pop de
	 pop hl
	pop af
	jp (ix)
	
decode_jump:
	ex af,af'
	exx
	push.l hl
	pop hl
	push bc
	 ld ix,(hl)
	 inc hl
	 inc hl
	 ld de,(hl)
	 inc hl
	 inc hl
	 ld a,(hl)
	 push hl
	  di
	  jp.lil decode_jump_helper
decode_jump_return:
	 pop hl
	 add a,(hl)	;calc cycle count
	 dec hl
	 ld (hl),ix
	 dec hl
	 ld (hl),$C3	;JP
	 dec hl
	 ld (hl),$08	;EX AF,AF'
	 dec hl
	 ld (hl),RST_CYCLE_CHECK
	 dec hl
	 ld (hl),a
	 dec hl
	 ld (hl),$33
	 dec hl
	 ld (hl),$ED	;LEA IY,IY+offset
decode_jump_waitloop_return:
	pop bc
	push hl
	pop.l hl
	exx
	ex af,af'
	ret
	
decode_call:
	ex af,af'
	ex (sp),hl
	push bc
	 push de
	  ld de,(hl)
	  inc hl
	  inc hl
	  push hl
	   di
	   call.il decode_call_helper
_
	  pop hl
	  sub (hl)
	  inc hl
	  ld (hl),a
	  ld de,-5
	  add hl,de
	  ld (hl),ix
	  dec hl
	  ld (hl),b
	 pop de
	pop bc
	ex (sp),hl
	ex af,af'
	ret
	
decode_rst:
	ex af,af'
	ex (sp),hl
	push bc
	 push de
	  ld de,(hl)
	  inc hl
	  inc hl
	  push hl
	   di
	   call.il decode_rst_helper
	   jr -_
	   
decode_ret_cond:
	ex af,af'
	ex (sp),hl
	push bc
	 push de
	  di
	  call.il decode_ret_cond_helper
	  ld (hl),$C9
	  dec hl
	  ld (hl),a
	  dec hl
	  ld (hl),$33
	  dec hl
	  ld (hl),$ED	;LEA IY,IY+d
	 pop de
	pop bc
	ex (sp),hl
	ex af,af'
	ret
	
wait_for_interrupt_stub:
	ei
	halt
	ret.l
	
flush_handler:
	exx
flush_address = $+1
	ld de,0
	di
	jp.lil flush_normal
	
flush_mem_handler:
	exx
	pop de
	di
	jp.lil flush_mem
	
coherency_handler:
	ex af,af'
	ex (sp),hl
	ld ix,(hl)
	inc hl
	inc hl
	ex (sp),hl
	push hl
	 push de
	  push bc
	   ld.lil de,recompile_struct
	   add.l ix,de
	   ld.l de,(ix+2)
	   ld.l bc,(ix+5)
	   ld.l hl,(ix+8)
	   sbc hl,bc
check_coherency_loop:
	   ld.l a,(de)
	   inc.l de
	   cpi
	   jr nz,check_coherency_failed
	   jp pe,check_coherency_loop
coherency_return:
	   ld.l a,(ix+7)
	   ld (_+2),a
_
	   lea iy,iy+0
	   ld a,iyh
	   or a
	   jr z,check_coherency_cycle_overflow
	  pop bc
	 pop de
	pop hl
	ex af,af'
	ret
	
check_coherency_cycle_overflow:
	   ld hl,(event_address)
	   ld a,(event_value)
	   ld (hl),a
	   di
	   jp.lil coherency_cycle_overflow
	
check_coherency_failed:
	   di
	   jp.lil rerecompile
	   
do_swap:
	inc a
	jr nz,do_swap_generic
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
	ex af,af'
	push af
	 inc iy	;Consume 1 extra cycle
	 call mem_read_any
	 rrca
	 rrca
	 rrca
	 rrca
	 or a
	 inc iy	;Consume 1 more cycle
	 call mem_write_any
	pop ix
	ld a,ixh
	ret
	
do_bits:
	sub $30
	sub 8
	jr c,do_swap
	add a,$38-1	;Use L instead of (HL)
	cp $C0
	inc iy	;Consume 1 extra cycle
	jp pe,do_bits_readonly
	ld (do_bits_smc),a
	call mem_read_any
	; Use L because we have to affect flags, bleh
	push hl
	 ld l,a
	 ex af,af'
	 ld h,a
do_bits_smc = $+1
	 rlc l
	 ld a,l
	 ex (sp),hl
	 inc iy	;Consume 1 more cycle
	 call mem_write_any
	pop ix
	ld a,ixh
	ret
do_bits_readonly:
	ld (do_bits_readonly_smc),a
	call mem_read_any
	; Use L because we have to affect flags, bleh
	push hl
	 ld l,a
	 ex af,af'
do_bits_readonly_smc = $+1
	 bit 0,l
	pop hl
	ret
	
ophandler08:
	pop ix
	pea ix+2
	push af
	 push de
	  exx
	  push hl
	   exx
	   ex (sp),hl
	   ld de,(sp_base_address)
	   or a
	   sbc hl,de
	   ex de,hl
	   ld hl,(ix)
	   ld a,e
	   dec iy
	   call mem_write_any
	   inc hl
	   ld a,d
	   inc iy
	   call mem_write_any
	  pop hl
	 pop de
	pop af
	ret
	
ophandler27:
	push af
	 ex (sp),hl
	 daa
	 bit 1,l
	 jr nz,_
	 ; If N was 0, behavior is same as Game Boy
	pop hl
	; Reset H and N flags, preserve C flag, set Z flag properly
	rla
	rr a
	ret
	
	; Emulate N=1 case manually
_
	 ld a,h
	 bit 4,l
	 jr z,_
	 sub $06
_
	 srl l
	 jr c,++_
	 sub $00
_
	pop hl
	ret
_
	 sub $60
	 jr c,--_
	 ; Set C flag and don't touch other flags
	 push af
	 pop hl
	 inc l
	 ex (sp),hl
	pop af
	ret
	
ophandler31:
	pop ix
	exx
	ld hl,(ix)
	lea ix,ix+2
	di
	jp.lil set_gb_stack
	
ophandler34:
	ex af,af'
	dec iy
	call mem_read_any
	ld ixl,a
	ex af,af'
	inc ixl
	jr _
	
ophandler35:
	ex af,af'
	dec iy
	call mem_read_any
	ld ixl,a
	ex af,af'
	dec ixl
_
	inc iy
	push af
	 call mem_write_any_ixl
	pop af
	ret
	
ophandler36:
	pop ix
	pea ix+1
	push af
	 ld a,(ix)
	 call mem_write_any
	pop af
	ret
	
ophandler39:
	ex af,af'
	push de
	 exx
	 push hl
	  exx
	  ex (sp),hl
	  ld de,(sp_base_address)
	  or a
	  sbc hl,de
	  ex de,hl
	 pop hl
	 ex af,af'
	 add hl,de
	pop de
	ret
	
handle_waitloop_stat:
	jr handle_waitloop_stat
	
handle_waitloop_variable:
	ex af,af'
	push hl
handle_waitloop_main:
	 ; Skip straight to the counter expiration
	 xor a
	 ld iyh,a
	 sub (ix+2)
	 ld iyl,a
handle_waitloop_noskip:
	 ; Wipe the previous event trigger
	 ld hl,(event_address)
	 ld a,(event_value)
	 ld (hl),a
	 ; Run an event using our precomputed lookup
	 ex de,hl
	 ld.lil de,z80codebase
	 add.l ix,de
	 ex de,hl
	 ld.l hl,(ix+4)
	 ld.lil (event_gb_address),hl
	 ld a,(ix+2)
	 ld (event_cycle_count),a
	 ld ix,(ix)
	 jp do_event_pushed
	
handle_waitloop_ly:
	ex af,af'
	push hl
	 push de
	  ; Get the (negative) number of cycles until the next event
	  ld a,(ix+2)
	  call get_cycle_offset
	  push de
	   ; Get the (negative) number of cycles until the next scanline
	   call get_scanline_from_cycle_offset
	   ld d,a
	   ld a,e
	   cp 153
	   ld a,d
	   jr nz,_
	   sub 1
	   jr c,++_
	   sub CYCLES_PER_SCANLINE - 1
_
	   sub CYCLES_PER_SCANLINE
_
	  pop de
	  ; Choose the smaller absolute value
	  inc d
	  jr nz,_
	  cp e
	  jr nc,_
	  ld a,e
_
	  ld e,a
	  ; Step by a multiple of the loop length
	  ld d,(ix+3)
	  ; Always advance at least one iteration
	  add a,d
	  jr c,++_
	  ; Advance as many iterations as possible without exceeding the cycle count
_
	  add a,d
	  jr nc,-_
	  sub d
_
	  sub e
	 pop de
	 ; Add in the cycles and check for overflow
	 add a,iyl
	 ld iyl,a
	 jr c,_
	 dec iyh
_
	 inc iyh
	 jr z,_
	 ld ix,(ix)
	pop hl
	ex af,af'
	jp (ix)
_
	 ; If the count has already expired, do an event immediately
	 add a,(ix+2)
	 jr c,handle_waitloop_noskip
	 
	 ; Wipe the previous event trigger
	 ld hl,(event_address)
	 ld a,(event_value)
	 ld (hl),a
	 ; Schedule an event using our precomputed lookup
	 ld hl,(ix)
	ex (sp),hl
	push hl
	 push de
	  push bc
	   ld.lil de,z80codebase
	   add.l ix,de
	   ld de,(ix)
	   ld a,(ix+2)
	   ld.l ix,(ix+4)
	   di
	   jp.lil schedule_event_helper_post_lookup
	
ophandler76:
	ex af,af'
	push hl
	 ld hl,IF
	 ld a,(hl)
	 ld l,h
	 and (hl)
	pop hl
	jr nz,haltdone
	ex af,af'
	ex (sp),hl
	dec hl
	dec hl
	dec hl
	ex (sp),hl
	
trigger_event_startup:
	ex af,af'
trigger_event_fast_forward:
	scf
trigger_event:
	push hl
trigger_event_pushed:
	 push de
	  push bc
	   push af
	    ld hl,(event_address)
	    ld a,(event_value)
	    ld (hl),a
	   
	    ; Get the address of the recompiled code: the bottom stack entry
	    ld hl,myz80stack - 2 - ((CALL_STACK_DEPTH + 1) * 4) - 2
	    exx
	    ld a,b
	    exx
	    ld d,a
	    ld e,4
	    mlt de
	    add hl,de
	    ld de,(hl)
	
	    di
	    call.il lookup_gb_code_address
	    ei
	    ld c,a
	   pop af
	   push de
	    jr c,event_fast_forward
	    ld a,c
	    call get_cycle_offset
	    ; If the end of this instruction is already past the target, don't rewind
	    ld a,d
	    or a
	    jr z,trigger_event_already_triggered
	    ld hl,(serial_cycle_count)
	    sbc hl,de
	    ld (serial_cycle_count),hl
	    ld hl,(div_cycle_count)
	    add hl,de
	    ld (div_cycle_count),hl
	    ld hl,(frame_cycle_target)
	    add hl,de
	    jr c,_
	    ld de,CYCLES_PER_FRAME
	    add hl,de
_
	    ld (frame_cycle_target),hl
event_fast_forward:
	    xor a
	    ld iyh,a
	    sub c
	    ld iyl,a
trigger_event_already_triggered:
	   pop hl
	   ld a,c
	   
schedule_event_enable:
	   ld.lil (event_gb_address),ix
	   ld (event_cycle_count),a
	   ld a,(hl)
	   ld (event_value),a
	   ld (hl),RST_EVENT
schedule_event_disable:
	   ld (event_address),hl
	  pop bc
	 pop de
	pop hl
	ex af,af'
	ret
	
haltdone:
	ex af,af'
	ret
	
ophandlerE2:
	ld ixh,$FF
	ld ixl,c
	jp mem_write_ports_always
	
ophandlerE8:
	exx
	ld c,a
	pop ix
	ld a,(ix)
	inc ix
	ld de,(sp_base_address)
	or a
	sbc hl,de
	ld e,a
	rla
	sbc a,a
	ld d,a
	ld a,l
	add hl,de
	add a,e
	call z,reset_z_flag_only
	ld a,c
	di
	jp.lil set_gb_stack
	
ophandlerE9:
	ex af,af'
	ex de,hl
	push bc
	 push de
	  di
	  call.il lookup_code_cached
	  ei
	 pop de
	pop bc
	ex de,hl
	jp dispatch_cycles
	
ophandlerF1:
	exx
	ld.l e,(hl)
	inc.l hl
	ld d,flags_lut >> 8
	res 3,e
	ld a,(de)
	ld e,a
	ld.l d,(hl)
	inc.l hl
	push de
	pop af
	exx
	ret
	
ophandlerF2:
	ld ixh,$FF
	ld ixl,c
	ex af,af'
	call mem_read_ports_always
	ld a,ixl
	ret
	
ophandlerF3:
	ex af,af'
	xor a
	ld (intstate),a
	ex af,af'
	ret
	
ophandlerF5:
	exx
	ld c,a
	dec.l hl
do_push_smc_5 = $+1
	ld.l (hl),a
	push af
	pop de
	ld d,flags_lut >> 8
	set 3,e
	ld a,(de)
	dec.l hl
do_push_smc_6 = $+1
	ld.l (hl),a
	ld a,c
	exx
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
	 ld de,(sp_base_address)
	 or a
	 sbc hl,de
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
	
ophandlerF9:
	pop ix
	push hl
	 exx
	pop hl
	di
	jp.lil set_gb_stack
	
ophandlerEI:
	ex af,af'
	ld a,$1F
	ld (intstate),a
	jp checkIntPostEnable
	
ophandlerRETI:
	ex af,af'
	ld a,$1F
	ld (intstate),a
	exx
	push bc
	 di
	 call.il pop_and_lookup_code_cached
	 ei
	pop bc
	exx
	push ix
	 push af
	  call checkIntPostEnable
	  ex af,af'
	 pop af
	pop ix
	jp dispatch_cycles
	
ophandlerRET:
	di
	dec sp
	dec sp
	ex af,af'
	exx
	push bc
	 call.il pop_and_lookup_code_cached
	 ei
	pop bc
	jp dispatch_cycles_exx
	
write_vram_handler:
	pop ix
	pea ix+2
	ld ix,(ix)
	jp mem_write_vram_always
	
write_cart_handler:
	pop ix
	pea ix+2
	ld ix,(ix)
	jp mem_write_cart_always
	
write_cram_bank_handler:
	pop ix
	pea ix+2
	exx
	ld de,(ix)
	ld.lil ix,(cram_bank_base)
	ex af,af'
write_cram_bank_handler_smc_1 = $+2
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
read_cram_bank_handler_smc = $+2
	add.l ix,de
	ex af,af'
	ld.l a,(ix)
	exx
	ret
	
readP1handler:
	ex af,af'
	call readP1
	ld a,ixl
	ret
	
readDIVhandler:
	ex af,af'
	call readDIV
	ld a,ixl
	ret
	
readTIMAhandler:
	ex af,af'
	call readTIMA
	ld a,ixl
	ret
	
readLYhandler:
	ex af,af'
	call readLY
	ld a,ixl
	ret
	
readSTAThandler:
	ex af,af'
	call readSTAT
	ld a,ixl
	ret
	
writeSCYhandler:
	ld ix,SCY
	jp write_scroll_swap
	
writeSCXhandler:
	ld ix,SCX
	jp write_scroll_swap
	
writeWYhandler:
	ld ix,WY
	jp write_scroll_swap
	
writeWXhandler:
	ld ix,WX
	jp write_scroll_swap
	
writeDMAhandler:
	ex af,af'
	jp writeDMA
	
writeBGPhandler:
	ld ix,BGP
	jp write_scroll_swap
	
writeIFhandler:
	ld (IF),a
	jp writeINTswap
	
writeIEhandler:
	ld (IE),a
	jp writeINTswap
	
readP1:
	ld a,(P1)
	or $CF
	ld ix,(keys)
	bit 4,a
	jr nz,_
	and ixl 
_
	bit 5,a
	jr nz,_
	and ixh
_
	ld ixl,a
	ex af,af'
	ret
	
readSTAT:
	exx
	push.l hl
	 call get_read_cycle_offset
	 call get_scanline_from_cycle_offset
	 ld d,a
	pop.l hl
	ld a,(STAT)
	and $F8
	ld c,a
	ld a,(LCDC)
	add a,a
	jr nc,readSTAT_mode0
	ld a,(LYC)
	cp e
	jr nz,_
	set 2,c
_
	ld a,e
	cp 144
	jr nc,readSTAT_mode1
	ld a,d
	cp MODE_2_CYCLES + MODE_3_CYCLES
	jr nc,readSTAT_mode0
	set 1,c
	cp MODE_2_CYCLES
	jr c,readSTAT_mode0
readSTAT_mode1:
	inc c
readSTAT_mode0:
	ld ixl,c
	exx
	ex af,af'
	ret
	
readLY:
	ld a,(LCDC)
	add a,a
	jr nc,readLY_force0
	exx
	push.l hl
	 call get_read_cycle_offset
	 call get_scanline_from_cycle_offset
	pop.l hl
	or a
	ld a,e
	exx
	jr z,_
	cp 153
	jr z,readLY_force0
_
	ld ixl,a
	ex af,af'
	ret
	
readLY_force0:
	xor a
	jr -_
	
	;IX=GB address, reads into IXL
mem_read_ports:
	ex af,af'
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
	ex af,af'
	ret
	
readTIMA:
	call updateTIMA
	 ld ixl,a
	pop.l hl
	exx
	ex af,af'
	ret
	
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
	ret
_
	add a,a
	jr c,_
	ld.lil ix,vram_base
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
	
mem_read_bail:
	pop ix
mem_write_bail_a:
	lea ix,ix-8
	jp (ix)
	
_
	ld.lil ix,(cram_bank_base)
mem_read_any_rtc_smc = $+2
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
	
readDIV:
	call updateTIMA
	 ex de,hl
	 add hl,hl
	 add hl,hl
	 ex de,hl
	 ld ixl,d
	pop.l hl
	exx
	ex af,af'
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
	
mem_write_any_cart:
	push hl
	pop ix
	jp mem_write_cart_swap
	
	;HL=GB address, IXL=data, destroys A,AF'
mem_write_any_ixl:
	ld a,ixl
	;HL=GB address, A=data, preserves AF, destroys AF'
mem_write_any:
	ex af,af'
	ld a,h
	cp $FE
	jr nc,mem_write_any_ports
	add a,a
	jr nc,mem_write_any_cart
	sub $40
	jr c,mem_write_any_vram
	ex de,hl
	sub $40
	jr c,mem_write_any_cram
	ld.lil ix,wram_base
	add.l ix,de
	ex de,hl
	ex af,af'
	ld.l (ix),a
	ret
mem_write_any_cram:
	ld.lil ix,(cram_bank_base)
mem_write_any_cram_smc_1 = $+2
	add.l ix,de
	ex de,hl
	ex af,af'
mem_write_any_cram_smc_2 = $+3
	ld.l (ix),a
	ret
	
mem_write_bail:
	pop ix
	ld a,(ix-8)
	cp RST_MEM
	jr z,mem_write_bail_a
	lea ix,ix-10
	pop af
	ex af,af'
	jp (ix)
	
writeLCDChandler:
	ex af,af'
writeLCDC:
	call get_scanline_from_write
	ex de,hl
	call get_last_cycle_offset
	jp.lil lcdc_write_helper
	
writeTAChandler:
	ex af,af'
writeTAC:
	call updateTIMA
	di
	jp.lil tac_write_helper
	
writeTIMAhandler:
	ex af,af'
writeTIMA:
	call updateTIMA
	ex af,af'
	ld (TIMA),a
	ex af,af'
_
	di
	jp.lil tima_write_helper
	
writeDIVhandler:
	ex af,af'
writeDIV:
	call updateTIMA
	or a
	sbc hl,hl
	ld (div_cycle_count),hl
	ex de,hl
	jr -_
	
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_vram:
	ex af,af'
	ld a,ixh
	sub $20
	jp po,mem_write_bail
mem_write_vram_swap:
	ex af,af'
	;IX=GB address, A=data
mem_write_vram_always:
	di
	jp.lil write_vram_and_expand
	
mem_write_any_ports:
	push hl
	pop ix
	jr nz,mem_write_ports_swap
	jr mem_write_oam_swap
mem_write_any_vram:
	push hl
	pop ix
	jr mem_write_vram_swap
	
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_ports:
	ex af,af'
	ld a,ixh
	inc a
	jr z,mem_write_ports_swap
	inc a
	jr nz,mem_write_bail
mem_write_oam_swap:
	ex af,af'
	ld (ix),a
	ret
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_ports_always:
	ex af,af'
mem_write_ports_swap:
	ld a,ixl
	inc a
	jp m,mem_write_oam_swap
	jr z,writeINT
	sub (DIV & $FF) + 1
	jr z,writeDIV
	dec a
	jr z,writeTIMA
	sub TAC - TIMA
	jr z,writeTAC
	sub IF - TAC
	jr z,writeINT
	sub LCDC - IF
	jr z,writeLCDC
	dec a
	jr z,writeSTAT
	dec a
	cp 2
	jr c,write_scroll
	sub WY - SCY
	cp 2
	jr c,write_scroll
	sub SC - WY
	jr z,writeSC
	sub LYC - SC
	jr z,writeLYC
	sub BGP - LYC
	jr z,write_scroll
	jr nc,mem_write_oam_swap
writeDMA:
	di
	jp.lil oam_transfer_helper
	
write_scroll_swap:
	ex af,af'
write_scroll:
	push ix
	 call get_scanline_from_write
	pop hl
	jp.lil scroll_write_helper
	
writeLYChandler:
	ex af,af'
writeLYC:
	call get_scanline_from_write
	jp.lil lyc_write_helper
	
writeSTAT:
	ex af,af'
writeSTAThandler:
	ld (STAT),a
	ex af,af'
	or a
	jp trigger_event
	
writeSC:
	ex af,af'
writeSChandler:
	push hl
	 ld hl,SC
	 ld (hl),a
	 ex af,af'
	 ld a,(hl)
	 cpl
	 and $81
	 ld a,$30	;JR NC
	 jr nz,_
	 call trigger_event
	 ex af,af'
	 ; Set this cycle count after setting up the trigger
	 ld hl,1024
	 ld (serial_cycle_count),hl
	 ld a,$38	;JR C
_
	 ld (serial_enable_smc),a
	pop hl
	ex af,af'
	ret
	
writeINT:
	ex af,af'
	ld (ix),a
writeINTswap:
	ex af,af'
checkIntPostUpdate:
	ld a,(intstate)
	or a
	jr z,checkIntDisabled
checkIntPostEnable:
	push hl
	 ld hl,IF
	 ld a,(hl)
	 ld l,h
	 and (hl)
	 jp nz,trigger_event_pushed
	pop hl
checkIntDisabled:
	ex af,af'
	ret
	
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_cart:
	ex af,af'
	ld a,ixh
	rla
	jp c,mem_write_bail
mem_write_cart_swap:
	ex af,af'
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_cart_always:
	ex af,af'
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
	ex af,af'
	ret
	
_
	di
	jp.lil mbc_rtc_latch_helper
	
mbc_4000:
	push bc
	 ld b,$60
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,(mbc_z80)
	 dec a
	 jr nz,_
mbc1_ram_smc:
	 jr z,mbc_ram
	 ld a,c
	 rrca
	 rrca
	 rrca
	 ld c,a
	 jr mbc_2000_finish
_
	 dec a
	 dec a
	 jr z,mbc_ram
	 dec a
	 jr nz,mbc_4000_denied
	 di
	 jp.lil mbc_rtc_helper
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
	 exx
	 ld de,(sp_base_address)
	 ld a,l
	 sub e
	 ld e,a
	 ld a,h
	 sbc a,d
	 ld d,a
	 sub $A0
	 cp $20
	 jr c,mbc_fix_sp
	 exx
mbc_4000_denied:
	pop bc
	ex af,af'
	ret
	
mbc_2000:
	push bc
	 ex af,af'
	 ld c,a
	 ex af,af'
mbc_z80 = $+1
	 ld b,1
	 djnz _
	 ld b,$1F
	 jr mbc_2000_finish
_
	 djnz _
	 ld b,$0F
	 jr mbc_2000_finish
_
	 ld b,$7F
mbc_2000_finish:
	 ld a,c
curr_rom_bank = $+1
	 ld c,1
	 xor c
	 and b
	 xor c
	 ld c,a
	 ld (curr_rom_bank),a
	 ld (rom_bank_check_smc_1),a
	 ld (rom_bank_check_smc_2),a
	 ld b,3
	 mlt bc
	 ld.lil ix,rombankLUT
	 add.l ix,bc
	 ld.l ix,(ix)
	 ld.lil (rom_bank_base),ix
	 ; See if SP is pointing into the swapped bank
	 exx
	 ld de,(sp_base_address)
	 ld a,l
	 sub e
	 ld e,a
	 ld a,h
	 sbc a,d
	 cp $C0
	 jp po,mbc_no_fix_sp
	 ; If so, update it
	 ld d,a
mbc_fix_sp:
	 lea.l hl,ix
	 ld.lil (z80codebase+sp_base_address),hl
	 add.l hl,de
mbc_no_fix_sp:
	 exx
mbc_2000_denied:
	pop bc
	ex af,af'
	ret
	
get_read_cycle_offset:
	ld ix,read_cycle_LUT
; Inputs: IY = current block cycle base
;         IX = pointer to cache LUT
;         B = number of empty call stack entries
;         (bottom of stack) = JIT return address
;         AFBCDEHL' have been swapped
; Outputs: DE = (negative) cycle offset
; Destroys HL,IX
get_mem_cycle_offset:
	push bc
	 ; Get the address of the recompiled code: the bottom stack entry
	 ld hl,myz80stack - 2 - ((CALL_STACK_DEPTH + 1) * 4) - 2
	 ld c,4
	 mlt bc
	 add hl,bc
	 ld bc,(hl)
	 
	 lea hl,ix+MEM_CYCLE_LUT_SIZE-2
	 ld a,-MEM_CYCLE_LUT_SIZE
mem_cycle_lookup_loop:
	 ld de,(hl)
	 dec hl
	 ex de,hl
	 sbc hl,bc
	 jr z,mem_cycle_lookup_found
	 ex de,hl
	 dec hl
	 dec hl
	 add a,3
	 jr nc,mem_cycle_lookup_loop
	 ld d,b
	 ld e,c
	 push ix
	  di
	  call.il lookup_gb_code_address
	  ei
	  ex de,hl
	  ex (sp),hl
	  ex de,hl
	  ld hl,MEM_CYCLE_LUT_SIZE
	  add hl,de
	  ld c,MEM_CYCLE_LUT_SIZE-3
	  jr mem_cycle_lookup_shift
mem_cycle_lookup_found:
	 lea hl,ix+MEM_CYCLE_LUT_SIZE
	 add a,MEM_CYCLE_LUT_SIZE
	 jr z,mem_cycle_lookup_found_fast
	 cp (hl)
	 jr nz,mem_cycle_lookup_found_fast
	 push bc
	  ld c,a
	  ld a,(de)
mem_cycle_lookup_shift:
	  ld (hl),h
	  ld hl,3
	  ld b,h
	  add hl,de
	  ldir
	  ld (de),a
	 pop bc
	 dec hl
	 ld (hl),b
	 dec hl
	 ld a,c
mem_cycle_lookup_found_fast:
	 ld (hl),a
	 ld a,(de)
	pop bc
	
; Inputs: IY = (negative) cycles until target
;         A = (negative) number of cycles to subtract
; Outputs: DE = (negative) cycle offset
;          May be positive if target lies within an instruction
get_cycle_offset:
	ld (get_cycle_offset_smc),a
get_last_cycle_offset:
get_cycle_offset_smc = $+2
	lea de,iy+0
	ret
	
; Inputs: BC = current cycle count
;         DE = upper bound of cycle target
;         Carry is reset
; Outputs: DE = new cycle target (possibly offset by CYCLES_PER_FRAME)
update_cycle_target:
	ld hl,CYCLES_PER_SCANLINE * 144
	ASSERT_NC
	sbc hl,bc
	jr z,_
	add hl,bc
	jr nc,++_
_
	ld hl,CYCLES_PER_SCANLINE * 144 + CYCLES_PER_FRAME
	or a
_
	sbc hl,de
	jr nc,_
	add hl,de
	ex de,hl
_
	
	ld a,(STAT)
	and $40
	ret z
	
	ld hl,(current_lyc_target_count)
	sbc hl,bc
	jr z,_
	add hl,bc
	jr nc,++_
_
	ld a,l
	add a,CYCLES_PER_FRAME & $FF
	ld l,a
	ld a,h
	adc a,CYCLES_PER_FRAME >> 8
	ld h,a
_
	ASSERT_NC
	sbc hl,de
	ret nc
	add hl,de
	ex de,hl
	ret
	
get_scanline_from_write:
	exx
	push.l hl
	ld ix,write_cycle_LUT
	call get_mem_cycle_offset
	di
	
; Inputs: DE = (negative) cycles until target
;         May be positive if target falls within an instruction
; Outputs: A = cycle count within scanline
;          E = scanline index (0-153)
; Destroys: D, HL
get_scanline_from_cycle_offset:
frame_cycle_target = $+1
	ld hl,0
	add hl,de
	jr c,get_scanline_from_cycle_count
	inc d
	dec d
	ld de,CYCLES_PER_FRAME
	jr nz,_
	sbc hl,de	; Carry is reset
	jr nc,get_scanline_from_cycle_count
_
	add hl,de
	
; Inputs: HL = cycle count within frame
; Outputs: A = cycle count within scanline
;          E = scanline index (0-153)
; Destroys: D, HL
get_scanline_from_cycle_count:
scanline_cycle_count_cache = $+1
	ld de,0
	xor a
	sbc hl,de
	cp h
	jr nz,++_
	ld a,l
scanline_index_cache = $+1
	ld de,CYCLES_PER_SCANLINE * 256 + 0
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
	ld de,-(CYCLES_PER_SCANLINE * 128)
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	ld a,h
	ld h,CYCLES_PER_SCANLINE
	jr get_scanline_from_cycle_count_finish
	
; Output: BCDEHL' are swapped
;         (SPL) = saved HL'
;         DE = current DIV counter
;         A = current TIMA value
;         (TIMA) updated to current value
updateTIMA:
	exx
	push.l hl
	 call get_read_cycle_offset
	 ld hl,(div_cycle_count)
	 add hl,de
	 ex de,hl
	 ld a,(TAC)
	 and 4
	 ld a,(TIMA)
	 ret z
	 ld hl,(timer_cycle_target)
	 sbc hl,de
updateTIMA_smc = $+1
	 jr $+8
	 add hl,hl
	 add hl,hl
	 add hl,hl
	 add hl,hl
	 add hl,hl
	 add hl,hl
	 xor a
	 sub l
	 sbc a,a
	 sub h
	 ld (TIMA),a
	 ret
	
keys:
	.dw $FFFF
	
sp_base_address:
	.dl 0
memroutine_next:
	.dl 0
render_save_sps:
	.dw 0
	
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
	
	.assume adl=1
z80codesize = $-0
	.org z80code+z80codesize
	
	.echo "Z80 mode code size: ", z80codesize