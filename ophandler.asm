	.assume adl=0

handle_overlapped_op_1_1:
	ex (sp),ix
	lea ix,ix+4
	jp.lil handle_overlapped_op_1_1_helper

handle_overlapped_op_1_2:
	ex (sp),ix
	lea ix,ix+5
	jp.lil handle_overlapped_op_1_2_helper

handle_overlapped_op_2_1:
	ex (sp),ix
	lea ix,ix+5
	jp.lil handle_overlapped_op_2_1_helper

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


ophandlerDAA:
	; Save input A value
	ld l,a
	; Split execution path on input carry, to extract both H and N
	jr nc,ophandlerDAA_no_carry
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
	jr c,ophandlerDAA_add
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

ophandlerDAA_no_carry:
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
	jr c,ophandlerDAA_sub_no_carry
ophandlerDAA_add:
	; N=0, C and H were restored
	daa
	; Reset H and N, preserve C and Z
	rla
	rra
	ret

ophandlerDAA_sub_no_carry:
	; Case for N=1, C=0
	jr nz,_
	; Subtract the adjustment for H=1
	sub 6
_
	; Set N, reset H and C, update Z
	sub 0
	ret


ophandlerDI:
	ld hl,intstate_smc_1
	ld (hl),$08 ;EX AF,AF'
	ld hl,intstate_smc_2
	ld (hl),0
	; Disable any delayed EI
	ld hl,event_counter_checkers_done
event_counter_checkers_ei_delay_smc_2 = $+1
	ld (event_counter_checkers_ei_delay),hl
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

ophandlerEI_delay_expired:
	; An event is scheduled for the current cycle, so we have to delay the
	; actual enabling of the IME flag.
	ld hl,schedule_ei_delay
event_counter_checkers_ei_delay_smc_1 = $+1
	ld (event_counter_checkers_ei_delay),hl
ophandlerEI_no_interrupt:
	; IME did not change, which also means no need to check for interrupts
	ld a,e
	exx
	ex af,af'
	ret


decode_halt:
	ex (sp),ix
	jp.lil decode_halt_helper
decode_halt_continue:
	ld (ix),a
	ld (ix+1),hl
	ld bc,ophandler_halt
	ld (ix-2),bc
	ex (sp),ix
	ld a,h
	cp flush_handler >> 8
	jr nz,_
	ld a,l
	cp flush_handler & $FF
	jr nz,_
	; If the JIT needs to be flushed, flush at the HALT address itself
	ld b,d
	ld c,e
	ld de,(flush_address)
	dec de
	jp.lil flush_for_halt

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
	 exx
	 ex (sp),ix
	 ld de,(ix-3)
	 dec de
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_event_helper
#else
	 jp.lil schedule_event_helper
#endif
