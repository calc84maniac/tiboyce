; Sets the stack base address and boundary values for the current bank
;
; Inputs:  A = stack bank identifier
; Outputs: IX = stack info pointer for the specified bank
;          BC = stack base address
;          DE = old value of HL
; Destroys: AF, HL
set_gb_stack_bounds_helper:
	ex de,hl
	ld ix,stack_bank_info_table
	scf
	rra
	ld ixl,a
	; Get the bank base in BC
	ld bc,(ix)
	ld (z80codebase+sp_base_address),bc
	; Adjust inc/dec sp handlers based on the address alignment
	bit 0,c
	ld a,$20	;JR NZ
	jr z,_
	ld a,$28	;JR Z
_
	ld (z80codebase+ophandler33_jr_smc),a
	ld (z80codebase+ophandler3B_jr_smc),a
	; Calculate the negative of the bank base
	sbc hl,hl
	sbc hl,bc
	ld.sis (sp_base_address_neg),hl
	; Get the MSBs of the boundaries for the bank
	ld hl,(ix+3)
	; Lower boundary triggers when pushing with SP <= lower_bound+1
	inc bc
	ld a,b
	add a,l
	ld (z80codebase+do_push_bound_smc_1),a
	ld (z80codebase+do_push_bound_smc_2),a
	ld (z80codebase+ophandler3B_bound_smc),a
	; Upper boundary triggers when popping with SP >= upper_bound-2
	dec bc
	dec bc
	dec bc
	ld a,b
	add a,h
	ld (z80codebase+do_pop_bound_smc_1),a
	ld (z80codebase+do_pop_bound_smc_2),a
	ld (z80codebase+do_pop_bound_smc_3),a
	ld (z80codebase+do_pop_bound_smc_4),a
	ld (z80codebase+ophandler33_bound_smc),a
	inc bc
	inc bc
	ret.l

; Sets the current stack access routines for the current bank.
;
; Inputs:  IX = stack info pointer for the specified region
; Outputs: HL = old value of DE
;          Z if the region is HRAM, else NZ
; Destroys: AF, DE, IX
set_gb_stack_bank_helper:
	; Special case for RTC when stack points to CRAM 
	ld a,ixl
	cp cram_bank_base & $FF
	jr nz,_
	ld a,(ix+2)
	cp z80codebase >> 16
; Inputs: IX = cram_bank_base
;         Z if setting RTC routines, NZ if setting CRAM routines
set_gb_stack_cram_bank_helper:
	jr nz,++_
	ld ixl,(stack_bank_info_table - $10) & $FF
_
	; Set callstack memory access prefixes
	add a,a	; For high RAM, use NOP as the prefix
	jr z,++_
_
	ld a,$49	; Otherwise, use .lis
_
	ld (z80codebase+callstack_ret_pop_prefix_smc_1),a
	ld (z80codebase+callstack_ret_pop_prefix_smc_2),a
	; Set push jump offsets
	ld hl,(ix+5)
	ld a,l
	ld (z80codebase+do_push_jump_smc_1),a
	sub do_push_jump_smc_2 - do_push_jump_smc_1
	ld (z80codebase+do_push_jump_smc_2),a
	sub do_push_jump_smc_3 - do_push_jump_smc_2
	ld (z80codebase+do_push_jump_smc_3),a
	sub do_push_jump_smc_4 - do_push_jump_smc_3
	ld (z80codebase+do_push_jump_smc_4),a
	sub do_push_jump_smc_5 - do_push_jump_smc_4
	ld (z80codebase+do_push_jump_smc_5),a
	sub do_push_and_return_jump_smc - do_push_jump_smc_5
	ld (z80codebase+do_push_and_return_jump_smc),a
	ld a,h
	ld (z80codebase+do_push_for_call_jump_smc_1),a
	; Jump 1 more byte forward to skip the PUSH BC at each entry point
	sub do_push_for_call_jump_smc_2 - do_push_for_call_jump_smc_1 - 1
	ld (z80codebase+do_push_for_call_jump_smc_2),a
	; Set pop jump offsets
	ld hl,(ix+7)
	ld a,l
	ld (z80codebase+do_pop_jump_smc_1),a
	sub do_pop_jump_smc_2 - do_pop_jump_smc_1
	ld (z80codebase+do_pop_jump_smc_2),a
	ld a,h
	ld (z80codebase+do_pop_for_ret_jump_smc_1),a
	sub do_pop_for_ret_jump_smc_2 - do_pop_for_ret_jump_smc_1
	ld (z80codebase+do_pop_for_ret_jump_smc_2),a
	ld a,(ix+9)
	ld (z80codebase+ophandlerF1_jump_smc_1),a
	add a,(ophandlerF1_jump_smc_1 + 1) & $FF
	ld l,a
	adc a,(ophandlerF1_jump_smc_1 + 1) >> 8
	sub l
	ld h,a
	ld.sis (ophandlerF1_jump_smc_2),hl
	; Copy callstack pop overflow check
	ld hl,(ix+10)
	ld (z80codebase+callstack_ret_check_overflow_smc),hl
	ld a,ixl
	add a,a
	ex de,hl
	ret.l

	; Propagate the bank mismatch value to the next callstack entry
	; in the same region as the return address
callstack_ret_bank_mismatch_helper:
	ld c,a
	ld h,d
callstack_ret_skip_propagate_helper:
	; Ensure the return address is in the $4000-$7FFF bank
	ld a,$BF
	cp h
#ifdef DEBUG
	jp po,$
#else
	jp po,++_
#endif
	; Get the pointer to the upper byte of the first GB return address
	; Skip over the saved GB stack pointer and the return value
	ld ix,6 - ((myADLstack & $FF) - $BF)
	add ix,sp
_
	; Check if we reached the end of the callstack
	cp ixl
	jr z,_
	; Check if the address is in the $4000-$7FFF range
	cp (ix+((myADLstack & $FF) - $BF)+1)
	lea ix,ix+CALL_STACK_ENTRY_SIZE_ADL
	jp po,-_
	; Get the bank mismatch value in A
	dec sp
	push hl
	inc sp
	pop af
	; Combine the mismatch value with this entry's value
	lea hl,ix+((myADLstack & $FF) - $BF)-3+2
	xor (hl)
	ld (hl),a
_
	ld a,c
	ret.l
	
	; This is called when a CALL, RST, or interrupt occurs
	; which exceeds the defined callstack limit.
	; Inputs: SPL = myADLstack - (CALL_STACK_DEPTH * CALL_STACK_ENTRY_SIZE_ADL) - 3
	;         SPS = myz80stack - 4 - (CALL_STACK_DEPTH * CALL_STACK_ENTRY_SIZE_Z80) - 2
	;         (SPL) = return value (to Z80 mode, 3 bytes)
	;         (SPS) = value to preserve on Z80 callstack
	; Outputs: SPL = myADLstack - 3
	;          SPS = myz80stack - 4 - 2
	;          (SPS) = preserved Z80 callstack value
callstack_overflow_helper:
	; For now, don't bother with special handling to preserve interrupt entries
	ld sp,myADLstack
	push hl
	 pop.s hl
	 ld.sis sp,myz80stack - 4
	 push.s hl
	 ld hl,(myADLstack - (CALL_STACK_DEPTH * CALL_STACK_ENTRY_SIZE_ADL) - 3)
	 ex (sp),hl
	ret.l

; Flushes the JIT code and recompiles anew.
; Does not use a traditional call/return, must be jumped to directly.
;
; When the recompiler overflows, it returns a pointer to flush_handler,
; which provides this routine with the GB address.
;
; Inputs:  DE = GB address to recompile
;          BCDEHL' have been swapped
; Outputs: JIT is flushed and execution begins at the new recompiled block
;          BCDEHL' have been unswapped
flush_normal:
	ex af,af'
	ld c,a
	push hl
flush_for_halt:
	 push bc
	  ld hl,$C3 | (do_event_pushed << 8)	;JP do_event_pushed
	  ld (flush_event_smc_1),hl
	  ld (flush_event_smc_2),hl
	  ld hl,$D2 | (schedule_event_finish_for_call_now << 8)	;JP NC,schedule_event_finish_for_call
	  ld (flush_event_for_call_smc),hl
flush_mem_finish:
	  call flush_code
	  push de
	   call lookup_code
	  pop de
	 pop bc
	pop hl
	; Flush entire call stack, including interrupt returns
	ld sp,myADLstack
	ld.sis sp,myz80stack-4
	add a,c
	jr c,_
	dec iyh
_
	inc iyh
	jr z,_
	exx
	ex af,af'
	jp.s (ix)
_
	ld iyl,a
	sub c
	ld c,a
	push.s ix
	push hl
#ifdef VALIDATE_SCHEDULE
	call schedule_event_helper
#else
	jp schedule_event_helper
#endif
	
; Flushes the JIT code and recompiles anew.
; Does not use a traditional call/return, must be jumped to directly.
;
; When the memory routine generator overflows, it returns a pointer to
; flush_mem_handler, which provides this routine with the JIT address.
; The JIT address is reversed into the GB address before flushing normally.
; Recompilation starts at the offending memory instruction.
;
; Inputs:  BC = address directly following recompiled memory instruction
;          A = stack overflow counter
;          Z if instruction is LD (HL),n
;          BCDEHL' have been swapped
; Outputs: JIT is flushed and execution begins at the new recompiled block
;          BCDEHL' have been unswapped
flush_mem:
	push hl
	 push af
	  call lookup_gb_code_address
	  ; Most memory routines are for 1-byte, 2-cycle instructions
	  dec de
	  cpl
	  dec a
	 pop bc
	 bit 6,c
	 jr z,_
	 ; Special handling for 2-byte, 3-cycle instructions
	 dec de
	 dec a
_
	 add a,iyl
	 ld c,a
	 push bc
	  jr c,flush_mem_finish
	  dec iyh
	  jr flush_mem_finish
	
	
; Catches up sprite rendering before changing sprite state.
; Must be called only if render_catchup has already been called.
;
; Outputs: A = current scanline
; Destroys: AF, BC, DE, HL, IX
sprite_catchup:
	push iy
	 call draw_sprites
	pop iy
	
	ld a,(myLY)
	ld (myspriteLY),a
	ld hl,(scanlineLUT_ptr)
	ld (scanlineLUT_sprite_ptr),hl
	ret
	
scroll_write_DMA:
	 push bc
	  ; Render the existing OAM data if applicable
	  ld a,(z80codebase+updateSTAT_enable_catchup_smc)
	  rra
	  call c,sprite_catchup
	  ; Copy 160 bytes from the specified source address to OAM
	  ld de,0
	  ex af,af'
	  ld d,a
	  ex af,af'
	  GET_BASE_ADDR_FAST
	  add hl,de
	  ld bc,$00A0
	  ld de,hram_start
	  ldir
	 pop bc
	pop hl
	jp.sis z80_restore_swap_ret
	
; Writes to an LCD scroll register (SCX,SCY,WX,WY). Also BGP and DMA.
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  (LY), (STAT) = current PPU state
;          C = A' = value being written
;          AF' has been swapped
;          BCDEHL' have been swapped
;          (SPS) = 16-bit register address
;          (SPS+2) = Z80 return address
;          (SPL) = saved HL'
; Outputs: Scanlines rendered if applicable, SMC applied, value written
;          AF' has been unswapped
scroll_write_helper:
	 ld a,r
	 call m,render_catchup
	 pop.s hl
	 ld a,l
	 sub SCX - ioregs
	 jr c,scroll_write_SCY
	 jr z,scroll_write_SCX
	 sub BGP - SCX
	 jr c,scroll_write_DMA
	 jr z,scroll_write_BGP
	 rra
	 jr nc,scroll_write_WX
	 ld a,iyl
	 ex af,af'
	 ld (WY_smc),a
	 jr scroll_write_done
	 
scroll_write_SCY:
	 ld a,iyl
	 ex af,af'
	 ld (SCY_smc),a
	 jr scroll_write_done
	
scroll_write_BGP:
	 ; Only do things if the current frame is being rendered
	 ld a,(z80codebase+updateSTAT_enable_catchup_smc)
	 rra
	 jr nc,scroll_write_done_swap
	 push bc
	  push hl
	   call sprite_catchup
mypaletteLY = $+1
	   ld c,0
	   ld (mypaletteLY),a
	   sub c
scanlineLUT_palette_ptr = $+2
	   ld ix,0
	   call nz,convert_palette
	   ld (scanlineLUT_palette_ptr),ix
	  pop hl
	 pop bc
	 jr scroll_write_done_swap
	 
scroll_write_WX:
	 ld a,c
	 ld (WX_smc_2),a
	 cp 167
	 inc a
	 ld (WX_smc_3),a
	 sbc a,a
	 and $20
	 or $18	;JR vs JR C
	 ld (WX_smc_1),a
	 jr scroll_write_done_swap
	
scroll_write_SCX:
	 ld a,c
	 rrca
	 rrca
	 and $3E
	 ld (SCX_smc_1),a
	 ld a,c
	 cpl
	 and 7
	 inc a
	 ld (SCX_smc_2),a
scroll_write_done_swap:
	 ld a,iyl
	 ex af,af'
scroll_write_done:
	 ld.s (hl),a
	pop hl
	jp.sis z80_swap_ret
	
lyc_write_0:	
	; Special case for line 0, thanks silly PPU hardware
	ld de,-(CYCLES_PER_SCANLINE * 9 + 1)
	ld a,1-CYCLES_PER_SCANLINE
	ld (z80codebase+ppu_lyc_scanline_length_smc),a
	jr lyc_write_continue
	
; Writes to the LY compare register (LYC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Sets the new cycle target according to the new value of LYC.
; Triggers a GB interrupt if LY already matches the new LYC value,
; but only if LYC is changing.
;
; Inputs:  C = A' = value being written
;          (LY), (STAT) = current PPU state
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
;          BCDEHL' are swapped
; Outputs: LYC and cycle targets updated
lyc_write_helper:
	 ; Set the new value of LYC
	 ld hl,hram_base+LYC
	 ld (hl),c
	 ; Calculate the new LYC cycle offset (from vblank)
	 ld a,c
	 or a
	 jr z,lyc_write_0
	 ld d,-CYCLES_PER_SCANLINE
	 add a,10
	 ld e,a
	 ; Wrap vblank lines to the 0-9 range
	 daa
	 jr nc,_
	 ld e,a
	 ; Set scanline length to -1 for line 153, or -CYCLES_PER_SCANLINE otherwise
	 add a,-9
	 sbc a,a
	 or d
	 ld (z80codebase+ppu_lyc_scanline_length_smc),a
_
	 ; Multiply by -CYCLES_PER_SCANLINE
	 ; Note that this produces 0 cycles for LYC=144, but the cycle offset is not used
	 ; in that particular case (vblank collision is special-cased)
	 xor a
	 sub e
	 mlt de
	 add a,d
	 ld d,a
lyc_write_continue:
	 ld (lyc_cycle_offset),de
lyc_write_disable_smc = $
	 ; Check if LY=LYC
	 dec hl
	 ld a,c
	 cp (hl)
	 ; Set or reset the LYC coincidence bit in STAT
	 ld l,STAT & $FF
	 ld c,(hl)
	 res 2,c
	 ld d,c
	 jr nz,_
	 set 2,c
	 ; If LYC interrupts are enabled, handle interrupt blocking logic
	 bit 6,c
	 jr z,_
	 ; Transform mode into interrupt source mask:
	 ; 0 -> $08, 1 -> $10, 2 -> $24, 3 -> $00
	 ld a,d
	 inc a
	 and 3
	 add a,a
	 add a,a
	 daa
	 add a,a
	 ; Check if any enabled interrupt modes block the interrupt
	 ; Note that bit 2 has been reset, so bit $04 will be ignored
	 and d
	 jr nz,_
	 ; Set the LYC coincidence bit and IF STAT bit
	 ld (hl),c
	 ld l,IF & $FF
	 set 1,(hl)
	 ; Check if the IE STAT bit is set
	 ld l,h
	 bit 1,(hl)
	 jr z,++_
	 ; Perform STAT scheduling updates
	 call stat_setup
	 ; Trigger a new event immediately
	 jp.sis trigger_event_pushed
_
	 ld (hl),c
_
	 
	 ; Perform STAT scheduling updates
	 call stat_setup
	 ; Reschedule the current PPU event
	 jp.sis reschedule_event_PPU
	 
stat_lyc_write_no_reschedule:
	 pop hl
	jp.sis z80_restore_swap_ret
	 
stat_write_helper:
	 ld hl,hram_base + STAT
	 ex af,af'
	 ld d,a
	 ex af,af'
	 ld a,(hl)
	 ld c,a
	 ; Save changed mode interrupt bits
	 xor d
	 ld e,a
	 ; Update writable bits in STAT
	 and $87
	 xor d
	 ld (hl),a
	 ; Transform coincidence flag and mode into interrupt mask
stat_write_disable_smc = $
	 rrca
	 rrca
	 add a,$40
	 and $C1
	 daa
	 rra
	 rra
	 rra
	 and $78
	 ; Check if interrupt condition was already true
	 tst a,c
	 jr nz,_
	 ; If not, this write can cause an interrupt to trigger
	 ; Emulate DMG STAT write bug by just checking if any source is enabled
	 or a ; Replace with AND D for GBC emulation, to check against new bits
	 jr z,_
	 ; If so, set IF STAT bit
	 ld l,IF & $FF
	 set 1,(hl)
	 ; Check if IE STAT bit is set
	 ld l,h
	 bit 1,(hl)
	 jr z,_
	 ; Handle changes to mode 0 and mode 2 interrupt bits
	 ld a,e
	 and $28
	 call nz,stat_setup
	 ; Trigger a new event immediately
	 jp.sis trigger_event_pushed
_
	 ; Handle changes to mode 0 and mode 2 interrupt bits
	 ld a,e
	 and $28
	 jr z,stat_lyc_write_no_reschedule
	 call stat_setup
	 ; Reschedule the current PPU event
	 jp.sis reschedule_event_PPU
	
do_lcd_disable:
	; Set STAT mode 0 and LY=0
	ld hl,hram_base+STAT
	ld a,(hl)
	and $FC
	ld (hl),a
	ld l,LY & $FF
	xor a
	ld (hl),a
	
	; Disable cache updates for STAT and LY registers
	ld a,$C9 ;RET
	ld (z80codebase+updateSTAT_disable_smc),a
	ld (z80codebase+updateLY_disable_smc),a
	
	; Disable interrupt and rescheduling effects for LYC and STAT writes
	ld hl,(stat_lyc_write_no_reschedule - (lyc_write_disable_smc+2))<<8 | $18 ;JR
	ld (lyc_write_disable_smc), hl
	ld h,stat_lyc_write_no_reschedule - (stat_write_disable_smc+2)
	ld (stat_write_disable_smc),hl
	 
	; Update PPU scheduler to do events once per "frame"
	ld ix,z80codebase+ppu_expired_lcd_off
	ld hl,-CYCLES_PER_FRAME
	ld.sis (ppu_post_vblank_event_handler),ix
	ld.sis (ppu_post_vblank_event_offset),hl
	jr stat_setup_next_vblank_lcd_off
	
stat_setup_hblank:
	ld ix,z80codebase+ppu_expired_mode0_line_0
	ld (ix-ppu_expired_mode0_line_0+ppu_mode0_event_line),144
	ex de,hl
	ld hl,-((CYCLES_PER_SCANLINE * 10) + MODE_2_CYCLES + MODE_3_CYCLES)
	; Check if LYC match is during vblank
	cp 144
	call nc,stat_setup_lyc_mode1_filter
	ld.sis (ppu_post_vblank_event_handler),ix
	ld.sis (ppu_post_vblank_event_offset),hl
	ld ix,z80codebase+ppu_expired_mode0
	; If currently in mode 1, schedule the first post-vblank event
	ld a,c
	and 3
lcd_on_stat_setup_mode_smc = $+1
	cp 1
	jr z,stat_setup_hblank_post_vblank
	; Get LY and add 1 if hblank has been reached
	ld a,e
	adc a,0
	; If the result is 0, schedule the first post-vblank event
	jr z,stat_setup_hblank_post_vblank
	; If during hblank of line 143, schedule vblank
	cp 144
	jr z,stat_setup_next_vblank
	ld c,a
	; Determine the cycle time of next hblank
	cp e
	ld a,d
	; Normally, schedule before the end of the current scanline
	ld de,MODE_0_CYCLES
	jr z,_
	; For hblank, schedule in the next scanline
	dec d
	ld e,-(MODE_2_CYCLES + MODE_3_CYCLES)
_
	ld.sis hl,(nextupdatecycle_LY)
	add hl,de
	; Set the current line with the adjusted LY
	ld (ix-ppu_expired_mode0+ppu_mode0_LY),c
	; If LYC < adjusted LY, keep vblank as next event line 
	cp c
	jr c,stat_setup_done_trampoline
	; If LYC == adjusted LY, update entry point and keep vblank as next event
	jr z,stat_setup_hblank_lyc_match
	; If LYC > adjusted LY and LYC < 144. update the event line
	cp 144
	jr nc,stat_setup_done
	ld (ix-ppu_expired_mode0+ppu_mode0_event_line),a
	jr stat_setup_done
	
stat_setup_hblank_trampoline:
	jr stat_setup_hblank
	
stat_setup_hblank_post_vblank:
	ld ix,z80codebase+ppu_expired_mode0_line_0
	ld hl,-((CYCLES_PER_SCANLINE * 10) + MODE_2_CYCLES + MODE_3_CYCLES)
	jr stat_setup_next_from_vblank
	
stat_setup_hblank_lyc_match:
	lea ix,ix-ppu_expired_mode0+ppu_expired_mode0_lyc_match
	jr stat_setup_done
	
stat_setup_lyc_vblank:
	; The next event from vblank is vblank, unless LYC is during mode 1
	ex de,hl
	ld hl,-CYCLES_PER_FRAME
	call nz,stat_setup_lyc_mode1_filter
	ld.sis (ppu_post_vblank_event_handler),ix
	ld.sis (ppu_post_vblank_event_offset),hl
stat_setup_next_vblank:
	; The next event from now is vblank
	ld ix,z80codebase+ppu_expired_vblank
stat_setup_next_vblank_lcd_off:
	or a
	sbc hl,hl
	ld.sis de,(vblank_counter)
	sbc hl,de
stat_setup_done_trampoline:
	jr stat_setup_done
	
	; Input: C = current value of STAT
stat_setup_c:
	ld d,c
	; Input: D = current writable bits of STAT, C = current read-only bits of STAT
stat_setup:
	ld ix,z80codebase+ppu_expired_vblank
	; Get the line to match, but treat offscreen lines as vblank match
	ld.sis hl,(LY)
	ld a,h
	cp 154
	jr c,_
	ld h,144
	ld a,h
_
	dec a
	; Check if hblank interrupt is enabled (blocks OAM)
	bit 3,d
	jr nz,stat_setup_hblank_trampoline
	; Check if OAM interrupt is enabled
	bit 5,d
	jr nz,stat_setup_oam
	; Check if LYC match is at or during vblank
	cp 143
	jr nc,stat_setup_lyc_vblank
	; Check if LY >= LYC
	cp l
	ld a,l
	; The next event from vblank is LYC (mode 2)
	lea ix,ix-ppu_expired_vblank+ppu_expired_lyc_mode2
	ld.sis (ppu_post_vblank_event_handler),ix
	ld hl,(lyc_cycle_offset)
	ld.sis (ppu_post_vblank_event_offset),hl
	jr nc,stat_setup_next_from_vblank
	; Check if LY < 144
	cp 144
	jr c,stat_setup_next_vblank
stat_setup_next_from_vblank:
	; The next event from now is relative to start of vblank
	ld.sis de,(vblank_counter)
	sbc hl,de
	; Offset is from start of vblank, so adjust back to previous vblank
	ld de,CYCLES_PER_FRAME
	add hl,de
stat_setup_done:
	ld.sis (ppu_counter),hl
lcd_on_stat_setup_event_smc = $+3
	ld.sis (event_counter_checker_slot_PPU),ix
	ret
	
stat_setup_oam:
	ld ix,z80codebase+ppu_expired_mode2_line_0
	ld (ix-ppu_expired_mode2_line_0+ppu_mode2_event_line),143
	ex de,hl
	ld hl,-(CYCLES_PER_SCANLINE * 10)
	; Check if LYC match is during vblank
	cp 144
	call nc,stat_setup_lyc_mode1_filter
	ld.sis (ppu_post_vblank_event_handler),ix
	ld.sis (ppu_post_vblank_event_offset),hl
	; If LY is 143, schedule vblank
	ld a,e
	cp 143
	jr z,stat_setup_next_vblank
	ld ix,z80codebase+ppu_expired_mode2
	; If currently in mode 1, schedule the first post-vblank event
	ld a,c
	dec a
	and 3
	jr z,stat_setup_oam_post_vblank
	; Set the current line to LY and get the time for the next event
	ld (ix-ppu_expired_mode2+ppu_mode2_LY),e
	ld.sis hl,(nextupdatecycle_LY)
	; If LYC < LY, keep vblank as next event line
	ld a,d
	cp e
	jr c,stat_setup_done
	; If LYC == LY, update entry point and keep vblank as next event
	jr z,stat_setup_oam_lyc_blocking
	; If LYC > LY and LYC < 143, update the event line
	cp 143
	jr nc,stat_setup_done
	ld (ix-ppu_expired_mode2+ppu_mode2_event_line),a
	jr stat_setup_done
	
stat_setup_oam_post_vblank:
	lea ix,ix-ppu_expired_mode2+ppu_expired_mode2_line_0
	ld hl,-(CYCLES_PER_SCANLINE * 10)
	jr stat_setup_next_from_vblank
	
stat_setup_oam_lyc_blocking:
	lea ix,ix-ppu_expired_mode2+ppu_expired_mode2_lyc_blocking
	jr stat_setup_done
	
	; Input: A=LYC-1, L=LY, HL=event cycle offset, IX=event handler, carry reset
	; Output: HL=new event cycle offset, IX=new event handler,
	;         unless the PPU is currently between vblank and LYC,
	;         in which case this call does not actually return.
stat_setup_lyc_mode1_filter:
	; Record the filtered event handler and offset to follow LYC
	ld.sis (ppu_post_mode1_lyc_event_handler),ix
	push de
lyc_cycle_offset = $+1
	 ld de,0
	 ASSERT_NC
	 sbc hl,de
	 ld.sis (ppu_post_mode1_lyc_event_offset),hl
	 ex de,hl
	pop de
	; The next event from vblank is LYC (mode 1)
	ld ix,z80codebase+ppu_expired_lyc_mode1
	; Return if LY >= LYC (unless LYC=0)
	cp e
	ret c
	; Return if LY < 144
	ld a,e
	cp 144
	ret c
	; The next event from now is LYC (mode 1)
	pop de
	jp stat_setup_next_from_vblank
	
	
; Writes to the LCD control register (LCDC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  A' = value being written
;          (LY), (STAT) = current PPU state
;          AF' has been swapped
;          BCDEHL' have been swapped
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
; Outputs: Scanlines rendered if applicable, SMC applied, value written
;          AF' has been unswapped
;          BCDEHL' have been unswapped
lcdc_write_helper:
	 ld a,r
	 call m,render_catchup
	 ld hl,hram_base+LCDC
	 ld a,(hl)
	 ex af,af'
	 ld (hl),a
	 ex af,af'
	 xor (hl)
	 ld c,a
	 and $06
	 jr z,_
	 ld a,(z80codebase+updateSTAT_enable_catchup_smc)
	 rra
	 push bc
	  call c,sprite_catchup
	 pop bc
_
	 bit 0,c
	 jr z,_
	 ld a,(LCDC_0_smc)
	 xor $39 ^ $31	;ADD.SIS HL,SP \ LD.SIS SP,HL vs LD.SIS SP,$F940
	 ld (LCDC_0_smc),a
_
	 bit 1,c
	 jr z,_
	 ld a,(LCDC_1_smc)
	 xor $0E ^ $C9	;LD C,myspriteLY vs RET
	 ld (LCDC_1_smc),a
_
	 bit 2,c
	 jr z,_
	 ld a,(LCDC_2_smc_1)
	 xor $38^$78
	 ld (LCDC_2_smc_1),a
	 ld a,(LCDC_2_smc_2)
	 xor 7^15
	 ld (LCDC_2_smc_2),a
	 ld (LCDC_2_smc_4),a
	 xor 7^9
	 ld (LCDC_2_smc_5),a
	 ld a,(LCDC_2_smc_3)
	 xor $80 ^ $81	;RES 0,B vs RES 0,C
	 ld (LCDC_2_smc_3),a
_
	 bit 3,c
	 jr z,_
	 ld a,(LCDC_3_smc)
	 xor (vram_tiles_start ^ (vram_tiles_start + $2000)) >> 8
	 ld (LCDC_3_smc),a
_
	 bit 4,c
	 jr z,_
	 ld a,(LCDC_4_smc)
	 xor $80
	 ld (LCDC_4_smc),a
	 ld (window_tile_ptr),a
_
	 bit 5,c
	 jr z,_
	 ld a,(LCDC_5_smc)
	 xor $08	;JR C vs JR NC
	 ld (LCDC_5_smc),a
_
	 bit 6,c
	 jr z,_
	 ld a,(window_tile_ptr+1)
	 sub (vram_tiles_start >> 8) & $FF
	 xor $20
	 add a,(vram_tiles_start >> 8) & $FF
	 ld (window_tile_ptr+1),a
_
	 bit 7,c
	 jp z,return_from_write_helper
	 ld a,(LCDC_7_smc)
	 xor $08	;JR NZ vs JR Z
	 ld (LCDC_7_smc),a
	 jp.sis pe,lcd_enable_helper
	 
	 ; Disable the LCD
	 call do_lcd_disable
	 jp.sis reschedule_event_PPU
	 
do_lcd_enable:
	 ; Enable cache updates for STAT and LY registers
	 ld a,$ED ;LD HL,I
	 ld (z80codebase+updateSTAT_disable_smc),a
	 ld (z80codebase+updateLY_disable_smc),a
	
	 ; Enable interrupt and rescheduling effects for LYC and STAT writes
	 .db $21 ;ld hl,
	  dec hl
	  ld a,c
	  cp (hl)
	 ld (lyc_write_disable_smc), hl
	 .db $21 ;ld hl,
	  rrca
	  rrca
	  .db $C6 ;add a,
	 ld (stat_write_disable_smc),hl
	 
	 ; Set up special handling for transitioning from fake mode 0
	 ld a,$5B ;.LIL prefix
	 ld (z80codebase+lcd_on_STAT_restore),a
	 ld a,lcd_on_STAT_handler - (lcd_on_updateSTAT_smc + 1)
	 ld (z80codebase+lcd_on_updateSTAT_smc),a
	 xor a
	 ld (lcd_on_stat_setup_mode_smc),a
	 ld hl,($C9 << 16) | lcd_on_ppu_event_checker
	 ld (lcd_on_stat_setup_event_smc),hl
	 ld hl,lcd_on_STAT_handler
	 ld.sis (event_counter_checker_slot_PPU),hl
	 
	 ; Get the value of DIV
	 ld hl,i
	 add hl,de
	 ex de,hl
	 ; Schedule vblank relative to now (minus 1 cycle because the LCD is wack)
	 ld hl,(CYCLES_PER_SCANLINE * 144) - 1
	 add hl,de
	 ld.sis (vblank_counter),hl
	 
	 ; Check if LYC=0
	 ld hl,hram_base+LYC
	 ld a,(hl)
	 or a
	 ld l,STAT & $FF
	 ld a,(hl)
	 ld c,a
	 ; Set/reset LY=LYC coincidence bit (based on LY being 0)
	 res 2,c
	 jr nz,_
	 set 2,c
	 ; Check if coincidence bit transitioned from 0 to 1
	 ; and LYC interrupt bit was set
	 xor l ;$41
	 and $44
_
	 ; Leave mode as 0, even though it's really mode 2
	 ld (hl),c
	 jr nz,_
	 ld l,IF & $FF
	 set 1,(hl)
_
	  
	 ; Set LY and STAT cache times for line 0, mode 2 (fake mode 0)
	 ; This is reduced by 1 cycle because of course it is
	 ld l,1-CYCLES_PER_SCANLINE
	 sbc hl,de
	 ld.sis (nextupdatecycle_LY),hl
	 ld de,MODE_0_CYCLES + MODE_3_CYCLES
	 add hl,de
	 ld.sis (nextupdatecycle_STAT),hl
	 
	 ; Update PPU scheduler based on current value of STAT
	 call stat_setup_c
	 jp.sis reschedule_event_PPU
	
lcd_on_STAT_restore_helper:
	ld a,$C9
	ld (z80codebase+lcd_on_STAT_restore),a
	ld a,updateSTAT_mode0_mode1 - (lcd_on_updateSTAT_smc + 1)
	ld (z80codebase+lcd_on_updateSTAT_smc),a
	ld a,1
	ld (lcd_on_stat_setup_mode_smc),a
	push hl
	 ld hl,($C9 << 16) | event_counter_checker_slot_PPU
	 ld (lcd_on_stat_setup_event_smc),hl
	 ; If this is called from the PPU event, it overwrites the return address
	 ld.sis hl,(lcd_on_ppu_event_checker)
	 ld.sis (event_counter_checker_slot_PPU),hl
	pop hl
	jp.sis z80_ret
	
div_write_helper:
	 push de
	  ld hl,i
	  add hl,de
	  ex de,hl
	  ; If bit 11 of DIV was already reset, delay audio counter
	  ld a,d
	  cpl
	  add a,a
	  and $10 ;4096 >> 8
	  ld (z80codebase+audio_counter+1),a
	  ld.sis hl,(vblank_counter)
	  sbc hl,de
	  ld.sis (vblank_counter),hl
	  ld.sis hl,(ppu_counter)
	  add hl,de
	  ld.sis (ppu_counter),hl
	  ld.sis hl,(nextupdatecycle_STAT)
	  add hl,de
	  ld.sis (nextupdatecycle_STAT),hl
	  ld.sis hl,(nextupdatecycle_LY)
	  add hl,de
	  ld.sis (nextupdatecycle_LY),hl
	  ld.sis hl,(serial_counter)
	  xor a
	  sbc hl,de
	  ld.sis (serial_counter),hl
	  ; Update timer schedule, with logic to cause an instant TIMA increment
	  ; if the specified bit of DIV is moving from 1 to 0
	  ld d,a
	  ld a,(hram_base+TIMA)
	  cpl
	  ld l,a
	  ld a,(writeTIMA_smc)
	  ld h,a
	  ; Only if the old bit of DIV is 0, add to the scheduled time
	  ; In effect, if the bit was 1, this schedules the next increment immediately
	  and e
	  xor h
	  ld e,a
	  mlt hl
	  add.s hl,de
	  add hl,hl
	  ld.sis (timer_counter),hl
	 pop de
	 sbc hl,hl
	 sbc hl,de
	 ld i,hl
	 jp.sis trigger_event_pushed
	
; Writes to the GB timer control (TAC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Updates the GB timer based on the new mode; applies SMC to getters/setters
;
; Inputs:  DE = current cycle offset
;          A' = value being written
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
;          BCDEHL' are swapped
; Outputs: Current value written to (TAC)
;          GB timer updated
tac_write_helper:
	 ; Set new TAC value
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,c
	 or $F8
	 ld (hram_base+TAC),a
	 add a,4
	 jr c,_
	 ld hl,disabled_counter_checker
	 ld.sis (event_counter_checker_slot_timer),hl
return_from_write_helper:
	pop hl
	jp.sis z80_restore_swap_ret
_
	 ; Get SMC data
	 ld c,a
	 ld a,b
	 ld b,2
	 mlt bc
	 ld hl,timer_smc_data
	 add hl,bc
	 ld b,a
	 
	 ld a,(hl)
	 ld (z80codebase + updateTIMA_smc),a
	 inc hl
	 ld a,(hl)
	 ld (z80codebase + timer_cycles_reset_factor_smc),a
	 ld (writeTIMA_smc),a

	 ld hl,timer_counter_checker
	 ld.sis (event_counter_checker_slot_timer),hl
	
; Writes to the GB timer count (TIMA).
; Does not use a traditional call/return, must be jumped to directly.
;
; Updates the GB timer based on the new value, if enabled.
;
; Inputs:  DE = current cycle offset
;          (TIMA) = value written
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
;          BCDEHL' are swapped
; Outputs: GB timer updated
;          Event triggered
tima_write_helper:
	 ld hl,hram_base+TAC
	 bit 2,(hl)
	 jr z,return_from_write_helper
	 
	 ld l,TIMA & $FF
	 ld a,(hl)
	 
	 ld hl,i
	 add hl,de
	 
	 cpl
	 ld e,a 
writeTIMA_smc = $+1
	 ld d,0
	 ld a,d
	 add a,a
	 dec a
	 or l
	 ld l,a
	 inc hl
	 mlt de
	 add hl,de
	 add hl,de
	 ld.sis (timer_counter),hl
	 jp.sis trigger_event_pushed
	
timer_smc_data:
	.db 6,$80
	.db 0,$02
	.db 2,$08
	.db 4,$20
	
NR52_write_helper:
	exx
	push hl
	 ld hl,hram_base+NR52
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,(hl)
	 xor c
	 add a,a
	 jr nc,return_from_write_helper
	 ; Whether enabling or disabling audio, all channels are off
	 ld a,c
	 and $80
	 or $70
	 ld (hl),a
	 add a,a
	 jr c,NR52_write_enable
	 ; Zero all audio registers
	 ld hl,z80codebase+audio_port_values
	 push hl
	 pop de
	 inc de
	 ld a,b
	 ld bc,audio_port_masks-audio_port_values-1
	 ld (hl),b
	 ldir
	 ; Copy masks to CPU-visible registers
	 inc hl
	 ld de,hram_base+NR10
	 ld c,NR52 - NR10
	 ldir
	 ld b,a
	 ; If disabling, ignore all writes
	 ld a,$C9-($F5-$E0)	;RET
NR52_write_enable:
	 ; If enabling, un-ignore all writes
	 add a,$F5-$E0	;PUSH AF
	 ld (z80codebase+write_audio_disable_smc),a
	 jp return_from_write_helper
	
overlapped_op_1_1_mismatch:
	lea de,ix-7
overlapped_op_2_1_mismatch_continue:
	ld a,(hl)
	lea hl,ix-3
	ld (hl),a
	; Recompile an overlapped instruction again
	; Input: DE=start of overlapped handler call
	;        HL=overlap point in copied instruction
	;        IX=recompiled code start
	;        C=cycle count
opgen_overlap_rerecompile:
	ld a,l
	ld (opgen_base_address_smc_1),a
	ld a,h
	lea hl,ix-2
	sub (hl)
	ld (opgen_base_address_smc_2),a
	ld ix,opgenroutines
	push.s de
	inc de
	inc de
	inc de
	push bc
	 push iy
	  call opgen_emit_overlapped_opcode
	 pop iy
	pop bc
	pop hl
	jp.sis z80_restore_c_swap_ret
	
handle_overlapped_op_1_1_helper:
	ex af,af'
	exx
	push hl
	ld c,a
	ld de,z80codebase+4
	add ix,de
	ld.s de,(ix-3)
	ld a,e
	ld hl,mem_region_lut
	ld e,l ;E=0
	ld l,d
	ld l,(hl)
	dec h
	ld hl,(hl)
	add hl,de
	cp (hl)
	jr nz,overlapped_op_1_1_mismatch
	ld a,(ix-1)
	add a,c
	jr nc,handle_overlapped_op_done
	inc iyh
	jr nz,handle_overlapped_op_done
	lea hl,ix-4
	jr schedule_overlapped_event_helper_x_1
	
overlapped_op_2_1_mismatch:
	lea de,ix-8
	jr overlapped_op_2_1_mismatch_continue
	
overlapped_op_1_2_mismatch:
	lea hl,ix-4
	ld.s (hl),de
	lea de,ix-8
	jr opgen_overlap_rerecompile
	
handle_overlapped_op_1_2_helper:
	ex af,af'
	exx
	push hl
	ld c,a
	ld de,z80codebase+5
	add ix,de
	ld.s de,(ix-3)
	ld a,e
	ld hl,mem_region_lut
	ld e,l ;E=0
	ld l,d
	ld l,(hl)
	dec h
	ld hl,(hl)
	add hl,de
	ld de,(hl)
	ld h,a
	ld l,(ix-4)
	sbc.s hl,de
	jr nz,overlapped_op_1_2_mismatch
	ld a,(ix-1)
	add a,c
	jr nc,handle_overlapped_op_done
	inc iyh
	jr nz,handle_overlapped_op_done
	lea hl,ix-5
	lea de,ix-4
	jr schedule_overlapped_event_helper
	
handle_overlapped_op_2_1_helper:
	ex af,af'
	exx
	push hl
	ld c,a
	ld de,z80codebase+5
	add ix,de
	ld.s de,(ix-3)
	ld a,e
	ld hl,mem_region_lut
	ld e,l ;E=0
	ld l,d
	ld l,(hl)
	dec h
	ld hl,(hl)
	add hl,de
	cp (hl)
	jr nz,overlapped_op_2_1_mismatch
	ld a,(ix-1)
	add a,c
	jr c,_
handle_overlapped_op_done:
	pop hl
	exx
	ex af,af'
	jp.s (ix)
_
	inc iyh
	jr nz,handle_overlapped_op_done
	lea hl,ix-5
schedule_overlapped_event_helper_x_1:
	lea de,ix-3
; Inputs: HL = pointer to first byte of copied opcode
;         DE = pointer to first overlapping byte of copied opcode
;         IX = starting recompiled address
;         A = cycle count at end of overlapped opcode (>= 0)
;         C = cycle count at start of overlapped opcode (< 0)
schedule_overlapped_event_helper:
	push.s ix
	ld iyl,a
	ld a,d
	sub (ix-2)
	ld d,a
	; Check for a prefixed opcode
	ld a,(hl)
	cp $CB
	ld a,c
	jr nz,schedule_event_later_resolved
	; Prefixed overlapped opcodes are always 3 bytes recompiled
	lea ix,ix+3
	inc hl
	inc hl
	sbc hl,de
	ld a,iyl
	jp.sis schedule_event_finish

; Inputs: DE = Game Boy address of jump instruction
;         IX = starting recompiled address
;         IY = cycle count at end of sub-block (>= 0)
;         C = cycles until end of sub-block (plus jump cycles, if applicable)
schedule_jump_event_helper:
	dec c
	dec c
	dec c
schedule_jump_event_helper_adjusted:
#ifdef VALIDATE_SCHEDULE
	ex (sp),hl
#endif
	push hl
	GET_BASE_ADDR_FAST
	ex de,hl
	add hl,de
	bit 7,(hl)
	inc hl
	jr nz,schedule_jump_event_absolute
	push de
	 ld a,(hl)
	 inc hl
	 ex de,hl
	 rla
	 sbc hl,hl
	 rra
	 ld l,a
	 add hl,de
#ifdef VALIDATE_SCHEDULE
	pop de
	call validate_schedule_resolved
	pop af
	push de
#endif
	 ld a,iyl
	 sub c
	 jr c,schedule_event_later_resolved_pushed
	pop de
	sbc hl,de
schedule_event_now_unresolved:
	ld.sis (event_gb_address),hl
	ld iyl,a
#ifdef DEBUG
	ld hl,event_value
	ld.sis (event_address),hl
#endif
	; This is a code path that could target the flush handler
flush_event_smc_1 = $+1
	jp.sis do_event_pushed
	
schedule_jump_event_absolute:
	dec c
	ld hl,(hl)
#ifdef VALIDATE_SCHEDULE
	inc hl
	dec.s hl
	ex de,hl
	call validate_schedule
	ex de,hl
	pop af
#endif
	ld a,iyl
	sub c
	jr nc,schedule_event_now_unresolved
	inc hl
	dec.s hl
	add hl,de
schedule_event_later_resolved:
	push de
schedule_event_later_resolved_pushed:
	 push bc
	  ld de,opcounttable
	  ld bc,3
	  call opcycle_first
	 pop bc
	pop de
	or a
	sbc hl,de
	jp.sis schedule_event_finish
	
; Inputs: DE = Game Boy address at conditional branch
;         IX = recompiled address after conditional branch
;         IY = cycle count at end of sub-block (>= 0)
;         C = cycles until end of sub-block (including conditional branch cycles)
schedule_subblock_event_helper:
#ifdef VALIDATE_SCHEDULE
	ex (sp),hl
#endif
	push hl
	GET_BASE_ADDR_FAST
	ex de,hl
	add hl,de
	ld a,(hl)
	add a,a
	jr nc,_
	and 4
	jr z,++_
	;jp cond
	inc hl
	dec c
_
	;jr cond
	inc hl
_
	;ret cond
	inc hl
	dec c
	dec c
#ifdef VALIDATE_SCHEDULE
	call validate_schedule_resolved
	pop af
#endif
	ld a,iyl
	sub c
	jr c,schedule_event_later_resolved
	sbc hl,de
	ld.sis (event_gb_address),hl
	ld iyl,a
#ifdef DEBUG
	ld hl,event_value
	ld.sis (event_address),hl
#endif
	jp.sis do_event_pushed
	
schedule_jump_event_relative_slow:
	inc de
	ld l,(hl)
	ld a,l
	rla
	sbc a,a
	ld h,a
	add.s hl,de
	ex de,hl
	jr schedule_event_helper
	
schedule_jump_event_absolute_slow:
	; Handle possibly overlapped memory region
	ld a,(hl)
	inc d
	GET_BASE_ADDR_FAST
	add hl,de
	ld d,(hl)
	ld e,a
	jr schedule_event_helper
	
; Inputs: DE = Game Boy address of jump instruction plus 1
;         IX = starting recompiled address
;         IY = cycle count at end of sub-block (>= 0)
;         A = negative cycles for jump
;  (-1 for untaken JR/RET, -2 for untaken JP, -3 for taken JR, -4 for taken JP)
;         C = cycles until end of sub-block (plus 1 for non-taken jump)
schedule_slow_jump_event_helper:
#ifdef VALIDATE_SCHEDULE
	ex (sp),hl
#endif
	push hl
	add a,2
	jr c,schedule_bridge_event_slow
	inc.s de
	GET_BASE_ADDR_FAST
	add hl,de
	inc a
	jr z,schedule_jump_event_relative_slow
	; Check if jump target may overlap memory regions
	inc e
	jr z,schedule_jump_event_absolute_slow
	ld e,(hl)
	inc hl
	ld d,(hl)
	
; Inputs:  DE = starting Game Boy address
;          IX = starting recompiled address
;          IY = cycle count at end of sub-block (>= 0)
;          C = cycles until end of sub-block
; Outputs: HL = event Game Boy address
;          IX = event recompiled address
;          A = event cycles to sub-block end
schedule_event_helper:
	ld a,iyl
	sub c
	jr nc,schedule_event_now
schedule_event_later:
#ifdef VALIDATE_SCHEDULE
	call validate_schedule
	pop hl
#endif
	GET_BASE_ADDR_FAST
	push hl
	 add hl,de
	 ld de,opcounttable
	 push bc
	  ld bc,3
	  call opcycle_first
	 pop bc
	pop de
	or a
	sbc hl,de
	jp.sis schedule_event_finish

schedule_bridge_event_slow:
	dec c
	ld a,iyl
	sub c
	jr c,schedule_event_later
schedule_event_now:
#ifdef VALIDATE_SCHEDULE
	call validate_schedule
	pop hl
#endif
	ld.sis (event_gb_address),de
	ld iyl,a
#ifdef DEBUG
	ld hl,event_value
	ld.sis (event_address),hl
#endif
	; This is a code path that could target the flush handler
flush_event_smc_2 = $+1
	jp.sis do_event_pushed
	
_
	ld a,(hl)
	inc d
	GET_BASE_ADDR_FAST
	add hl,de
	ld e,a
	jr _
	
; Inputs: DE = Game Boy address at second byte of call instruction
;         IX = starting recompiled address
;         IY = cycle count at end of sub-block (>= 0)
;         C = cycles until end of sub-block (plus jump cycles, if applicable)
schedule_call_event_helper:
#ifdef VALIDATE_SCHEDULE
	ex (sp),hl
#endif
	push hl
	GET_BASE_ADDR_FAST
	add hl,de
	inc e
	jr z,-_
	ld e,(hl)
	inc hl
_
	ld d,(hl)
schedule_event_helper_for_call:
#ifdef VALIDATE_SCHEDULE
	call validate_schedule
	pop af
#endif
	ld a,iyl
	sub c
	; This is a code path that could target the flush handler
flush_event_for_call_smc = $+1
	jp.sis nc,schedule_event_finish_for_call_now
	GET_BASE_ADDR_FAST
	push hl
	 add hl,de
	 ld de,opcounttable
	 push bc
	  ld bc,3
	  call opcycle_first
	 pop bc
	pop de
	or a
	sbc hl,de
	jp.sis schedule_event_finish_for_call
	
#ifdef VALIDATE_SCHEDULE
validate_schedule_resolved:
	push af
	 push ix
	  push hl
	   push de
	    or a
	    sbc hl,de
		push hl
		jr _

validate_schedule:
	push af
	 push ix
	  push hl
	   push de
	    push de
_
	     push bc
	      lea.s bc,ix
	      call lookup_gb_code_address
	     pop bc
	     sub c
	     call nz,validate_schedule_nops
	    pop hl
	    sbc hl,de
	    jr nz,$
	   pop de
	  pop hl
	 pop ix
	pop af
	ret

validate_schedule_nops:
	jr c,$
	; Special case to handle NOPs, ugh
	ld b,a
	ld ix,opcounttable
	GET_BASE_ADDR_FAST
	add hl,de
_
	ld a,(hl)
	ld ixl,a
	ld a,(ix)
	sub opcycleNOP - opcycleroutines
	jr nz,$
	inc hl
	inc de
	djnz -_
	ret
#endif
	
	
; Inputs:  BCDEHL' are swapped
;          IX = current JIT address
;          HLA = desired trampoline opcodes
; Outputs: BCDEHL' are swapped
;          IX = current JIT address
;          HL = pointer to emitted cycle offset
; Preserves: BC
;	
resolve_mem_cycle_offset_helper:
	push bc
	 push af
	  push hl
	   lea bc,ix
	   push bc
	    call lookup_gb_code_address
	    push de
	     ld hl,(z80codebase+memroutine_next)
	     ld de,-6
	     add hl,de	;Sets C flag
	     ; Check if enough room for trampoline
	     ex de,hl
	     ld hl,(recompile_struct_end)
	     ld hl,(hl)
	     sbc hl,de	;Carry is set
	     ex de,hl
	    pop de
	   pop ix
	   jr nc,_
	   ld (z80codebase+memroutine_next),hl
	   ; Emit the error catcher
	   ld bc,ERROR_CATCHER
	   ld (hl),bc
	   inc hl
	   inc hl
	   inc hl
	   ; Emit the Game Boy address
	   ld (hl),de
	   inc hl
	   inc hl
	   ; Emit the block cycle offset
	   ld (hl),a
	   inc hl
	   ; Overwrite the JIT code with a call to the trampoline
	   ld.s (ix-2),hl
	   ld.s (ix-3),$CD
	  pop bc
	  ; Emit the trampoline code
	  ld (hl),bc
	  inc hl
	  inc hl
	 pop af
	 ld (hl),a
	pop bc
	; Return the trampoline pointer minus 1
	dec hl
	dec hl
	dec hl
	jp.sis get_mem_cycle_offset_continue
	
_
	  pop bc
	 pop bc
	pop bc
	scf
	; No trampoline was emitted, so use a scratch area to return the info
	ld hl,z80codebase + mem_cycle_scratch
	; Emit the Game Boy address
	ld (hl),de
	inc hl
	inc hl
	; Emit the block cycle offset
	ld (hl),a
	jp.sis get_mem_cycle_offset_continue
	
; Inputs:  BCDEHL' are swapped
;          IX = current JIT dispatch address
;          (SP+3) = call return GB address
; Outputs: BCDEHL' are swapped
;          IX = current JIT dispatch address
;          HL = second byte of scratch buffer, holding target GB address
;          A = negative cycle offset for CALL instruction
; Preserves: BC
get_mem_cycle_offset_for_call_helper:
	pop hl
	pop de
	push de
	push hl
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
	ld hl,z80codebase + mem_cycle_scratch
	ld (hl),a
	inc hl
	ld (hl),d
	ld a,-6
	ASSERT_NC
	jp.sis get_mem_cycle_offset_for_call_finish
	
	
mbc_change_rom_bank_helper:
	; Propagate the bank change to the topmost rom callstack entry
	ld c,a
	ld ix,0 - ((myADLstack & $FF) - $BF)
	add ix,sp
	ld a,$BF
_
	cp ixl
	jr z,_
	cp (ix+((myADLstack & $FF) - $BF)+1)
	lea ix,ix+CALL_STACK_ENTRY_SIZE_ADL
	jp po,-_
	lea hl,ix+((myADLstack & $FF) - $BF)-3+2
	ld a,(hl)
	xor c
	ld (hl),a
_
	ld c,3
	mlt bc
	ld hl,rombankLUT
	add hl,bc
	ld hl,(hl)
	ld (rom_bank_base),hl
	ld a,(z80codebase+curr_gb_stack_bank)
	cp 3 << 5
	jp.sis nz,mbc_2000_finish
	pop.s hl
	jp.sis mbc_fix_sp
	
mbc_rtc_latch_helper:
	call c,update_rtc_always
	ld ix,z80codebase+rtc_latched
	ld bc,(ix+5)
	ld a,c
	and $3F
	ld c,a
	ld a,b
	and $3F
	ld b,a
	ld (ix),bc
	ld bc,(ix+7)
	ld a,c
	and $1F
	ld c,a
	ld (ix+2),bc
	ld a,(ix+9)
	and $C1
	ld (ix+4),a
	ld a,mbc_6000_denied - (mbc_rtc_latch_smc+1)
	ld (z80codebase+mbc_rtc_latch_smc),a
	jp.sis mbc_6000_finish
	
mbc_rtc_helper:
	ld a,(z80codebase+cram_actual_bank_base+2)
	cp z80codebase>>16
	jr z,++_
	bit 3,c
	jp.sis z,mbc_ram
	call mbc_rtc_toggle_smc
_
	call update_rtc
	ld a,c
	and 7
	ld c,a
	ld b,0
	ld ix,z80codebase+rtc_latched
	jp.sis mbc_ram_any
_
	ld ix,(z80codebase+cram_actual_bank_base)
	ld a,(ix)
	cp (ix+5)
	jr nz,mbc_rtc_mark_latch_dirty
mbc_rtc_mark_latch_dirty_continue:	
	bit 3,c
	jr nz,--_
	call mbc_rtc_toggle_smc
	call update_rtc
	ld b,$60
	jp.sis mbc_ram
	
mbc_rtc_mark_latch_dirty:
	xor a
	ld (z80codebase+mbc_rtc_latch_smc),a
	jr mbc_rtc_mark_latch_dirty_continue
	
mbc_rtc_toggle_smc:
	ld a,(z80codebase+read_cram_bank_handler_smc)
	xor $DD ^ $FE	;ADD.L IX,DE vs CP.L $19
	ld (z80codebase+read_cram_bank_handler_smc),a
	ld (z80codebase+write_cram_bank_handler_smc_1),a
	ld (z80codebase+mem_read_any_rtc_smc),a
	ld (z80codebase+mem_write_any_cram_smc_1),a
	ld a,(memroutine_rtc_smc_1)
	ld b,a
	xor 0^5
	ld (memroutine_rtc_smc_1),a
	ld (z80codebase+write_cram_bank_handler_smc_2),a
	ld (z80codebase+mem_write_any_cram_smc_2),a
	ld a,(memroutine_rtc_smc_2)
	xor $18^$38	;JR vs JR C
	ld (memroutine_rtc_smc_2),a
	push hl
	 push de
	  ; Update stack access routines if current stack bank is CRAM
	  ld a,(z80codebase+curr_gb_stack_bank)
	  sub 5 << 5
	  jr nz,_
	  ld ix,cram_bank_base
	  cp b
	  call.il set_gb_stack_cram_bank_helper
_
	  ld ix,z80codebase
	  ld hl,memroutineLUT + $1A0
	  ld b,32
mbc_rtc_memroutine_smc_loop_restore:
	  xor a
mbc_rtc_memroutine_smc_loop:
	  or (hl)
	  jr nz,_
	  inc l
	  djnz mbc_rtc_memroutine_smc_loop
	 pop de
	pop hl
	ret
_
	  ld ixh,a
	  dec h
	  ld a,(hl)
	  ld ixl,a
	  inc h
	  ld a,l
	  cp $A4
	  lea de,ix+15
	  jr c,_
	  inc de
	  lea ix,ix+2
	  cp $BE
	  jr nz,_
	  inc ix
_
	  ld a,(de)
	  xor $DD ^ $FE	;ADD.L IX,rr vs CP.L nn
	  ld (de),a
	  ld de,(ix+20)
	  ld a,e
	  sub $70
	  cp 8
	  jr nc,_
	  ld a,d
	  xor 0^5
	  ld (ix+21),a
_
	  inc l
	  djnz mbc_rtc_memroutine_smc_loop_restore
	 pop de
	pop hl
	ret
	
update_rtc_always:
	ld ix,mpRtcSecondCount
	jr _
update_rtc:
	ld ix,mpRtcSecondCount
	; Early out if the timer has not changed since last update
	bit 0,(ix-mpRtcSecondCount+mpRtcIntStatus)
	ret z
	xor a
	ld (z80codebase+mbc_rtc_latch_smc),a
_
	push hl
	 push de
_
	  ld b,(ix)
	  ld e,(ix-mpRtcSecondCount+mpRtcMinuteCount)
	  ld d,(ix-mpRtcSecondCount+mpRtcHourCount)
	  ld hl,(ix-mpRtcSecondCount+mpRtcDayCount)
	  ; Clear the second interrupt status before reloading the second counter
	  ld (ix-mpRtcSecondCount+mpRtcIntStatus),1
	  ld a,(ix)
	  cp b
	  jr nz,-_
	  
	  ; If the RTC is halted, set the last time to now
	  ld ix,z80codebase+rtc_last
	  bit 6,(ix-1)
	  jp nz,update_rtc_halted
	
	  ; Get the time since last update
	  sub (ix)
	  ld (ix),b
	  jr nc,_
	  add a,60
_
	  ld b,a

	  ld a,e
	  sbc a,(ix+1)
	  ld (ix+1),e
	  jr nc,_
	  add a,60
_
	  ld e,a
	  
	  ld a,d
	  sbc a,(ix+2)
	  ld (ix+2),d
	  jr nc,_
	  add a,24
_
	  ld d,a
	  
	  ld a,l
	  sbc a,(ix+3)
	  ld (ix+3),l
	  ld l,a
	  
	  ld a,h
	  sbc a,(ix+4)
	  ld (ix+4),h
	  ld h,a
	  
	  lea ix,ix-5
	  ld a,(ix)
	  ; Adjust carry range
	  add a,64-60
	  or $C0
	  ; Add elapsed seconds
	  add a,b
	  jr c,++_
_
	  ; Adjust back to normal range
	  add a,60
	  jr c,++_
	  ; Value was out of range, but if any minutes elapsed
	  ; then consume one of them to go back into range
	  call update_rtc_dec_minutes
	  jr nc,-_
	  ; No minutes available, so remain out of range
	  ld b,a
	  jr update_rtc_seconds_only
_
	  ; Increment elapsed minutes due to carry
	  inc e
_
	  ld b,a
	  
	  ld a,(ix+1)
	  ; Adjust carry range
	  add a,64-60
	  or $C0
	  ; Add elapsed minutes
	  add a,e
	  jr c,++_
_
	  ; Adjust back to normal range
	  add a,60
	  jr c,++_
	  ; Value was out of range, but if any hours elapsed
	  ; then consume one of them to go back into range
	  call update_rtc_dec_hours
	  jr nc,-_
	  ; No hours available, so remain out of range
	  ld (ix+1),a
	  jr update_rtc_seconds_only
_
	  ; Increment elapsed hours due to carry
	  inc d
_
	  ld e,a
	  
	  ld a,(ix+2)
	  ; Adjust carry range
	  add a,32-24
	  or $E0
	  ; Add elapsed hours
	  add a,d
	  jr c,++_
_
	  ; Adjust back to normal range and reset carry
	  sub -24
	  jr nc,_
	  ; Value was out of range, but if any days elapsed
	  ; then consume one of them to go back into range
	  call update_rtc_dec_days
	  jr nc,-_
	  ; No days available, so remain out of range
	  ld d,a
	  jr update_rtc_hours_only
_
	  ld d,a
	  
	  ; Add low byte of days, along with carry
	  ld a,(ix+3)
	  adc a,l
	  ld l,a
	  
	  ; Add carry to days, carry out if at least 2 days passed
	  ld a,h
	  adc a,$FE
	  ; Put carry in the high bit and bit 8 of the day in the low bit
	  rla
	  rrca
	  and $81
	  ld h,a
	  ; Add to the existing value, ensuring carry from low bit
	  ; propagates to the high bit
	  ld a,(ix+4)
	  or $7E
	  add a,h
	  jr nc,_
	  ; Make sure overflow bit sticks
	  or $80
_
	  ; Mask result (ensures halt bit is reset)
	  and $81
	  ld h,a
	  
update_rtc_halted:
	  ld.s (ix+3),hl
update_rtc_hours_only:
	  ld.s (ix+1),de
update_rtc_seconds_only:
	  ld (ix),b
	 pop de
	pop hl
	ret
	
update_rtc_dec_minutes:
	dec e
	ret p
	ld e,59
update_rtc_dec_hours:
	dec d
	ret p
	ld d,23
update_rtc_dec_days:
	dec hl
	push hl
	 add hl,hl
	pop hl
	ret