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
	ld (z80codebase+callstack_pop_compare_smc_1),a
	ld (z80codebase+callstack_pop_compare_smc_2),a
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
	; Override offset for CALL to special-case return value
	cp do_push_ports - (do_push_and_return_jump_smc+1)
	jr nz,_
	ld a,do_push_and_return_ports - (do_push_and_return_jump_smc+1)
_
	ld (z80codebase+do_push_and_return_jump_smc),a
	ld a,h
	ld (z80codebase+do_push_for_call_jump_smc_1),a
	sub do_push_for_call_jump_smc_2 - do_push_for_call_jump_smc_1
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
	ld (z80codebase+callstack_pop_check_overflow_smc),hl
	ld hl,(ix+13)
	ld (z80codebase+callstack_pop_check_overflow_smc+3),hl
	ld a,ixl
	add a,a
	ex de,hl
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
	ld iyl,a
	push bc
	 push hl
	  ld hl,$C3 | (schedule_event_finish << 8)	;JP schedule_event_finish
	  ld (flush_event_smc),hl
flush_mem_finish:
	  call flush_code
	  push de
	   call lookup_code
	  pop de
	 pop hl
	pop bc
	ld.sis sp,myz80stack-2
	ld c,a
	exx
	ld (_+2),a
	ld a,CALL_STACK_DEPTH+1
	ld i,a
_
	lea iy,iy+0
	xor a
	cp iyh
	jr z,_
	ld a,iyl
	ex af,af'
	jp.s (ix)
_
	push.s hl
	 push.s de
	  push.s bc
	   push.s ix
	    exx
	    ld a,c
	    push de
	     exx
	    pop de
	    ld c,a
	    jp schedule_event_helper
	
; Flushes the JIT code and recompiles anew.
; Does not use a traditional call/return, must be jumped to directly.
;
; When the memory routine generator overflows, it returns a pointer to
; flush_mem_handler, which provides this routine with the JIT address.
; The JIT address is reversed into the GB address before flushing normally.
; Recompilation starts at the offending memory instruction.
;
; Inputs:  BC = address directly following recompiled memory instruction
;          BCDEHL' have been swapped
; Outputs: JIT is flushed and execution begins at the new recompiled block
;          BCDEHL' have been unswapped
flush_mem:
	push af
	 push hl
	  call lookup_gb_code_address
	  ; All memory routines are for 1-byte, 2-cycle instructions
	  dec de
	  cpl
	  dec a
	  ld c,a
	  ld b,$FF
	  add iy,bc
	  jr flush_mem_finish
	
	
; Catches up the renderer before changing an LCD register.
; Must be called only if the current frame is being rendered.
;
; Inputs:  E = current scanline
;          AF' has been swapped
;          BCDEHL' have been swapped
; Outputs: Scanlines rendered if applicable
; Destroys: AF, DE
render_catchup:
	ld a,(hram_base+LCDC)
	add a,a
	ret nc
	ld a,d
	cp (MODE_2_CYCLES + MODE_3_CYCLES)<<1
	ld a,e
	sbc a,9
	ret c
	push bc
	 push hl
	  call render_scanlines
	 pop hl
	pop bc
	ret
	
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
	  ld a,(render_this_frame)
	  or a
	  call nz,sprite_catchup
	  ; Copy 160 bytes from the specified source address to OAM
	  ld de,0
	  ex af,af'
	  ld d,a
	  ex af,af'
	  call get_base_address
	  add hl,de
	  ld bc,$00A0
	  ld de,hram_start
	  ldir
	 pop bc
scroll_write_no_change:
	pop hl
	exx
	ld a,iyl
	ex af,af'
	pop.s ix
	jp.s (ix)
	
; Writes to an LCD scroll register (SCX,SCY,WX,WY). Also BGP and DMA.
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  HL = 16-bit register address
;          E = current scanline
;          A' = value being written
;          AF' has been swapped
;          BCDEHL' have been swapped
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
; Outputs: Scanlines rendered if applicable, SMC applied, value written
;          AF' has been unswapped
scroll_write_helper:
	 ld d,a
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,c
	 cp.s (hl)
	 jr z,scroll_write_no_change
render_this_frame = $+1
	 ld a,1
	 or a
	 call nz,render_catchup
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
	 ; Only do things if BGP is changing
	 ld.s a,(hl)
	 cp c
	 jr z,scroll_write_done_swap
	 ld a,(render_this_frame)
	 or a
	 jr z,scroll_write_done_swap
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
	exx
	pop.s ix
	jp.s (ix)
	
; Writes to the LY compare register (LYC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Sets the new cycle target according to the new value of LYC.
; Triggers a GB interrupt if LY already matches the new LYC value,
; but only if LYC is changing.
;
; Inputs:  A' = value being written
;          A = cycles into scanline
;          E = current scanline
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
;          BCDEHL' are swapped
; Outputs: LYC and cycle targets updated
lyc_write_helper:
	 ; If LYC interrupts are disabled, update the value and return
	 ld hl,hram_base + STAT
	 bit 6,(hl)
	 ld l,LYC - ioregs
	 jr z,scroll_write_done_swap
	 ld c,a
	 ex af,af'
	 ld d,a
	 ex af,af'
	 ; If writing the same value, don't adjust any event schedules
	 ld a,(hl)
	 cp d
	 jr z,scroll_write_done_swap
	 ld (hl),d
lyc_update_helper:
	 ; If the old value is an invalid line number, possibly enable LYC counter checking
	 cp SCANLINES_PER_FRAME
	 ld a,d
	 jr c,_
	 cp SCANLINES_PER_FRAME
	 jr nc,scroll_write_done_swap
	 ld ix,lyc_counter_checker
	 ld.sis (event_counter_checker_slot_LYC),ix
_
	 cp SCANLINES_PER_FRAME
	 jr nc,lyc_write_helper_disable_checker
	 
	 ; Convert LYC to vblank-relative range
	 sbc a,143	; Sets half-carry flag
	 jr nc,_
	 daa
_
	 ld d,a
	 
	 ; Scanline 153 changes to scanline 0 after 2 cycles for LYC purposes
	 ld a,e
	 cp 9
	 jr nz,_
	 ld a,c
	 cp 2<<1
	 sbc a,a
	 inc a
	 add a,e
_
	 ; Trigger an interrupt if setting the current scanline
	 cp d
	 jr nz,_
	 ld l,IF - ioregs
	 set 1,(hl)
_
	 
	 ; Set new target
	 ld.sis hl,(vblank_counter)
	 ; If the current scanline is prior to the scheduled scanline, schedule
	 ; relative to this frame instead of next frame
	 jr nc,_
	 push de
	  ld de,-CYCLES_PER_FRAME
	  add hl,de
	 pop de
_
	 ; Adjust scanline 0 if necessary
	 ld a,d
	 cp 10
	 jr nz,_
	 dec d
	 ; TODO: Double-speed mode should increase by 4
	 inc hl
	 inc hl
_
	 ld e,CYCLES_PER_SCANLINE
	 mlt de
	 add hl,de
	 ld.sis (lyc_counter),hl
	 jp.sis trigger_event_pushed
	
lyc_write_helper_disable_checker:
	 ld hl,disabled_counter_checker
	 ld.sis (event_counter_checker_slot_LYC),hl
	 jp.sis trigger_event_pushed
	
	
stat_write_helper:
	 ld hl,hram_base + STAT
	 ld c,a
	 ex af,af'
	 ld d,a
	 ex af,af'
	 ld a,(hl)
	 ld (hl),d
	 ; Save changed mode interrupt bits
	 xor d
	 push af
	  ; Check for newly set mode interrupt bits
	  and d
	  and $38
	  jr z,stat_write_no_interrupt
	  ld l,a
	  ld a,e
	  cp 10
	  jr nc,_
	  bit 4,l
	  jr z,stat_write_no_interrupt
	  jr stat_write_set_interrupt
_
	  ld a,c
	  bit 5,l
	  jr z,_
	  cp MODE_2_CYCLES<<1
	  jr c,stat_write_set_interrupt
_
	  bit 3,l
	  jr z,stat_write_no_interrupt
	  cp (MODE_2_CYCLES + MODE_3_CYCLES)<<1
	  jr c,stat_write_no_interrupt
stat_write_set_interrupt:
	  ld l,IF & $FF
	  set 1,(hl)
stat_write_no_interrupt:
	 pop af
	 push af
	  and $28
	  jr z,stat_write_helper_no_change
	  ld ix,disabled_counter_checker
	  ld a,d
	  and $28
	  jr z,stat_write_helper_set_checker
	  push de
	   ld ix,stat_counter_checker_mode0
	   ld a,(MODE_2_CYCLES + MODE_3_CYCLES)<<1 - 1
	   cp c
	   inc a
	   jr nc,_
	   bit 5,d
	   jr nz,++_
	   jr +++_
_
	   bit 3,d
	   jr nz,++_
_
	   lea ix,ix-stat_counter_checker_mode0+stat_counter_checker_mode2
	   xor a
	   scf
_
	   ld d,a
	   ; Calculate the cycle offset relative to vblank
	   ld a,SCANLINES_PER_FRAME
	   sbc a,e
	   jr nz,_
	   ld (z80codebase + stat_line_count),a
	   ld a,d
	   ld de,-(CYCLES_PER_SCANLINE * 10)
	   jr +++_
_
	   cp 144
	   jr c,_
	   ld a,144
_
	   ld (z80codebase + stat_line_count),a
	   ld e,a
	   ld a,d
	   ld d,CYCLES_PER_SCANLINE
	   mlt de
_
	   ld.sis hl,(vblank_counter)
	   add a,l
	   ld l,a
	   jr nc,_
	   inc h
	   or a
_
	   sbc hl,de
	   ld.sis (stat_counter),hl
	  pop de
	  ld a,d
	  cpl
	  and $28
	  jr z,stat_write_helper_set_checker
	  ld ix,stat_counter_checker_single
stat_write_helper_set_checker:
	  ld.sis (event_counter_checker_slot_STAT),ix
stat_write_helper_no_change:
	
	 pop af
	 bit 6,a
	 jr z,++_
	 bit 6,d
	 jr z,_
	 ld hl,hram_base + LYC
	 ld d,(hl)
	 ld a,SCANLINES_PER_FRAME
	 jp lyc_update_helper
_
	 ld hl,disabled_counter_checker
	 ld.sis (event_counter_checker_slot_LYC),hl
_
	 and $28
	 jp.sis nz,trigger_event_pushed
	pop hl
	exx
	ld a,iyl
	ex af,af'
	pop.s ix
	jp.s (ix)
	
; Writes to the LCD control register (LCDC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  A' = value being written
;          HL = current cycle offset (negative)
;          E = current scanline
;          AF' has been swapped
;          BCDEHL' have been swapped
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
; Outputs: Scanlines rendered if applicable, SMC applied, value written
;          AF' has been unswapped
;          BCDEHL' have been unswapped
lcdc_write_helper:
	 ld d,a
	 ld a,(render_this_frame)
	 or a
	 call nz,render_catchup
	 ex de,hl
	 ld hl,hram_base+LCDC
	 ld a,(hl)
	 ex af,af'
	 ld (hl),a
	 ex af,af'
	 xor (hl)
	 ld c,a
	 bit 0,c
	 jr z,_
	 ld a,(LCDC_0_smc)
	 xor $39 ^ $31	;ADD.SIS HL,SP \ LD.SIS SP,HL vs LD.SIS SP,$F940
	 ld (LCDC_0_smc),a
_
	 bit 1,c
	 jr z,_
	 ld a,(render_this_frame)
	 or a
	 push bc
	  push de
	   call nz,sprite_catchup
	  pop de
	 pop bc
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
	 ; Forcibly skip to scanline 0
	 ld.sis hl,(div_counter)
	 add hl,de
	 ex de,hl
	 ld hl,MODE_2_CYCLES + MODE_3_CYCLES
	 ld a,(hram_base+STAT)
	 ld c,a
	 ld a,144
	 ld (z80codebase + stat_line_count),a
	 xor a
	 bit 5,c
	 jr z,++_
	 bit 3,c
	 jr z,_
	 ld hl,stat_counter_checker_mode2
	 ld.sis (event_counter_checker_slot_STAT),hl
_
	 sbc hl,hl
_
	 add hl,de
	 ld.sis (stat_counter),hl
	 ld hl,CYCLES_PER_SCANLINE * 144
	 add hl,de
	 ld.sis (vblank_counter),hl
	 ld hl,(hram_base+LYC)
	 cp l
	 ld h,CYCLES_PER_SCANLINE
	 mlt hl
	 jr nz,++_
	 bit 6,c
	 jr z,_
	 ld hl,hram_base+IF
	 set 1,(hl)
_
	 ld hl,CYCLES_PER_SCANLINE * 153 + 2
_
	 add hl,de
	 ld.sis (lyc_counter),hl
	 jp.sis trigger_event_pushed
	
	
div_write_helper:
	 push de
	  or a
	  sbc hl,hl
	  ld.sis (audio_counter),hl
	  sbc hl,de
	  ld.sis de,(div_counter)
	  ld.sis (div_counter),hl
	  or a
	  sbc hl,de
	  ex de,hl
	  ld.sis hl,(vblank_counter)
	  add hl,de
	  ld.sis (vblank_counter),hl
	  ld.sis hl,(lyc_counter)
	  add hl,de
	  ld.sis (lyc_counter),hl
	  ld.sis hl,(stat_counter)
	  add hl,de
	  ld.sis (stat_counter),hl
	  ld.sis hl,(serial_counter)
	  add hl,de
	  ld.sis (serial_counter),hl
	 pop de
	 jr tima_write_helper
	
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
	exx
	ld a,iyl
	ex af,af'
	pop.s ix
	jp.s (ix)
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
	 
	 ld.sis hl,(div_counter)
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
	
; Inputs: DE = Game Boy address at conditional branch
;         IX = recompiled address after conditional branch
;         IY = cycle count at end of sub-block (>= 0)
;         C = cycles until end of sub-block (including conditional branch cycles)
schedule_subblock_event_helper:
	inc ix
	push.s ix
	call get_base_address
	push hl
	 add hl,de
	 ld a,(hl)
	 add a,a	
	 jr nc,_
	 and 4
	 jr z,++_
	 ;jp cond
	 inc de
	 inc hl
	 dec c
_
	 ;jr cond
	 inc de
	 inc hl
_
	 ;ret cond
	 inc de
	 inc hl
	 dec c
	 dec c
	 jr schedule_event_helper_resolved
	
; Inputs: DE = Game Boy address at last byte of call instruction
;         IX = starting recompiled address
;         IY = cycle count at end of sub-block (>= 0)
;         C = cycles until end of sub-block (plus jump cycles, if applicable)
schedule_call_event_helper:
	call get_base_address
	add hl,de
	ld d,(hl)
	dec hl
	ld e,(hl)
	jr schedule_event_helper
	

; Inputs: DE = Game Boy address of jump instruction
;         IX = starting recompiled address
;         IY = cycle count at end of sub-block (>= 0)
;         B = recompiled jump opcode
;         C = cycles until end of sub-block (plus jump cycles, if applicable)
schedule_jump_event_helper:
	call get_base_address
	add hl,de
	ld a,(hl)
	cp b
	jr z,schedule_jump_event_absolute
	cp $18
	jr z,schedule_jump_event_relative
	bit 0,b
	jr nz,schedule_event_helper
schedule_jump_event_relative:
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
	jr _
schedule_jump_event_absolute:
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	dec c
_
	dec c \ dec c \ dec c
	
; Inputs:  DE = starting Game Boy address
;          IX = starting recompiled address
;          IY = cycle count at end of sub-block (>= 0)
;          C = cycles until end of sub-block
; Outputs: HL = event Game Boy address
;          IX = event recompiled address
;          A = event (negative) cycles to sub-block end
schedule_event_helper:
	call get_base_address
	push hl
	 add hl,de
schedule_event_helper_resolved:
	 bit 7,d
	 jr nz,schedule_event_check_ram_prefix
schedule_event_continue:
#ifdef 0
	 ld a,ixh
	 cp flush_handler >> 8
	 jr nz,_
	 ld a,ixl
	 cp flush_handler & $FF
	 jr z,++_
_
	 push ix
	  push hl
	   push de
	    push bc
	     lea.s bc,ix
	     call lookup_gb_code_address
	    pop bc
	    sub c
	    call nz,validate_gb_code_address
	    ex de,hl
	   pop de
	   sbc hl,de
	   jr nz,$
	  pop hl
	 pop ix
_
#endif
	 
	 ld a,iyl
	 sub c
	 jr nc,schedule_event_now
	 
	 ld de,opcounttable
	 ld bc,3
	 call opcycle_first
schedule_event_now:
	 sub iyl
	 or a
schedule_event_ram_prefix:
	pop de
	sbc hl,de
flush_event_smc = $+1
	jp.sis schedule_event_finish
	
schedule_event_check_ram_prefix:
	 ld.s a,(ix)
	 cp $CD
	 jr nz,schedule_event_continue
	 ld.s de,(ix+1)
	 ld a,e
	 cp coherency_handler & $FF
	 jr nz,schedule_event_continue
	 ld a,d
	 sub coherency_handler >> 8
	 jr nz,schedule_event_continue
	 jr schedule_event_ram_prefix
	
#ifdef DEBUG
validate_gb_code_address:
	jr c,$
	; Special case to handle NOPs, ugh
	ld b,a
	ld ix,opcounttable
	call get_base_address
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
;          HLC = desired trampoline opcodes
; Outputs: BCDEHL' are swapped
;          IX = current JIT address
;          HL = current GB address
;          A = NEGATIVE cycles to add for current sub-block position
; Preserves: B
;	
resolve_mem_cycle_offset_helper:
	push bc
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
	pop bc
	ld (hl),c
	; Return the trampoline pointer minus 1
	dec hl
	dec hl
	dec hl
	jp.sis get_mem_cycle_offset_continue
	
	
; Inputs:  BCDEHL' are swapped
;          IX = current JIT address
;          DE = call return GB address
;          A = cycles taken by call
; Outputs: BCDEHL' are swapped
;          IX = current JIT address
;          HL = current GB address
;          A = NEGATIVE cycles to add for current sub-block position
; Preserves: B
resolve_mem_cycle_offset_for_call_helper:
	push af
	 call get_base_address
	 add hl,de
	 dec hl
	 ld d,(hl)
	 dec hl
	 ld e,(hl)
	pop af
	add a,-6	; Sets the carry flag
	ASSERT_C
	.db $21	;LD HL,
_
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
;          DE = current GB address
;          IX = current JIT address
;          A = NEGATIVE cycles corresponding to current sub-block position
;          C = NEGATIVE cycles to add for current sub-block position (may differ when exiting HALT)
; Outputs: BCDEHL' are swapped
;          DE = current GB address with bank information
;          IX = current JIT address
;          A = NEGATIVE cycles to add for current sub-block position
; Preserves: HL,B
;
dispatch_int_helper:
	exx
int_return_sp = $+1
	ld hl,0
	ld b,(hl)
	djnz _
	scf
	sbc hl,hl
	ld (int_cached_return),hl
	ld hl,z80codebase+int_return_stack
_
	ld (hl),a
	ld bc,(int_cached_return)
	inc hl
	ld (hl),bc
	inc hl
	inc hl
	inc hl
	ld.s (hl),ix
	inc hl
	inc hl
	ld (int_return_sp),hl
	exx
	call get_banked_address
	ld (int_cached_return),de
	xor a
	jp.sis dispatch_int_continue
	
mbc_rtc_latch_helper:
	exx
	ex af,af'
	ld c,a
	ex af,af'
mbc_rtc_last_latch = $+1
	ld a,0
	cpl
	and c
	rra
	jr nc,_
	push bc
	 call update_rtc
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
	pop bc
_
	ld a,c
	and 1
	ld (mbc_rtc_last_latch),a
	exx
	jp.sis mbc_6000_denied
	
mbc_rtc_helper:
	ld a,(cram_bank_base+2)
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
	bit 3,c
	jr nz,--_
	call mbc_rtc_toggle_smc
	call update_rtc
	ld b,$60
	jp.sis mbc_ram
	
	
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