; Get a literal 24-bit pointer to the Game Boy stack.
; Does not use a traditional call/return, must be jumped to directly.
;
; This routine is jumped to whenever SP is set to a new value, excluding
; increment/decrement operations. In addition, if SP points to ROM,
; push operations are modified to disable the memory writes.
;
; Inputs:  HL = 16-bit Game Boy SP
;          IX = Z80-mode return address
;          BCDEHL' have been swaped
; Outputs: HL' = 24-bit literal SP
;          BCDEHL' have been unswapped
;          SMC applied to push operations
set_gb_stack:
	ex af,af'
	ex de,hl
	ld a,d
	add a,a
	jr c,_
	add a,a
	ld a,$53	;LD D,E
	ld hl,(rom_start)
	jr nc,set_gb_stack_done
	ld hl,(rom_bank_base)
	jr set_gb_stack_done
_
	cp -2*2
	jr nc,_
	ld hl,vram_base
	add a,$40
	jp po,set_gb_stack_done_ram
	ld hl,(cram_bank_base)
	jr set_gb_stack_done_ram
_
	ld hl,hram_base
set_gb_stack_done_ram:
	ld a,$72	;LD (HL),D
set_gb_stack_done:
	ld (z80codebase+sp_base_address),hl
	add hl,de
	exx
	ld (z80codebase+do_push_smc_1),a
	ld (z80codebase+do_push_smc_3),a
	or 1	;LD D,E or LD (HL),E
	ld (z80codebase+do_push_smc_2),a
	ld (z80codebase+do_push_smc_4),a
	ex af,af'
	ei
	jp.s (ix)

	
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
	push hl
flush_mem_finish:
	 call lookup_code
	pop hl
	ld.sis sp,myz80stack-2
	ld bc,(CALL_STACK_DEPTH+1)*256
	exx
	ex af,af'
	ei
	jp.s (ix)
	
; Flushes the JIT code and recompiles anew.
; Does not use a traditional call/return, must be jumped to directly.
;
; When the memory routine generator overflows, it returns a pointer to
; flush_mem_handler, which provides this routine with the JIT address.
; The JIT address is reversed into the GB address before flushing normally.
; Recompilation starts at the offending memory instruction.
;
; Inputs:  DE = address directly following recompiled memory instruction
;          BCDEHL' have been swapped
; Outputs: JIT is flushed and execution begins at the new recompiled block
;          BCDEHL' have been unswapped
flush_mem:
	ex af,af'
	dec de
	dec de
	dec de
	push hl
	 call.il lookup_gb_code_address
	 jr flush_mem_finish
	
; Handles an OAM transfer operation.
; Does not use a traditional call/return, must be jumped to directly.
; 
; Inputs:  A' = value being written to DMA register
;          AF' has been swapped
;          (SPS) = Z80 return address
; Outputs: OAM written with 160 bytes from source address
;          AF' has been unswapped
oam_transfer_helper:
	xor a
	ld (mpTimerCtrl),a
	exx
	sbc hl,hl
	ex de,hl
	ex af,af'
	ld d,a
	ex af,af'
	call get_base_address
	add hl,de
	ld a,b
	ld bc,$00A0
	ld de,hram_start
	ldir
	ld b,a
	exx
	pop.s ix
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ex af,af'
	jp.s (ix)
	
; Gets the current value of LY, after disabling timers.
; Returns 0 if the LCD is disabled.
;
; Inputs:  AFBCDEHL' have been swapped
; Outputs: A = current LY (0-153)
;          Timers are disabled
updateLY_ADL:
	xor a
	ld (mpTimerCtrl),a
	ld a,(hram_base+LCDC)
	add a,a
	sbc a,a
	ret nc
	ld hl,(mpTimer1Count+1)
	ld de,-SCANDELAY*128
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	ld a,153
	sub l
	ret
	
; Catches up the renderer before changing an LCD register.
; Must be called only if the current frame is being rendered.
;
; Inputs:  AF' has been swapped
; Outputs: Scanlines rendered if applicable
render_catchup:
	exx
	call updateLY_ADL
	cp 144
	push bc
	 call c,render_scanlines
	pop bc
	exx
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ret
	
; Writes to an LCD scroll register (SCX,SCY,WX,WY).
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  IX = 16-bit register address
;          A' = value being written
;          AF' has been swapped
;          (SPS) = Z80 return address
; Outputs: Scanlines rendered if applicable, SMC applied, value written
;          AF' has been unswapped
scroll_write_helper:
render_this_frame = $+1
	ld a,1
	or a
	call nz,render_catchup
	ld a,ixl
	sub SCY - ioregs
	jr nz,_
	ex af,af'
	ld (SCY_smc),a
	jr scroll_write_done
_
	sub WY - SCY
	jr c,scroll_write_SCX
	jr nz,scroll_write_WX
	ex af,af'
	ld (WY_smc),a
	jr scroll_write_done
	
scroll_write_WX:
	exx
	ex af,af'
	ld c,a
	ex af,af'
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
	exx
	ex af,af'
	ld c,a
	ex af,af'
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
	exx
	ex af,af'
scroll_write_done:
	ld.s (ix),a
	pop.s ix
	jp.s (ix)
	
; Writes to the LCD control register (LCDC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  A' = value being written
;          AF' has been swapped
;          (SPS) = Z80 return address
; Outputs: Scanlines rendered if applicable, SMC applied, value written
;          AF' has been unswapped
lcdc_write_helper:
	ld a,(render_this_frame)
	or a
	call nz,render_catchup
	exx
	ld hl,hram_base+LCDC
	ld a,(hl)
	ex af,af'
	ld (hl),a
	ex af,af'
	xor (hl)
	ld l,a
	bit 0,l
	jr z,_
	ld a,(LCDC_0_smc)
	xor $39 ^ $31	;ADD.SIS SP,HL \ LD.SIS SP,HL vs LD.SIS SP,$F940
	ld (LCDC_0_smc),a
_
	bit 1,l
	jr z,_
	ld a,(LCDC_1_smc)
	xor $77 ^ $1F	;LD (IY),A vs LD (IY),DE
	ld (LCDC_1_smc),a
_
	bit 2,l
	jr z,_
	ld a,(LCDC_2_smc_1)
	xor $38^$78
	ld (LCDC_2_smc_1),a
	ld a,(LCDC_2_smc_2)
	xor 8^16
	ld (LCDC_2_smc_2),a
	ld a,(LCDC_2_smc_3)
	xor $80 ^ $81	;RES 0,B vs RES 0,C
	ld (LCDC_2_smc_3),a
	
_
	bit 3,l
	jr z,_
	ld a,(LCDC_3_smc)
	xor (vram_tiles_start ^ (vram_tiles_start + $2000)) >> 8
	ld (LCDC_3_smc),a
_
	bit 4,l
	jr z,_
	ld a,(LCDC_4_smc)
	xor $80
	ld (LCDC_4_smc),a
	ld (window_tile_ptr),a
_
	bit 5,l
	jr z,_
	ld a,(LCDC_5_smc)
	xor $08	;JR NC vs JR C
	ld (LCDC_5_smc),a
_
	bit 6,l
	jr z,_
	ld a,(window_tile_ptr+1)
	sub (vram_tiles_start >> 8) & $FF
	xor $20
	add a,(vram_tiles_start >> 8) & $FF
	ld (window_tile_ptr+1),a
_
	bit 7,l
	jr z,_
	ld a,(LCDC_7_smc)
	xor $08	;JR NZ vs JR Z
	ld (LCDC_7_smc),a
	xor a
	ld (mpTimerCtrl),a
	sbc hl,hl
	ld (mpTimer1Count),hl
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
_
	ex af,af'
	exx
	pop.s ix
	jp.s (ix)
	
; Post-write operation for the LY compare register (LYC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Sets the host timer match value according to the new value of LYC.
; Triggers a GB interrupt if LY already matches the new LYC value.
;
; Inputs:  A = value being written
;          (SPS) = Z80 return address
; Outputs: Host timer match register updated
lyc_write_helper:
	exx
	ld c,a
	ex af,af'
	call updateLY_ADL
	cp c
	jr nz,_
	ld hl,hram_base+STAT
	bit 6,(hl)
	jr z,_
	ld l,IF & $FF
	set 1,(hl)
_
	ld a,154
	sub c
	ld l,a
	ld h,SCANDELAY
	mlt hl
	dec hl
	ld (mpTimer1Match1+1),hl
	exx
	pop.s ix
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ex af,af'
	jp.s (ix)
	
; Skips a certain number of emulated cycles. Used for HALT or waitloops.
; Attempting to skip past the end of a scanline is undefined behavior.
;
; In every case it attempts to skip to a multiple of 256 on the scanline timer,
; so only the minimum number of cycles times 256 is passed to the routine.
; Generally, the residue output from updateLY is used to skip to the end of the
; current scanline, but skipping to a different scanline mode is also possible.
; If the number of consumed cycles causes a GB timer overflow, the number of
; skipped cycles is limited to the remaining cycles on that timer.
;
; Inputs:  H = number of cycles to skip, times 256
;          HLU = 0
;          Timers are disabled
; Outputs: Cycles skipped from all applicable timers
;          Timers are enabled
skip_cycles:
	ld ix,mpTimer1Count
	ld l,(ix)
	ex de,hl
	ld a,(hram_base+TAC)
	and 4
	jr z,++_
	ld hl,(ix-mpTimer1Count+mpTimer2Count)
	sbc hl,de
	jr nc,_
	add hl,de
	ex de,hl
	or a
	sbc hl,hl
_
	ld (ix-mpTimer1Count+mpTimer2Count),hl
_
	ld hl,(ix)
	sbc hl,de
	ld (ix),hl
	ld (ix-mpTimer1Count+mpTimerCtrl),TMR_ENABLE
	ret.l
	
; Updates the current value of the GB timer counter (TIMA).
;
; Inputs:  AF' has been swapped
; Outputs: Current value written to (TIMA)
updateTIMA:
	xor a
	ld (mpTimerCtrl),a
	exx
	ld hl,(mpTimer2Count)
	dec hl
updateTIMA_smc = $+1
	ld de,0
	add hl,de \ jr c,$+4 \ sbc hl,de \ rla \ add hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ rla \ add hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ rla \ add hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ rla \ add hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ rla \ add hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ rla \ add hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ rla \ add hl,hl
	add hl,de
	exx
	rla
	cpl
	ld (hram_base+TIMA),a
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ret.l
	
; Writes to the GB timer control (TAC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Updates the GB timer based on the new mode; applies SMC to getters/setters
;
; Inputs:  A' = value being written
;          (SPS) = Z80 return address
;          AF' has been swapped
; Outputs: Current value written to (TAC)
;          GB timer updated
tac_write_helper:
	; If the timer was enabled, update TIMA
	ld a,(hram_base+TAC)
	bit 2,a
	call.il nz,updateTIMA
	; Set new TAC value
	exx
	ex af,af'
	ld (hram_base+TAC),a
	ld l,a
	ex af,af'
	bit 2,l
	jr nz,_
	; If disabling timer, disable interrupt as well
	ld hl,mpIntEnable
	res 2,(hl)
	exx
	jr ++_
_
	; Apply SMC to write shifts
	xor a
	sub l
	and 3
	add a,a
	ld (timer_update_smc),a
	; Apply SMC to TIMA update divisor
	ld hl,-TIMA_LENGTH * 128
	ld de,updateTIMA_smc
	call timer_smc_calculate
	; Update TMA with current value
	ld hl,TMA
	or a
	call timer_update
	; Update TIMA with current value
	ex de,hl
	ld hl,TIMA
	call timer_update_count
	; Acknowledge any pending GB timer interrupt
	ld a,4
	ld (mpIntAcknowledge),a
	; If scanline timer interrupt is enabled, enable GB timer too
	ld a,(mpIntEnable)
	bit 1,a
	jr z,_
	or 4
	ld (mpIntEnable),a
_
	ex af,af'
	pop.s ix
	jp.s (ix)
	
	
; Writes to the GB timer count or modulo (TIMA/TMA).
; Does not use a traditional call/return, must be jumped to directly.
;
; Updates the GB timer based on the new value, if enabled.
;
; Inputs:  A=0 for TIMA, A=1 for TMA
;          A' = value being written
;          (SPS) = Z80 return address
;          AF' has been swapped
; Outputs: Current value written to the register
;          GB timer updated
;          AF' has been unswapped
timer_write_helper:
	exx
	ld hl,TIMA
	add a,l
	ld l,a
	ex af,af'
	ld.s (hl),a
	ex af,af'
	rra	; Set carry flag if TIMA
	ld a,(hram_base+TAC)
	bit 2,a
	call nz,timer_update
	exx
	ex af,af'
	pop.s ix
	jp.s (ix)
	
; Updates a host timer register based on a GB timer register (TIMA/TMA).
;
; Disables timers first to ensure atomicity.
;
; Inputs:  HL = zero-extended pointer to GB timer register
;          CA = reset for TMA, set for TIMA
; Outputs: HL = output host timer register
;          Timer value is written to the output register
;          Timers are enabled
timer_update:
	ld de,mpTimer2Reset
	jr nc,_
timer_update_count:
	ld e,mpTimer2Count & $FF
_
	
	xor a
	ld (mpTimerCtrl),a
	; Multiply timer step length by (256-TMA)
	sub.s (hl)
	ld l,a
	ld h,TIMA_LENGTH
	jr z,_
	mlt hl
_
	; This entry point is for calculating and setting TIMA read SMC
timer_smc_calculate:
	; Do shifting based on the TAC mode
timer_update_smc = $+1
	jr $
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	; Set new value
	ex de,hl
	ld (hl),de
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ret
	