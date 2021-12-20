; Sets the stack base address and boundary values for the current bank
;
; Inputs:  A = stack bank identifier
; Outputs: HL = stack info pointer for the specified bank, plus 2
;          DE = stack base address
; Destroys: AF
set_gb_stack_bounds_helper:
	ld hl,stack_bank_info_table
	scf
	rra
	ld l,a
	; Get the bank base in DE
	ld de,(hl)
	ld (z80codebase+sp_base_address),de
	; Adjust inc/dec sp handlers based on the address alignment
	bit 0,e
	ld a,$20	;JR NZ
	jr z,_
	ld a,$28	;JR Z
_
	ld (z80codebase+ophandler33_jr_smc),a
	ld (z80codebase+ophandler3B_jr_smc),a
	; Get the MSBs of the boundaries for the bank
	inc hl
	inc hl
	push hl
	 inc hl
	 ld hl,(hl)
	 ; Lower boundary triggers when pushing with SP <= lower_bound+1
	 inc de
	 ld a,d
	 add a,l
	 ld (z80codebase+do_push_bound_smc_1),a
	 ld (z80codebase+do_push_bound_smc_2),a
	 ld (z80codebase+ophandler3B_bound_smc),a
	 ; Upper boundary triggers when popping with SP >= upper_bound-2
	 dec de
	 dec de
	 dec de
	 ld a,d
	 add a,h
	 ld (z80codebase+do_pop_bound_smc_1),a
	 ld (z80codebase+do_pop_bound_smc_2),a
	 ld (z80codebase+do_pop_bound_smc_3),a
	 ld (z80codebase+do_pop_bound_smc_4),a
	 ld (z80codebase+ophandler33_bound_smc),a
	 inc de
	 inc de
	 ; Calculate the negative of the bank base
	 or a
	 sbc hl,hl
	 sbc hl,de
	 ld.sis (sp_base_address_neg),hl
	pop hl
	ret.l

; Sets the current stack access routines for the current bank.
;
; Inputs:  HL = stack info pointer for the specified region, plus 3
; Outputs: Z if the region is HRAM, else NZ
; Destroys: AF, DE, HL
set_gb_stack_bank_helper:
	; Special case for RTC when stack points to CRAM 
	ld a,l
	cp (cram_bank_base + 2) & $FF
	jr nz,_
	ld a,(hl)
	cp z80codebase >> 16
; Inputs: IX = cram_bank_base
;         Z if setting RTC routines, NZ if setting CRAM routines
set_gb_stack_cram_bank_helper:
	jr nz,_
	ld l,(stack_bank_info_table - $10 + 2) & $FF
_
	; Set push jump offsets
	inc hl
	inc hl
	inc hl
	ld a,(hl)
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
	inc hl
	ld a,(hl)
	ld (z80codebase+do_push_for_call_jump_smc_1),a
	sub do_push_for_call_jump_smc_2 - do_push_for_call_jump_smc_1
	ld (z80codebase+do_push_for_call_jump_smc_2),a
	; Set pop jump offsets
	inc hl
	ld a,(hl)
	ld (z80codebase+do_pop_jump_smc_1),a
	sub do_pop_jump_smc_2 - do_pop_jump_smc_1
	ld (z80codebase+do_pop_jump_smc_2),a
	inc hl
	ld a,(hl)
	ld (z80codebase+ophandlerF1_jump_smc_1),a
	add a,(ophandlerF1_jump_smc_1 + 1) & $FF
	ld e,a
	adc a,(ophandlerF1_jump_smc_1 + 1) >> 8
	sub e
	ld d,a
	ld.sis (ophandlerF1_jump_smc_2),de
	; Set callstack memory access prefixes
	inc hl
	ld a,(hl)
	ld (z80codebase+callstack_ret_pop_prefix_smc),a
	ld (z80codebase+callstack_ret_cond_pop_prefix_smc),a
	; Copy callstack pop overflow check
	inc hl
	ld hl,(hl)
	ld (z80codebase+callstack_ret_check_overflow_smc),hl
	ld (z80codebase+callstack_ret_cond_check_overflow_smc),hl
	add a,a
	add a,a
_
	jp.sis c,set_gb_stack_bank_done	
	; Force overflow for all hmem pops
	sub 2
	ld (z80codebase+do_pop_bound_smc_2),a
	ld (z80codebase+do_pop_bound_smc_4),a
	jr -_

	; Propagate the bank mismatch value to the next callstack entry
	; in the same region as the return address
callstack_ret_bank_mismatch_helper:
	ld d,h
	ld e,a
	; Ensure the return address is in the $4000-$7FFF bank
	ld a,$BF
	cp l
#ifdef DEBUG
	jp po,$
#else
	jp po,++_
#endif
	ld (callstack_ret_bank_mismatch_smc),sp
_
	pop hl
	cp l
	jp po,-_
	; Combine the callstack entry high bytes
	ld a,d
	xor h
	ld h,a
	push hl
callstack_ret_bank_mismatch_smc = $+1
	ld sp,0
_
	ld a,e
	pop.s de
	pop.s hl
	jp.s (hl)
	
	
; Flushes the JIT code and recompiles anew.
; Does not use a traditional call/return, must be jumped to directly.
;
; When the memory routine generator overflows, it returns a pointer to
; flush_mem_handler, which provides this routine with the JIT address.
; The JIT address is reversed into the GB address before flushing normally.
; Recompilation starts at the offending memory instruction.
;
; Inputs:  HL = address directly following recompiled memory instruction
;          Z' if instruction is LD (HL),n
;          BCDEHL' have been swapped
; Outputs: JIT is flushed and execution begins at the new recompiled block
;          BCDEHL' have been unswapped
flush_mem:
	ex af,af'
	push bc
	 push af
	  ld b,h
	  ld c,l
	  call lookup_gb_code_address
	  ; Most memory routines are for 1-byte, 2-cycle instructions
	  dec de
	  cpl
	  ld b,a
	  dec b
	 pop af
	 jr nz,_
	 ; Special handling for 2-byte, 3-cycle instructions
	 dec de
	 dec b
_
	 add a,b
	pop bc
	jr c,flush_normal
	dec c

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
	push af
	 push bc
	  ld hl,$C3 | (do_event_pushed << 8)	;JP do_event_pushed
	  ld (flush_event_smc_1),hl
	  ld (flush_event_smc_2),hl
	  ld hl,$D2 | (schedule_event_finish_for_call_now << 8)	;JP NC,schedule_event_finish_for_call
	  ld (flush_event_for_call_smc),hl
	  call flush_code
	  push de
	   call lookup_code
	  pop de
	 pop bc
	 ld l,a
	pop af
	; Flush entire call stack, including interrupt returns
	ld sp,myADLstack
	ld.sis sp,myz80stack-4
	add a,l
	jr c,_
	dec c
_
	inc c
	jr z,_
	exx
	ex af,af'
	jp.s (ix)
_
	push.s ix
	ld c,l
	push.s bc
	ld b,a
#ifdef VALIDATE_SCHEDULE
	call schedule_event_helper_a
#else
	jp schedule_event_helper_a
#endif
	
write_vram_check_sprite_catchup:
	ex de,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld a,h
	ld hl,oam_tile_usage_lut
	ld l,a
	ld a,(myspriteLY)
	cp (hl)
	jp nc,render_catchup
	call render_catchup
	ld a,(myLY)
; Catches up sprite rendering before changing sprite state.
; Must be called only if render_catchup has already been called.
;
; Inputs: A = current scanline
; Destroys: AF, BC, DE, HL
sprite_catchup:
	push ix
	 push iy
	  call draw_sprites
	 pop iy
	pop ix
	
	ld a,(myLY)
	ld (myspriteLY),a
	ld hl,(scanlineLUT_ptr)
	ld (scanlineLUT_sprite_ptr),hl
	jp sync_frame_flip
	
dma_write_helper:
	; Catch up background rendering
	ld a,r
	call m,render_catchup
	; Render the existing OAM data if applicable
	ld a,(myLY)
	or a
	call nz,sprite_catchup
	; This returns BC=0, DE=0
	MEMSET_FAST(oam_tile_usage_lut, 256, 0)
	; Copy 160 bytes from the specified source address to OAM
	ex af,af'
	ld (hram_base+DMA),a
	ld d,a
	ex af,af'
	ld a,$DF
	cp d
	jr nc,_
	and d
	ld d,a
_
	GET_BASE_ADDR_FAST
	add hl,de
	ld de,hram_start
	ld c,$A0
	ldir
	ld hl,oam_tile_usage_lut
	ld c,143+16
oam_tile_usage_gen_loop:
	dec e
	dec e
	ld a,(de)
	ld l,a
	dec e
	dec e
	ld a,(de)
	jr z,_
	dec a
	cp c
	jr nc,oam_tile_usage_gen_loop
	cp (hl)
	jr c,oam_tile_usage_gen_loop
	inc a
	ld (hl),a
	jr oam_tile_usage_gen_loop
_
	cp 144+16
	jr nc,_
	cp (hl)
	jr c,_
	ld (hl),a
_
	jp.sis z80_pop_restore_swap_ret
	
; Writes to an LCD scroll register (SCX,SCY,WX,WY). Also OBP0, OBP1, and DMA.
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  (LY), (STAT) = current PPU state
;          A' = value being written
;          AF' has been swapped
;          BCDEHL' have been swapped
;          (SPS) = 16-bit register address
;          (SPS+2) = Z80 return address
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
	sub WY - SCX
	jr c,scroll_write_OBP
	jr nz,scroll_write_WX
	ld a,ixh
	ex af,af'
	ld (WY_smc),a
	jr scroll_write_done
	 
scroll_write_SCY:
	ld a,ixh
	ex af,af'
	ld (SCY_smc),a
	jr scroll_write_done
	
scroll_write_OBP:
	 push bc
	  push hl
	   ; Catchup sprites if at least one scanline has been rendered
	   ld a,(myLY)
	   or a
	   call nz,sprite_catchup
	   ; Get the palette index for this written value
	   ld hl,overlapped_palette_index_lut + OBP0_COLORS_START
	   ld a,l
	   ex af,af'
	   ld l,a
	   ex af,af'
	   add a,(hl)
	  pop hl
	 pop bc
	 ; Check whether this is OBP0 or OBP1
	 bit 0,l
	 jr z,_
	 add a,OBP1_COLORS_START - OBP0_COLORS_START
	 ld (overlapped_obp1_palette_index),a
	 jr scroll_write_done_swap
_
	 ld (overlapped_obp0_palette_index),a
	 jr scroll_write_done_swap
	 
scroll_write_WX:
	ex af,af'
	ld e,a
	ex af,af'
	ld a,e
	ld (WX_smc_2),a
	cp 167
	inc a
	ld (WX_smc_3),a
	sbc a,a
	and $08
	add a,$18	;JR vs JR NZ
	ld (WX_smc_1),a
	jr scroll_write_done_swap
	
scroll_write_SCX:
	ex af,af'
	ld e,a
	ex af,af'
	ld a,e
	rrca
	rrca
	and $3E
	ld (SCX_smc_1),a
	ld a,e
	cpl
	and 7
	inc a
	ld (SCX_smc_2),a
scroll_write_done_swap:
	ld a,ixh
	ex af,af'
scroll_write_done:
	ld.s (hl),a
	jp.sis z80_swap_ret
	
; Tracks a list of writes to the BGP register.
; Also tracks which value is held by BGP for the largest number of scanlines.
BGP_write_helper:
	ld hl,hram_base+STAT
	; If rendering is caught up, query most recently rendered line
	ld a,r
	rla
	ld a,(myLY)
	jr nc,_
	; Otherwise, calculate from LY/STAT
	ld a,(hl)
	cpl
	; Set A to 1 if in hblank, 0 otherwise
	rrca
	and l ;$41
	; Get value of LY, plus 1 if in hblank
	ld l,LY & $FF
	add a,(hl)
_
	ld d,a
	ld l,BGP & $FF
	ld e,(hl)
	ld (hl),b
	; Check how many lines passed since the last write
mypaletteLY = $+1
	sub 0
	jr z,BGP_write_done
BGP_write_queue_next = $+1
	ld hl,BGP_write_queue
	ld c,a
	dec a
	jr nz,BGP_write_multiple_lines
	ld a,l
BGP_write_queue_literal_start = $+1
	ld l,BGP_write_queue & $FF
	; Increment the old literal run length, and check for overflow
	inc (hl)
	jr nz,_
	; Restore the length and start a new literal here
	dec (hl)
	ld l,a
	ld (hl),144
	ld (BGP_write_queue_literal_start),a
	inc a
_
	ld l,a
	; Emit the BGP value
	ld (hl),e
	inc a
	jr BGP_write_finish
BGP_write_multiple_lines:
	; Emit the number of lines (minus 1)
	ld (hl),a
	inc l
	ld (hl),e
	inc l
	; Set no literal run active
	ld (hl),$FF
	ld a,l
	ld (BGP_write_queue_literal_start),a
BGP_write_finish:
	; Save the address of the next queue entry
	ld (BGP_write_queue_next),a
	; Save the current line
	ld a,d
	ld (mypaletteLY),a
	; Track the maximum number of lines per BGP value
	inc h
	ld l,e
	ld a,(hl)
	add a,c
	ld (hl),a
BGP_max_frequency = $+1
	cp 0
	jp.sis c,z80_pop_restore_swap_ret
	ld (BGP_max_frequency),a
	ld a,e
	ld (BGP_max_value),a
BGP_write_done:
	jp.sis z80_pop_restore_swap_ret
	
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
; Inputs:  B = A' = value being written
;          (LY), (STAT) = current PPU state
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
;          BCDEHL' are swapped
; Outputs: LYC and cycle targets updated
lyc_write_helper:
	; Set the new value of LYC
	ld hl,hram_base+LYC
	ld (hl),b
	; Calculate the new LYC cycle offset (from vblank)
	ld a,b
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
	ld a,b
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
	; Set the LYC coincidence bit
	ld (hl),c
	; Check if the IE STAT bit is set
	ld l,h ;ld l,IE & $FF
	bit 1,(hl)
	inc hl ;ld hl,active_ints
	; Set the STAT interrupt active bit
	set.s 1,(hl)
	jr z,++_
	; Perform STAT scheduling updates
	call stat_setup
	; Trigger a new event immediately
	jp.sis trigger_event_pop
_
	ld (hl),c
_
	
	; Perform STAT scheduling updates
	call stat_setup
	; Reschedule the current PPU event
	jp.sis reschedule_event_PPU_pop
	
stat_lyc_write_no_reschedule:
	jp.sis z80_pop_restore_swap_ret
	
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
	; If so, check if IE STAT bit is set and set IF STAT bit
	ld l,h ;ld l,IE & $FF
	bit 1,(hl)
	inc hl ;ld hl,active_ints
	set.s 1,(hl)
	jr z,_
	; Handle changes to mode 0 and mode 2 interrupt bits
	ld a,e
	and $28
	call nz,stat_setup
	; Trigger a new event immediately
	jp.sis trigger_event_pop
_
	; Handle changes to mode 0 and mode 2 interrupt bits
	ld a,e
	and $28
	jr z,stat_lyc_write_no_reschedule
	call stat_setup
	; Reschedule the current PPU event
	jp.sis reschedule_event_PPU_pop
	
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
	push ix
	ld ix,z80codebase+ppu_expired_lcd_off
	ld hl,-CYCLES_PER_FRAME
	ld.sis (ppu_post_vblank_event_handler),ix
	ld.sis (ppu_post_vblank_event_offset),hl
	jr stat_setup_next_vblank_lcd_off
	
stat_setup_hblank:
	ld ix,z80codebase+ppu_expired_mode0_line_0
	ld (ix-ppu_expired_mode0_line_0+ppu_mode0_event_line),b ;144
	ex de,hl
	ld hl,-((CYCLES_PER_SCANLINE * 10) + MODE_2_CYCLES + MODE_3_CYCLES)
	; Check if LYC match is during vblank
	cp b ;144
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
	cp b ;144
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
	cp b ;144
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
stat_setup_next_vblank_post_vblank:
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
	push ix
	ld ix,z80codebase+ppu_expired_vblank
	ld b,144
	; Get the line to match, but treat offscreen lines as vblank match
	ld.sis hl,(LY)
	ld a,h
	cp 154
	jr c,_
	ld h,b ;144
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
	ld hl,(lyc_cycle_offset)
	jr nc,stat_setup_next_from_vblank_post_vblank
	; Check if LY < 144
	cp b ;144
	jr c,stat_setup_next_vblank_post_vblank
stat_setup_next_from_vblank_post_vblank:
	ld.sis (ppu_post_vblank_event_handler),ix
	ld.sis (ppu_post_vblank_event_offset),hl
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
	pop ix
	ret
	
stat_setup_oam:
	ld ix,z80codebase+ppu_expired_mode2_line_0
	ld (ix-ppu_expired_mode2_line_0+ppu_mode2_event_line),143
	ex de,hl
	ld hl,-(CYCLES_PER_SCANLINE * 10)
	; Check if LYC match is during vblank
	cp b ;144
	call nc,stat_setup_lyc_mode1_filter
	ld.sis (ppu_post_vblank_event_handler),ix
	ld.sis (ppu_post_vblank_event_offset),hl
	; If LY is 143, schedule vblank
	ld a,e
	cp 143
	jp z,stat_setup_next_vblank
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
	cp b ;144
	ret c
	; The next event from now is LYC (mode 1)
	pop de
	jp stat_setup_next_from_vblank_post_vblank
	
	
; Writes to the LCD control register (LCDC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  A' = value being written
;          (LY), (STAT) = current PPU state
;          IX = cycle info
;          AF' has been swapped
;          BCDEHL' have been swapped
;          (SPS) = Z80 return address
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
	ld e,a
	and $06
	jr z,_
	push bc
	 push de
	  ld a,(myLY)
	  or a
	  call nz,sprite_catchup
	 pop de
	pop bc
_
	bit 0,e
	jr z,_
	ld hl,LCDC_0_smc
	ld a,(hl)
	xor $39 ^ $31	;ADD.SIS HL,SP \ LD.SIS SP,HL vs LD.SIS SP,$F940
	ld (hl),a
_
	bit 1,e
	jr z,_
	ld hl,LCDC_1_smc
	ld a,(hl)
	xor $0E ^ $C9	;LD C,myspriteLY vs RET
	ld (hl),a
_
	bit 2,e
	jr z,_
	ld hl,LCDC_2_smc_1
	ld a,(hl)
	xor $38^$78
	ld (hl),a
	ld hl,LCDC_2_smc_2
	ld a,(hl)
	xor 7^15
	ld (hl),a
	ld (LCDC_2_smc_4),a
	xor 7^9
	ld (LCDC_2_smc_5),a
	ld hl,LCDC_2_smc_3
	ld a,(hl)
	xor $80 ^ $81	;RES 0,B vs RES 0,C
	ld (hl),a
_
	bit 3,e
	jr z,_
	ld hl,LCDC_3_smc
	ld a,(hl)
	xor (vram_tiles_start ^ (vram_tiles_start + $2000)) >> 8
	ld (hl),a
_
	bit 4,e
	jr z,_
	ld hl,LCDC_4_smc
	ld a,(hl)
	xor $80
	ld (hl),a
	ld (window_tile_ptr),a
_
	bit 5,e
	jr z,_
	ld hl,LCDC_5_smc
	ld a,(hl)
	xor $08	;JR C vs JR NC
	ld (hl),a
_
	bit 6,e
	jr z,_
	ld hl,window_tile_ptr+1
	ld a,(hl)
	sub (vram_tiles_start >> 8) & $FF
	xor $20
	add a,(vram_tiles_start >> 8) & $FF
	ld (hl),a
_
	bit 7,e
	jp.sis z,z80_restore_swap_ret
	; Get the current cycle offset safely
	jp.sis lcd_enable_disable_helper
	
lcd_enable_disable_continue:
	ld a,(LCDC_7_smc)
	xor $08	;JR NZ vs JR Z
	ld (LCDC_7_smc),a
	jp po,lcd_disable_helper
	; Enable the LCD
	
	; Force skip rendering the first frame after the LCD is enabled,
	; this is consistent with hardware behavior to avoid glitch frames
	ld a,$7E ;RSMIX
	ld (z80codebase+updateSTAT_enable_catchup_smc),a
	ld (z80codebase+updateSTAT_full_enable_catchup_smc),a
	ld (z80codebase+ppu_mode0_enable_catchup_smc),a
	ld (z80codebase+ppu_mode2_enable_catchup_smc),a
	ld (z80codebase+ppu_lyc_enable_catchup_smc),a
	
	; Enable cache updates for STAT and LY registers
	ld a,$ED ;LD HL,I
	ld (z80codebase+updateSTAT_disable_smc),a
	ld (z80codebase+updateLY_disable_smc),a
	
	; Enable interrupt and rescheduling effects for LYC and STAT writes
	.db $21 ;ld hl,
	 dec hl
	 ld a,b
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
	ld hl,($DD << 16) | lcd_on_ppu_event_checker
	ld (lcd_on_stat_setup_event_smc),hl
	ld hl,lcd_on_STAT_handler
	ld.sis (event_counter_checker_slot_PPU),hl
	
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
	sbc hl,hl ;ld hl,active_ints
	set.s 1,(hl)
	dec h
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
	; We didn't track whether an interrupt was requested, so just
	; trigger an event unconditionally
	jp.sis trigger_event_pop
	
lcd_disable_helper:
	; Determine whether the persistent vblank time has already passed
	ld.sis bc,(vblank_counter)
	; Check how many cycles the persistent vblank is before the current vblank
	ASSERT_NC
	sbc hl,bc
	; Check if the persistent vblank is at or after the current vblank
	; This should work in both single and double speed
	ld a,h
	cp (((CYCLES_PER_SCANLINE * 10) + 1) >> 7) + 1
	; If so, use the persistent vblank
	jr c,_
	; Get the number of cycles left until the current vblank
	ex de,hl
	sbc hl,bc
	; If current vblank was passed in this instruction, make no change
	ld a,h
	or a
	jr z,++_
	; Check whether the current time is after the persistent vblank time
	sbc.s hl,de
	; If not, use the persistent vblank time
	ex de,hl
	jr c,_
	; If so, use the current time
	add hl,de
_
	add hl,bc
	ld.sis (vblank_counter),hl
_
	
	; Disable the LCD
	call do_lcd_disable
	jp.sis reschedule_event_PPU_pop
	
lcd_on_STAT_restore_helper:
	ld a,$C9
	ld (z80codebase+lcd_on_STAT_restore),a
	ld a,updateSTAT_mode0_mode1 - (lcd_on_updateSTAT_smc + 1)
	ld (z80codebase+lcd_on_updateSTAT_smc),a
	ld a,1
	ld (lcd_on_stat_setup_mode_smc),a
	push hl
	 ld hl,($DD << 16) | event_counter_checker_slot_PPU
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
	 and $08 ;(4096 >> 8) >> 1
	 ld.sis hl,(vblank_counter)
	 sbc hl,de
	 ld.sis (vblank_counter),hl
	 add a,a
	 ld (z80codebase+audio_counter+1),a
	 ld.sis hl,(persistent_vblank_counter)
	 sbc hl,de
	 ld.sis (persistent_vblank_counter),hl
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
	 ld a,(z80codebase+timer_cycles_reset_factor_smc)
	 ld h,a
	 ; Only if the old bit of DIV is 0, add to the scheduled time
	 ; In effect, if the bit was 1, this schedules the next increment immediately
	 and e
	 xor h
	 ld e,a
	 mlt hl
	 add.s hl,de
	 add hl,hl
	 inc hl
	 ld.sis (timer_counter),hl
	pop de
	sbc hl,hl
	sbc hl,de
	ld i,hl
	jp.sis trigger_event
	
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
	ld l,a
	ex af,af'
	ld a,l
	or $F8
	ld (hram_base+TAC),a
	add a,4
	jr c,tac_write_enable
	ld hl,disabled_counter_checker
	ld.sis (event_counter_checker_slot_timer),hl
	ld a,$C3
	ld (z80codebase+enableTIMA_smc),a
	; If the corresponding bit of DIV is 1, increment TIMA
	ld hl,i
	add hl,de
	ld a,(z80codebase+timer_cycles_reset_factor_smc)
	and l
	jp.sis z,z80_restore_swap_ret
	ld hl,hram_base+TIMA
	inc (hl)
	jp.sis nz,z80_restore_swap_ret
	sbc hl,hl
	set.s 2,(hl) ;active_ints
	jp.sis trigger_event
	 
tac_write_enable:
	ld hl,timer_counter_checker
	ld.sis (event_counter_checker_slot_timer),hl

	; Get SMC data
	push bc
	 ld c,a
	 ld b,2
	 mlt bc
	 ld hl,timer_smc_data
	 add hl,bc
	
	 ld a,(hl)
	 ld (z80codebase + updateTIMA_smc),a
	 inc hl
	 ld a,(hl)
	 ld (z80codebase + timer_cycles_reset_factor_smc),a
	
	 ld.sis hl,(TIMA)
	 ld c,l
	 ld l,h
	 ld h,a
	 xor a
	 sub l
	 ld l,a
	 jr z,_
	 mlt hl
_
	 ld.sis (timer_period),hl
	
	 ld a,$CD
	 ld (z80codebase+enableTIMA_smc),a
	 ld a,c
	pop bc
	jp.sis tima_reschedule_helper
	
timer_smc_data:
	.db 6,$80
	.db 0,$02
	.db 2,$08
	.db 4,$20
	
NR52_write_helper:
	ld hl,hram_base+NR52
	ex af,af'
	ld e,a
	ex af,af'
	ld a,(hl)
	xor e
	add a,a
	jr nc,return_from_write_helper
	; Whether enabling or disabling audio, all channels are off
	ld a,e
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
	push bc
	 ld bc,audio_port_masks-audio_port_values-1
	 ld (hl),b
	 ldir
	 ; Copy masks to CPU-visible registers
	 inc hl
	 ld de,hram_base+NR10
	 ld c,NR52 - NR10
	 ldir
	pop bc
	; If disabling, ignore all writes
	ld a,$C9-($D9-$E0)	;RET
NR52_write_enable:
	; If enabling, un-ignore all writes
	add a,$D9-$E0	;EXX
	ld (z80codebase+write_audio_disable_smc),a
return_from_write_helper:
	jp.sis z80_restore_swap_ret
	
overlapped_op_1_1_mismatch:
	lea hl,ix-3
	ld (hl),a
	ld a,e
	lea de,ix-7
	; Recompile an overlapped instruction again
	; Input: DE=start of overlapped handler call
	;        HL=overlap point in copied instruction
	;        IX=recompiled code start
	;        A=cycle count
opgen_overlap_rerecompile:
	push.s de
	push.s af
	ld a,l
	ld (opgen_base_address_smc_1),a
	ld a,h
	lea hl,ix-2
	sub (hl)
	ld (opgen_base_address_smc_2),a
	ld ix,opgenroutines
	inc de
	inc de
	inc de
	push bc
	 push iy
	  call opgen_emit_overlapped_opcode
	 pop iy
	pop bc
	jp.sis trigger_event_already_triggered
	
handle_overlapped_op_1_1_helper:
	ex af,af'
	exx
	ld de,z80codebase+4
	add ix,de
	ld d,(ix-2)
	ld hl,mem_region_lut
	ld e,l ;E=0
	ld l,d
	ld l,(hl)
	dec h
	ld hl,(hl)
	add hl,de
	ld e,a
	ld a,(hl)
	cp (ix-3)
	jr nz,overlapped_op_1_1_mismatch
	ld a,(ix-1)
	add a,e
	jr nc,handle_overlapped_op_done
	inc c
	jr nz,handle_overlapped_op_done
	lea hl,ix-4
	jr schedule_overlapped_event_helper_x_1
	
overlapped_op_2_1_mismatch:
	lea hl,ix-3
	ld (hl),a
	ld a,e
	lea de,ix-8
	jr opgen_overlap_rerecompile
	
overlapped_op_1_2_mismatch:
	lea hl,ix-4
	ld.s (hl),de
	lea de,ix-8
	jr opgen_overlap_rerecompile
	
handle_overlapped_op_1_2_helper:
	ex af,af'
	exx
	ld de,z80codebase+5
	add ix,de
	ld d,(ix-2)
	ld hl,mem_region_lut
	ld e,l ;E=0
	ld l,(hl)
	dec h
	ld hl,(hl)
	add hl,de
	ld de,(hl)
	ld hl,(ix-4)
	sbc.s hl,de
	jr nz,overlapped_op_1_2_mismatch
	add a,(ix-1)
	jr nc,handle_overlapped_op_done
	inc c
	jr nz,handle_overlapped_op_done
	lea hl,ix-5
	lea de,ix-4
	jr schedule_overlapped_event_helper
	
handle_overlapped_op_2_1_helper:
	ex af,af'
	exx
	ld de,z80codebase+5
	add ix,de
	ld d,(ix-2)
	ld hl,mem_region_lut
	ld e,l ;E=0
	ld l,d
	ld l,(hl)
	dec h
	ld hl,(hl)
	add hl,de
	ld e,a
	ld a,(hl)
	cp (ix-3)
	jr nz,overlapped_op_2_1_mismatch
	ld a,(ix-1)
	add a,e
	jr c,_
handle_overlapped_op_done:
	exx
	ex af,af'
	jp.s (ix)
_
	inc c
	jr nz,handle_overlapped_op_done
	lea hl,ix-5
schedule_overlapped_event_helper_x_1:
	lea de,ix-3
; Inputs: HL = pointer to first byte of copied opcode
;         DE = pointer to first overlapping byte of copied opcode
;         IX = starting recompiled address
;         A = cycle count at end of overlapped opcode (>= 0)
;         (IX-1) = cycles taken by overlapped opcode
schedule_overlapped_event_helper:
	push.s ix
	push.s bc
	ld b,a
	sub (ix-1)
	ld c,a
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
	ld a,b
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
	ld a,l
	GET_BASE_ADDR_FAST
	ex de,hl
	add hl,de
	bit 7,(hl)
	inc hl
	jr nz,schedule_jump_event_absolute
	push.s bc
	ld b,a
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
	push de
#endif
	 ld a,b
	 sub c
	 jr c,schedule_event_later_resolved_pushed
	pop de
	sbc hl,de
schedule_event_now_unresolved:
	ld.sis (event_gb_address),hl
	ld ixl,a
	ld ixh,0
#ifdef DEBUG
	ld hl,event_value
	ld.sis (event_address),hl
#endif
	; This is a code path that could target the flush handler
flush_event_smc_1 = $+1
	jp.sis do_event_pushed
	
schedule_jump_event_absolute:
	dec c
	push.s bc
	ld hl,(hl)
#ifdef VALIDATE_SCHEDULE
	inc hl
	dec.s hl
	ex de,hl
	call validate_schedule
	ex de,hl
#endif
	ld b,a
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
	  ld b,e
	  ld c,3
	  call opcycle_first
	 pop bc
	pop de
	or a
	sbc hl,de
	jp.sis schedule_event_finish
	
; Inputs: DE = Game Boy address at conditional branch
;         IX = recompiled address after conditional branch
;         L = cycle count at end of sub-block (>= 0)
;         C = cycles until end of sub-block (including conditional branch cycles)
schedule_subblock_event_helper:
	ld a,l
	GET_BASE_ADDR_FAST
	ex de,hl
	add hl,de
	bit 7,(hl)
	jr z,_
	bit 1,(hl)
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
#endif
	push.s bc
	ld b,a
	sub c
	jr c,schedule_event_later_resolved
	sbc hl,de
	ld.sis (event_gb_address),hl
	ld ixl,a
	ld ixh,0
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
;         B = cycle count at end of sub-block (>= 0)
;         A = negative cycles for jump
;  (-1 for untaken JR/RET, -2 for untaken JP, -3 for taken JR, -4 for taken JP)
;         C = cycles until end of sub-block (plus 1 for non-taken jump)
schedule_slow_jump_event_helper:
	add a,2
	jr c,schedule_bridge_event_slow
	push.s bc
	ld b,l
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
;          B = cycle count at end of sub-block (>= 0)
;          C = cycles until end of sub-block
; Outputs: HL = event Game Boy address
;          IX = event recompiled address
;          A = event cycles to sub-block end
;          B = cycle count at end of sub-block (>= 0)
schedule_event_helper:
	ld a,b
schedule_event_helper_a:
	sub c
	jr nc,schedule_event_now
schedule_event_later:
#ifdef VALIDATE_SCHEDULE
	call validate_schedule
#endif
	GET_BASE_ADDR_FAST
	push hl
	 add hl,de
	 ld de,opcounttable
	 push bc
	  ld b,e
	  ld c,3
	  call opcycle_first
	 pop bc
	pop de
	or a
	sbc hl,de
	jp.sis schedule_event_finish

schedule_bridge_event_slow:
	dec c
	push.s bc
	ld b,l
	ld a,b
	sub c
	jr c,schedule_event_later
schedule_event_now:
#ifdef VALIDATE_SCHEDULE
	call validate_schedule
#endif
	ld.sis (event_gb_address),de
	ld ixl,a
	ld ixh,0
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
;         B = cycle count at end of sub-block (>= 0)
;         C = cycles until end of sub-block (plus jump cycles, if applicable)
schedule_call_event_helper:
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
#endif
	ld a,b
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
	ex (sp),hl
	inc sp \ inc sp \ inc sp
	ex (sp),hl
	dec sp \ dec sp \ dec sp
	pop hl
	ret

validate_schedule_nops:
	jr c,$
	; Special case to handle NOPs, ugh
	push bc
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
	pop bc
	ret
#endif
	
resolve_mem_info_for_prefix_helper:
	push de
	 push bc
	  push hl
	   jr z,_
	   ld bc,do_bits
	   push bc
	    ;CB opcode (rotated right by 1)
	    ;Bit 0 set, indicates full IX load
	    ;Bit 7 reset only if no reschedule allowed (i.e. BIT)
	    ld c,ixl
	    jr resolve_mem_info_any_jump
_
	   ld c,ixh ;POP opcode
	   ld b,$C9
	   push bc
	    ;Bit 0 reset, indicates IXL load
	    ;Bit 7 reset, indicates no reschedule allowed
	    ld c,0
	    ld b,RST_POP
	    jr resolve_mem_info_any
	
; Inputs:  BCDEHL' are swapped
;          HL = current JIT address minus 3
;          IX = memory routine to jump to
;          D = NO_CYCLE_INFO offset
; Outputs: BCDEHL' are swapped
;          HL = current JIT address
;          IX = pointer to emitted cycle offset
; Preserves: BC, DE
;	
resolve_mem_info_for_routine_helper:
	push de
	 push bc
	  push hl
	   push ix
	    ld a,e
	    sub NO_RESCHEDULE
	    add a,a
	    ;Bit 0 reset, indicates IXL load
	    ;Bit 7 reset only if no reschedule allowed
	    ld c,a
resolve_mem_info_any_jump:
	    ld b,$C3
resolve_mem_info_any:
	    push bc
	     inc hl
	     inc hl
	     inc hl
	     ld b,h
	     ld c,l
	     call lookup_gb_code_address
	     cpl
	    pop bc
	    push de
	     ; Put the trampoline size minus 2 in DE
	     ld de,1
	     ld l,c
	     sla l
	     rl e
	     rrc c
	     rl e
	     ; Check if enough room for trampoline
	     ld hl,(z80codebase+memroutine_next)
	     scf
	     sbc hl,de ; Resets carry flag
	     ex de,hl
	     ld hl,(recompile_struct_end)
	     ld hl,(hl)
	     sbc hl,de	;Carry is reset, but DE is already one larger
	     ex de,hl
	    pop de
	    jr nc,resolve_mem_info_no_trampoline
	    dec hl
	    ld (z80codebase+memroutine_next),hl
	    ; Emit the error catcher
	    ld (hl),ERROR_CATCHER & $FF
	    inc hl
	    ld (hl),(ERROR_CATCHER >> 8) & $FF
	    inc hl
	    ld (hl),ERROR_CATCHER >> 16
	    inc hl
	    ; Emit the Game Boy address if required
	    rlc c
	    jp p,_
	    ld (hl),de
	    inc hl
	    inc hl
_
	    push hl
	    pop ix
	    ; Emit the LD IXL or LD IX instruction
	    ld (hl),$DD
	    inc hl
	    ld (hl),$2E
	    ld d,a
	    jr nc,_
	    ld (hl),$21
	    inc hl
	    ld (hl),d
	    ld d,c
_
	    inc hl
	    ld (hl),d
	    inc hl
	    ; Emit the target jump
	    ld (hl),b
	    inc hl
	   pop bc
	   ld (hl),c
	   inc hl
	   ld (hl),b
	  ; Overwrite the JIT code with a call to the trampoline
	  pop hl
	  jr nc,_
	  ; For CB prefixes only, overwrite the preceding byte
	  dec hl
	  ld.s (hl),$7F ;LD A,A
	  inc hl
_
	  ld.s (hl),$CD
	  inc hl
	  ld.s (hl),ix
_
	 pop bc
	pop de
	; Combine the passed NO_CYCLE_INFO offset
	dec a
	res 1,e
	add a,e
	jp.sis get_mem_info_finish_inc2
	
resolve_mem_info_no_trampoline:
	    ; No trampoline was emitted, so use a scratch area to return the info
	    ld ix,z80codebase + mem_info_scratch
	    ; Emit the Game Boy address
	    ld (ix-2),de
	    ; Emit the block cycle offset
	    ld (ix+2),a
	   pop bc
	  pop hl
	  inc hl
	  jr -_
	
; Inputs:  BCDEHL' are swapped
;          HL = current JIT dispatch address (points to jump followed by cycle)
;          D = current cycle, E = cycle info
;          (SP) = call return GB address
; Outputs: BCDEHL' are swapped
;          HL = current JIT dispatch address plus 3 (points to cycle)
;          IX = scratch buffer, holding target GB address
;          A = negative cycle offset for CALL instruction
; Preserves: BC, DE
get_mem_info_for_call_helper:
	ex (sp),hl
	pop ix
	push hl
	push de
	 dec hl
	 dec hl
	 ex.s de,hl
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
	 lea hl,ix+3
	 ld ix,z80codebase + mem_info_scratch
	 ld (ix-2),a
	 ld (ix-1),d
	pop de
	; Subtract from 5 cycles for CALL
	ld a,5
	jp.sis get_mem_info_for_call_finish
	
	
mbc_change_rom_bank_helper:
	; Propagate the bank change to the topmost rom callstack entry
	ld e,a
	ld (mbc_change_rom_bank_smc),sp
	inc sp
	ld a,$BF
_
	pop hl
	cp l
	jp po,-_
	ld a,e
	xor h
	ld h,a
	push hl
mbc_change_rom_bank_smc = $+1
	ld sp,0
	ld e,3
	mlt de
	ld hl,rombankLUT
	add hl,de
	ld hl,(hl)
	ld (rom_bank_base),hl
	ld a,(z80codebase+curr_gb_stack_bank)
	cp 3 << 5
	jp.sis nz,mbc_2000_finish
	jp.sis mbc_fix_sp
	
mbc_rtc_latch_helper:
	call c,update_rtc_always
	ld ix,z80codebase+rtc_latched
	ld de,(ix+5)
	ld a,e
	and $3F
	ld e,a
	ld a,d
	and $3F
	ld d,a
	ld (ix),de
	ld de,(ix+7)
	ld a,e
	and $1F
	ld e,a
	ld (ix+2),de
	ld a,(ix+9)
	and $C1
	ld (ix+4),a
	ld a,mbc_finish - (mbc_rtc_latch_smc+1)
	ld (z80codebase+mbc_rtc_latch_smc),a
	pop.s ix
	jp.sis mbc_finish
	
mbc_rtc_switch_to_rtc_helper:
	push ix
	 push de
	  call mbc_rtc_toggle_smc
mbc_rtc_switch_between_rtc:
	  call update_rtc
	 pop de
	pop ix
	ld a,e
	and 7
	sbc hl,hl
	ld l,a
	ld de,z80codebase+rtc_latched
	jp.sis mbc_ram_any

mbc_rtc_switch_from_rtc_helper:
	push ix
	 push de
	  ld ix,(z80codebase+cram_actual_bank_base)
	  ld a,(ix)
	  cp (ix+5)
	  jr nz,mbc_rtc_mark_latch_dirty
mbc_rtc_mark_latch_dirty_continue:	
	  bit 3,e
	  jr nz,mbc_rtc_switch_between_rtc
	  call mbc_rtc_toggle_smc
	  call update_rtc
	 pop de
	pop ix
	jp.sis mbc_ram
	
mbc_rtc_mark_latch_dirty:
	  xor a
	  ld (z80codebase+mbc_rtc_latch_smc),a
	  jr mbc_rtc_mark_latch_dirty_continue
	
mbc_rtc_toggle_smc:
	push bc
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
	pop bc
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
	pop bc
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
	push bc
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
	pop bc
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