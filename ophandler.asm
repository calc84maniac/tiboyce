#ifdef SHADOW_STACK
set_shadow_stack_contiguous_helper:
	ld c,a ;0
	ld a,b
	; Flush the previously loaded shadow stack if needed
	; First reset the mem_read_lut and mem_write_lut entries
	ld l,d
	ld (hl),e
	inc l
	; If the shadow stack was not loaded, skip the flush
	jr z,set_shadow_stack_no_flush
	ld (hl),e
	ld h,mem_write_lut >> 8
	ld (hl),e
	dec l
	ld (hl),e
	; Get the destination address to flush the shadow stack to
	dec h ;mem_get_ptr_routines
	ld l,e
	inc l
	inc l
	ld hl,(hl)
	ld b,d
	add hl,bc
	ex de,hl
	; Do the flush
	ld hl,z80codebase+shadow_stack_start
	ld b,512>>8
	ldir
	ld h,mem_read_lut >> 8
	ld b,a
set_shadow_stack_no_flush:
	; Set the new shadow stack base address
	ld l,a
	cpl
	add a,(shadow_stack_start>>8)+1
	ld (shadow_stack_base+1),a
	; Set the new mem_read_lut and mem_write_lut entries
	ld a,shadow_stack_get_ptr & $FF
	ld e,(hl)
	ld (hl),a
	inc l
	ld (hl),a
	ld h,mem_write_lut >> 8
	ld (hl),a
	dec l
	ld (hl),a
	; Get the source address to load the data from
	dec h ;mem_get_ptr_routines
	ld l,e
	inc l
	inc l
	ld hl,(hl)
	add hl,bc
	; Load the data into the shadow stack
	ld de,z80codebase+shadow_stack_start
	ld b,512>>8
	ldir
	; Get the original stack pointer back
	lea bc,iy
	jp.sis set_shadow_stack_finish
#endif

	; Propagate the bank mismatch value to the next callstack entry
	; in the same region as the return address
callstack_ret_bank_mismatch_helper:
	ld b,h
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
	ld a,b
	xor h
	ld h,a
	push hl
callstack_ret_bank_mismatch_smc = $+1
	ld sp,0
_
	pop.s bc
	pop.s hl
	jp.s (hl)
	
write_vram_check_sprite_catchup:
	ld h,b
	ld l,c
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
	exx
	ld a,l
	exx
	ld (hram_base+DMA),a
	cp $E0
	jr c,_
	res 5,a
_
	ld d,a
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
	pop.s de
	jp.sis z80_restore_swap_ret
	
; Writes to an LCD scroll register (SCX,SCY,WX,WY). Also OBP0, OBP1, and DMA.
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  (LY), (STAT) = current PPU state
;          L' = value being written
;          AFBCDEHL' have been swapped
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
	add a,SCX - WY
	jr nc,scroll_write_OBP
	exx
	ld a,l
	exx
	ld.s (hl),a
	jr nz,scroll_write_WX
	ld hl,WY_smc
	ld (hl),a
	; Check for window trigger after last rendered line
	ld a,(myLY)
	sbc a,(hl)
	jp.sis nz,z80_restore_swap_ret
	jr c,_ ; Handle edge case of LY=0, WY=$FF
	; Check if window is currently enabled
	ld a,(hram_base+LCDC)
	bit 5,a
	jr z,_
	inc hl ;window_trigger_smc
	ld (hl),$37 ;SCF
_
	jp.sis z80_restore_swap_ret
	
scroll_write_SCY:
	exx
	ld a,l
	exx
	ld (SCY_smc),a
	ld.s (hl),a
	jp.sis z80_restore_swap_ret
	
scroll_write_OBP:
	push de
	 push hl
	  ; Catchup sprites if at least one scanline has been rendered
	  ld a,(myLY)
	  or a
	  call nz,sprite_catchup
	 pop hl
	pop de
	exx
	ld a,l
	exx
	ld.s (hl),a
	; Check whether this is OBP0 or OBP1
	bit 0,l
	; Get the palette index for this written value
	ld hl,overlapped_palette_index_lut
	ld l,a
	ld a,(hl)
	jr z,_
	add a,OBP1_COLORS_START
	ld (overlapped_obp1_palette_index),a
	jp.sis z80_restore_swap_ret
_
	add a,OBP0_COLORS_START
	ld (overlapped_obp0_palette_index),a
	jp.sis z80_restore_swap_ret
	
scroll_write_WX:
	ld (WX_smc_2),a
	cp 167
	inc a
	ld (WX_smc_3),a
	sbc a,a
	and $30-$18 ;JR NC or JR
	add a,$18
	ld (WX_smc_1),a
	jp.sis z80_restore_swap_ret
	
scroll_write_SCX:
	exx
	ld a,l
	exx
	ld.s (hl),a
	ld c,a
	and $F8
	rrca
gbc_scroll_write_SCX_smc = $
	rrca ;NOP on GBC
	ld (SCX_smc_1),a
	ld a,c
	cpl
	and 7
	inc a
	ld (SCX_smc_2),a
	jp.sis z80_restore_swap_ret
	
; Tracks a list of writes to the BGP register.
; Also tracks which value is held by BGP for the largest number of scanlines.
BGP_write_helper:
	exx
	ld a,l
	exx
	ld b,a
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
	ld l,BGP & $FF
	ld c,(hl)
	ld (hl),b
	; Check how many lines passed since the last write
mypaletteLY = $+1
	ld b,0
	; Save the current line
	ld (mypaletteLY),a
	sub b
	jr z,BGP_write_done
BGP_write_queue_next = $+1
	ld hl,BGP_write_queue
	ld b,a
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
	ld (hl),c
	inc a
	jr BGP_write_finish
BGP_write_multiple_lines:
	; Emit the number of lines (minus 1)
	ld (hl),a
	inc l
	ld (hl),c
	inc l
	; Set no literal run active
	ld (hl),$FF
	ld a,l
	ld (BGP_write_queue_literal_start),a
BGP_write_finish:
	; Save the address of the next queue entry
	ld (BGP_write_queue_next),a
	; Track the maximum number of lines per BGP value
	inc h
	ld l,c
	ld a,(hl)
	add a,b
	ld (hl),a
BGP_max_frequency = $+1
	cp 0
	jp.sis c,z80_restore_swap_ret
	ld (BGP_max_frequency),a
	ld a,c
	ld (BGP_max_value),a
BGP_write_done:
	jp.sis z80_restore_swap_ret
	
; Handles writes to the LY compare register (LYC).
; Only called if the value of LYC is changed, and before the LYC event line.
; Does not use a traditional call/return, must be jumped to directly.
;
; Sets the new cycle target according to the new value of LYC.
; Triggers a GB interrupt if LY already matches the new LYC value.
;
; Inputs:  L' = (LYC) = new value of LYC
;          (LY), (STAT) = current PPU state
;          BC, DE, return address on Z80 stack
;          BCDEHL' are swapped
; Outputs: LYC and cycle targets updated
lyc_write_helper:
	; Compare the new LYC to LY
	ld hl,hram_base + LYC
	ld a,(hl)
	dec hl
	cp (hl)
	; Get STAT
	ld l,STAT & $FF
	ld c,(hl)
	; If they're the same, handle new coincidence
	jr z,lyc_write_new_coincidence
	; Reset LY=LYC coincidence bit
	res 2,c
	ld (hl),c
	; If LYC > LY, LYC is on this frame
	jr nc,lyc_write_this_frame
	; If LYC < LY, LYC is on the next frame
	; Check if the new LYC is less than the initial line prediction
	ld hl,lyc_prediction_list+0
	cp (hl)
	jr nc,_
	; If LYC is zero, this is not a valid initial prediction
	or a
	jr z,_
	ld (hl),a
_
	; If during vblank, we need an event reschedule
	ld a,c
	dec a
	and 3
	jr z,lyc_write_reschedule
lyc_write_no_reschedule:
stat_write_no_reschedule:
	jp.sis z80_pop_restore_swap_ret
	
lyc_write_this_frame:
	; If LYC <= 144, update the LYC event line and prediction
	cp VBLANK_SCANLINE+1
	jr nc,lyc_write_reschedule
	ld (z80codebase+writeLYC_event_line_smc),a
	ld hl,(z80codebase+last_lyc_match)
	ld (hl),a
lyc_write_reschedule:
	; Perform STAT scheduling updates
	call stat_setup_c
	; Reschedule the current PPU event
	jp.sis reschedule_event_PPU
	
lyc_write_new_coincidence:
	; Set LY=LYC coincidence bit
	set 2,c
	ld (hl),c
	; If LYC interrupts are enabled, handle interrupt blocking logic
	bit 6,c
	jr z,lyc_write_no_reschedule
	; Transform mode into interrupt source mask:
	; 0 -> $08, 1 -> $10, 2 -> $24, 3 -> $00
	ld a,c
	inc a
	and 3
	add a,a
	add a,a
	daa
	add a,a
	; Check if any enabled interrupt modes block the interrupt
	and $38
	and c
	jr nz,lyc_write_no_reschedule
	; Check if the IE STAT bit is set and IME is set
	ld a,(z80codebase+intstate_smc_1)
	ld l,h ;ld l,IE & $FF
	add a,a
	and (hl)
	and 2
	inc hl ;ld hl,active_ints
	; Set the STAT interrupt active bit
	set.s 1,(hl)
	jr z,lyc_write_no_reschedule
	; Trigger a new event immediately
	jp.sis trigger_event_pop
	
stat_write_helper:
	exx
	ld a,l
	exx
	ld hl,hram_base + STAT
	ld c,(hl)
	ld d,a
	; Save changed mode interrupt bits
	xor c
	ld e,a
	; Update writable bits in STAT
	and $78
	xor c
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
stat_write_bug_smc = $
	and a ; Replace with AND D for GBC emulation, to check against new bits
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
	jr z,stat_write_no_reschedule
	call stat_setup
	; Reschedule the current PPU event
	jp.sis reschedule_event_PPU
	
do_lcd_disable:
	; Set STAT mode 0 and LY=0
	ld hl,hram_base+STAT
	ld a,(hl)
	ld c,a
	and $FC
	ld (hl),a
	ld l,LY & $FF
	ld (hl),0
	
	; Update HDMA handler if enabled
	ld l,HDMA5 & $FF
	bit 7,(hl)
	jr nz,++_
	; Trigger HDMA if STAT mode was not 0
	xor c
	ld hl,hdma_lcd_off_handler
	jr z,_
	ld hl,hdma_immediate_handler
_
	ld.sis (event_counter_checker_slot_HDMA),hl
_
	
	; Disable cache updates for STAT and LY registers
	ld a,$C9 ;RET
	ld (z80codebase+updateSTAT_disable_smc),a
	ld (z80codebase+updateLY_disable_smc),a
	
	; Disable interrupt and rescheduling effects for LYC and STAT writes
	ld hl,(writeLYC_same - (writeLYC_disable_smc+3))<<16 | $1877 ;LD (HL),A \ JR writeLYC_same
	ld (z80codebase+writeLYC_disable_smc),hl
	ld l,h ;JR stat_write_no_reschedule
	ld h,stat_write_no_reschedule - (stat_write_disable_smc+2)
	ld (stat_write_disable_smc),hl
	
	; Force scanline fill and set its color
	ld a,l ;JR
	ld (LCDC_0_7_smc),a
	ld a,SCREEN_OFF_COLOR
	ld (scanline_fill_color_smc),a
	
	; Update PPU scheduler to do events once per "frame"
	ld de,ppu_expired_lcd_off
cpu_speed_ram_start = $
	CPU_SPEED_START()
	CPU_SPEED_IMM16($+2)
	ld.sis bc,-CYCLES_PER_FRAME
	jr stat_setup_next_from_vblank
	
stat_setup_oam:
	; Set the event line
	ld (z80codebase+ppu_mode2_event_line),a
	ld a,l
	ld (z80codebase+ppu_mode2_LY),a
	; Check if LY=LYC
	cp h
	ld.sis hl,(nextupdatecycle_LY)
	ld.sis (ppu_counter),hl
	ld de,ppu_expired_mode2
	jr nz,stat_setup_done
	ld de,ppu_expired_mode2_maybe_lyc_block
	jr stat_setup_done
	
stat_setup_hblank_this_line:
	ld (z80codebase+ppu_mode0_LY),a
	; Check if LY=LYC
	bit 2,c
	CPU_SPEED_IMM8($+2)
	ld.sis bc,MODE_0_CYCLES
	jr nz,stat_setup_done_add
	ld de,ppu_expired_mode0_maybe_lyc_block
	jr stat_setup_done_add
	
	; Input: C = current value of STAT
	; Output: HL = (ppu_counter) = new PPU event time
	;         (event_counter_checker_slot_PPU) = new PPU event handler
	; Ensures rescheduling is enabled
stat_setup_c_forced:
	; Enable rescheduling which may have been disabled by vblank
	ld a,$40 ;.SIS
	ld (stat_setup_impending_vblank_smc),a
	; Enable interrupt and rescheduling effects for STAT writes
	.db $21 ;ld hl,
	 rrca
	 rrca
	 .db $C6 ;add a,
	ld (stat_write_disable_smc),hl
	
	; Input: C = current value of STAT
	; Output: HL = (ppu_counter) = new PPU event time
	;         (event_counter_checker_slot_PPU) = new PPU event handler
stat_setup_c:
	ld d,c
	; Input: D = current writable bits of STAT, C = current read-only bits of STAT
	; Output: HL = (ppu_counter) = new PPU event time
	;         (event_counter_checker_slot_PPU) = new PPU event handler
stat_setup:
	; If vblank was crossed in this instruction, don't reschedule
	; The vblank event handler does its own equivalent rescheduling
	; Note that the impending vblank event means the return event time is ignored
stat_setup_impending_vblank_smc = $
	; Get LY and LYC
	ld.sis hl,(LY) ; Replaced with RET
	; Get the currently scheduled line event
	ld a,(z80codebase+writeLYC_event_line_smc)
	; Check if in post-vblank state
	cp SCANLINES_PER_FRAME
	jr nz,_
	; Check if LY is still after vblank
	ld a,l
	cp VBLANK_SCANLINE
	jr nc,stat_setup_vblank
	; If not, LY has wrapped back around to 0
	; Get the initial LYC prediction for the frame and set it as the current event line
	ld a,(lyc_prediction_list+0)
	ld (z80codebase+writeLYC_event_line_smc),a
_
	; Check if hblank interrupt is enabled (blocks OAM)
	bit 3,d
	jr nz,stat_setup_hblank
	; Check if OAM interrupt is enabled
	bit 5,d
	jr nz,stat_setup_oam
	ld de,ppu_expired_active_lyc
stat_setup_specific_line:
	; Calculate cycle offset from vblank
	add a,SCANLINES_PER_VBLANK
	ld b,a
	CPU_SPEED_IMM8($+1)
	ld c,-CYCLES_PER_SCANLINE
	xor a
	sub b
	mlt bc
	add a,b
	ld b,a
stat_setup_next_from_vblank:
	; The next event from now is relative to start of vblank
	ld.sis hl,(vblank_counter)
stat_setup_done_add:
	add hl,bc
stat_setup_done:
	ld.sis (ppu_counter),hl
lcd_on_stat_setup_event_smc = $+3
	ld.sis (event_counter_checker_slot_PPU),de
	ret
	
stat_setup_hblank:
	; Set the event line
	ld (z80codebase+ppu_mode0_event_line),a
	ld b,a
	ld a,l
	ld.sis hl,(nextupdatecycle_LY)
	ld de,ppu_expired_mode0
	; Check if hblank has already been reached this line
	bit 1,c
lcd_on_stat_setup_mode_smc = $
	jr nz,stat_setup_hblank_this_line
	; Schedule either LYC match or hblank on the next line
	inc a
	ld (z80codebase+ppu_mode0_LY),a
	; Check if the event line is next line
	cp b
	CPU_SPEED_IMM8($+2)
	ld.sis bc,-(MODE_2_CYCLES + MODE_3_CYCLES)
	jr nz,stat_setup_done_add
	ld de,ppu_expired_mode0_maybe_lyc_match
	; Check for vblank scanline
	cp VBLANK_SCANLINE
	jr nz,stat_setup_done
	ld de,ppu_expired_vblank
	jr stat_setup_done
	
stat_setup_vblank:
	; Check if LY < LYC
	cp h
	ld a,h
	ex de,hl
	ld de,ppu_expired_lyc_mode1
	jr c,stat_setup_vblank_maybe_lyc_match
	; Check if LYC=0
	or a
	jr nz,stat_setup_post_vblank
	CPU_SPEED_IMM16($+2)
	ld.sis bc,-(CYCLES_PER_SCANLINE * 9 + 1)
	jr stat_setup_next_from_vblank
	
stat_setup_vblank_maybe_lyc_match:
	; Check if LYC < 154
	sub SCANLINES_PER_FRAME
	jr c,stat_setup_specific_line
stat_setup_post_vblank:
	ld a,(lyc_prediction_list+0)
	bit 3,h
	jr nz,stat_setup_post_vblank_mode0
	bit 5,h
	ld de,ppu_expired_active_lyc_post_vblank
	jr z,stat_setup_specific_line
	ld (z80codebase+ppu_mode2_event_line),a
	ld de,ppu_expired_mode2_line_0
	CPU_SPEED_IMM16($+2)
	ld.sis bc,-(CYCLES_PER_SCANLINE * SCANLINES_PER_VBLANK)
	jr stat_setup_next_from_vblank
	
stat_setup_post_vblank_mode0:
	ld (z80codebase+ppu_mode0_event_line),a
	ld de,ppu_expired_mode0_line_0
	CPU_SPEED_IMM16($+2)
	ld.sis bc,-((CYCLES_PER_SCANLINE * SCANLINES_PER_VBLANK) + MODE_2_CYCLES + MODE_3_CYCLES)
	jr stat_setup_next_from_vblank
	
; Writes to the LCD control register (LCDC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  L' = value being written
;          (LY), (STAT) = current PPU state
;          AFBCDEHL' have been swapped
;          (SPS) = saved cycle offset
;          (SPS+2) = Z80 return address
; Outputs: Scanlines rendered if applicable, SMC applied, value written
;          AF' has been unswapped
;          BCDEHL' have been unswapped
lcdc_write_helper:
	ld a,r
	call m,render_catchup
	pop.s bc
	ld hl,hram_base+LCDC
	exx
	ld a,l
	exx
	ld b,(hl)
	ld (hl),a
	xor b
	ld b,a
lcdc_write_sprite_change_smc_1 = $+1
	and $06 ;$07 for GBC
lcdc_write_sprite_change_smc_2 = $+1
	jr z,lcdc_write_no_sprite_change_gbc
	push bc
	 push de
	  ld a,(myLY)
	  or a
	  call nz,sprite_catchup
	 pop de
	pop bc
	bit 1,b
	jr z,_
	ld hl,LCDC_1_smc
	ld a,(hl)
	xor $0E ^ $C9	;LD C,myspriteLY vs RET
	ld (hl),a
_
	bit 2,b
	jr z,_
LCDC_2_change_smc_1 = $+1
	ld hl,LCDC_2_smc_1_gb ;vs. LCDC_2_smc_1_gbc
	ld a,(hl)
LCDC_2_change_smc_2 = $+1
	xor $38^$78 ;vs. (gbc_tile_attributes_lut ^ gbc_tile_attributes_lut_2) >> 8
	ld (hl),a
	ld hl,LCDC_2_smc_2
	ld a,(hl)
	xor 7^15
	ld (hl),a
	ld (LCDC_2_smc_4),a
	xor 7^9
	ld (LCDC_2_smc_5),a
LCDC_2_change_smc_3 = $+1
	ld hl,LCDC_2_smc_3_gb ;vs. LCDC_2_smc_3_gbc
	ld a,(hl)
	xor $80 ^ $81	;RES 0,B vs RES 0,C
	ld (hl),a
_
lcdc_write_no_sprite_change_gb:
	bit 0,b
	jr z,_
LCDC_0_change_smc = $
	; GBC impl
	;ld hl,LCDC_0_smc_1_gbc
	;ld a,(hl)
	;xor (low_normal_prio_sprite_palette_lut ^ high_prio_sprite_palette_lut) >> 8
	;ld (hl),a
	;ld hl,LCDC_0_smc_2_gbc
	;ld a,(hl)
	;xor $80 ^ $00
	;ld (hl),a
	; GB impl
	ld a,(LCDC_0_7_smc)
	; Only update this SMC if the LCD is not disabled
	cp $18 ;JR
	jr z,_
	xor $20 ^ $28	;JR NZ vs. JR Z
	ld (LCDC_0_7_smc),a
	nop
	nop
_
lcdc_write_no_sprite_change_gbc:
	bit 3,b
	jr z,_
	ld hl,LCDC_3_smc
	ld a,(hl)
	xor (vram_tiles_start ^ (vram_tiles_start + $2000)) >> 8
	ld (hl),a
_
	bit 4,b
	jr z,_
	ld hl,LCDC_4_smc
	ld a,(hl)
	xor $80
	ld (hl),a
	ld (window_tile_ptr),a
_
	bit 5,b
	jr z,_
	ld hl,LCDC_5_smc
	ld a,(hl)
	xor $08	;JR C vs JR NC
	ld (hl),a
	jp pe,_
	; Check for window trigger after last rendered line
	inc hl \ inc hl \ inc hl ;WY_smc
	scf
	ld a,(myLY)
	sbc a,(hl) 
	jr nz,_
	jr c,_ ; Handle edge case of LY=0, WY=$FF
	inc hl ;window_trigger_smc
	ld (hl),$37 ;SCF
_
	bit 6,b
	jr z,_
	ld hl,window_tile_ptr+1
	ld a,(hl)
	sub (vram_tiles_start >> 8) & $FF
	xor $20
	add a,(vram_tiles_start >> 8) & $FF
	ld (hl),a
_
	bit 7,b
	jp.sis z,z80_restore_swap_ret
	; Get the current cycle offset
	push.s de
	push.s bc
	; Get the value of DIV
	ld hl,i
	add hl,de
	ld b,$FF
	add hl,bc
	ex de,hl
	
	; Check the actual LCDC value
	ld a,(hram_base+LCDC)
	rlca
	jr c,lcd_enable_helper

	push de
	 ; Get the number of cycles passed since the previous vblank
	 ld.sis hl,(vblank_counter)
	 add hl,de
	 ex.s de,hl
	 ; Get the number of cycles between persistent vblank and previous vblank
	 ld.sis bc,(persistent_vblank_counter)
	 add hl,bc
	 ASSERT_NC
	 sbc.s hl,de
	 ; Check if the difference is unsigned (previous vblank can be earlier
	 ; due to rescheduling when the LCD is turned on).
	 ; This comparison should work in either single or double speed modes.
	 ld a,h
	 add a,((CYCLES_PER_VBLANK + 1) >> 7) + 1
	 jr c,_
	 ; Add the cycles since previous vblank, with 24-bit result
	 add hl,de
	 ASSERT_NC
	 ; Check if less than a frame passed since persistent vblank,
	 CPU_SPEED_IMM16($+2)
	 ld.sis de,CYCLES_PER_FRAME
	 sbc hl,de
_
	 ; Use persistent vblank if so
	 ld h,b
	 ld l,c
	; Restore value of DIV
	pop bc
	jr c,_
	ex de,hl
	sbc hl,bc
_
	ld.sis (vblank_counter),hl
	
	; Disable the LCD
	call do_lcd_disable
	jp.sis reschedule_event_PPU
	
	
lcd_enable_helper:
	; Enable the LCD
	
	; Check bit 0 of LCDC to update scanline fill logic
	bit 1,a
	ld a,$28 ;JR Z
	jr nz,_
	ASSERT_C
	sbc a,a ;A=BG_PALETTE_COLOR_0
	ld (scanline_fill_color_smc),a
LCDC_0_smc_gb = $+1
	ld a,$20 ;JR NZ or JR Z
_
	ld (LCDC_0_7_smc),a
	
	; Force skip rendering the first frame after the LCD is enabled,
	; this is consistent with hardware behavior to avoid glitch frames
	ld a,$7E ;RSMIX
	ld (z80codebase+updateSTAT_enable_catchup_smc),a
	ld (z80codebase+updateSTAT_full_enable_catchup_smc),a
	ld (z80codebase+ppu_mode0_enable_catchup_smc),a
	ld (z80codebase+ppu_mode2_enable_catchup_smc),a
	ld (z80codebase+ppu_lyc_enable_catchup_smc),a
	
	; Enable cache updates for STAT and LY registers
	ld a,$25 ;DEC H
	ld (z80codebase+updateSTAT_disable_smc),a
	ld (z80codebase+updateLY_disable_smc),a
	
	; Enable interrupt and rescheduling effects for LYC writes
	.db $21 ;ld hl,
	 cp SCANLINES_PER_FRAME
	 .db $38 ;jr c,
	ld (z80codebase+writeLYC_disable_smc),hl
	
	; Set up special handling for transitioning from fake mode 0
	ld a,$5B ;.LIL prefix
	ld (z80codebase+lcd_on_STAT_restore),a
	ld a,lcd_on_STAT_handler - (lcd_on_updateSTAT_smc + 1)
	ld (z80codebase+lcd_on_updateSTAT_smc),a
	ld a,$18 ;JR
	ld (lcd_on_stat_setup_mode_smc),a
	ld hl,($C9 << 16) | lcd_on_ppu_event_checker
	ld (lcd_on_stat_setup_event_smc),hl
	ld hl,lcd_on_STAT_handler
	ld.sis (event_counter_checker_slot_PPU),hl
	
	; Schedule previous vblank relative to now (plus 1 cycle because the LCD is wack)
	CPU_SPEED_IMM16($+1)
	ld hl,-(CYCLES_PER_VBLANK+1)
	xor a
	sbc hl,de
	ld.sis (vblank_counter),hl
	
	; Set the initial LYC prediction
	; Lines 1-144 are passed through, all others use 144
	ld (z80codebase+last_lyc_match),a
	ld hl,hram_base+LYC
	ld a,(hl)
	dec a
	cp VBLANK_SCANLINE
	; Also check if LYC=0
	inc a
	jr c,_
	ld a,VBLANK_SCANLINE
_
	ld (lyc_prediction_list+0),a
	ld l,STAT & $FF
	ld c,(hl)
	ld a,c
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
	ld l,h
	inc hl ;ld hl,active_ints
	set.s 1,(hl)
	dec hl
_
	
	ld l,HDMA5 & $FF
	bit 7,(hl)
	jr nz,_
	ld hl,hdma_counter_checker
	ld.sis (event_counter_checker_slot_HDMA),hl
	CPU_SPEED_IMM8($+1)
	ld hl,MODE_2_CYCLES + MODE_3_CYCLES - 1
	add hl,de
	ld.sis (hdma_counter),hl
	ld a,256-144
	ld (z80codebase+hdma_line_counter),a
	ld h,$FF
_
	
	; Set LY and STAT cache times for line 0, mode 2 (fake mode 0)
	; This is reduced by 1 cycle because of course it is
	CPU_SPEED_IMM8($+1)
	ld l,1-CYCLES_PER_SCANLINE
	sbc hl,de
	ld.sis (nextupdatecycle_LY),hl
	CPU_SPEED_IMM8($+1)
	ld de,MODE_0_CYCLES + MODE_3_CYCLES
	add hl,de
	ld.sis (nextupdatecycle_STAT),hl
	
	; Update PPU scheduler based on current value of STAT
	call stat_setup_c_forced
	; We didn't track whether an interrupt was requested, so just
	; trigger an event unconditionally
	jp.sis trigger_event_pop
	
	
lcd_on_STAT_restore_helper:
	ld a,$C9
	ld (z80codebase+lcd_on_STAT_restore),a
	ld a,updateSTAT_mode0_mode1 - (lcd_on_updateSTAT_smc + 1)
	ld (z80codebase+lcd_on_updateSTAT_smc),a
	ld a,$20 ;JR NZ,
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
	push bc
	 call.il reset_div
	pop bc
	sbc hl,bc
	ld i,hl
	jp.sis trigger_event
	
reset_div:
	ld hl,i
	add hl,bc
	ld b,h
	ld c,l
	; If bit 11 of DIV was already reset, delay audio counter
	ld a,b
	cpl
	CPU_SPEED_IMM8($+1) ; Use bit 12 for double speed
	and $08 ;(4096 >> 8) >> 1
	ld.sis hl,(hdma_counter)
	sbc hl,bc
	ld.sis (hdma_counter),hl
	add a,a
	ld (z80codebase+audio_counter+1),a
	ld.sis hl,(serial_counter)
	sbc hl,bc
	ld.sis (serial_counter),hl
	ld.sis hl,(vblank_counter)
	add hl,bc
	ld.sis (vblank_counter),hl
	ld.sis hl,(persistent_vblank_counter)
	add hl,bc
	ld.sis (persistent_vblank_counter),hl
	ld.sis hl,(ppu_counter)
	add hl,bc
	ld.sis (ppu_counter),hl
	ld.sis hl,(nextupdatecycle_STAT)
	add hl,bc
	ld.sis (nextupdatecycle_STAT),hl
	ld.sis hl,(nextupdatecycle_LY)
	add hl,bc
	ld.sis (nextupdatecycle_LY),hl
	; Update timer schedule, with logic to cause an instant TIMA increment
	; if the specified bit of DIV is moving from 1 to 0
	xor a
	ld b,a
	ld a,(hram_base+TIMA)
	cpl
	ld l,a
	ld a,(z80codebase+timer_cycles_reset_factor_smc)
	ld h,a
	; Only if the old bit of DIV is 0, add to the scheduled time
	; In effect, if the bit was 1, this schedules the next increment immediately
	and c
	xor h
	ld c,a
	mlt hl
	add.s hl,bc
	add hl,hl
	inc hl
	ld.sis (timer_counter),hl
	sbc hl,hl
	ret.l
	
gdma_transfer_helper:
	push bc
	 push de
	  ld hl,hram_base+HDMA5
	  ld b,(hl)
	  ld (hl),$FF
	  inc b
	  ld c,b
	  ld a,$C9 ;RET
	  ld (gbc_write_tilemap_for_dma_ret_smc_1),a
	  ld (gbc_write_tilemap_for_dma_ret_smc_2),a
	  ld (gbc_write_pixels_for_dma_ret_smc),a
	  ld h,hdma_port_value_base >> 8
	  dec hl
	  ld a,(hl)
	  dec hl
	  ld d,(hl)
	  and $F0
	  ld e,a
	  dec hl
	  ld a,(hl)
	  dec hl
	  ld h,(hl)
	  and $F0
	  ld l,a
	  ex.s de,hl
	  push ix
gdma_transfer_loop:
	   jr c,gdma_transfer_overflow
	   push bc
	    call hdma_single_transfer
	   pop bc
	   djnz gdma_transfer_loop
gdma_transfer_finish:
	  pop ix
	  ld a,d
	  ld (z80codebase+hdma_src_ptr),a
	  ld a,l
	  ld l,e
	  ld (z80codebase+hdma_src_ptr+1),hl
	  ld (z80codebase+hdma_dst_ptr+1),a
	  ld a,$40 ;.SIS
	  ld (gbc_write_tilemap_for_dma_ret_smc_1),a
	  ld (gbc_write_tilemap_for_dma_ret_smc_2),a
	  ld (gbc_write_pixels_for_dma_ret_smc),a
	  ; Consume cycles from the transfer
	  CPU_SPEED_IMM8($+1)
	  ld b,8
	  mlt bc
	  add ix,bc
	 pop de
	pop bc
	pop.s hl
	jp.s (hl)
	
gdma_transfer_overflow:
	   ld a,c
	   sub b
	   ld c,a
	   ld a,b
	   add a,$7F
	   ld (hram_base+HDMA5),a
	   jr gdma_transfer_finish
	
hdma_transfer_helper:
	; Consume cycles from the transfer
	CPU_SPEED_IMM8($+2)
	CPU_SPEED_END()
	pea ix+8
	 push bc
	  push de
	   ld a,$C9 ;RET
	   ld (gbc_write_tilemap_for_dma_ret_smc_1),a
	   ld (gbc_write_tilemap_for_dma_ret_smc_2),a
	   ld (gbc_write_pixels_for_dma_ret_smc),a
	   ld hl,z80codebase+hdma_dst_ptr+1
	   ld a,(hl)
	   dec hl
	   ld d,(hl)
	   and $F0
	   ld e,a
	   dec hl
	   ld a,(hl)
	   dec hl
	   ld h,(hl)
	   and $F0
	   ld l,a
	   ex.s de,hl
	   call hdma_single_transfer
	   ld a,h
	   ld h,l
	   ld l,a
	   ld.sis (hdma_dst_ptr),hl
	   ld h,e
	   ld l,d
	   ld.sis (hdma_src_ptr),hl
	   ld a,$40 ;.SIS
	   ld (gbc_write_tilemap_for_dma_ret_smc_1),a
	   ld (gbc_write_tilemap_for_dma_ret_smc_2),a
	   ld (gbc_write_pixels_for_dma_ret_smc),a
	  pop de
	 pop bc
	pop ix
	ld hl,hram_base+HDMA5
	dec (hl)
	jp.sis nc,hdma_transfer_finish
	set 7,(hl)
	jp.sis hdma_transfer_overflow
	
	; Input: DE = Game Boy source pointer
	;        HL = Game Boy destination pointer
	; Output: DE = source pointer plus 16
	;         HL = dest pointer plus 16
	;         Carry set if dest pointer overflowed
	; Destroys: AF, BC, IX
hdma_single_transfer:
	push hl
	 push de
	  ld a,h
	  ld c,l
	  ld hl,z80codebase+mem_read_lut
	  ld l,d
	  ld l,(hl)
	  inc h ;mem_get_ptr_routines
	  inc l \ inc l
	  ld ix,(hl)
	  add ix,de
	  ld e,c
	  and $1F
	  add a,$80
	  ld d,a
	  ld hl,(vram_bank_base)
	  add hl,de
	  cp $98
	  jr nc,hdma_single_transfer_tilemap_loop
hdma_single_transfer_pixel_loop:
	  ld de,(ix)
	  ld (hl),e
	  inc hl
	  ld (hl),d
	  push hl
	   call gbc_write_pixels_for_dma
	  pop hl
	  inc hl
	  lea ix,ix+2
	  ld a,l
	  and $0F
	  jr nz,hdma_single_transfer_pixel_loop
	  jr hdma_single_transfer_finish
	  
hdma_single_transfer_tilemap_loop:
	  ld a,(ix)
	  ld e,a
	  xor (hl)
	  ld (hl),e
	  ld d,a
	  push hl
	   call nz,gbc_write_tilemap_for_dma
	  pop hl
	  inc hl
	  inc ix
	  ld a,l
	  and $0F
	  jr nz,hdma_single_transfer_tilemap_loop
hdma_single_transfer_finish:
	  ld bc,16
	 pop hl
	 add.s hl,bc
	 ex de,hl
	pop hl
	add.s hl,bc
	ret
	
	; Input: Bit 7 of A indicates CPU speed
set_cpu_speed:
	APTR(CpuSpeedRelocs)
	ex de,hl
	rla
	ld hl,z80codebase+audio_counter+1
	ld b,5
	jr nc,set_cpu_single_speed
	sla (hl)
_
	ex de,hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ex de,hl
	sla (hl)
	inc hl
	rl (hl)
	djnz -_
	
	ld l,$3F ;CCF
	ld h,b ;NOP
	ld a,$8F ;ADC A,A
	jr set_cpu_any_speed
	
set_cpu_single_speed:
	srl (hl)
_
	ex de,hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ex de,hl
	inc hl
	scf
	rr (hl)
	dec hl
	rr (hl)
	djnz -_
	
	ld hl,$1DCB ;RR L
	ld a,b ;NOP
	ld b,$29 ;ADD HL,HL
set_cpu_any_speed:
	ld.sis (cpu_speed_factor_smc_1),hl
	ld.sis (cpu_speed_factor_smc_2),hl
	ld (apply_cpu_speed_shift_smc_1),a
	; Convert ADC A,A to ADD A,A and NOP to NOP
	res 3,a
	ld (apply_cpu_speed_shift_smc_2),a
	ld a,b
	ld (z80codebase+updateSTAT_full_speed_smc),a
	
	ld ix,(ArcBase)
	ld bc,cpu_speed_ram_start - program_end
	add ix,bc
	ld hl,cpu_speed_ram_start
	inc.s bc
	call apply_cpu_speed
	
	ld ix,(ArcBase)
	ld bc,z80code
	add ix,bc
	ld hl,z80codebase
	call apply_cpu_speed
	ret.l
	
apply_cpu_speed_byte:
	add ix,bc
	add hl,bc
apply_cpu_speed_word_finish:
	ld a,(ix)
apply_cpu_speed_shift_smc_1 = $
	adc a,a
	ld (hl),a
apply_cpu_speed:
	ld a,(de)
	inc de
	or a
	ret z
	ld b,0
	rra
	jr nc,apply_cpu_speed_short_offset
	ld b,a
	srl b
	ld a,(de)
	inc de
	rla
apply_cpu_speed_short_offset:
	rra
	ld c,a
	jr nc,apply_cpu_speed_byte
	add ix,bc
	add hl,bc
	ld a,(ix)
apply_cpu_speed_shift_smc_2 = $
	add a,a
	ld (hl),a
	inc ix
	inc hl
	jr apply_cpu_speed_word_finish
	
; Writes to the GB timer control (TAC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Updates the GB timer based on the new mode; applies SMC to getters/setters
;
; Inputs:  BC = current cycle offset
;          L' = value being written
;          (SPS) = Z80 return address
;          AFBCDEHL' are swapped
; Outputs: Current value written to (TAC)
;          GB timer updated
tac_write_helper:
	; Set new TAC value
	exx
	ld a,l
	exx
	or $F8
	ld (hram_base+TAC),a
	add a,4
	jr c,tac_write_enable
	ld hl,disabled_counter_checker
	ld.sis (event_counter_checker_slot_timer),hl
	ld hl,$C93D ;DEC A \ RET
	ld.sis (enableTIMA_smc),hl
	; If the corresponding bit of DIV is 1, increment TIMA
	ld hl,i
	add hl,bc
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
	
	 ld hl,$09AF ;XOR A \ ADD HL,BC
	 ld.sis (enableTIMA_smc),hl
	 ld a,c
	pop bc
	jp.sis tima_reschedule_helper
	
timer_smc_data:
	.db 6,$80
	.db 0,$02
	.db 2,$08
	.db 4,$20
	
	
writeVBK_helper:
	exx
	inc a
	ld a,(gbc_write_vram_last_slice)
	rra
	jr z,_
	call c,gbc_write_vram_catchup
	ld hl,vram_tiles_start-(((vram_start+$1800)*8) & $FFFFFF)
	ld (gbc_write_tilemap_bank_smc),hl
	ld hl,vram_pixels_start-((vram_start*4) & $FFFFFF)
	ld (gbc_write_pixels_bank_smc),hl
	ld a,(vram_gbc_base >> 8) & $FF
	ld (vram_bank_base_for_write+1),a
	ld hl,vram_bank_base+1-z80codebase
	jp.sis writeVBK_finish
_
	call c,gbc_write_vram_catchup
	ld hl,vram_tiles_start-(((vram_start+$3800)*8) & $FFFFFF)+1
	ld (gbc_write_tilemap_bank_smc),hl
	ld hl,vram_pixels_start-(((vram_start+$2000)*4) & $FFFFFF)+4
	ld (gbc_write_pixels_bank_smc),hl
	ld a,((vram_gbc_base + $2000) >> 8) & $FF
	ld (vram_bank_base_for_write+1),a
	ld hl,vram_bank_base+1-z80codebase
	jp.sis writeVBK_finish
	
	
overlapped_op_1_1_mismatch:
	lea hl,ix-3
	ld (hl),a
	lea bc,ix-7
	; Recompile an overlapped instruction again
	; Input: BC=start of overlapped handler call
	;        HL=overlap point in copied instruction
	;        IX=recompiled code start
	;        DE=cycle count
opgen_overlap_rerecompile:
	push bc
	 exx
	pop hl
	exx
	ld a,l
	ld (opgen_base_address_smc_1),a
	ld a,h
	lea hl,ix-2
	sub (hl)
	ld (opgen_base_address_smc_2),a
	ld ix,opgenroutines
	inc bc
	inc bc
	inc bc
	push de
	 push iy
	  call opgen_emit_overlapped_opcode
	 pop iy
	pop de
	jp handle_overlapped_op_done
	
overlapped_op_1_2_mismatch:
	lea hl,ix-4
	ld.s (hl),bc
	lea bc,ix-8
	jr opgen_overlap_rerecompile
	
handle_overlapped_op_1_1_helper:
	ex af,af'
	lea hl,ix+4
	exx
	ld e,a
	ld bc,z80codebase+4
	add ix,bc
	ld.s bc,(ix-3)
	ld hl,z80codebase+mem_read_lut
	ld a,c
	ld c,l ;C=0
	ld l,b
	ld l,(hl)
	inc h ;mem_get_ptr_routines
	inc l \ inc l
	ld hl,(hl)
	add hl,bc
	cp (hl)
	jr nz,overlapped_op_1_1_mismatch
	ld a,(ix-1)
	add a,e
	jr nc,handle_overlapped_op_done
	inc d
	jr nz,handle_overlapped_op_done
	lea hl,ix-4
	jr schedule_overlapped_event_helper_x_1
	
overlapped_op_2_1_mismatch:
	lea hl,ix-3
	ld (hl),a
	lea bc,ix-8
	jr opgen_overlap_rerecompile
	
handle_overlapped_op_1_2_helper:
	ex af,af'
	lea hl,ix+5
	exx
	ld e,a
	ld bc,z80codebase+5
	add ix,bc
	ld.s bc,(ix-3)
	ld hl,z80codebase+mem_read_lut
	ld a,c
	ld c,l ;C=0
	ld l,b
	ld l,(hl)
	inc h ;mem_get_ptr_routines
	inc l \ inc l
	ld hl,(hl)
	add hl,bc
	ld bc,(hl)
	ld h,a
	ld l,(ix-4)
	sbc.s hl,bc
	jr nz,overlapped_op_1_2_mismatch
	ld a,(ix-1)
	add a,e
	jr nc,handle_overlapped_op_done
	inc d
	jr nz,handle_overlapped_op_done
	lea hl,ix-5
	inc.s bc
	ld b,e
	lea de,ix-4
	jr schedule_overlapped_event_helper
	
handle_overlapped_op_2_1_helper:
	ex af,af'
	lea hl,ix+5
	exx
	ld e,a
	ld bc,z80codebase+5
	add ix,bc
	ld.s bc,(ix-3)
	ld hl,z80codebase+mem_read_lut
	ld a,c
	ld c,l ;C=0
	ld l,b
	ld l,(hl)
	inc h ;mem_get_ptr_routines
	inc l \ inc l
	ld hl,(hl)
	add hl,bc
	cp (hl)
	jr nz,overlapped_op_2_1_mismatch
	ld a,(ix-1)
	add a,e
	jr c,_
handle_overlapped_op_done:
	exx
	ex af,af'
	pop.s ix
	jp.s (hl)
_
	inc d
	jr nz,handle_overlapped_op_done
	lea hl,ix-5
schedule_overlapped_event_helper_x_1:
	ld b,e
	lea de,ix-3
; Inputs: HL = pointer to first byte of copied opcode
;         DE = pointer to first overlapping byte of copied opcode
;         IX = HL' = starting recompiled address
;         BCU = 0
;         B = cycle count before executing overlapped opcode (< 0)
;         A = cycle count at end of overlapped opcode (>= 0)
schedule_overlapped_event_helper:
	ld c,a
	ld a,d
	sub (ix-2)
	ld d,a
	; Check for a prefixed opcode
	ld a,(hl)
	cp $CB
	ld a,b
	jr nz,schedule_event_later_resolved
	; Prefixed overlapped opcodes are always 8 bytes recompiled
	lea ix,ix+8
	inc hl
	inc hl
	sbc hl,de
	ld d,0
	ld a,e
	jp.sis schedule_event_finish

; Inputs: DE = Game Boy address of jump instruction
;         IX = HL' = starting recompiled address
;         BCU = 0
;         B = cycles until end of sub-block (plus jump cycles, if applicable)
;         C = cycle count at end of sub-block (>= 0)
schedule_jump_event_helper:
	dec b
	dec b
	dec b
schedule_jump_event_helper_adjusted:
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
	push de
#endif
	 ld a,c
	 sub b
	 jr c,schedule_event_later_resolved_pushed
	pop de
	sbc hl,de
schedule_event_now_unresolved:
	ld.sis (event_gb_address),hl
	ld e,b
	ld d,0
	ld ixl,a
	ld ixh,d
#ifdef DEBUG
	ld hl,event_debug_address
	ld.sis (event_address),hl
#endif
	; This is a code path that could target the flush handler
flush_event_smc_1 = $+1
	jp.sis do_event_pushed
	
schedule_jump_event_absolute:
	dec b
	ld hl,(hl)
#ifdef VALIDATE_SCHEDULE
	inc hl
	dec.s hl
	ex de,hl
	call validate_schedule
	ex de,hl
#endif
	ld a,c
	sub b
	jr nc,schedule_event_now_unresolved
	inc hl
	dec.s hl
	add hl,de
schedule_event_later_resolved:
	push de
schedule_event_later_resolved_pushed:
	 ld de,opcounttable
	 ld b,e
	 push bc
	  ld c,3
	  call opcycle_first
	 pop de
	pop bc
	or a
	sbc hl,bc
	jp.sis schedule_event_finish
	
; Inputs: DE = Game Boy address at conditional branch
;         IX = HL' =  recompiled address after conditional branch
;         BCU = 0
;         B = cycles until end of sub-block (including conditional branch cycles)
;         C = cycle count at end of sub-block (>= 0)
schedule_subblock_event_helper:
	GET_BASE_ADDR_FAST
	ex de,hl
	add hl,de
	ld a,(hl)
	rlca
	jr nc,_
	bit 2,a
	jr z,++_
	;jp cond
	inc hl
	dec b
_
	;jr cond
	inc hl
_
	;ret cond
	inc hl
	dec b
	dec b
#ifdef VALIDATE_SCHEDULE
	call validate_schedule_resolved
#endif
	ld a,c
	sub b
	jr c,schedule_event_later_resolved
	sbc hl,de
	ld.sis (event_gb_address),hl
	ld e,b
	ld d,0
	ld ixl,a
	ld ixh,d
#ifdef DEBUG
	ld hl,event_debug_address
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
	
; Inputs: DE = Game Boy address at second byte of call instruction
;         IX = HL' =  starting recompiled address
;         BCU = 0
;         B = cycles until end of sub-block (plus jump cycles, if applicable)
;         C = cycle count at end of sub-block (>= 0)
schedule_call_event_helper:
	GET_BASE_ADDR_FAST
	add hl,de
	inc e
	jr nz,schedule_call_event_fast
schedule_jump_event_absolute_slow:
	; Handle possibly overlapped memory region
	ld a,(hl)
	inc d
	GET_BASE_ADDR_FAST
	add hl,de
	ld e,a
	jr schedule_event_helper_slow_finish
	
; Inputs: DE = Game Boy address of jump instruction plus 1
;         IX = HL' = starting recompiled address
;         BCU = 0
;         B = cycles until end of sub-block (plus 1 for non-taken jump)
;         C = cycle count at end of sub-block (>= 0)
;         A = negative cycles for jump
;  (-1 for untaken JR/RET, -2 for untaken JP, -3 for taken JR, -4 for taken JP)
schedule_slow_jump_event_helper:
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
schedule_call_event_fast:
	ld e,(hl)
	inc hl
schedule_event_helper_slow_finish:
	ld d,(hl)
	
; Inputs:  DE = starting Game Boy address
;          IX = HL' = starting recompiled address
;          B = cycles until end of sub-block
;          C = cycle count at end of sub-block (>= 0)
;          (SPS+1) = stack overflow counter
;          (SPS+2) = Game Boy BC
; Outputs: HL = event Game Boy address
;          IX = event recompiled address
;          A = event cycles to sub-block end
;          DE = cycle count at end of sub-block (>= 0)
schedule_event_helper:
	ld a,c
schedule_event_helper_a:
	sub b
	jr nc,schedule_event_now
schedule_event_later:
#ifdef VALIDATE_SCHEDULE
	call validate_schedule
#endif
	GET_BASE_ADDR_FAST
	push hl
	 add hl,de
	 ld de,opcounttable
	 ld b,e
	 push bc
	  ld c,3
	  call opcycle_first
	 pop de
	pop bc
	or a
	sbc hl,bc
	jp.sis schedule_event_finish

schedule_bridge_event_slow:
	dec b
	ld a,c
	sub b
	jr c,schedule_event_later
schedule_event_now:
#ifdef VALIDATE_SCHEDULE
	call validate_schedule
#endif
	ld.sis (event_gb_address),de
	ld e,b
	ld d,0
	ld ixl,a
	ld ixh,d
#ifdef DEBUG
	ld hl,event_debug_address
	ld.sis (event_address),hl
#endif
	; This is a code path that could target the flush handler
flush_event_smc_2 = $+1
	jp.sis do_event_pushed
	
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
	      ; Verify that BCU=0
	      ld hl,$FF0000
	      add hl,bc
	      ASSERT_NC
	      ; Check if target block is expected to be 0 cycles
	      ld a,b
	      or a
	      lea.s bc,ix
	      jr nz,_
	      ; Skip reverse lookup if the flush handler is the target
	      ; This is only valid when the target block is 0 cycles
	      ld hl,flush_handler
	      sbc hl,bc
	      ld.sis de,(flush_address)
_
	      call nz,lookup_gb_code_address
	     pop bc
	     sub b
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
	
	
mbc_change_rom_bank_helper:
	; Propagate the bank change to the topmost rom callstack entry
	ld c,a
	ld (mbc_change_rom_bank_smc),sp
	inc sp
	ld a,$BF
_
	pop hl
	cp l
	jp po,-_
	ld a,c
	xor h
	ld h,a
	push hl
	ld c,4
	mlt bc
	ld hl,rombankLUT
	add hl,bc
	ld a,(hl)
	inc hl
	ld hl,(hl)
	ld (rom_bank_base),hl
	ld (rom_bank_base_for_read),hl
curr_rom_bank_trim_msb = $+1
	ld c,0
	cp c
	jr nz,mbc_change_rom_bank_trim_msb
mbc_change_rom_bank_smc = $+1
	ld sp,0
	ld a,(z80codebase+curr_gb_stack_region)
	cp rom_bank_base & $FF
	jp.sis nz,mbc_no_fix_sp
	jp.sis mbc_fix_sp
	
mbc_change_rom_bank_trim_msb:
	ld (curr_rom_bank_trim_msb),a
	ld hl,z80codebase+mem_read_lut
	jr nc,mbc_change_rom_bank_trim_less
	ld l,c
	sub c
	ld bc,(rom_trimmed_get_ptr & $FF) * $010101
	ld (hl),c
	ld sp,hl
	ld hl,rom_bank_fill_routines
	ld l,a
	ld l,(hl)
	ld a,(z80codebase+curr_gb_stack_region)
	cp rom_bank_base & $FF
	jp (hl)
	
mbc_change_rom_bank_trim_less:
	ld l,a
	sub c
	ld bc,(rom_banked_get_ptr & $FF) * $010101
	ld (hl),c
	ld sp,hl
	ld hl,rom_bank_fill_routines
	ld l,a
	ld l,(hl)
	ld a,(z80codebase+curr_gb_stack_region)
	cp rom_bank_base & $FF
	jp (hl)
	
update_rtc_helper:
	ld ix,mpRtcSecondCount
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
	bit 6,(ix-4)
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
	 
	lea ix,ix-8
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
	ret.l
	
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