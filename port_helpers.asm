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
	; vblank_counter and persistent_vblank_counter are in the past, so >= 0
	; ppu_counter and nextupdatecycles are in the future, so < 0
	ld a,b
	cp 4
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
	ld bc,arc_program_start + (cpu_speed_ram_start - program_start)
	add ix,bc
	ld hl,cpu_speed_ram_start
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


reset_div:
	ld hl,i
	add hl,bc
	ld b,h
	ld c,l
	; If bit 11 of DIV was already reset, delay audio counter
	ld a,b
	cpl
cpu_speed_ram_start = $
	CPU_SPEED_START()
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

dma_write_helper:
	; Catch up background rendering
	ld a,r
	call m,render_catchup
	; Render the existing OAM data if applicable
	ld a,(z80codebase+sprite_catchup_available)
	rla
	call c,sprite_catchup
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
	GET_GB_ADDR_FAST
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
	  ; Catchup sprites if the BG has been rendered ahead
	  ld a,(z80codebase+sprite_catchup_available)
	  rla
	  call c,sprite_catchup
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
	ld (z80codebase+ppu_post_vblank_initial_lyc_prediction),a
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

stat_setup_hblank_this_line:
	ld (z80codebase+ppu_mode0_LY),a
	; Check if LY=LYC
	bit 2,c
	CPU_SPEED_IMM8($+2)
	ld.sis bc,MODE_0_CYCLES
	jr nz,stat_setup_done_add
	ld de,ppu_expired_mode0_maybe_lyc_block
	jr stat_setup_done_add

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
	  ld a,(z80codebase+sprite_catchup_available)
	  rla
	  call c,sprite_catchup
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
#ifdef NO_PORTS
	push.s de
#else
	call.is push_z80_de_safe
#endif
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
	jp.sis trigger_event_resolved

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
	jp.sis trigger_event_resolved

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
