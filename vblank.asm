; VBlank handler.
;
; This is called once per frame, even when the LCD is disabled.
; It finishes rendering the current frame and updates palettes and key inputs.
; In addition, it handles frame synchronization and frameskip logic.
;
; Inputs:  BC = current DIV time
;          DE = (negative) cycles until next PPU event
; Outputs: None
; Destroys AF,HL
vblank_helper:
	; Update emulated frame counter
frame_emulated_count = $+1
	ld hl,0
	ld a,l
	add a,1
	daa
	ld l,a
	ld a,h
	adc a,0
	daa
	ld h,a
	ld (frame_emulated_count),hl
	
	; Update real frame count if expired
	push de
	 push ix
	  push bc
	   push iy
	    ; Finish rendering, if applicable
	    ld a,(z80codebase+updateSTAT_enable_catchup_smc)
	    and 1
	    jp z,skip_this_frame
	    
	    ; Finish rendering the frame
	    ld a,144
	    inc.s bc ;BCU=0
	    call render_scanlines
	    
	    ; Draw sprites and do palette conversion, if the LCD is enabled
	    ld a,(hram_base+LCDC)
	    rla
	    jr nc,_
	    ld a,144
	    call draw_sprites
	    call sync_frame_flip
	    ld a,(regs_saved + STATE_SYSTEM_TYPE)
	    or a
	    call z,convert_palette
_
	    
preservedAreaHeight = $+1
	    ld a,0
	    or a
	    jr z,_
	    ld hl,(current_display)
	    ld de,(current_buffer)
PreservedAreaCopyLoop:
preservedAreaWidth = $+1
	    ld bc,0
	    ldir
preserve_copy_smc = $+1
	    ld c,160
	    add hl,bc
	    ex de,hl
	    add hl,bc
	    ex de,hl
	    dec a
	    jr nz,PreservedAreaCopyLoop
	    jr NoSpeedDisplay
	
_
	    ld hl,emulatorMessageText
	    or (hl)
speed_display_smc_1 = $+1
	    jr z,NoSpeedDisplay ;smc'd to YesSpeedDisplay
	    AJUMP(PutEmulatorMessage)
YesSpeedDisplay:
	    ld a,(turbo_active)
	    or a
speed_display_smc_2 = $
	    jr z,_
speed_display_smc_0 = $+1
	    ld a,0
low_perf_digits_smc = $+1
	    add a,0
	    daa
high_perf_digits_smc = $+1
	    ld a,0
speed_display_smc_3 = $
	    adc a,$FF
	    jr c,NoSpeedDisplay
	
_
	    xor a
	    ld c,a
	    ld hl,perf_digits
	    ld b,4
_
	    or (hl)
	    jr nz,_
	    inc hl
	    djnz -_
	    dec hl
	    inc b
_
	    inc b
_
	    push hl
	     push bc
	      ld b,(hl)
	      call display_digit
	     pop bc
	    pop hl
	    inc hl
	    inc c
	    djnz -_
	    ld l,c
	    ld h,6
PutEmulatorMessageRet:
	    call set_preserved_area
NoSpeedDisplay:
	
	    ; Swap buffers
	    call prepare_next_frame
	    call do_frame_flip
	    ld a,(active_scaling_mode)
	    or a
	    call nz,do_scale_fill
	    call sync_frame_flip
	
	    xor a
	    ; Signify frame was rendered
	    scf
skip_this_frame:

	    ld hl,frame_excess_count
turbo_active = $+1
	    ld b,1
	    dec b	; We want this to affect Z flag
	    jr nz,no_frame_sync
	   
	    ; Handle frame synchronization
	    dec (hl)
	    jp p,no_frame_sync
	    ; If we didn't render, save for later
	    jr nc,frame_sync_later
frame_sync_loop:
	    push hl
	     call sync_frame_flip_wait
	    pop hl
	    bit 7,(hl)
	    jr nz,frame_sync_loop
frame_sync_later:
	    ; Set Z
	    xor a
no_frame_sync:
	    
	    ; Handle frameskip logic
	    ; At this point A=0, Z holds auto state
	    ex de,hl
	    ld hl,skippable_frames
frameskip_type_smc:
	    jr z,no_frameskip	;JR no_frameskip when off, JR Z,$+2 when manual
	    dec (hl)
	    jr nz,frameskip_end
no_frameskip:
frameskip_value_smc = $+1
	    ld a,1
	    ld (hl),a
	    ex de,hl
	    bit 7,(hl)
	    jr nz,_
	    cp (hl)
	    jr nc,_
	    ld (hl),a
_
	    ld a,$4F-$7E
frameskip_end:
	    ;LD R,A if frame should be rendered, otherwise RSMIX
	    add a,$7E
	    ld (z80codebase+updateSTAT_enable_catchup_smc),a
	    ld (z80codebase+updateSTAT_full_enable_catchup_smc),a
	    ld (z80codebase+ppu_mode0_enable_catchup_smc),a
	    ld (z80codebase+ppu_mode2_enable_catchup_smc),a
	    ld (z80codebase+ppu_lyc_enable_catchup_smc),a

parse_keys:
	    ; Get keys
	    ld ix,mpKeypadGrp0
	    jp nz,parse_menu_keys

key_smc_turbo = $+2
	    bit 2,(ix+1*2)	;ZOOM
turbo_keypress_smc = $
	    jr z,_
turbo_toggle_smc = $+1
	    jr z,turbo_skip_toggle
turbo_active_no_toggle = $+1
	    ld a,(turbo_active)
	    xor 1
	    ld (turbo_active),a
turbo_skip_toggle:
	    ld a,(turbo_keypress_smc)
	    xor 8
	    ld (turbo_keypress_smc),a
_
	    
	    ld a,$FF
key_smc_right = $+2
	    bit 2,(ix+7*2)	;Right
	    jr z,_
	    dec a
_
key_smc_left = $+2
	    bit 1,(ix+7*2)	;Left
	    jr z,_
	    rlca
	    xor 2
_
key_smc_up = $+2
	    bit 3,(ix+7*2)	;Up
	    jr z,_
	    res 2,a
_
key_smc_down = $+2
	    bit 0,(ix+7*2)	;Down
	    jr z,_
	    sub 4
	    xor 8
	    set 2,a
_
	    ld (z80codebase+keys_low),a
	    ld l,a
	    ld a,$FF
key_smc_a = $+2
	    bit 5,(ix+1*2)	;2ND
	    jr z,_
	    dec a
_
key_smc_b = $+2
	    bit 7,(ix+2*2)	;ALPHA
	    jr z,_
	    res 1,a
_
key_smc_select = $+2
	    bit 7,(ix+3*2)	;X,T,0,n
	    jr z,_
	    res 2,a
_
key_smc_start = $+2
	    bit 6,(ix+1*2)	;MODE
	    jr z,_
	    res 3,a
_
	    ld (z80codebase+keys_high),a
	    ld h,a
	    ; Check if any key groups are selected
	    ld de,hram_base+P1
	    ld a,(de)
	    inc a
	    ret z
	    dec a
	    or $CF
	    bit 4,a
	    jr nz,_
	    and l
	    bit 5,a
	    jr nz,++_
_
	    and h
_
	    ld (de),a
	    ; Check if the old value was $F and the new value is not $F
	    or $F0
	    inc a
	    ld hl,z80codebase+lastP1state
	    tst a,(hl)
	    cpl
	    ld (hl),a
	    ret p
	    ; Request a joypad interrupt
	    ld l,e ;active_ints & $FF
	    ld h,e ;active_ints >> 8
	    set 4,(hl)
	    ret

parse_menu_keys:
key_smc_menu = $+2
	    bit 6,(ix+6*2)	;CLEAR
	    jr z,_
	    ex af,af'
	    push af
	     ACALL(emulator_menu_ingame)
	    pop af
	    ex af,af'
	    or a
	    jr nz,do_exit_trampoline
state_operation_denied:
	    ACALL(SetScalingMode)
	    call reset_preserved_area
	    jr keys_done
_
	    ld a,2
key_smc_save_state = $+2
	    bit 1,(ix+2*2)	;STO>
	    jr nz,_
key_smc_load_state = $+2
	    bit 2,(ix+2*2)	;LN
	    jr z,++_
	    call check_valid_state
	    ld hl,(current_state)
	    ld de,StateNotFoundMessage
	    jr z,load_state_not_found
_
	    ld (main_menu_selection),a
	    ACALL(BackupAndShowConfirmStateOperation)
	    jr z,state_operation_denied
	    ld a,4
	    ld (exitReason),a
do_exit_trampoline:
	    jr do_exit
_
key_smc_state_slot = $+2
	    bit 3,(ix+2*2)	;LOG
	    jr z,_
	    call update_state_with_numpad
	    ld a,l
	    ld (current_state),a
	    ld de,StateSlotMessage
load_state_not_found:
	    push hl
	     ld a,30
	     ACALL(SetEmulatorMessageWithDuration)
	    pop hl
	    jr keys_done
_

	    xor a
key_smc_brightness_up = $+2
	    bit 1,(ix+1*2)
	    jr nz,_
	    inc a
key_smc_brightness_down = $+2
	    bit 2,(ix+1*2)
	    jr z,++_
_
	    add a,a
	    dec a
	    ld hl,mpBlLevel
	    add a,(hl)
	    jr z,_
	    ld (hl),a
_
	
keys_done:
	    ld a,(mpIntMaskedStatus)
	    or a
	    jr z,_
	    ld (mpIntAcknowledge),a
#ifdef FASTLOG
	    ld hl,runtime_error
	    jr do_exit_any
#else
	    inc a
	    ld (exitReason),a
#endif
_
	    ; Reasons for exiting:
	    ; 1: load new game
	    ; 2: exit emulator
	    ; 3: restart game
	    ; 4: load or save state (depending on main_menu_selection)
	    ; 5: delete save state or ROM (depending on current_menu)
	    ; 5+(error*4): exit rom and show error
exitReason = $+1
	    or 0
	    jr z,_
do_exit:
	    APTR(ExitEmulation)
do_exit_any:
	    ex de,hl
	    ld hl,z80codebase+event_not_expired
	    ; Emit JP.LIL ExitEmulation
	    ld (hl),$5B \ inc hl \ ld (hl),$C3 \ inc hl \ ld (hl),de
_
	   pop iy
	  pop bc
	 pop ix
	pop de
	jp.sis ppu_scheduled
	
	; Input: IX = mpKeypadGrp0
	; Output: L = new state
update_state_with_numpad:
	ld a,(ix+3*2)
	ld l,'0'
	rra
	ret c
	inc l
	rra
	ret c
	ld e,3
	add hl,de
	rra
	ret c
	add hl,de
	rra
	ret c
	ld a,(ix+4*2)
	ld l,'2'
	rra
	rra
	ret c
	add hl,de
	rra
	ret c
	add hl,de
	rra
	ret c
	ld a,(ix+5*2)
	ld l,'3'
	rra
	rra
	ret c
	add hl,de
	rra
	ret c
	add hl,de
	rra
	ret c
	ld hl,(current_state)
	ret
	
; Acknowledges one or more interrupt sources and then waits on them.
; Interrupt is neither acknowledged nor handled once it triggers.
;
; Inputs:  DE = interrupt source mask to wait on
;          Interrupts are disabled
; Outputs: Original interrupt mask is restored
; Destroys IX,DE,HL
ack_and_wait_for_interrupt:
	ld (mpIntAcknowledge),de
	
; Waits on one or more interrupt sources. May return immediately.
; Interrupt is neither acknowledged nor handled once it triggers.
;
; Inputs:  DE = interrupt source mask to wait on
;          Interrupts are disabled
; Outputs: Original interrupt mask is restored
; Destroys BC,DE,HL
wait_for_interrupt:
	ld hl,mpIntEnable
	ld bc,(hl)
	ld (hl),de
	ex de,hl
	ld hl,z80codebase+rst38h
	ld (hl),$C9	;RET
	call.is wait_for_interrupt_stub
	ld (hl),$D9	;EXX
	ex de,hl
	ld (hl),bc
	ret
	
polled_interrupt:
	push ix
	 push de
	  push bc
	   call sync_frame_flip
	   ld hl,mpKeypadIntStat
	   ld a,(hl)
	   ld (hl),a
	   cpl
	   and 2
	   call z,parse_keys
	  pop bc
	 pop de
	pop ix
	jp.sis polled_interrupt_return

; Prepares to render the next frame.
; This swaps the current buffer, resets internal render variables,
; and prepares the current palette pointers.
;
; Inputs:  None
; Destroys: AF, BC, DE, HL, IX
prepare_next_frame:
	; Swap buffers
	ld de,(current_display)
	ld hl,(current_buffer)
	ld (current_display),hl
	ld (current_buffer),de
	ld hl,(scanlineLUT_ptr)
	ld a,l
	cp scanlineLUT_2 & $FF
	jr z,_
	ld hl,scanlineLUT_1
_
	ld (scanlineLUT_ptr),hl
	ld (scanlineLUT_sprite_ptr),hl
	ld (scanlineLUT_palette_ptr),hl
prepare_initial_frame:
	; Reset window state
	ld a,(hram_base+LCDC)
	rrca
	and $20
	add a,(vram_tiles_start >> 8) & $FF
	ld (window_tile_ptr+1),a
	ld a,$A9 ;XOR C
	ld (window_trigger_smc),a
	; Disable rendering catchup during vblank (or LCD off)
	xor a
	ld r,a
	ld (window_tile_offset),a
	ld (myLY),a
	ld (myspriteLY),a
	ld (mypaletteLY),a
	ld (z80codebase+sprite_catchup_available),a
	ld (BGP_max_frequency),a
	; Make the next rendering operation sync with frame flip,
	; if the frame flip hasn't happened yet
	ld hl,sync_frame_flip_or_wait
	ld (render_scanlines_wait_smc),hl
	; Get the negative end-of-buffer pointer
frame_dma_size_smc = $+1
	ld hl,0
	sbc hl,de
	ld (frame_flip_end_check_smc),hl
prepare_next_frame_gbc_smc = $
	; Clear the frequency count for the native BGP value
	ld a,(BGP_max_value)
	ld hl,BGP_frequencies
	ld l,a
	ld (hl),0
	; Get the indices for each palette type
	and 3
	add a,a
	add a,bg_palette_colors & $FF
	ld (update_palettes_bgp0_index),a
	inc h
	ld a,(hl)
	add a,a
	ld (update_palettes_bgp123_index),a
	ret
	
prepare_next_frame_gbc_no_adjust:
	; Fill the scanline usage LUT
	or a
	sbc hl,hl
	add hl,sp
	ld b,144/18
	ld de,$0A0A0A
	ld sp,scanline_sprite_counts + 144
_
	push de
	push de
	push de
	push de
	push de
	push de
	djnz -_
	ld sp,hl
	
	; Copy current palette values to the internal palette buffer,
	; without adjusting colors
	ld hl,z80codebase+gbc_bg_palette_data
	ld de,gbc_bg_transparent_colors
	ld bc,$0816
	ld a,l
_
	ldi
	ldi
	add a,b
	ld l,a
	jr nz,-_
	ld l,gbc_bg_palette_data & $FF
	ld b,a
	ld a,c
_
	inc hl
	inc hl
	ld c,a
	ldir
	cp l
	jr c,-_
	ld hl,z80codebase+gbc_obj_palette_data
_
	inc hl
	inc hl
	ld c,a
	ldir
	cp l
	jr c,-_
	ret
	
prepare_next_frame_gbc_adjust:
	; Fill the scanline usage LUT
	or a
	sbc hl,hl
	add hl,sp
	ld b,144/18
	ld de,$0A0A0A
	ld sp,scanline_sprite_counts + 144
_
	push de
	push de
	push de
	push de
	push de
	push de
	djnz -_
	ld sp,hl
	
	; Copy current palette values to the internal palette buffer,
	; adjusting colors
	ld ix,z80codebase+gbc_bg_palette_data
	ld hl,adjust_color_lut
	ld de,gbc_bg_transparent_colors
_
	ld bc,(ix)
	ld l,c
	ld a,b
	and h ;$73
	or (hl)
	ld l,a
	dec h ;adjust_green_lsb_lut
	ld a,(hl)
	add a,c
	ld (de),a
	inc de
	inc h
	inc h ;adjust_green_msb_lut
	ld a,(hl)
	adc a,b
	ld (de),a
	inc de
	dec h ;adjust_color_lut
	ld a,ixl
	add a,8
	ld ixl,a
	jr nc,-_
	call _
	inc ixh ;gbc_obj_palette_data >> 8
_
	ld ixl,(gbc_bg_palette_data+2) & $FF
_
	lea ix,ix+2
_
	ld bc,(ix-2)
	ld l,c
	ld a,b
	and h ;$73
	or (hl)
	ld l,a
	dec h ;adjust_green_lsb_lut
	ld a,(hl)
	add a,c
	ld (de),a
	inc de
	inc h
	inc h ;adjust_green_msb_lut
	ld a,(hl)
	adc a,b
	ld (de),a
	inc de
	dec h ;adjust_color_lut
	ld a,ixl
	and 7
	jr nz,--_
	or ixl
	lea ix,ix+4
	jr nz,-_
	ret
	
convert_palette_for_menu:
	; Get the current native BGP palette
	ld a,(BGP_max_value)
	ld e,a
	call convert_palette_setup
	ld hl,(current_display)
	ld c,240
_
	ld b,convert_palette_row_loop_count
convert_palette_row_smc_3 = $+1
	call convert_palette_row
	dec c
	jr nz,-_
	ret
	
do_scale_fill:
	ld hl,(current_display)
	ld ix,160
active_scaling_type = $+1
	ld b,0
	djnz do_scale_full
	ld a,(hram_base+SCY)
last_frame_scy = $+1
	ld b,0
	ld (last_frame_scy),a
last_scale_offset = $+1
	add a,0
	sub b
	jp m,++_
_
	sub 3
	jr nc,-_
_
	add a,3
	jr nc,-_
	ld (last_scale_offset),a
	ld b,a
	ld a,(hram_base+LCDC)
	and $20
	jr z,do_scale_full
	ld a,(hram_base+WY)
	dec a
	cp 143
	jr c,_
do_scale_full:
	ld a,143
_
	inc a
	push af
	 cpl
do_scale_fill_smc = $+1
	 call scale_offset
	 inc a
	pop bc
	ld c,a
	add a,b
	sub 144+6
	ld b,c
	
scale_offset:
	add a,3
	ret c
	djnz _
scale_offset_1_loop:
	lea de,ix
	ld c,e
	add hl,de
	ex de,hl
	add hl,de
	ldir
	ex de,hl
	lea hl,ix
	ld c,l
	add hl,de
	ldir
	add a,3
	jr nc,scale_offset_1_loop
	ret
_
	djnz _
	lea de,ix
scale_offset_2_loop:
	ld c,e
	ex de,hl
	add hl,de
	ex de,hl
	ldir
	lea hl,ix
	ld c,l
	add hl,de
	ex de,hl
	ldir
	ex de,hl
	lea de,ix
	add hl,de
	add a,3
	jr nc,scale_offset_2_loop
	ret
_
	ld b,0
scale_offset_0_loop:
	ex de,hl
	lea hl,ix
	ld c,l
	add hl,de
	ex de,hl
	ldir
	lea hl,ix
	ld c,l
	ex de,hl
	add hl,bc
	ex de,hl
	add hl,de
	ldir
	add a,3
	jr nc,scale_offset_0_loop
	ret
	
	
scale_offset_preserve:
scale_offset_preserve_smc_1 = $+1
	ld c,2
scale_offset_preserve_loop:
	add a,3
	jr nc,_
	; Switch to window offset
	pop de
	pop de
	ld b,a
	add a,d
	sub 144+6
	ld de,scale_offset
	push de
_
	push bc
	 call scale_offset_preserve_draw
	pop bc
	dec c
	jr nz,scale_offset_preserve_loop
	jr scale_offset
	
scale_offset_preserve_draw:
scale_offset_preserve_smc_2 = $+1
	ld c,0
	lea de,ix
	add hl,de
	dec hl
	push de
	 djnz _
	 add hl,de
	 ex de,hl
	 add hl,de
	 jr +++_
_
	 ex de,hl
	 add hl,de
	 ex de,hl
	 djnz _
	 push bc
	  lddr
	 pop bc
	 ex de,hl
	 inc hl
	pop de
	add hl,de
	ex de,hl
	add hl,de
	ex de,hl
	ldir
	lea hl,ix
	add hl,de
	ret
_
	 ld b,0
_
	 push bc
	  lddr
	 pop bc
	 ex de,hl
	 inc hl
	pop de
	add hl,de
	add hl,de
	ex de,hl
	add hl,de
	ldir
	ret
	
	
; Displays a digit onscreen at the given framebuffer offset in bytes.
; Draws to the current buffer.
;
; Inputs:  B = digit (0-9)
;          C = offset
; Outputs: A = 0
; Destroys AF,BC,DE,HL
display_digit:
	ld hl,(current_buffer)
	ld d,4
	ld e,c
	mlt de
	add hl,de
	ex de,hl
	ld hl,digits
	ld c,24
	mlt bc
	add hl,bc
	ld b,0
	ld a,6
_
	ld c,4
	ldir
	ex de,hl
	ld c,160 - 4
	add hl,bc
	ex de,hl
	dec a
	jr nz,-_
	ret

sync_frame_flip_or_wait:
	ld a,(mpLcdMis)
	or a
inc_real_frame_count_smc_1 = $+1
	call nz,inc_real_frame_count
	ld hl,(mpLcdCurr)
	ld de,(frame_flip_end_check_smc)
	add hl,de
	jr c,do_frame_flip_always
sync_frame_flip_wait:
	ld de,$000800
	call wait_for_interrupt
	ld a,(mpLcdMis)
	bit 3,a
	jr nz,sync_frame_flip_always
	call flip_gram_display
	jr sync_frame_flip_wait
	
sync_frame_flip:
	ld a,(mpLcdMis)
	or a
	ret z
sync_frame_flip_always:
inc_real_frame_count_smc_2 = $+1
	call inc_real_frame_count
do_frame_flip:
	ld hl,(mpLcdCurr)
frame_flip_end_check_smc = $+1
	ld de,0
	add hl,de
	ret nc
do_frame_flip_always:
	ld (frame_flip_end_check_smc),hl
	ld hl,(current_display)
	ld (mpLcdBase),hl
active_scaling_mode = $+1
	ld a,0
	or a
	call nz,flip_gram_draw_buffer
do_frame_flip_always_no_gram_flip:
	ld hl,sync_frame_flip
	ld (render_scanlines_wait_smc),hl
	
; Update the host LCD palettes based on the currently set GB palettes.
;
; Uses the overlapped_bg_palette_colors table as the source BG colors.
;
; Destroys AF,BC,DE,HL
update_palettes:
update_palettes_gbc_smc = $
update_palettes_bgp0_index = $+1
	ld hl,bg_palette_colors
	ld de,mpLcdPalette + (255*2)
	ld bc,2
	ldir
update_palettes_bgp123_index = $+1
	ld l,overlapped_bg_palette_colors & $FF
	ld d,mpLcdPalette >> 8 & $FF
	ld c,3*2
	ldir
	ret
	
update_palettes_gbc:
	ld hl,gbc_bg_transparent_colors
	ld de,mpLcdPalette + (GBC_BG_TRANSPARENT_COLORS*2)
	ld bc,8*2
	ldir
	ld e,GBC_BG_OPAQUE_COLORS*2
	ld c,24*2
	ld a,c
	ldir
	ld e,GBC_OBJ_OPAQUE_COLORS*2
	ld c,a
	ldir
	ld l,gbc_bg_opaque_colors & $FF
	ld e,GBC_BG_HIGH_PRIO_COLORS*2
	ld c,a
	ldir
	ret
	
	; Swap GRAM sub-buffers in stretched display mode to avoid tearing.
	; Input: A=1
	; Destroys AF,BC,DE,HL
flip_gram_draw_buffer:
gram_curr_draw_buffer = $+1
	xor 0
	ld (gram_curr_draw_buffer),a
	ld de,spiDrawBufferLeft
	jr z,_
	ld de,spiDrawBufferRight
_
	ld b,spiDrawBufferSize
	call spiFastTransfer
	; Check if VSYNC was reached before this command completed
	ld hl,mpLcdRis
	bit 2,(hl)
	ld (next_vertical_scroll),de
	jr nz,_
	
	; Enable LCD base address update interrupt for display buffer flip
	ld l,mpLcdImsc & $FF
	ld (hl),$0C
	ret
	
_
	; Manually reset the window address because VSYNC may have set the old one
	ld de,spiResetWindowAddress
	ld b,spiResetWindowAddressSize
	call spiFastTransfer
	; Go ahead and issue the GRAM display flip command instead of scheduling it
	ld a,4
	
flip_gram_display:
	ld (mpLcdIcr),a
	ld c,a
	
next_vertical_scroll = $+1
	ld de,spiVerticalScrollLeft
	ld b,spiVerticalScrollSize
	call spiFastTransfer
	
	; Reset LCD interrupt mask
	ld a,8
	ld (mpLcdImsc),a
	and c
	ret z
	jr inc_real_frame_count_always
	
inc_real_frame_count_or_flip_gram_display:
	bit 2,a
	jr nz,flip_gram_display
	ld a,(mpLcdImsc)
	xor 4
inc_real_frame_count:
	ld (mpLcdIcr),a
inc_real_frame_count_always:
frame_excess_count = $+1
	ld a,0
	inc a
	jp pe,_
	ld (frame_excess_count),a
_
	ld hl,emulatorMessageDuration
	xor a
	cp (hl)
	jr z,_
	dec (hl)
	jr nz,_
	inc hl ;emulatorMessageText
	ld (hl),a
	call reset_preserved_area
_
frame_real_count = $+1
	ld a,0
	add a,1
	daa
	ld (frame_real_count),a
	ret nc
	ld hl,(frame_emulated_count)
	ld de,perf_digits+3
	ld a,l
	ld (low_perf_digits_smc),a
	and $0F
	ld (de),a
	dec de
	xor l
	rrca
	rrca
	rrca
	rrca
	ld (de),a
	dec de
	ld a,h
	ld (high_perf_digits_smc),a
	and $0F
	ld (de),a
	dec de
	xor h
	rrca
	rrca
	rrca
	rrca
	ld (de),a
	sbc hl,hl
	ld (frame_emulated_count),hl
	;FALLTHROUGH
	
reset_preserved_area:
	or a
	sbc hl,hl
	
	; H = height, L = width / 4
	; Height must be a multiple of 5 plus-or-minus 1, or fullscreen mode will mess up
set_preserved_area:
	ld a,h
	ld (preservedAreaHeight),a
	or a
	jr z,set_no_preserved_area
	ld h,$FF
	inc a
_
	inc h
	sub 5
	jr nc,-_
	ld a,h
	ld (scale_offset_preserve_smc_1),a
	ld a,l
	add a,a
	add a,a
	ld (preservedAreaWidth),a
	cpl
	add a,161
	ld (preserve_copy_smc),a
	ld (scale_offset_preserve_smc_2),a
	ld hl,scale_offset_preserve
	jr _
set_no_preserved_area:
	ld hl,scale_offset
_
	ld (do_scale_fill_smc),hl
	ret
	
convert_palette:
	; Do setup for the final stretch of scanlines,
	; adding it to the queue but always run-length encoded
	ld hl,(BGP_write_queue_next)
	ld c,l
	ld a,(BGP_max_frequency)
	ld b,a
	ld a,(hram_base+BGP)
	ld d,a
	; If all 144 lines have the same palette take an early-out
	; The queue is guaranteed empty at this point, so just set the palette
	ld a,(mypaletteLY)
	or a
	jr z,_
	; Get the number of lines in the final stretch
	cpl
	add a,145
	; If zero, don't add anything to the queue
	jr z,++_
	; Add the line count (minus 1) and palette value to the queue
	dec a
	ld (hl),a
	inc l
	ld (hl),d
	inc l
	ld c,l
	; Add the line count to the frequency for this palette value
	inc h
	ld l,d
	ASSERT_C
	adc a,(hl)
	ld (hl),a
	dec h
	; If the frequency is greater than the previous max, set the new max
	cp b
	jr c,++_
_
	ld a,d
	ld (BGP_max_value),a
_
	; Consume from the start of the queue
	ld l,BGP_write_queue & $FF
	ld a,c
	cp l
	ret z
	ld b,0
scanlineLUT_palette_ptr = $+2
	ld ix,0
convert_palette_loop:
	push af
	 ; Check whether this is run-length or literal run of BGP values
	 ld a,(hl)
	 inc l
	 cp 144
	 jr nc,convert_multiple_palettes
	 ; Save the number of scanlines to iterate
	 inc a
	 ld c,a
	 ; Get the BGP value
	 ld e,(hl)
	 inc l
	 ; If it's the native palette value, skip conversion
	 ld a,(BGP_max_value)
	 xor e
	 jr z,_
	 push hl
	  ; Clear the frequency for this value
	  inc h
	  ld l,e
	  ld (hl),b
	  ; Set up palette conversion LUT
	  call convert_palette_setup
	  ; Convert all scanlines
convert_palette_multiple_row_loop:
	  ld hl,(ix+3)
	  ld b,convert_palette_row_loop_count
convert_palette_row_smc_1 = $+1
	  call convert_palette_row
	  lea ix,ix+6
	  dec c
	  jr nz,convert_palette_multiple_row_loop
	 pop hl
	 jr convert_palette_loop_continue
_
	 ; Advance the scanline LUT pointer by the number of lines
	 ld b,6
	 mlt bc
	 add ix,bc
	 jr convert_palette_loop_continue
	 
convert_multiple_palettes:
	 ; Get the number of literal BGP values to parse
	 sub 143
	 ld c,a
convert_multiple_palettes_row_loop:
	 ; Get the BGP value
	 ld e,(hl)
	 inc l
	 ; If it's the native palette value, skip conversion
BGP_max_value = $+1
	 ld a,0
	 xor e
	 jr z,_
	 push hl
	  ; Clear the frequency for this value
	  inc h
	  ld l,e
	  ld (hl),b
	  ; Set up palette conversion LUT
	  call convert_palette_setup
	  ; Convert this scanline
	  ld hl,(ix+3)
	  ld b,convert_palette_row_loop_count
convert_palette_row_smc_2 = $+1
	  call convert_palette_row
	 pop hl
_
	 lea ix,ix+6
	 dec c
	 jr nz,convert_multiple_palettes_row_loop
convert_palette_loop_continue:
	 ; Check if the end of the queue was reached
	pop af
	cp l
	jr nz,convert_palette_loop
	; Clear the queue
	ld l,BGP_write_queue & $FF
	ld (hl),$FF
	ld (BGP_write_queue_next),hl
	ld a,l
	ld (BGP_write_queue_literal_start),a
	jp sync_frame_flip
	
convert_palette_setup:
	ld hl,convert_palette_LUT + 3
	ld b,4
	ld a,e
_
	rlca
	rlca
	ld d,a
	and 3
	add a,BG_COLOR_0
	dec l
	ld (hl),a
	ld a,d
	djnz -_
	ex de,hl
	ret
	
#if 0
; Adjusts a 15-bit BGR color to more closely match a Game Boy Color screen.
;
; Input:  HL = 15-bit color
; Output: HL = 15-bit color (adjusted), carry clear
; Destroys: AF, BC, DE
;
; Uses a modification of an algorithm from https://byuu.net/video/color-emulation/:
;   R = min(30, (r * 13 + g *  2 + b *  1) >> 4);
;   G = min(30, (         g * 12 + b *  4) >> 4);
;   B = min(30, (r *  3 + g *  2 + b * 11) >> 4);
;
; To make the algorithm simpler, the combination portion is split into two parts:
;   B,G,R = (b,g,r * 12) >> 4 = b,g,r - (b,g,r >> 2);
;
;   R += (r * 1 + g * 2 + b *  1) >> 4;
;   G += b >> 2;
;   B += (r * 3 + g * 2 + b * -1) >> 4;
;
adjust_color:
	; D = r
adjust_color_enable_smc = $
	ld a,l
	and %00011111
	ld d,a
	
	; BC = bgr >> 2
	ld b,h
	ld a,l
	srl b \ rra
	srl b \ rra
	ld c,a
	
	; A = g * 2
	rrca
	rrca
	and %00111110
	; A += r
	add a,d
	; A += b
	add a,b
	; A >>= 1
	rra
	; E = (r + g*2 + b) >> 1
	ld e,a
	
	; A += (r*2) >> 1
	add a,d
	; A -= (b*2) >> 1
	sub b
	; A = (A >> 3) << 2
	rra
	and %11111100
	; D = ((r*3 + g*2 - b) >> 4) << 2
	; This is the offset to add to the blue component
	ld d,a
	
	; E = ((b >> 2) << 5) | (E >> 3)
	; These are the offsets to add to the green and red components
	ld a,e
	rrca
	rrca
	rrca
	rrca
	xor h
	and %10001111
	xor h
	rlca
	ld e,a

	; Mask out the upper two bits for each component of (bgr >> 2)
	ld a,b
	and %00011100
	ld b,a
	ld a,c
	and %11100111
	ld c,a
	; Subtract (bgr >> 2)
	sbc hl,bc
	; Add remaining adjustments
	add hl,de
	
	; Clamp all color components of 31 to 30
	; Check if lower 5 bits (red component) were all 1
	ld a,l
	cpl
	and %00011111
	jr nz,_
	; If so, clear the low bit (and set the low bit of A for the following)
	dec l
	inc a
_
	; Set lower 5 bits to 1 and get high 3 bits
	xor l
	; Check if low 3 bits of green component are all 1
	inc a
	ld a,h
	cpl
	jr nz,_
	; Check if high 2 bits of green component were also all 1
	tst a,%00000011
	jr nz,_
	; If so, clear the low bit of the green component
	res 5,l
_
	; Check if the bits of the blue component were all 1
	and %01111100
	ret nz
	; If so, clear the low bit of the blue component
	res 2,h
	ret
#else
; Adjusts a 15-bit BGR color to more closely match a Game Boy Color screen.
;
; Input:  BC = 15-bit color
; Output: BC = 15-bit color (adjusted)
; Destroys: AF, HL
;
; Modifies the green component by an amount specified by the upper 3 bits
; of blue and all 5 bits of green. The adjustment is gamma-aware,
; using 3/4 green plus 1/4 blue for GBC and 5/6 green plus 1/6 blue for GBA.
;
adjust_color:
adjust_color_enable_smc = $
	ld hl,adjust_color_lut
	ld l,c
	ld a,b
	and h ;$7C
	or (hl)
	ld l,a
	dec h
	ld a,(hl)
	add a,c
	ld c,a
	inc h
	inc h
	ld a,(hl)
	adc a,b
	ld b,a
	ret
#endif


render_scanline_fill:
	push de
	pop hl
	inc de
	ld bc,159
scanline_fill_color_smc = $+1
	ld (hl),WHITE
	ldir
	jp render_scanline_next
	
render_catchup_safe:
	inc.s bc ;BCU=0
; Catches up the renderer before changing an LCD register.
; Must be called only if the current frame is being rendered.
;
; Inputs:  (LY) = current scanline (0-143)
;          (STAT) = current mode (0, 2, 3)
;          AF' has been swapped
;          BCDEHL' have been swapped
; Outputs: Scanlines rendered if applicable
; Destroys: AF, BC, HL
render_catchup:
	; Disable catch-up until at least one more scanline passes
	ld hl,hram_base+STAT
	ld a,(hl)
	cpl
	ld r,a
	; Set A to 1 if in hblank, 0 otherwise
	rrca
	and l ;$41
	; Get value of LY, plus 1 if in hblank
	ld l,LY & $FF
	add a,(hl)
	; Input: A=LY, should always be <=144; BCU=0
render_scanlines:
#ifdef DEBUG
	cp 145
	jr nc,$
#endif
myLY = $+1
	ld c,0
	sub c
	ret z
	ASSERT_NC
	push de
	 ld b,a
	 push bc
render_scanlines_wait_smc = $+1
	  call sync_frame_flip
	  ; Handle any deferred VRAM writes
gbc_write_vram_last_slice_smc = $+1
	  ld a,(write_vram_last_slice)
	  rra
gbc_write_vram_catchup_smc = $+1
	  call c,write_vram_catchup
	 pop bc
	 push ix
	  push iy
scanlineLUT_ptr = $+2
	   ld iy,0
	   ld.sis (render_save_sps),sp
	   ld a,vram_tiles_start >> 16
	   ld mb,a
	   or a
	   ld hl,-6
	   add hl,sp
gbc_render_save_spl_smc = $+1
	   ld (render_save_spl),hl
render_scanline_loop:
	   push bc
	    ; Get current scanline pointer from LUT
	    ld de,(iy+3)
	    lea iy,iy+6
	   
	    ; Zero flag is reset
LCDC_0_7_smc = $
	    jr z,render_scanline_fill
SCY_smc = $+1
	    ld l,0
	    add hl,bc
	    ld h,32
	    mlt hl
	    ld a,l
SCX_smc_1 = $+1
	    ld l,0
	
LCDC_4_smc = $+2
LCDC_3_smc = $+3
	    ld.sis sp,(vram_tiles_start & $FFFF) + $80
	    add.s hl,sp
	    ld.s sp,hl
	 
	    ld hl,vram_pixels_start
	    rrca
	    rrca
	    ld l,a
	   
SCX_smc_2 = $+1
	    ld b,8
	 
LCDC_5_smc = $
	    ; Carry flag is reset
	    jr nc,scanline_no_window
	 
WY_smc = $+1
	    ld a,0
window_trigger_smc = $
	    xor c ; Replaced with SCF (Z flag remains reset)
	    jr z,do_window_trigger
do_window_trigger_continue:
WX_smc_1 = $
	    jr nc,scanline_no_window ; Replaced with JR
WX_smc_2 = $+1
	    ld a,0
	    sub b
gbc_scanline_do_render_smc_1 = $+1
	    call nc,scanline_do_render
	 
window_tile_ptr = $+2
	    ld.sis sp,(vram_tiles_start & $FFFF) + $80	;(+$2000) (-$80)
	 
window_tile_offset = $+1
	    ld hl,vram_pixels_start
	    ld a,l
	    add a,8
	    cp 64
	    jr c,_
	    ld a,(window_tile_ptr+1)
	    inc a
	    ld (window_tile_ptr+1),a
	    xor a
_
	    ld (window_tile_offset),a
	 
WX_smc_3 = $+1
	    ld b,0
	 
scanline_no_window:
	    ld a,167
	    sub b
gbc_scanline_do_render_smc_2 = $+1
	    call scanline_do_render
	 
render_scanline_next:
	    ; Advance to next scanline
	   pop bc
	   inc c
	   djnz render_scanline_loop
	   ld (scanlineLUT_ptr),iy
	   ld a,c
	   ld (myLY),a
	   ; Restore important Z80 things
	   ld a,z80codebase >> 16
	   ld mb,a
	   ; Allow sprite catchup
	   ld (z80codebase+sprite_catchup_available),a
	   ld.sis sp,(render_save_sps)
	  pop iy
	 pop ix
	 call sync_frame_flip
	pop de
	ret
	
do_window_trigger:
	    sub -$37 ; SCF (also set C here)
	    ld (window_trigger_smc),a
	    jr do_window_trigger_continue

draw_sprite_offscreen:
	pop hl
	ld b,ixl
_
	dec (hl)
	inc hl
	djnz -_
	pop.s ix
	jp draw_next_sprite_2

spiDrawBufferLeft:
	SPI_START
	SPI_CMD($2A)     ; Column address set
	SPI_PARAM16(0)   ;  Left bound
	SPI_PARAM16(159) ;  Right bound
	SPI_END
spiDrawBufferSize = $ - spiDrawBufferLeft
	
spiVerticalScrollLeft:
	SPI_START
	SPI_CMD($33)     ; Vertical scroll parameters
	SPI_PARAM16(160) ;  Top fixed area
	SPI_PARAM16(160) ;  Scrolling area
	SPI_PARAM16(0)   ;  Bottom fixed area
	SPI_CMD($37)     ; Vertical scroll amount
	SPI_PARAM16(0)   ;  Duplicate left side to right
	SPI_END
spiVerticalScrollSize = $ - spiVerticalScrollLeft
	
spiDrawBufferRight:
	SPI_START
	SPI_CMD($2A)     ; Column address set
	SPI_PARAM16(160) ;  Left bound
	SPI_PARAM16(319) ;  Right bound
	SPI_END
	
spiVerticalScrollRight:
	SPI_START
	SPI_CMD($33)     ; Vertical scroll parameters
	SPI_PARAM16(0)   ;  Top fixed area
	SPI_PARAM16(160) ;  Scrolling area
	SPI_PARAM16(160) ;  Bottom fixed area
	SPI_CMD($37)     ; Vertical scroll amount
	SPI_PARAM16(320) ;  Duplicate right side to left (somehow)
	SPI_END
	
spiResetWindowAddress:
	SPI_START
	SPI_CMD($B0)     ; RAM Control
	SPI_PARAM($02)   ;  RAM access from SPI, VSYNC interface
	SPI_PARAM($F0)
spiDisableRamAccessSize = $+1 - spiResetWindowAddress
	SPI_CMD($2C)     ; Reset to window start
	SPI_CMD($B0)     ; RAM Control
	SPI_PARAM($12)   ;  RAM access from RGB, VSYNC interface
	SPI_PARAM($F0)
	SPI_END
spiResetWindowAddressSize = $ - spiResetWindowAddress

