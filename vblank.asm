; VBlank handler.
;
; This is called once per frame, even when the LCD is disabled.
; It finishes rendering the current frame and updates palettes and key inputs.
; In addition, it handles frame synchronization and frameskip logic.
;
; Inputs:  None
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
	ld h,a
	ld (frame_emulated_count),hl
	
	; Finish rendering, if applicable
	push de
	 push ix
	  push bc
	   push iy
	    ld a,(render_this_frame)
	    or a
	    jp z,skip_this_frame
	
	    ; Finish rendering the frame
	    ld a,144
	    call render_scanlines
	  
	    ; Display sprites
	    ld a,(hram_base+LCDC)
	    rla
	    call c,draw_sprites
	
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
	    add hl,hl
	    ld h,6
PutEmulatorMessageRet:
	    call set_preserved_area
NoSpeedDisplay:
	
	    ; Swap buffers
	    call prepare_next_frame
	    ld (mpLcdBase),hl
	    ld a,(ScalingMode)
	    or a
	    call nz,do_scale_fill
	
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
	     ld de,$000800
	     call wait_for_interrupt
	     call update_palettes
	     ld hl,mpLcdIcr
	     ld (hl),4
	     call inc_real_frame_count
	    pop hl
	    inc (hl)
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
	    ld a,1
frameskip_end:
	    ld (render_this_frame),a
	  
	    ; Get keys
	    scf
	    sbc hl,hl
	    ld ix,mpKeypadGrp0

key_smc_turbo:
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
	   
key_smc_right:
	    bit 2,(ix+7*2)	;Right
	    jr z,_
	    dec l
_
key_smc_left:
	    bit 1,(ix+7*2)	;Left
	    jr z,_
	    bit 0,l
	    set 0,l
	    jr z,_
	    res 1,l
_
key_smc_up:
	    bit 3,(ix+7*2)	;Up
	    jr z,_
	    res 2,l
_
key_smc_down:
	    bit 0,(ix+7*2)	;Down
	    jr z,_
	    bit 2,l
	    set 2,l
	    jr z,_
	    res 3,l
_
key_smc_a:
	    bit 5,(ix+1*2)	;2ND
	    jr z,_
	    dec h
_
key_smc_b:
	    bit 7,(ix+2*2)	;ALPHA
	    jr z,_
	    res 1,h
_
key_smc_select:
	    bit 7,(ix+3*2)	;X,T,0,n
	    jr z,_
	    res 2,h
_
key_smc_start:
	    bit 6,(ix+1*2)	;MODE
	    jr z,_
	    res 3,h
_
	    ld.sis (keys),hl

key_smc_menu:
	    bit 6,(ix+6*2)	;CLEAR
	    jr z,_
	    ex af,af'
	    push af
	     ACALL(emulator_menu_ingame)
	    pop af
	    ex af,af'
	    ld hl,(curr_palettes)
	    call update_palettes_always
	    ACALL(SetScalingMode)
	    call reset_preserved_area
_
exitReason = $+1
	    ld a,0
	    or a
	    jr z,_
	    APTR(ExitEmulation)
	    ex de,hl
	    ld hl,z80codebase+not_expired
	    ; Emit DI
	    ld (hl),$F3 \ inc hl
	    ; Emit JP.LIL ExitEmulation
	    ld (hl),$5B \ inc hl \ ld (hl),$C3 \ inc hl \ ld (hl),de
_
	   pop iy
	  pop bc
	 pop ix
	pop de

	; Trigger VBLANK
	ld hl,hram_base+LCDC
	bit 7,(hl)
	jr z,_
	inc hl
	bit 4,(hl)
	ld l,IF & $FF
	set 0,(hl)
	jr z,_
	set 1,(hl)
_
	ei
	jp.sis vblank_handler_ret
	
	
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
; Destroys IX,DE,HL
wait_for_interrupt:
	ld hl,mpIntEnable
	ld ix,(hl)
	ld (hl),de
	ex de,hl
	ld hl,z80codebase+rst38h
	ld (hl),$C9	;RET
	call.is wait_for_interrupt_stub
	ld (hl),$F5	;PUSH AF
	ex de,hl
	ld (hl),ix
	ret
	
frame_interrupt:
	ld (mpLcdIcr),a
	push hl
	 push de
	  push bc
	   push ix
	    call update_palettes
	    call inc_real_frame_count
	   pop ix
	  pop bc
	 pop de
	pop hl
frame_excess_count = $+1
	ld a,0
	inc a
_
	ld (frame_excess_count),a
	jp.sis po,frame_interrupt_return
	; Revert $80 to $7F and set parity odd
	xor $FF
	jr -_
	
; Prepares to render the next frame.
; This swaps the current buffer and resets internal render variables.
;
; Inputs:  None
; Outputs: HL = old framebuffer
;          A = 0
prepare_next_frame:
	ld hl,(scanlineLUT_ptr)
	ld a,l
	cp scanlineLUT_2 & $FF
	jr z,_
	ld hl,scanlineLUT_1
	ld (scanlineLUT_ptr),hl
_
	ld (scanlineLUT_sprite_ptr),hl
	ld (scanlineLUT_palette_ptr),hl
	ld hl,(hram_base+BGP)
	ld (curr_palettes),hl
	ld a,(hram_base+LCDC)
	rrca
	and $20
	add a,(vram_tiles_start >> 8) & $FF
	ld (window_tile_ptr+1),a
	xor a
	ld (window_tile_offset),a
	ld (myLY),a
	ld (myspriteLY),a
	ld (mypaletteLY),a
swap_buffers:
	ld de,(current_display)
	ld hl,(current_buffer)
	ld (current_display),hl
	ld (current_buffer),de
	ret
	
	
inc_real_frame_count:
	ld hl,emulatorMessageDuration
	ld a,(hl)
	or a
	jr z,_
	dec a
	ld (hl),a
	jr nz,_
	ld (emulatorMessageText),a
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
	
	; H = height, L = width / 2
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
	ld a,(ScalingMode)
	or a
	ld a,l
	jr nz,_
	add a,a
_
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
	
	
do_scale_fill:
	ld ix,160
	ld a,(ScalingType)
	ld b,a
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
display_digit_smc_1 = $+1
	ld d,2
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
display_digit_smc_2 = $+1
	ld c,2
	ldir
	ex de,hl
display_digit_smc_3 = $+1
	ld c,160 - 2
	add hl,bc
	ex de,hl
	dec a
	jr nz,-_
	ret

; Update the host LCD palettes based on the currently set GB palettes.
; No operation if the GB palettes have not changed since this was last called.
;
; Uses the palette_XXX_colors arrays as the source colors for each type.
;
; Destroys AF,DE,HL,IX
update_palettes:
curr_palettes = $+1
	ld hl,$FFFFFF
old_palettes = $+1
	ld de,$FFFFFF
	or a
	sbc hl,de
	ret z
	add hl,de
	ld (old_palettes),hl
	; Input: Palettes in HL
update_palettes_always:
	ld c,(9*2) + 3
update_palettes_partial:
	ld de,mpLcdPalette + (9*2)-1
	ld ix,palette_obj1_colors+1+8
update_palettes_next_loop:
	lea ix,ix-8
	ld b,4
update_palettes_loop:
	xor a
	add hl,hl
	adc a,a
	add hl,hl
	adc a,a
	add a,a
	djnz _
	dec c
	jr nz,update_palettes_next_loop
	; Early out for partial update
	inc e
	ret nz
	ld de,mpLcdPalette + (256*2)-1
	scf
_
	ld (update_palettes_smc),a
	push hl
update_palettes_smc = $+2
	 lea hl,ix
	 ldd
	 ldd
	pop hl
	jr nc,update_palettes_loop
	ex de,hl
	inc hl
	ld de,mpLcdPalette + (16*2)-2
	ldi
	ldi
	ret
	
convert_palette:
	ld c,a
	call convert_palette_setup
convert_palette_row_loop:
	ld hl,(ix)
convert_palette_any_row:
	ld b,160 / 4
convert_palette_pixel_loop:
	ld e,(hl)
	ld a,(de)
	ld (hl),a
	inc hl
	ld e,(hl)
	ld a,(de)
	ld (hl),a
	inc hl
	ld e,(hl)
	ld a,(de)
	ld (hl),a
	inc hl
	ld e,(hl)
	ld a,(de)
	ld (hl),a
	inc hl
	djnz convert_palette_pixel_loop
	lea ix,ix+3
	dec c
	jr nz,convert_palette_row_loop
	ret
	
convert_palette_setup:
	ld a,(ScalingMode)
	or a
	ld hl,convert_palette_LUT + $23
	ld b,4
	ld a,(hram_base+BGP)
	jr z,convert_palette_setup_noscale
_
	rlca
	rlca
	ld d,a
	and 3
	add a,BG_COLOR_0
	ld e,a
	ld a,d
	ld d,$11
	mlt de
	dec l
	ld (hl),e
	jr z,_
	ld de,-$10
	add hl,de
_
	djnz --_
	ex de,hl
	ret
	
convert_palette_setup_noscale:
	ld l,3
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
	
convert_palette_for_menu:
	call convert_palette_setup
	
	ld hl,(mpLcdBase)
	ld bc,240*256+1
_
	push bc
	 call convert_palette_any_row
	pop bc
	djnz -_
	ret
	
setup_menu_palette:
	; (MAG)ENTA | BLUE
	ld hl,($EA56 << 16) | $2882
	ld (mpLcdPalette),hl
	; OLIVE | MAG(ENTA)
	ld hl,($CA8B << 8) | ($EA56 >> 8)
	ld (mpLcdPalette+3),hl
	; (WH)ITE | BLACK
	ld hl,($FFFF << 16) | $0000
	ld (mpLcdPalette+26),hl
	; GRAY | WH(ITE)
	ld hl,($4210 << 8) | ($FFFF >> 8)
	ld (mpLcdPalette+29),hl
	ld hl,(current_buffer)
	AJUMP(Set4BitWindowAny)