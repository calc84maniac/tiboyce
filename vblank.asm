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
	   ld a,(render_this_frame)
	   or a
	   jr z,skip_this_frame
	
	   ; Finish rendering the frame
	   ld a,144
	   call render_scanlines
	  
	   ; Display sprites
	   ld a,(hram_base+LCDC)
	   cpl
	   and $82
	   push iy
	    call z,draw_sprites
	   pop iy
	
	   ; Swap buffers
	   call prepare_next_frame
	   
	   ld a,(ScalingMode)
	   or a
	   jr z,++_
	   ; EXPAND DONG
	   ld a,144/3
	   ex de,hl
	   ld b,e	;B=0
_
	   push de
	   pop hl
	   ld c,160
	   add hl,bc
	   ex de,hl
	   ldir
	   ex de,hl
	   ld c,160
	   add hl,bc
	   push hl
	   pop de
	   add hl,bc
	   ex de,hl
	   ldir
	   dec a
	   jr nz,-_
_
	   
speed_display_smc_1 = $
	   jr z,NoSpeedDisplay
	   
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
	   sbc a,a
	   inc a
	   jr z,skip_this_frame	;Carry is set
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
	   push hl
	    push bc
	     ld b,(hl)
	     call display_digit
	    pop bc
	   pop hl
	   inc hl
	   inc c
	   djnz -_
	   ld b,10
	   call display_digit
NoSpeedDisplay:
	  
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
	   ACALL(emulator_menu)
	   ld hl,(curr_palettes)
	   call update_palettes_always
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
	   ; Emit JP.LIL CmdLoadSaveState
	   ld (hl),$5B \ inc hl \ ld (hl),$C3 \ inc hl \ ld (hl),de
_
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
	ld (frame_excess_count),a
	jp.sis frame_interrupt_return
	
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
	ld hl,(mpLcdBase)
	ld (current_buffer),hl
	ld a,h
	xor (gb_frame_buffer_1 ^ gb_frame_buffer_2)>>8
	ld h,a
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
	ld (mpLcdBase),hl
	ret
	
inc_real_frame_count:
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
	ret

	
; Displays a digit onscreen at the given framebuffer offset in bytes.
; Draws to the old buffer.
;
; Inputs:  B = digit (0-9)
;          C = offset
; Outputs: A = 0
; Destroys AF,BC,DE,HL
display_digit:
	ld hl,(current_buffer)
	ld a,h
	xor (gb_frame_buffer_1 ^ gb_frame_buffer_2)>>8
	ld h,a
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
	
setup_menu_palette:
	call convert_palette_setup
	
	ld hl,(mpLcdBase)
	ld bc,240*256+1
_
	push bc
	 call convert_palette_any_row
	pop bc
	djnz -_
	 
	; (MAG)ENTA | BLUE
	ld hl,($EA56 << 16) | $2882
	ld (mpLcdPalette),hl
	; OLIVE | MAG(ENTA)
	ld hl,($CA8B << 8) | ($EA56 >> 8)
	ld (mpLcdPalette+3),hl
	AJUMP(Set4BitWindow)