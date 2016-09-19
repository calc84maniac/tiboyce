vblank_stuff:
	; Reset match bit
	ld (hl),2
	
	; Update FPS counter
vfps = $+1
	ld a,0
	add a,1
	daa
	ld (vfps),a
	
	; Finish rendering, if applicable
	push de
	 push ix
	  ld a,(render_this_frame)
	  or a
	  jr z,skip_this_frame
	
	  push bc
	   ; Finish rendering the frame
	   ld a,144
	   call render_scanlines
	  
	   ; Display sprites
	   ld a,(hram_base+LCDC)
	   rla
	   push iy
	    call c,draw_sprites
	   pop iy
	
	   ; Swap buffers
	   call prepare_next_frame
	   
#ifndef DBGNOSCALE
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
#else
	   xor a
#endif
	   
fps_display_smc:
	   jr z,NoFPSDisplay
	   
fps = $+1
	   ld a,0
	   add a,1
	   daa
	   ld (fps),a
	   
	   ld a,(mpRtcSecondCount)
last_second = $+1
	   cp -1
	   call nz,update_fps
	   
	   ld de,0
vfps_display_tens = $+1
	   ld a,0
	   call display_digit
	   ld de,4
vfps_display_ones = $+1
	   ld a,0
	   call display_digit
	   ld de,12
fps_display_tens = $+1
	   ld a,0
	   call display_digit
	   ld de,16
fps_display_ones = $+1
	   ld a,0
	   call display_digit
	   
	   xor a
NoFPSDisplay:
	  pop bc
	  
	  ; Signify frame was rendered
	  scf
skip_this_frame:
	  
	  ; Handle frame synchronization
	  ld hl,z80codebase+frame_excess_count
	  dec (hl)
	  jp p,no_frame_sync
	  ; If we didn't render, save for later
	  jr nc,frame_sync_later
frame_sync_loop:
	  push hl
	   ld de,$000800
	   call wait_for_interrupt
	   ld hl,mpLcdIcr
	   ld (hl),4
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
	  jr z,no_frameskip	;JR no_frameskip when off, JR $+2 when manual
	  dec (hl)
	  jr nz,frameskip_end
no_frameskip:
frameskip_value_smc = $+1
	  ld (hl),1
	  ex de,hl
	  bit 7,(hl)
	  jr nz,_
	  ld (hl),a
_
	  inc a
frameskip_end:
	  ld (render_this_frame),a
	  
	  ; Always update palettes because it's basically free!
	  call update_palettes
	  
	  ; Get keys
	  scf
	  sbc hl,hl
	  ld ix,mpKeypadGrp0

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
_

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
	ld hl,mpTimerIntStatus
	ld a,(hl)
	ret.l
	
	; DE = interrupt source mask to wait on
	; Destroys: IX, DE, HL
	; Interrupt is acknowledged before waiting,
	; but not handled or acknowledged afterward
ack_and_wait_for_interrupt:
	ld (mpIntAcknowledge),de
	
	; DE = interrupt source mask to wait on
	; Destroys: IX, DE, HL
	; Interrupt is not actually handled or acknowledged
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
	
prepare_next_frame:
	ld hl,scanlineLUT + (15*3)
	ld (scanlineLUT_ptr),hl
	ld hl,(mpLcdBase)
	ld (current_buffer),hl
	ld (scanline_ptr),hl
	ld a,h
	xor (gb_frame_buffer_1 ^ gb_frame_buffer_2)>>8
	ld h,a
#ifndef DBGNOSCALE
	ld a,48 + $55
	ld (scanline_scale_accumulator),a
#endif
	ld a,(hram_base+LCDC)
	rrca
	and $20
	add a,(vram_tiles_start >> 8) & $FF
	ld (window_tile_ptr+1),a
	xor a
	ld (window_tile_offset),a
	ld (myLY),a
	ld (mpLcdBase),hl
	ret
	
update_fps:
	ld (last_second),a
	
	ld a,(vfps)
	ld e,a
	and $0F
	ld (vfps_display_ones),a
	xor e
	rrca
	rrca
	rrca
	rrca
	ld (vfps_display_tens),a
	
	ld a,(fps)
	ld e,a
	and $0F
	ld (fps_display_ones),a
	xor e
	rrca
	rrca
	rrca
	rrca
	ld (fps_display_tens),a
	
	xor a
	ld (vfps),a
	ld (fps),a
	ret
	
	; Digit in A, output at offset DE
display_digit:
	ld c,a
	ld hl,(current_buffer)
	ld a,h
	xor (gb_frame_buffer_1 ^ gb_frame_buffer_2)>>8
	ld h,a
	add hl,de
	ex de,hl
	ld hl,digits
	ld b,40
	mlt bc
	add hl,bc
	ld a,10
_
	ld bc,160
	ldi
	ldi
	ldi
	ldi
	ex de,hl
	add hl,bc
	ex de,hl
	dec a
	jr nz,-_
	ret
	

palette_obj1_colors:
	.dw $0421 * 31 + $8000
	.dw $0421 * 21 + $8000
	.dw $0421 * 10
	.dw $0421 * 0

palette_obj0_colors:
	.dw $0421 * 31 + $8000
	.dw $0421 * 21 + $8000
	.dw $0421 * 10
	.dw $0421 * 0

palette_bg_colors:
	.dw $0421 * 31 + $8000
	.dw $0421 * 21 + $8000
	.dw $0421 * 10
	.dw $0421 * 0

update_palettes:
	ld hl,(hram_base+BGP)
curr_palettes = $+1
	ld de,$FFFFFF
	or a
	sbc hl,de
	ret z
	add hl,de
	ld (curr_palettes),hl
	ld de,mpLcdPalette + (9*2)-1
	push bc
	 ld ix,palette_obj1_colors+1-8
	 ld c,(9*2) + 3
update_palettes_next_loop:
	 lea ix,ix+8
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
	 inc de
	 ld e,16*2-1
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
	pop bc
	ret