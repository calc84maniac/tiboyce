set_gb_stack:
	ex af,af'
	push hl
	 ld a,iyh
	 add a,a
	 jr c,_
	 add a,a
	 ld a,do_push_no_write
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
	 ld a,do_push
set_gb_stack_done:
	 ex de,hl
	 add iy,de
	 ex de,hl
	 ld (z80codebase+sp_base_address),hl
	pop hl
	ld (z80codebase+do_call_write_smc),a
	ld (z80codebase+do_interrupt_write_smc),a
	sub r_push_jr_end
	ld (z80codebase+r_push_jr_end-1),a
	ex af,af'
	ei
	jp.s (ix)

	
flush_normal:
	ex af,af'
	xor a
	ld (mpTimerCtrl),a
	call lookup_code
	ld.sis sp,myz80stack-2
	ld bc,(CALL_STACK_DEPTH+1)*256
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	exx
	ex af,af'
	ei
	jp.s (ix)
	
flush_mem:
	ex af,af'
	xor a
	ld (mpTimerCtrl),a
	dec de
	dec de
	dec de
	call.il lookup_gb_code_address
	call lookup_code
	ld.sis sp,myz80stack-2
	ld bc,(CALL_STACK_DEPTH+1)*256
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	exx
	ex af,af'
	ei
	jp.s (ix)
	
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
	
oam_transfer:
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
	
scroll_write:
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
	
lcdc_write:
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
	
lyc_write:
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
	ld.lil (mpTimerCtrl),a
	ret.l
	
tac_write:
	ld a,(hram_base+TAC)
	bit 2,a
	call.il nz,updateTIMA
	exx
	ex af,af'
	ld (hram_base+TAC),a
	ld l,a
	ex af,af'
	bit 2,l
	jr nz,_
	ld hl,mpIntEnable
	res 2,(hl)
	exx
	jr ++_
_
	xor a
	sub l
	and 3
	add a,a
	ld (tac_write_smc),a
	ld (tma_write_smc),a
	ld (tima_write_smc),a
	ld hl,-TIMA_LENGTH * 128
tac_write_smc = $+1
	jr $
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld (updateTIMA_smc),hl
	exx
	call.il tma_write_always
	ex af,af'
	call.il tima_write_always
	ex af,af'
	ld a,4
	ld (mpIntAcknowledge),a
	ld a,(mpIntEnable)
	bit 1,a
	jr z,_
	or 4
	ld (mpIntEnable),a
_
	ex af,af'
	pop.s ix
	jp.s (ix)
	
tma_write:
	ex af,af'
	ld (hram_base+TMA),a
	ex af,af'
	ld a,(hram_base+TAC)
	bit 2,a
	jr z,++_
tma_write_always:
	xor a
	ld (mpTimerCtrl),a
	exx
	ld a,(hram_base+TMA)
	neg
	ld l,a
	ld h,TIMA_LENGTH
	jr z,_
	mlt hl
_
tma_write_smc = $+1
	jr $
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld (mpTimer2Reset),hl
	exx
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
_
	ex af,af'
	ret.l
	
tima_write:
	ex af,af'
	ld (hram_base+TIMA),a
	ex af,af'
	ld a,(hram_base+TAC)
	bit 2,a
	jr z,++_
tima_write_always:
	xor a
	ld (mpTimerCtrl),a
	exx
	ld a,(hram_base+TIMA)
	neg
	ld l,a
	ld h,TIMA_LENGTH
	jr z,_
	mlt hl
_
tima_write_smc = $+1
	jr $
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld (mpTimer2Count),hl
	exx
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
_
	ex af,af'
	ret.l
	
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