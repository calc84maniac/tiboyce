; Get a literal 24-bit pointer to the Game Boy stack.
; Does not use a traditional call/return, must be jumped to directly.
;
; This routine is jumped to whenever SP is set to a new value, excluding
; increment/decrement operations. In addition, if SP points to ROM,
; push operations are modified to disable the memory writes.
;
; Inputs:  HL = 16-bit Game Boy SP
;          IX = Z80-mode return address
;          BCDEHL' have been swapped
; Outputs: HL' = 24-bit literal SP
;          BCDEHL' have been unswapped
;          SMC applied to push operations
set_gb_stack:
	ex af,af'
	ex de,hl
	ld a,d
	; If $C000 or higher, assume pushes will be done first
	cp $C0
	jr c,_
	dec de
	ld a,d
	inc de
_
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
	ld hl,wram_base
	add a,a
	jr c,set_gb_stack_done_ram
	ld hl,vram_base
	add a,a
	jr nc,set_gb_stack_done_ram
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
	or 7    ;LD D,A or LD (HL),A
	ld (z80codebase+do_push_smc_5),a
	ld (z80codebase+do_push_smc_6),a
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
	 ld hl,$E5E5DD	;PUSH IX \ PUSH HL
	 ld (z80codebase+cycle_overflow_flush_smc),hl
flush_mem_finish:
	 call lookup_code
	pop hl
	ld.sis sp,myz80stack-2
	ld sp,myADLstack
	ld bc,(CALL_STACK_DEPTH+1)*256
	ei
	jp.sis dispatch_cycles_exx
	
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
	 ld (_+2),a
_
	 lea iy,iy+0
	 call.il get_gb_address
	 ex de,hl
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
	exx
	push hl
	 or a
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
	pop hl
	exx
	ex af,af'
	pop.s ix
	ei
	jp.s (ix)

	
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
	cp MODE_2_CYCLES + MODE_3_CYCLES
	ld a,e
	jr c,_
	inc e
_
	cp 144
	ld a,e
	push bc
	 push hl
	  call c,render_scanlines
	 pop hl
	pop bc
	ret
	
; Writes to an LCD scroll register (SCX,SCY,WX,WY). Also BGP.
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
render_this_frame = $+1
	 ld a,1
	 or a
	 call nz,render_catchup
	 ld a,l
	 sub SCY - ioregs
	 jr z,scroll_write_SCY
	 dec a
	 jr z,scroll_write_SCX
	 sub WY - SCX
	 jr c,scroll_write_BGP
	 jr nz,scroll_write_WX
	 ex af,af'
	 ld (WY_smc),a
	 jr scroll_write_done
	 
scroll_write_SCY:
	 ex af,af'
	 ld (SCY_smc),a
	 jr scroll_write_done
	
scroll_write_BGP:
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ; Only do things if BGP is changing
	 ld.s a,(hl)
	 cp c
	 jr z,scroll_write_done_swap
	 ld a,(render_this_frame)
	 or a
	 jr z,scroll_write_done_swap
	 ld a,(hram_base+LCDC)
	 bit 1,a
	 push bc
	  push hl
	   push iy
	    call nz,draw_sprites
	   pop iy
	   
	   ld a,(myLY)
	   ld (myspriteLY),a
	   ld hl,(scanlineLUT_ptr)
	   ld (scanlineLUT_sprite_ptr),hl
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
	 ex af,af'
scroll_write_done:
	 ld.s (hl),a
	pop hl
	exx
	pop.s ix
	ei
	jp.s (ix)
	
	
; Writes to the LCD control register (LCDC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Catches up the renderer before writing, and then applies SMC to renderer.
;
; Inputs:  A' = value being written
;          DE = current div cycle count
;          L = current scanline
;          AF' has been swapped
;          BCDEHL' have been swapped
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
; Outputs: Scanlines rendered if applicable, SMC applied, value written
;          AF' has been unswapped
;          BCDEHL' have been unswapped
lcdc_write_helper:
	 ex de,hl
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
	 jr z,_
	 bit 1,(hl)
	 push bc
	  push de
	   push iy
	    call z,draw_sprites
	   pop iy
	  pop de
	 pop bc
	 ld a,(myLY)
	 ld (myspriteLY),a
	 ld hl,(scanlineLUT_ptr)
	 ld (scanlineLUT_sprite_ptr),hl
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
	 jr z,return_from_write_helper
	 ld a,(LCDC_7_smc)
	 xor $08	;JR NZ vs JR Z
	 ld (LCDC_7_smc),a
	 ; Forcibly skip to scanline 0
	 sbc hl,hl
	 ld.sis (frame_cycle_target),hl
	 ld.sis hl,(div_cycle_count)
	 ld.sis (div_cycle_count),de
	 sbc hl,de
	 ld.sis de,(serial_cycle_count)
	 add hl,de
	 ld.sis (serial_cycle_count),hl
	pop hl
	exx
	ei
	jp.sis trigger_event_fast_forward
	
; Post-write operation for the LY compare register (LYC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Sets the new cycle target according to the new value of LYC.
; Triggers a GB interrupt if LY already matches the new LYC value.
;
; Inputs:  A' = value being written
;          A = cycles into scanline
;          E = current scanline
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
;          BCDEHL' are swapped
; Outputs: LYC and cycle targets updated
lyc_write_helper:
	 ; Scanline 153 changes to scanline 0 after 2 cycles for LYC purposes
	 cp 2
	 jr c,_
	 ld a,e
	 sub 153
	 jr nz,_
	 ld e,a
_
	 ex af,af'
	 ld d,a
	 ex af,af'
	 ld a,d
	 ld (hram_base + LYC),a
	 xor e
	 jr nz,_
	 ld hl,hram_base + STAT
	 bit 6,(hl)
	 jr z,_
	 ld l,IF - ioregs
	 set 1,(hl)
_
	 
	 ; Set new target
	 xor e
	 ld e,CYCLES_PER_SCANLINE
	 mlt de
	 jr nz,_
	 ld de,CYCLES_PER_SCANLINE * 153 + 2
_
	 ld.sis (current_lyc_target_count),de
	pop hl
	exx
	ei
	; Carry is reset
	jp.sis trigger_event
	
; Writes to the GB timer control (TAC).
; Does not use a traditional call/return, must be jumped to directly.
;
; Updates the GB timer based on the new mode; applies SMC to getters/setters
;
; Inputs:  DE = current div cycle count
;          A' = value being written
;          (SPS) = Z80 return address
;          (SPL) = saved HL'
;          BCDEHL' are swapped
; Outputs: Current value written to (TAC)
;          GB timer updated
tac_write_helper:
	 ; Set new TAC value
	 ex af,af'
	 ld (hram_base+TAC),a
	 ld c,a
	 ex af,af'
	 bit 2,c
	 jr nz,_
return_from_write_helper:
	pop hl
	exx
	ex af,af'
	pop.s ix
	ei
	jp.s (ix)
_
	 ; Get SMC data
	 ld a,c
	 and 3
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
	
; Writes to the GB timer count (TIMA).
; Does not use a traditional call/return, must be jumped to directly.
;
; Updates the GB timer based on the new value, if enabled.
;
; Inputs:  DE = current div cycle count
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
	 cpl
	 ld l,a
writeTIMA_smc = $+1
	 ld h,0
	 ld a,h
	 mlt hl
	 add hl,hl
	 add hl,de
	 add a,a
	 dec a
	 or l
	 ld l,a
	 inc hl
	 ld.sis (timer_cycle_target),hl
	pop hl
	exx
	ei
	jp.sis trigger_event
	
timer_smc_data:
	.db 6,$80
	.db 0,$02
	.db 2,$08
	.db 4,$20
	
; Inputs: DE = starting recompiled address
;         IY = cycle count at end of block (>= 0)
; Outputs: DE = event recompiled address
;          IX = event opcode address
;          A = event cycle count (negative)
	
schedule_event_helper:
	call.il lookup_gb_code_address
schedule_event_helper_post_lookup:
	or a
	jr z,schedule_event_never
	add a,iyl
	jr c,schedule_event_now
	
	ld hl,opcodecycles
	ld bc,0
	
schedule_event_cycle_loop:
	dec h
	ld l,(ix)
	ld c,(hl)
	add ix,bc
	dec h
	ld c,(hl)
	ex de,hl
	add hl,bc
	ex de,hl
	inc h
	inc h
	add a,(hl)	; If this is a block-ender, -1 is added and the loop exits.
	jr c,schedule_event_now
	inc c
	bit 3,c
	jr z,schedule_event_cycle_loop
	sub iyl
	jr schedule_event_anyway
	
schedule_event_now:
	sub iyl
	jr nc,schedule_event_never
	
schedule_event_anyway:
	ex de,hl
	ei
	jp.sis schedule_event_enable
	
schedule_event_never:
	; If we passed the end, disable event
	ld hl,event_value
	ei
	jp.sis schedule_event_disable
	
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
	 push hl
	  ld hl,(z80codebase+rtc_current)
	  ld (z80codebase+rtc_latched),hl
	  ld.sis hl,(rtc_current+3)
	  ld.sis (rtc_latched+3),hl
	 pop hl
	pop bc
_
	ld a,c
	and 1
	ld (mbc_rtc_last_latch),a
	exx
	ei
	jp.sis mbc_6000_denied
	
mbc_rtc_helper:
	ld a,(cram_bank_base+2)
	cp z80codebase>>16
	jr z,_
	bit 3,c
	jr nz,mbc_rtc_set_rtc_bank
mbc_rtc_helper_exit_ram:
	ei
	jp.sis mbc_ram
_
	bit 3,c
	jr nz,_
	call mbc_rtc_toggle_smc
	call update_rtc
	ld b,$60
	jr mbc_rtc_helper_exit_ram
mbc_rtc_set_rtc_bank:
	call mbc_rtc_toggle_smc
_
	call update_rtc
	ld a,c
	and 7
	ld c,a
	ld b,0
	ld ix,z80codebase+rtc_latched
	jp.sis mbc_ram_any
	
mbc_rtc_toggle_smc:
	ld a,(z80codebase+read_cram_bank_handler_smc)
	xor $19 ^ $94	;ADD.L IX,DE vs SUB.L A,IXH
	ld (z80codebase+read_cram_bank_handler_smc),a
	ld (z80codebase+write_cram_bank_handler_smc_1),a
	ld (z80codebase+mem_read_any_rtc_smc),a
	ld (z80codebase+mem_write_any_cram_smc_1),a
	ld a,(memroutine_rtc_smc_1)
	xor 0^5
	ld (memroutine_rtc_smc_1),a
	ld (z80codebase+write_cram_bank_handler_smc_2),a
	ld (z80codebase+mem_write_any_cram_smc_2),a
	ld a,(memroutine_rtc_smc_2)
	xor $18^$38	;JR vs JR C
	ld (memroutine_rtc_smc_2),a
	push hl
	 push de
	  ld hl,memroutineLUT + $A0
	  ld b,32
mbc_rtc_memroutine_smc_loop:
	  inc.s de
	  ld e,(hl)
	  inc h
	  ld d,(hl)
	  dec h
	  ld a,d
	  or e
	  jr z,++_ 
	  ld ix,z80codebase
	  add ix,de
	  ld a,l
	  cp $A4
	  lea de,ix+16
	  jr c,_
	  inc de
	  lea ix,ix+2
_
	  ld a,(de)
	  xor $09 ^ $84	;ADD.L IX,rr vs op.L A,IXH
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
	  djnz mbc_rtc_memroutine_smc_loop
	 pop de
	pop hl
	ret
	
update_rtc:
	push hl
	 push de
	  ld ix,mpRtcSecondCount
_
	  ld b,(ix)
	  ld e,(ix+4)
	  ld d,(ix+8)
	  ld hl,(ix+12)
	  ld a,(ix)
	  cp b
	  jr nz,-_
	  
	  ld ix,z80codebase+rtc_last
	  bit 6,(ix-1)
	  jr nz,update_rtc_halted
	
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
	  add a,b
	  ld b,a
	  add a,-60
	  jr nc,_
	  ld b,a
_
	  
	  ld a,(ix+1)
	  adc a,e
	  ld e,a
	  add a,-60
	  jr nc,_
	  ld e,a
_
	  
	  ld a,(ix+2)
	  adc a,d
	  ld d,a
	  add a,-24
	  jr nc,_
	  ld d,a
_
	  
	  ld a,(ix+3)
	  adc a,l
	  ld l,a
	  
	  ld a,(ix+4)
	  rla
	  or $FC
	  sra a
	  adc a,h
	  ld h,(ix+4)
	  rr h
	  rra
	  rl h
	  jr nc,_
	  set 7,h
_
	  
update_rtc_halted:
	  ld (ix),b
	  ld (ix+1),de
	  ld.s (ix+3),hl
	 pop de
	pop hl
	ret