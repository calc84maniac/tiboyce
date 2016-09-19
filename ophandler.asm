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
	