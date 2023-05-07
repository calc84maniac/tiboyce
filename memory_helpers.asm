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
sprite_catchup_full:
	call render_catchup
; Catches up sprite rendering before changing sprite state.
; Must be called only if render_catchup has already been called.
;
; Destroys: AF, BC, DE, HL
sprite_catchup:
	push ix
	 push iy
	  ld a,(myLY)
	  call draw_sprites
	 pop iy
	pop ix
	
	xor a
	ld (z80codebase+sprite_catchup_available),a
	ld a,(myLY)
	ld (myspriteLY),a
	ld hl,(scanlineLUT_ptr)
	ld (scanlineLUT_sprite_ptr),hl
	jp sync_frame_flip

write_oam_catchup:
	exx
	push de
	 call sprite_catchup_full
	pop de
	exx
	ret.l

gdma_transfer_helper:
	push bc
	 push de
	  ld a,r
	  call m,render_catchup_safe
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
