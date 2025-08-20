	.org gbc_render_start
	; This code starts in the middle of sprite rendering
	; SP = scanline LUT row pointer
	; C = number of rows to render, minus 1
	; A = tile row offset
	; IX = OAM pointer plus $60
	pea.s ix+5
	ld.s de,(ix-$60+2)
	inc c
	ld ixl,c
	; Apply flip and bank attributes
LCDC_2_smc_1_gbc = $+2 ; Replace with gbc_tile_attributes_lut_2 for 8x16
	ld hl,gbc_tile_attributes_lut + 64
	ld c,l
	ld l,d
	xor (hl)
	ld iy,vram_pixels_start
	ld iyl,a
	; Get the row direction offset
	ld a,d
	cpl
	and c ;64
	rrca
	rrca
	sub 8
	ld (gbc_draw_sprite_unclipped_dir_smc),a
	; Get pointer to tile index data
	ld a,b
	dec a
	cp 167
	jp nc,draw_sprite_offscreen
	ld b,e
LCDC_2_smc_3_gbc = $+1
	res 0,c		;res 0,b when 8x16 sprites
	mlt bc
	add iy,bc
	
	; Get the sprite palette offset
LCDC_0_smc_1_gbc = $+1 ;Replace with (high_prio_sprite_palette_lut >> 8) & $FF
	ld h,(low_normal_prio_sprite_palette_lut >> 8) & $FF
	ld c,(hl)
	
	; Get the pixel LUT for the current sprite priority
LCDC_0_smc_2_gbc = $+1 ;Replace low byte with 0
	ld de,normal_prio_sprite_pixel_lut - low_normal_prio_sprite_palette_lut + $80
	add hl,de
	ex de,hl
	
	ld ixh,4
	ld b,0
	; Do X clipping
	sub 7
	jr c,gbc_draw_sprite_clipped_left
	ld (gbc_draw_sprite_unclipped_offset_smc),a
	sub 153
	jr c,gbc_draw_sprite_unclipped_start
gbc_draw_sprite_clipped_right:
	; For 5-7 pixels wide, use a shorter second row half
	cpl
	add a,3
	inc a
	ld ixh,a
	jr c,gbc_draw_sprite_unclipped_start
	; For 1-4 pixels wide, use the first half only
	add a,4
	ld ixh,a
gbc_render_sprite_row_first_half_smc = $+1
	ld hl,gbc_render_sprite_row_first_half
	ld (gbc_render_sprite_row_smc_1),hl
	jr gbc_draw_sprite_unclipped_start
	
gbc_draw_sprite_unclipped_loop_reset_b:
	ld b,0
gbc_draw_sprite_unclipped_loop:
gbc_draw_sprite_unclipped_dir_smc = $+2
	lea iy,iy+8
	dec ixl
gbc_draw_sprite_clipped_left_smc = $+1
	jr z,gbc_draw_sprite_unclipped_finish
gbc_draw_sprite_unclipped_start:
	pop hl
	dec (hl)
	pop hl
	ld a,c
gbc_draw_sprite_unclipped_offset_smc = $+1
	ld c,0
	add hl,bc
	ld c,a
	ld b,4
	ld e,(iy)
gbc_render_sprite_row_smc_1 = $+1
	jp p,gbc_render_sprite_row
	jr gbc_draw_sprite_unclipped_loop_reset_b
	
gbc_draw_sprite_clipped_left_finish:
	.db $21 ;LD HL,
	 .db gbc_draw_sprite_unclipped_finish - (gbc_draw_sprite_clipped_left_smc+1)
	 pop hl
	 dec (hl)
	ld (gbc_draw_sprite_clipped_left_smc),hl
gbc_draw_sprite_unclipped_finish:
gbc_render_sprite_row_smc_2 = $+1
	ld hl,gbc_render_sprite_row
	ld (gbc_render_sprite_row_smc_1),hl
gbc_draw_sprite_finish:
	pop.s ix
	jp draw_next_sprite_2
	
gbc_draw_sprite_clipped_left:
	; For 5-7 pixels wide, use a shorter first row half
	add a,3
	jr c,gbc_draw_sprite_clipped_left_full
	; For 1-4 pixels wide, use the second half only
	add a,5
	ld ixh,a
	.db $21 ;LD HL,
	 .db gbc_draw_sprite_clipped_left_finish - (gbc_draw_sprite_clipped_left_smc+1)
	 .db $18 ;JR
	 .db gbc_draw_sprite_clipped_left_half_loop - (gbc_draw_sprite_clipped_left_smc+3)
	ld (gbc_draw_sprite_clipped_left_smc),hl
gbc_draw_sprite_clipped_left_half_loop:
	ld a,(iy)
	add a,8
	sub ixh
	pop hl
	dec (hl)
	pop hl
gbc_render_sprite_row_second_half_smc = $+1
	jp p,gbc_render_sprite_row_second_half
	jr gbc_draw_sprite_unclipped_loop
	
gbc_draw_sprite_clipped_left_full:
	inc a
	ld (gbc_draw_sprite_clipped_left_full_smc),a
	.db $21 ;LD HL,
	 .db gbc_draw_sprite_clipped_left_finish - (gbc_draw_sprite_clipped_left_smc+1)
	 .db $18 ;JR
	 .db gbc_draw_sprite_clipped_left_full_loop - (gbc_draw_sprite_clipped_left_smc+3)
	ld (gbc_draw_sprite_clipped_left_smc),hl
gbc_draw_sprite_clipped_left_full_loop:
gbc_draw_sprite_clipped_left_full_smc = $+1
	ld b,1
	ld a,(iy)
	add a,4
	sub b
	ld e,a
	pop hl
	dec (hl)
	pop hl
gbc_render_sprite_row_smc_3 = $+1
	jp p,gbc_render_sprite_row
	jr gbc_draw_sprite_unclipped_loop_reset_b
	
_
	 push de
	  call write_vram_check_sprite_catchup
	 pop de
	 jr _
gbc_write_vram_and_expand_catchup:
	push bc
	 bit 4,b
	 jr z,-_
	 call render_catchup
_
	pop bc
	ld a,e
	; Input: BC=Game Boy VRAM address, A=E=value to write
	; Output: Value written to VRAM, cache updated if needed
	; Destroys: AF, BC, DE, HL
gbc_write_vram_and_expand:
vram_bank_base_for_write = $+1
	ld hl,0
	add hl,bc
	xor (hl)
	jr z,gbc_write_vram_no_change
	ld (hl),e
	ld d,a
	ld a,b
	add a,256-$98
	jr nc,gbc_write_vram_pixels
gbc_write_tilemap_for_dma:
	add hl,hl
	add hl,hl
	add hl,hl
gbc_write_tilemap_bank_smc = $+1
	ld bc,0 ;vram_tiles_start-(((vram_start+$1800)*8) & $FFFFFF)
	add hl,bc
	srl l
	jr c,gbc_write_vram_tile_attrs
	; Calculate the tile offset
	ld d,64
	mlt de
	; Combine the low byte of the tile offset with the attributes
	ld a,(hl)
	and $3F
	or e
	ld (hl),a
	inc hl
	ld (hl),d
	; Set the tile offset for the alternate tile set
	bit 5,d
	jr nz,_
	set 6,d
_
	set 7,l
	ld (hl),d
	dec hl
	ld (hl),a
gbc_write_vram_no_change:
gbc_write_tilemap_for_dma_ret_smc_1 = $
	jp.sis write_vram_and_expand_finish
	
gbc_write_vram_tile_attrs:
	; Get a pointer to the mask for the changed attributes
	ld bc,gbc_tile_attributes_lut + $87
	; Also get the palette and priority bits
	ld a,e
	and c
	ld c,d
	; Get the palette offset by multiplying by (256+3)*2
	rlca
	rlca
	ld e,a
	add a,(gbc_overlapped_pixel_data - vram_pixels_start) >> 8
	ld d,a
	ld a,e
	add a,a
	add a,e
	ld e,a
	; Apply the changed attributes to the low tile offset
	ld a,(bc)
	xor (hl)
	ld (hl),a
	inc hl
	inc hl
	; Set the palette offset
	ld (hl),e
	inc hl
	ld (hl),d
	; Do the same for the alternate tile set
	set 7,l
	ld (hl),d
	dec hl
	ld (hl),e
	dec hl
	dec hl
	ld (hl),a
gbc_write_tilemap_for_dma_ret_smc_2 = $
	jp.sis write_vram_and_expand_finish
	
gbc_write_vram_catchup:
	; Disable the normal return to z80 mode by using a load instruction
	ld a,$21 ;LD HL,
	ld (gbc_write_vram_catchup_ret_smc),a
	push de
	; Carry is set, which forces the written value to have bit 0 reset
gbc_write_vram_pixels:
	; Check if writing to the same slice as the last write
	set 0,l
gbc_write_vram_last_slice = $+1
	ld de,0
	sbc hl,de
	; If equivalent, clear the last write slice
	jr z,_
	; Otherwise, save the currently written slice
	add hl,de
_
	ld (gbc_write_vram_last_slice),hl
	; If the last slice was cleared, defer pixel generation
	bit 0,e
	jr z,gbc_write_vram_defer_pixels
	; Write pixels from the last slice
	ex de,hl
gbc_write_pixels_for_dma:
	; Get the HIGH and low bitplanes with nibbles X and Y
	ld b,(hl)  ;B=[X][Y]
	dec hl
	ld a,(hl)  ;A=[x][y]
	add hl,hl
	add hl,hl
gbc_write_pixels_bank_smc = $+1
	ld de,0 ;vram_pixels_start-((vram_start*4) & $FFFFFF)
	add hl,de
	ex de,hl
	rlca \ rlca \ rlca \ rlca  ;A=[y][x]
	xor b   ;A=[X^y][Y^x]
	ld c,a  ;C=[X^y][Y^x]
	ld hl,overlapped_pixel_index_lut + $0F
	and l   ;A=[0][Y^x]
	xor b   ;A=[X][x]
	ld b,a  ;B=[X][x]
	xor c   ;A=[y][Y]
	rrca \ rrca \ rrca \ rrca  ;A=[Y][y]
	; Copy the index for nibble X
	ld l,b
	ld c,(hl)
	ld l,a
	ld a,c
	ld (de),a
	inc de
	; Calculate the relative offset for nibble Y and write it
	cpl
	add a,(hl)
	add a,-3
	ld (de),a
	inc de
	; Copy the index for reverse nibble Y
	inc h
	ld a,(hl)
	ld (de),a
	inc de
	; Calculate the relative offset for reverse nibble X and write it
	cpl
	ld l,b
	add a,(hl)
	add a,-3
	ld (de),a
gbc_write_vram_defer_pixels:
gbc_write_vram_catchup_ret_smc = $
gbc_write_pixels_for_dma_ret_smc = $
	jp.sis write_vram_and_expand_finish ;Replaced with LD HL, when catching up
	pop de
	ld a,$40 ;.SIS
	ld (gbc_write_vram_catchup_ret_smc),a
	ret
	
	; Input: B=Start X+8, A=Length-1, HL=pixel base pointer, DE=pixel output pointer, IY=scanline pointer, SPS=tilemap pointer
gbc_scanline_do_render:
	ld c,b
	ld b,l
	ld ixl,b
	ld b,a
	and 7
	ld (gbc_scanline_right_clip_smc),a
	xor b
	ld l,0
	ld sp,hl
	jr z,gbc_scanline_do_subtile
	ld b,l
	add.s hl,sp
	add hl,hl
	add a,l
	jr nc,gbc_scanline_no_wrap
	rrca
	rrca
	rrca
	ld (gbc_scanline_post_wrap_smc),a
	xor a
gbc_scanline_no_wrap:
	sub l
	rrca
	rrca
	rrca
	ld ixh,a
	ld a,c
	cp 8
gbc_render_tile_loop_smc_1 = $+1
	jp nc,gbc_render_tile_loop

gbc_scanline_left_clip:
	ld a,ixl
	pop.s hl
	xor l
	ld l,a
	add hl,sp
	ld a,8
	sub c
	bit 2,a
	jr z,gbc_scanline_left_clip_full
gbc_scanline_left_clip_half:
	add a,(hl)
	inc hl
	add a,(hl)
	pop.s hl
	add hl,sp
	inc h
	add a,l
	jr gbc_scanline_left_clip_finish
	
gbc_scanline_do_subtile:
	ld h,b
	ld b,l
	ld a,c
	sub 8
	jr nc,gbc_scanline_right_clip
	add a,h
	jr nc,gbc_scanline_finish
	cpl
	dec a
	ld (gbc_scanline_post_wrap_smc),a
	ld ixh,1
	jr gbc_scanline_left_clip
	
gbc_scanline_left_clip_full:
	res 2,c
	add a,(hl)
	inc hl
	ld b,(hl)
	pop.s hl
	add hl,sp
	add a,l
	ld l,a
	add a,c
	jr c,_
	inc h
_
	add a,b
	ld b,0
	ldir
	ld c,4
gbc_scanline_left_clip_finish:
	ld l,a
	ldir
	dec ixh
gbc_render_tile_loop_smc_3 = $+1
	jp nz,gbc_render_tile_loop
	
gbc_render_scanline_finish:
gbc_scanline_post_wrap_smc = $+1
	ld c,$FF
	inc c
	jr z,gbc_scanline_right_clip
	ld hl,-128
	ld a,h
	ld (gbc_scanline_post_wrap_smc),a
	add a,c
	ld ixh,a
	rla
	jr c,gbc_scanline_subtile_finish
	add.s hl,sp
	ld.s sp,hl
gbc_render_tile_loop_smc_2 = $+1
	jp nz,gbc_render_tile_loop
gbc_scanline_right_clip:
	ld a,ixl
	pop.s hl
	xor l
	ld l,a
	add hl,sp
	ld c,(hl)
	inc hl
	ld a,(hl)
	ld ixh,a
	pop.s hl
	add hl,sp
	add hl,bc
	ld c,4
gbc_scanline_right_clip_smc = $+1
	ld a,0
	cp c
	jr c,_
	sub c
	ldir
	ld c,ixh
	add hl,bc
_
	inc a
	ld c,a
	ldir
gbc_scanline_finish:
gbc_render_save_spl = $+1
	ld sp,0
	ret
	
gbc_scanline_subtile_finish:
	ld l,c
	add hl,de
	ex de,hl
	jr gbc_scanline_finish
	
gbc_cursorcodesize = $-mpLcdCursorImg
	.org cursorcode+cursorcodesize+($-gbc_render_start)
	
	.echo "GBC cursor memory code size: ", gbc_cursorcodesize
