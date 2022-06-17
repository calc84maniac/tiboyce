	.org gbc_render_start
	; This code starts in the middle of sprite rendering
	; SP = scanline LUT row pointer
	; C = number of rows to render, minus 1
	; A = tile row offset
	; IX = OAM pointer plus $60
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
	and c ;64
	rrca
	rrca
	sub 8
	ld ixh,a
	; Get pointer to tile index data
	ld a,b
	ld b,e
LCDC_2_smc_3_gbc = $+1
	res 0,c		;res 0,b when 8x16 sprites
	mlt bc
	add iy,bc
	
	; Get the sprite palette offset
LCDC_0_smc_1_gbc = $+1 ;Replace with (high_priority_sprite_palette_lut >> 8) & $FF
	ld h,(low_normal_prio_sprite_palette_lut >> 8) & $FF
	ld c,(hl)
	
	; Get the pixel LUT for the current sprite priority
LCDC_0_smc_2_gbc = $+1 ;Replace low byte with 0
	ld de,low_prio_sprite_pixel_lut - low_normal_prio_sprite_palette_lut + $80
	add hl,de
	ex de,hl
	
	; Do X clipping
	ld b,0
	sub 7
	jr c,gbc_draw_sprite_clipped_left
	ld (gbc_draw_sprite_unclipped_offset_smc),a
	add a,256-152
	jr nc,gbc_draw_sprite_unclipped_start
gbc_draw_sprite_clipped_right:
	FIXME
	
gbc_draw_sprite_unclipped_loop:
	ld a,iyl
	add a,ixh
	ld iyl,a
	dec ixl
gbc_draw_sprite_unclipped_start:
	pop hl
	ld a,c
gbc_draw_sprite_unclipped_offset_smc = $+1
	ld c,0
	add hl,bc
	ld c,a
	ld b,4
	jp nz,gbc_render_sprite_row
	pop.s ix
	jp draw_next_sprite_2
	
gbc_draw_sprite_clipped_left:
	FIXME
	
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
	ld hl,vram_base
	add hl,bc
	xor (hl)
	jr z,gbc_write_vram_no_change
	ld (hl),e
	ld d,a
	ld a,b
	cp $98
	jr c,gbc_write_vram_pixels
	add hl,hl
	add hl,hl
	add hl,hl
gbc_write_tilemap_bank_smc = $+1
	ld bc,vram_tiles_start-(((vram_start+$1800)*8) & $FFFFFF)
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
	set 7,d
	ld (hl),d
	dec hl
	ld (hl),e
	dec hl
	dec hl
	ld (hl),a
	jp.sis write_vram_and_expand_finish
	
gbc_write_vram_catchup:
	; Disable the normal return to z80 mode by using a false jump condition
	ld a,$EA ;JP PE,
	ld (gbc_write_vram_catchup_smc),a
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
	; Get the HIGH and low bitplanes with nibbles X and Y
	ld b,(hl)  ;B=[X][Y]
	dec hl
	ld a,(hl)  ;A=[x][y]
	add hl,hl
	add hl,hl
gbc_write_pixels_bank_smc = $+1
	ld de,vram_pixels_start-((vram_start*4) & $FFFFFF)
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
gbc_write_vram_catchup_smc = $+1
	jp.sis write_vram_and_expand_finish ;Replaced with JP PE when catching up
	ld a,$C3 ;JP
	ld (gbc_write_vram_catchup_smc),a
	ret
	
writeVBK_helper:
	inc a
	jr nz,_
	ld hl,vram_tiles_start-(((vram_start+$1800)*8) & $FFFFFF)
	ld (gbc_write_tilemap_bank_smc),hl
	ld hl,vram_pixels_start-((vram_start*4) & $FFFFFF)
	ld (gbc_write_pixels_bank_smc),hl
	ld a,(vram_gbc_base >> 8) & $FF
	ld (vram_bank_base_for_write+1),a
	ld hl,vram_bank_base+1-z80codebase
	jp.sis writeVBK_finish
_
	ld hl,vram_tiles_start-(((vram_start+$3800)*8) & $FFFFFF)+1
	ld (gbc_write_tilemap_bank_smc),hl
	ld hl,vram_pixels_start-(((vram_start+$2000)*4) & $FFFFFF)+4
	ld (gbc_write_pixels_bank_smc),hl
	ld a,(vram_gbc_base >> 8) & $FF
	ld (vram_bank_base_for_write+1),a
	ld hl,vram_bank_base+1-z80codebase
	jp.sis writeVBK_finish
	
gbc_cursorcodesize = $-mpLcdCursorImg
	.org cursorcode+cursorcodesize+($-gbc_render_start)
	
	.echo "GBC cursor memory code size: ", gbc_cursorcodesize
