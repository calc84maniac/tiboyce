#define MAX_LIT (1 << 5)
#define MAX_OFF (1 << 13)
#define MAX_REF ((1 << 8) + (1 << 3))

#define HLOG 13
#define HSIZE (1 << (HLOG))

lzf_hash_table = pixelShadow
lzf_compress_buffer = lzf_hash_table + (HSIZE * 2)

	; Inputs: HL = uncompressed data start
	;         BC = uncompressed data size
	; Outputs: HL = compressed data size
	;          Compressed data written to lzf_compress_buffer
lzf_compress:
	di
	; Fill the LZF hash table with -1 offsets
	exx
	MEMSET_FAST_SPS(lzf_hash_table, (HSIZE * 2), $FF)
	exx
	push iy
	 ld (lzf_compress_save_sp),sp
	; Set SP to the source data start
	ld sp,hl
	; Initialize the output pointer (past the uncompressed size)
	ld de,lzf_compress_buffer+2
	; Initialize the input offset
	ld ix,0
	; Save the uncompressed size to the start of the compressed data
	ld (lzf_compress_buffer),bc
	; Convert BC to a B-major counter
	dec bc
	inc b
	inc c
	ld a,b
	ld b,c
	ld c,a
	; Check for zero-length input
	or b
	jr nz,lzf_compress_start
	jr lzf_compress_zero_length
	
lzf_compress_loop:
	; If literal limit was reached, start a new literal
	cp MAX_LIT - 1
	jr z,lzf_max_literal_reached
lzf_compress_loop_no_literal_check:
	; Read the third input byte into H'
	inc hl
	ld a,(hl)
	exx
	ld h,a
	; Save the first input byte in A' for later
	ld a,c
	ex af,af'
	; Calculate HL = ((CBH >> (24 - HLOG)) - CBH) & (HSIZE - 1)
	; which is HL = ((CBH >> 11) - CBH) & 0x1FFF
	; which is HL = ((CB >> 3) - BH) & 0x1FFF
	ld a,b
	srl c \ rra
	srl c \ rra
	srl c \ rra
	sub h
	ld l,a
	ld a,c
	sbc a,b
	or %11100000
	; Also, set CB = BH, so C and B are the second and third input bytes
	ld c,b
	ld b,h
	ld h,a
	; Index the hash table by HL, and set carry flag
	add hl,hl
	add.s hl,sp
	; Old offset is loaded into DE, new offset is stored
	ld.s de,(hl)
	ld.s (hl),ix
	; Calculate the ref offset, if negative this means uninitialized
	; Carry is set because the ref offset is encoded as 1 less
	lea hl,ix
	sbc hl,de
	jr c,lzf_no_ref_found
	; Ensure the ref offset is within range
	ld a,h
	cp MAX_OFF >> 8
	jr nc,lzf_no_ref_found
	; Compare the first input byte (and place ref offset in DE)
	ex de,hl
	add hl,sp
	ex af,af'
	cp (hl)
	jr nz,lzf_no_ref_match
	; Compare the second and third input bytes
	inc hl
	ld hl,(hl)
	sbc.s hl,bc
	jr nz,lzf_no_ref_match
	
	; If this was the last input byte, force literal
	exx
	djnz lzf_ref_match
	dec c
	jr nz,lzf_ref_match
	dec b
lzf_force_literal_2:
	inc b
	inc b
	inc c
	jr lzf_force_literal
	
	
lzf_max_literal_reached:
	dec hl
	ld (iy-1),a
lzf_start_new_literal:
	inc de
lzf_compress_start:
	; Point IY at the literal size byte plus 1
	ld iy,1
	add iy,de
	; Read next two input bytes into C' and B'
	ld a,(hl)
	exx \ ld c,a \ exx
	inc hl
	ld a,(hl)
	exx \ ld b,a \ exx
	jr lzf_compress_loop_no_literal_check
	
	
lzf_no_ref_found:
	; Restore the first input byte
	ex af,af'
lzf_no_ref_match:
	exx
lzf_force_literal:
	; Save the next literal byte and check if the limit has been reached
	inc ix
	inc de
	ld (de),a
	; Calculate the current literal length (minus 1)
	ld a,e
	sub iyl
	djnz lzf_compress_loop
	dec c
	jr nz,lzf_compress_loop
	
	; Save the literal length
	ld (iy-1),a
lzf_compress_end:
	inc de
lzf_compress_zero_length:
	; Calculate the compressed size
	ld hl,-lzf_compress_buffer
	add hl,de
lzf_compress_save_sp = $+1
	 ld sp,0
	pop iy
	ei
	ret
	
	
lzf_ref_match_long:
	; Store (7 << 5) | (offset >> 8)
	ld a,%11100000
	or d
	exx
	ld (de),a
	inc de
	exx
	; Store (len - 7)
	ld a,l
	sub 7
lzf_ref_match_short:
	exx
	ld (de),a
	inc de
	exx
	; Store (offset & 0xFF)
	ld a,e
	exx
	ld (de),a
	exx
	
	; Add match length to input offset
	ex de,hl
	add ix,de
	; Prepare match length to be subtracted from remaining length
	; Put complemented low byte in A, and high byte in Z
	ld a,e
	cpl
	dec d
	; Restore input pointer from offset (does not modify Z)
	exx
	lea hl,ix
	add hl,sp
	; Subtract remaining length
	jr nz,_
	dec c
	jr z,lzf_compress_end
_
	dec b
	scf
	adc a,b
	ld b,a
	inc b
	jr c,lzf_start_new_literal
	dec c
	jr nz,lzf_start_new_literal
	jr lzf_compress_end
	
lzf_ref_match:
	; If this was the last input byte, force literal
	djnz _
	dec c
	jr z,lzf_force_literal_2
_

	; Increase input offset by 2, first 2 bytes are not counted in the length
	lea ix,ix+2

	; End the current literal
	ld a,e
	sub iyl
	ld (iy-1),a
	inc a
	jr z,_
	inc de
_
	
	; Get the maximum ref length based on remaining input bytes
	ld a,c
	dec a
	cp 1
	ld a,b
	exx
	ld b,6
	ld c,2
	jr z,_
	jr nc,+++_
	dec c
	jr ++_
_
	dec a
	cp b
	jr nc,++_
	inc a
_
	ld b,a
_
	
	; Get the current input pointer (minus 1) in HL
	lea hl,ix-1
	add hl,sp
	; Get the ref pointer in DE using the ref offset
	sbc hl,de
	ex de,hl
	; And get the current input pointer in HL again
	add hl,de
lzf_ref_match_loop:
	inc hl
	ld a,(de)
	cp (hl)
	inc de
	jr nz,lzf_ref_match_end
	djnz lzf_ref_match_loop
	dec c
	jr nz,lzf_ref_match_loop
	; Since the final byte matched, increment the pointers
	inc hl
	inc de
	
lzf_ref_match_end:
	; Recalculate the ref offset in DE
	xor a
	sbc hl,de
	ex de,hl
	; Get the input pointer back and convert to input offset in HL
	add hl,de
	sbc hl,sp
	; Get the length of the match in HL
	lea bc,ix
	sbc hl,bc
	; Check whether the length is at least 7
	cp h
	jr nz,lzf_ref_match_long
	ld a,l
	cp 7
	jr nc,lzf_ref_match_long
	; Store (len << 5) | (offset >> 8)
	rrca
	rrca
	rrca
	or d
	jr lzf_ref_match_short
	
	.echo "LZF compressor size: ", $ - lzf_compress
	
	
	; Inputs: HL = compressed data start
	;         DE = output buffer
	;         BC = compressed data size
	; Output: data written starting at DE
	;         NZ on failure
	;         HL = BC = 0 on success
lzf_decompress:
	; Save the input buffer end pointer
	add hl,bc
	push hl
	 ; Carry is reset
	 sbc hl,bc
	 ; Read the uncompressed data size and keep it on the stack
	 ld c,(hl)
	 inc hl
	 ld b,(hl)
	 inc hl
	 push bc
lzf_decompress_end_shortcut:
	  ld a,b
	  or c
	  jr z,lzf_decompress_end
	  ; The high byte of IX should be constant $FF
	  ld ix,-1
	  ; B is assumed 0 at the start of every loop
	  ld b,0
lzf_decompress_loop:
	  ; Check for literal data
	  ld a,(hl)
	  inc hl
	  cp MAX_LIT
	  jr nc,lzf_decompress_ref
	  ; Carry is set, subtract the count from the output size
	  ld c,a
	  ex (sp),hl
	  sbc hl,bc
	  jr c,lzf_decompress_failed
	  ex (sp),hl
	  ; Copy the specified number of bytes
	  inc bc
	  ldir
	  jr nz,lzf_decompress_loop
lzf_decompress_end:
	 pop de
	 ; Check whether the input pointer is exactly at the end
	pop de
	; Carry is reset
	sbc hl,de
	ret
	
lzf_decompress_failed:
	  ; Zero flag is reset
	 pop de
	pop de
	ret
	
lzf_decompress_ref:
	  ; Check the reference type
	  add a,MAX_OFF >> 8
	  jr c,lzf_decompress_ref_long
	  ; Get the high byte of the complemented offset
	  cpl
	  ld c,a
	  or -(MAX_OFF >> 8)
	  ld ixh,a
	  ; Get the number of bytes to copy
	  xor c
	  rlca
	  rlca
	  rlca
	  inc a
_
	  ; Reset carry
	  or a
_
	  ld c,a
	  ; Read the low byte of the offset
	  ld a,(hl)
	  ; Carry is reset, subtract the byte count from the output size
	  ex (sp),hl
	  sbc hl,bc
	  jr c,lzf_decompress_failed
	  ; Put the complemented offset in HL
	  push ix
	   ex (sp),hl
	   cpl
	   ld l,a
	   ; Add it to the output pointer and copy referenced data
	   add hl,de
	   ldir
	  pop hl
	  ex (sp),hl
	  inc hl
	  jr nz,lzf_decompress_loop
	  jr lzf_decompress_end_shortcut
	
lzf_decompress_ref_long:
	  ; Get the high byte of the complemented offset
	  cpl
	  ld ixh,a
	  ; Read the extended length byte and add to it
	  ld a,(hl)
	  inc hl
	  add a,9
	  jr nc,-_
	  inc b
	  jr --_
	
	.echo "LZF decompressor size: ", $ - lzf_decompress
	