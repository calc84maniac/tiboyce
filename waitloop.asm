; Filters output from decode_branch based on whether a waitloop is detected.
;
; In general, a waitloop consists of a loop for which the output state of the
; loop does not feed into the input state, which means that it will loop
; forever until an external source (usually an interrupt) changes the input.
; In most cases, this means the loop is just a memory read and a comparison.
; Occasionally a waitloop is nested and checks multiple sources - this
; is also allowed as long as all intermediate branches jump to the start.
; 
; If a waitloop is detected, a handler is inserted in the mem_routine buffer
; and the branch target address is set to that handler.
; The handler used is determined based on the memory address being accessed
; in the loop - MMIO such as the LY register uses a different handler.
;
; Inputs:  IX = branch target recompiled code
;          DE = branch target GB address
;          (SPS) = branch recompiled code address (plus 9, or 7 if uncond)
;          (SPL+4) = number of cycles to sub-block end from target
;          (SPL+7) = number of cycles taken by jump
;          BC',DE',HL' = Game Boy BC,DE,HL registers
; Outputs: IX = filtered branch target
identify_waitloop:
#ifdef 0
	push de
	 push ix
	 pop af
	 push af
	  APRINTF(WaitLoopSearchMessage)
	 pop ix
	pop de
#endif
	
	ld (waitloop_jp_smc),de
	GET_BASE_ADDR_FAST
	add hl,de
	ld (waitloop_jr_smc),hl
	lea de,ix
	xor a
	ld (waitloop_length_smc),a
	
	; Check for a read
	ld a,(hl)
	cp $BE		;CP (HL)
	jr z,waitloop_found_read_hl
	cp $CB		;Bitwise ops
	inc hl
	jr z,waitloop_found_read_bitwise
	; Consume 3 bytes of recompiled code
	inc de
	inc de
	inc de
	cp $F0		;LD A,($FF00+nn)
	jr z,waitloop_found_read_1
	cp $FA		;LD A,(nnnn)
	jr z,waitloop_found_read_2
	cp $18		;JR
	jr z,waitloop_found_uncond_jump
	cp $C3
	jr z,waitloop_found_uncond_jump
	cp $0A		;LD A,(BC)
	jr z,waitloop_found_read_bc
	cp $1A		;LD A,(DE)
	jr z,waitloop_found_read_de
	and $C7
	cp $46		;LD r,(HL)
	ret nz
	jr waitloop_found_read_hl
	
waitloop_found_uncond_jump:
	; See if we reached the loop end
	inc de
	inc de
	inc de
	inc de
	pop.s hl
	push.s hl
	sbc.s hl,de	;Carry is reset
	ret nz
	ld b,h
	ld c,l
	jr waitloop_identified_trampoline
	
waitloop_found_read_1:
	; Use 8-bit immediate as HRAM read address
	ld c,(hl)
	; Set -1 for read in 3rd cycle
	ld a,-1
	; Consume immediate value
	inc hl
	; Z flag is set from earlier comparison
	jr waitloop_resolve_read
	
waitloop_found_read_2:
	; Use 16-bit immediate as read address
	ld c,(hl)
	inc hl
	; Set Z flag if read is HRAM
	ld a,$FF
	cp (hl)
	inc hl
	; Set -2 for read in fourth cycle, and preserve Z flag
	rla
	; Consume 2 more bytes of recompiled code
	inc de
	inc de
	jr waitloop_resolve_read
	
waitloop_found_read_bc:
	exx
	; Use BC as read address
	push bc
	 jr waitloop_found_read_rr
	
waitloop_found_read_de:
	exx
	; Use DE as read address
	push de
	 jr waitloop_found_read_rr
	
waitloop_found_read_bitwise:
	ld a,(hl)
	and $C7
	cp $46		;BIT b,(HL)
	ret nz
	; Parse this opcode as a data op
	dec hl
	inc de
	; Adjust the read cycle forward by 1
	scf
waitloop_found_read_hl:
	exx
	; Use HL as read address
	push hl
waitloop_found_read_rr:
	 ; Use stack value as read address
	 exx
	pop bc
	; Set A to 0 for read in 2nd cycle, -1 for read in 3rd cycle
	sbc a,a
	; Set Z flag if read is HRAM
	inc b
waitloop_resolve_read:
	; Save the read cycle offset
	ld b,a
	; Put the waitloop variable type in C, or return if invalid
	; 0 = RAM-like variable, $41 = STAT, $44 = LY
	call z,waitloop_resolve_read_hram
	ret z
	xor b
	ld c,a
	; Consume first byte of recompiled code
	inc de
waitloop_find_data_op_again_loop:
	ld a,(hl)
	; Consume first byte of opcode
	inc hl
	cp $CB		;Bitwise ops
	jr z,waitloop_found_data_op_bitwise
	and $C7
	cp $C6		;Immediate ALU ops
	jr z,waitloop_found_data_op_1
	cp $86		;(HL) ALU ops
	jr z,waitloop_found_data_op_hl
	and $C0
	cp $80		;Register ALU ops
	jr z,waitloop_found_data_op
	ret
	
waitloop_found_data_op_hl:
	inc de
	inc de
	jr waitloop_found_data_op
	
waitloop_found_data_op_bitwise:
	ld a,(hl)
	add a,$40	;BIT b,r
	ret po
waitloop_found_data_op_1:
	; Consume second byte of opcode and recompiled code
	inc hl
	inc de
waitloop_found_data_op:
	; See if we reached the loop end
	push hl
	 ld hl,9
	 add hl,de
	 ex de,hl
	pop.s hl
	push.s hl
	 sbc.s hl,de	;Carry is reset
	pop hl
waitloop_identified_trampoline:
	jr z,waitloop_identified
	ret c
	
	ld a,(hl)
	and $E7
	cp $20		;JR cc
	jr z,waitloop_found_jr
	cp $C2		;JP cc
	jr nz,waitloop_find_data_op_again	;Allow multiple data operations
	
	; Validate the JP target address
	inc hl
	push de
waitloop_jp_smc = $+1
	 ld de,0
	 ld a,(hl)
	 cp e
	 ld a,d
	pop de
	ret nz
	cp (hl)
	ret nz
waitloop_try_next_target:
	push hl
	 ex de,hl
	 ld de,15-9
	 add hl,de
	 ld a,(waitloop_length_smc)
	 add.s a,(hl)
	 ld (waitloop_length_smc),a
	 ld e,(19+1)-15
	 add hl,de
	 ex de,hl
	pop hl
	inc hl
	jr waitloop_find_data_op_again_loop
	
waitloop_find_data_op_again:
	ld a,e
	sub 7
	ld e,a
	jr nc,waitloop_find_data_op_again_loop
	dec d
	jr waitloop_find_data_op_again_loop
	
waitloop_found_jr:
	; Validate the JR target address
	inc hl
	ld a,(hl)
	push de
	 ex de,hl
waitloop_jr_smc = $+1
	 ld hl,0
	 scf
	 sbc hl,de
	 ex de,hl
	 cp e
	 jr nz,_
	 rla
	 sbc a,a
	 cp d
_
	pop de
	jr z,waitloop_try_next_target
	ret
	
waitloop_identified:
#ifdef DEBUG
	push bc
	pop af
	push af
	 push ix
	 pop af
	 push af
	  APRINTF(WaitLoopIdentifiedMessage)
	 pop ix
	pop bc
#endif
	
	; Get the end of the recompiled code to overwrite
	pop.s hl
	pop de	; Pop the return address
	pop af  ; Pop the target cycle count into A
	pop de  ; Pop the negative jump cycle count into D
	; Store the target cycle count
	sub d
	inc hl
	ld.s (hl),a
	dec hl
	; Store the length of the loop in cycles
waitloop_length_smc = $+1
	add a,0
	ld.s (hl),a
	dec hl
	; Store the cycle offset of the variable read from the end of the loop
	add a,d
	add a,b
	cpl
	add a,2
	ld.s (hl),a
	; Store the target jump
	dec hl
	dec hl
	ld.s (hl),ix
	dec hl
	ld.s (hl),$C3   ;JP target
	dec hl
	dec hl
	; Choose handler based on variable accessed
	ld a,c
	or a
	ld bc,handle_waitloop_variable
	jr z,_
	ld bc,handle_waitloop_ly
	rra
	jr nc,_
	ld bc,handle_waitloop_stat
_
	ld.s (hl),bc
	dec hl
	ld.s (hl),$CD	;CALL handler
	jp.sis decode_jump_waitloop_return
	
waitloop_resolve_read_hram:
	; Fast return for HRAM, treat as normal variable
	bit 7,c
	ret nz
	; Check for special registers
	ld a,c
	; Allow LY and STAT
	cp LY & $FF
	jr z,_
	cp STAT & $FF
	jr z,_
	; Disallow DIV and TIMA
	cp DIV & $FF
	ret z
	cp TIMA & $FF
	; Treat everything else as a normal variable
	ld a,b
	ret
_
	; Resets Z flag
	xor b
	ret