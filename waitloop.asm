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
;          HL = branch target GB literal 24-bit address
;          (SPS) = branch recompiled code address (plus 9)
;          (SPL+4) = number of cycles to block end from target
;          BC',DE',HL' = Game Boy BC,DE,HL registers
; Outputs: IX = filtered branch target
identify_waitloop:
#ifdef 0
	push hl
	 push ix
	 pop af
	 push af
	  APRINTF(WaitLoopSearchMessage)
	 pop ix
	pop hl
#endif
	
	ld (waitloop_jr_smc),hl
	lea de,ix
	
	; Check for a read
	ld a,(hl)
	cp $BE		;CP (HL)
	jr z,waitloop_found_read_hl_swap
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
	exx
	push bc
	 cp $0A		;LD A,(BC)
	 jr z,waitloop_found_read_rr
	pop bc
	push de
	 cp $1A		;LD A,(DE)
	 jr z,waitloop_found_read_rr
	pop de
	and $C7
	cp $46		;LD r,(HL)
	jr z,waitloop_found_read_hl
	exx
	ret
	
waitloop_found_read_1:
	; Use 8-bit immediate as read address
	ld b,$FF
	ld c,(hl)
	; Consume immediate value
	inc hl
	jr waitloop_find_data_op
	
waitloop_found_read_2:
	; Use 16-bit immediate as read address
	ld c,(hl)
	inc hl
	ld b,(hl)
	; Consume 2 more bytes of recompiled code
	inc de
waitloop_try_next_target:
	inc de
	; Consume immediate value
	inc hl
	jr waitloop_find_data_op
	
waitloop_found_read_bitwise:
	ld a,(hl)
	and $C7
	cp $46		;BIT b,(HL)
	ret nz
	; Parse this opcode as a data op
	dec hl
	
waitloop_found_read_hl_swap:
	exx	
waitloop_found_read_hl:
	; Use HL as read address
	push hl
waitloop_found_read_rr:
	 ; Use stack value as read address
	 exx
	pop bc
waitloop_find_data_op:
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
	jr z,waitloop_identified
	
	ld a,(hl)
	and $E7
	cp $20		;JR cc
	jr z,waitloop_found_jr
	cp $C2		;JP cc
	jr nz,waitloop_find_data_op_again	;Allow multiple data operations
	
	; Validate the JP target address
	inc hl
	push de
	 push hl
	  ld hl,(waitloop_jr_smc)
	  ld de,(base_address)
	  sbc hl,de
	  ex de,hl
	 pop hl
	 ld a,(hl)
	 cp e
	 jr nz,_
	 inc hl
	 ld a,(hl)
	 cp d
_
	pop de
	jr z,waitloop_try_next_target
	ret
	
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
	
waitloop_find_data_op_again:
	ld a,e
	sub 8
	ld e,a
	jr nc,waitloop_find_data_op_again_loop
	dec d
	jr waitloop_find_data_op_again_loop
	
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
	
	; Don't do anything with STAT waits
	ld a,c
	add a,$FFFF - STAT
	and b
	inc a
	ret z
	
	ld hl,(z80codebase+memroutine_next)
	ld de,-7
	add hl,de	;Sets C flag
	
	; Bail if not enough room for trampoline
	ex de,hl
	ld hl,(recompile_struct_end)
	ld hl,(hl)
	sbc hl,de	;Carry is set
	ret nc
	ex de,hl
	ld (z80codebase+memroutine_next),hl
	inc hl
	
	; Choose handler based on variable accessed
	inc b
	jr nz,waitloop_variable
	ld a,c
	cp STAT & $FF
	jr z,waitloop_stat
	cp LY & $FF
	jr nz,waitloop_variable
	ld bc,handle_waitloop_ly
	jr waitloop_finish
waitloop_stat:
	ld bc,handle_waitloop_stat
	jr waitloop_finish
waitloop_variable:
	ld bc,handle_waitloop_variable
waitloop_finish:
	ld (hl),ix
	inc hl
	inc hl
	pop de	; Pop the return address
	pop de  ; Pop the target cycle count into D
	; Store the negative target cycle count
	xor a
	sub d
	ld (hl),a
	inc hl
	ld a,d
	; Get the end of the recompiled code to overwrite
	pop.s de
	ex de,hl
	; Compute the length of the loop in cycles
	add.s a,(hl)
	dec hl
	dec hl
	ld.s (hl),bc
	dec hl
	ld.s (hl),$C3	;JP handler
	ex de,hl
	ld (hl),a
	inc hl
	ld bc,(waitloop_jr_smc)
	ld (hl),bc
	dec hl
	dec hl
	dec hl
	dec hl
	ex de,hl
	dec hl
	dec hl
	ld.s (hl),de
	dec hl
	ld.s (hl),$21
	dec hl
	ld.s (hl),$DD	;LD IX,ptr
	ei
	jp.sis decode_jump_waitloop_return