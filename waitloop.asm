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
	jp waitloop_identified

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
;          (SPS+2),BC',DE' = Game Boy BC,DE,HL registers
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
#ifdef FASTLOG
	push ix
	push de
	FASTLOG_EVENT(WAITLOOP_CHECK, 5)
	inc sp
#endif
	
	GET_GB_ADDR_FAST
	lea de,ix
	xor a
	ld (waitloop_length_smc),a
	
	; Check for a read
	ld a,(hl)
	cp $BE		;CP (HL)
	jr z,waitloop_found_read_hl_trampoline
	cp $CB		;Bitwise ops
	inc hl
	jr z,waitloop_found_read_bitwise
	cp $F0		;LD A,($FF00+nn)
	jr z,waitloop_found_read_1
	cp $FA		;LD A,(nnnn)
	jr z,waitloop_found_read_2
	; Consume 3 bytes of recompiled code
	inc de
	inc de
	inc de
	cp $18		;JR
	jr z,waitloop_found_uncond_jump
	cp $C3		;JP
	jr z,waitloop_found_uncond_jump
	cp $0A		;LD A,(BC)
	jr z,waitloop_found_read_bc
	cp $1A		;LD A,(DE)
	jr z,waitloop_found_read_de
	cp $F2		;LD A,($FF00+C)
	jr z,waitloop_found_read_c
	xor $46		;LD r,(HL)
	tst a,$C7
	ret nz
	cp $10		;LD B/C,(HL)
	jr nc,waitloop_found_read_hl
	; Consume 4 bytes of recompiled code
	inc de
	inc de
	inc de
	inc de
waitloop_found_read_hl_trampoline:
	jr waitloop_found_read_hl
	
waitloop_found_read_1:
	; Use 8-bit immediate as HRAM read address
	ld c,(hl)
	; Check for direct vs. port read
	ld.s a,(de)
	rra
	; Set -1 for read in 3rd cycle
	ld a,-1
	; Z flag is set from earlier comparison
	jr nc,waitloop_found_read_3byte
	; Consume 3 bytes of recompiled code
	inc de
	inc de
	inc de
	jr waitloop_found_read_3byte
	
waitloop_found_read_2:
	; Use 16-bit immediate as read address
	ld c,(hl)
	inc hl
	; Set Z flag for HRAM read
	ld.s a,(de)
	bit 0,a
	jr z,_
	; Consume 2 bytes of recompiled code
	inc de
	ld.s a,(de)
	inc de
	; Reset Z flag for direct read
	bit 5,a
	jr nz,_
	; Consume 1 byte of recompiled code
	inc de
	; Set Z flag for port read
	ld a,(hl)
	inc a
	jr z,_
	; Reset Z flag for banked read
	; Consume 2 bytes of recompiled code
	inc de
	inc de
_
	; Set -2 for read in fourth cycle, and preserve Z flag
	ld a,-2
waitloop_found_read_3byte:
	; Consume immediate value
	inc hl
	; Consume 3 bytes of recompiled code
	inc de
	inc de
	inc de
	jr waitloop_resolve_read
	
waitloop_found_read_bc:
	; Consume one extra byte of recompiled code
	inc de
	; Use BC as read address
	pop.s af
	pop.s bc
	push.s bc
	push.s af
	xor a
	jr waitloop_found_read_any
	
waitloop_found_read_de:
	exx
	; Use DE as read address
	push bc
	 jr waitloop_found_read_rr

waitloop_found_read_c:
	; Use C as HRAM read address
	pop.s af
	pop.s bc
	push.s bc
	push.s af
	; Set 0 for read in 2nd cycle, and set Z flag to indicate HRAM
	xor a
	; Consume 3 more bytes of recompiled code
	inc de
	inc de
	inc de
	jr waitloop_resolve_read
	
waitloop_found_read_bitwise:
	ld a,(hl)
	and $C7
	cp $46		;BIT b,(HL)
	ret nz
	; Parse this opcode as a data op
	dec hl
	; Adjust the read cycle forward by 1
	scf
waitloop_found_read_hl:
	exx
	; Use HL as read address
	push de
waitloop_found_read_rr:
	 ; Use stack value as read address
	 exx
	pop bc
	; Set A to 0 for read in 2nd cycle, -1 for read in 3rd cycle
	sbc a,a
waitloop_found_read_any:
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
	jr z,waitloop_found_data_op_3byte
	cp $07		;Special data processing
	jr z,waitloop_found_data_op_special
	and $C6
	xor $80		;Register ALU ops using BC (IX prefix)
	jr z,waitloop_found_data_op_ix
	and $C0		;Register ALU ops
	jr z,waitloop_found_data_op
	ret
	
waitloop_found_data_op_special:
	dec hl
	ld a,(hl)
	inc hl
	cp $2F	;CPL
	jr z,waitloop_found_data_op
	cp $10  ;RLCA/RRCA
	jr c,waitloop_found_data_op_3byte
	cp $20	;RLA/RRA
	ret nc
	inc de
waitloop_found_data_op_3byte:
	inc de
	inc de
	jr waitloop_found_data_op
	
waitloop_found_data_op_bitwise:
	ld a,(hl)
	add a,$40	;BIT b,r
	ret po
	and 7
	cp 6	;BIT b,(HL)
	jr z,waitloop_found_data_op_4byte
	cp 2	;BIT b,B/C
	jr nc,waitloop_found_data_op_1
	inc de
waitloop_found_data_op_4byte:
	inc de
	inc de
waitloop_found_data_op_1:
	; Consume second byte of opcode and recompiled code
	inc hl
waitloop_found_data_op_ix:
	inc de
waitloop_found_data_op:
	; See if we reached the loop end
	push hl
	 ld hl,9
waitloop_found_jump_next:
	 add hl,de
	 ex de,hl
	pop.s hl
	push.s hl
	 sbc.s hl,de	;Carry is reset
	pop hl
	jr z,waitloop_identified
	ret c
	
	ld a,(hl)
	and $E7
	cp $20		;JR cc
	jr z,waitloop_found_jr
	cp $C2		;JP cc
	jr nz,waitloop_find_data_op_again	;Allow multiple data operations
	; Skip the JP opcode
	inc hl
waitloop_found_jr:
	; Skip the JR opcode
	inc hl
	inc hl
	; Advance past the JIT implementation and add its untaken cycles
	push hl
	 ex de,hl
	 ld de,15-9
	 add hl,de
	 ld a,(waitloop_length_smc)
	 add.s a,(hl)
	 ld (waitloop_length_smc),a
	 ld e,(19-15)+9
	 jr waitloop_found_jump_next
	
waitloop_find_data_op_again:
	ld a,e
	sub 8
	ld e,a
	jr nc,waitloop_find_data_op_again_loop
	dec d
	jp waitloop_find_data_op_again_loop
	
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
#ifdef FASTLOG
	push ix
	ld hl,2
	add hl,sp
	ld (hl),c
	FASTLOG_EVENT(WAITLOOP_IDENTIFIED, 3)
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
	; Store the target jump address
	dec hl
	dec hl
	ld.s (hl),ix
	dec hl
	ld.s (hl),$21   ;LD HL,target
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