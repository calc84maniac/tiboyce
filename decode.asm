decode_branch_slow:
	ex af,af'
	xor a
	ld (mpTimerCtrl),a
	dec hl
	dec hl
	dec hl
	ex de,hl
	call.il lookup_gb_code_address
	inc hl
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ex af,af'
	ret.l
	
decode_branch:
	ex af,af'
	xor a
	ld (mpTimerCtrl),a
	call get_base_address
	ld (base_address),hl
	add hl,de
	ld a,(hl)
	cp $C3
	jr z,decode_jp
	cp $CD
	jr z,decode_call
	and 7
	jr z,decode_jr
	rra
	jr c,decode_rst
	rra
	jr c,decode_jp_cond
	
decode_call:
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	call lookup_code
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ld a,RST_CALL
	ret.l
	
decode_rst:
	ld a,(hl)
	sub $C7
	ld e,a
	ld d,0
	call lookup_code
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ld a,RST_CALL
	ret.l
	
decode_jp_cond:
	ld a,(hl)
	ld (_),a
	ex af,af'
_
	jp _
	ex af,af'
	add a,$C4-$C2
	ld ix,do_branch_slow
	ld e,a
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ld a,e
	ret.l
_
	ex af,af'
decode_jp:
	push af
	 inc hl
	 ld e,(hl)
	 inc hl
	 ld d,(hl)
	 call get_base_address
	 ld a,(base_address+2)
	 ld (base_address),hl
	 jr decode_loop
	
decode_jr_cond:
	ld (_),a
	ex af,af'
_
	jr _
	ex af,af'
	add a,$C4-$20
	ld ix,do_branch_slow
	ld e,a
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ld a,e
	ret.l
decode_jr:
	ld a,(hl)
	cp $18
	jr nz,decode_jr_cond
	ld a,$21
	ex af,af'
_
	ex af,af'
	add a,$C2-$20
	push af
	 inc hl
	 ld a,(hl)
	 inc hl
	 ex de,hl
	 rla
	 sbc hl,hl
	 rra
	 ld l,a
	 ld a,(base_address+2)
decode_loop:
	 add hl,de
	 push hl
	  call lookup_code_link_internal
	 pop hl
	 
	 call identify_waitloop
	 
	 ld a,TMR_ENABLE
	 ld (mpTimerCtrl),a
	pop af
	ret.l
	
decode_mem:
	xor a
	ld (mpTimerCtrl),a
	dec ix
	
	; Get index 0-31
	ld.s a,(ix+1)
	sub $70
	cp 8
	jr c,++_
_
	add a,$70+$40
	rrca
	rrca
	rrca
	or $E0
_
	add a,24
	
	; Check for BC access
	cp 2
	jr nc,_
	ld d,b
	ld e,c
_
	; Check for HL access 
	cp 4
	jr c,_
	ex de,hl
_
	
	; Address is now in DE
	ld hl,memroutineLUT
	ld l,a
	
	; Get memory region, 0-7
	ld a,d
	cp $FE
	jr c,++_
	inc e	; Exclude $FFFF from region 0
	jr z,_
	dec e
_
	rrca
	and e
	rrca
	cpl
	and $40
	jr ++_
_
	and $E0
	jp m,_
	set 5,a
_
	or l
	ld l,a
	
	; Get routine address
	ld e,(hl)
	inc h
	ld d,(hl)
	ld a,d
	or e
	jr nz,memroutine_gen_ret
	
	; Routine doesn't exist, let's generate it!
	push bc \ push hl
	 ex de,hl
	 ld.s d,(ix+1)
	 
	 ; Emit RET and possible post-increment/decrement
	 ld hl,(z80codebase+memroutine_next)
	 ld (hl),$C9	;RET
	 dec hl
	 ld (hl),$FB	;EI
	 ld a,e
	 and $1C
	 cp 4
	 jr nz,++_
	 ld a,e
	 rra
	 ld d,$77	;LD (HL),A
	 jr nc,_
	 ld d,$7E	;LD A,(HL)
_
	 dec hl
	 rra
	 ld (hl),$23 ;INC HL
	 jr nc,_
	 ld (hl),$2B ;DEC HL
_
	 dec hl
	  
	 ; Get register pair index (BC=-4, DE=-2, HL=0)
	 ld a,e
	 and $1E
	 sub 4
	 jr c,_
	 xor a
_
	 ld c,a
	 
	 ld a,e
	 rlca
	 rlca
	 rlca
	 and 7
	 jr nz,memroutine_gen_not_high
	 
	 ld (hl),d
	 dec hl
	 ld (hl),$08 ;EX AF,AF'
	 dec hl
	 ld (hl),-10
	 dec hl
	 ld (hl),$20 ;JR NZ,$-8
	 dec hl
	 ld (hl),$3C ;INC A
	 dec hl
	 ld a,c
	 add a,$A4 ;AND B/D/H
	 ld (hl),a
	 dec hl
	 ld (hl),$9F ;SBC A,A
	 dec hl
	 ld (hl),$17 ;RLA
	 dec hl
	 add a,$7D-$A4 ;LD A,C/E/L
	 ld (hl),a
	 
memroutine_gen_end_swap:
	 dec hl
	 ld (hl),$08 ;EX AF,AF' 
memroutine_gen_end:
	 push hl
	  dec hl
	  ld.s a,(ix+1)
	  ld (hl),a
	  dec hl
	  ld (hl),RST_MEM
	  dec hl
	  ld (z80codebase+memroutine_next),hl
	  ex de,hl
	  ld hl,(recompile_struct_end)
	  ld hl,(hl)
	  scf
	  sbc hl,de
	 pop de
	 jr nc,memroutine_gen_flush
_
	pop hl \ pop bc
	
	ld (hl),d
	dec h
	ld (hl),e
memroutine_gen_ret:
	ld a,TMR_ENABLE
	ld (mpTimerCtrl),a
	ret.l
	
memroutine_gen_flush:
	ld hl,recompile_cache_end
	ld (recompile_cache),hl
	ld de,flush_mem_handler
	jr -_
	
memroutine_gen_not_high:
	 ld b,a
	 
	 ; Get HL-based access instruction for BC/DE accesses
	 ld a,e
	 and $1C
	 jr nz,_
	 bit 0,e
	 ld d,$77	;LD (HL),A
	 jr z,_
	 ld d,$7E	;LD A,(HL)
_
	 ; Set carry if write instruction
	 ld a,d
	 sub $70
	 sub 8
	 
	 djnz memroutine_gen_not_cart0
	 jr c,memroutine_gen_write_cart
	 
	 call memroutine_gen_index
	 ld de,(rom_start)
	 ld (hl),de
	 dec hl
	 ld (hl),$21
	 dec hl
	 ld (hl),$DD
	 dec hl
	 ld (hl),$5B	;LD.LIL IX,ACTUAL_ROM_START
	 dec hl
	 ld (hl),-8
	 dec hl
	 ld (hl),$30	;JR NC,$-6
	 dec hl
	 ld (hl),$40
	 dec hl
	 ld (hl),$FE	;CP $40
	 dec hl
	 ld a,c
	 add a,$7C	;LD A,B/D/H
	 ld (hl),a
	 jr memroutine_gen_end_swap
	 
memroutine_gen_write_ports:
	 ld de,mem_write_ports
memroutine_gen_write:
	 inc a
	 jr z,_
	 ld (hl),$F1	;POP AF
	 dec hl
_
	 ld (hl),d
	 dec hl
	 ld (hl),e
	 dec hl
	 ld (hl),$CD	;CALL routine
	 jr z,_
	 dec hl
	 add a,$7F	;LD A,r
	 ld (hl),a
	 dec hl
	 ld (hl),$F5	;PUSH AF
_
	 call memroutine_gen_load_ix
	 jp memroutine_gen_end
	 
memroutine_gen_not_cart0:
	 djnz memroutine_gen_not_ports
	 jr c,memroutine_gen_write_ports
	 
	 dec d
	 ld (hl),d	;Access IXL instead of (HL)
	 ;Special case for loading into H or L
	 ld a,d
	 and $F0
	 cp $60
	 jr nz,_
	 ld (hl),$EB	;EX DE,HL
	 dec hl
	 res 5,d
	 set 4,d
	 ld (hl),d
_
	 dec hl
	 ld (hl),$DD
	 jr nz,_
	 dec hl
	 ld (hl),$EB	;EX DE,HL
_
	 dec hl
	 ld (hl),mem_read_ports >> 8
	 dec hl
	 ld (hl),mem_read_ports & $FF
	 dec hl
	 ld (hl),$CD	;CALL mem_read_ports
	 call memroutine_gen_load_ix	;LD IX,BC/DE/HL
	 jp memroutine_gen_end
	 
memroutine_gen_write_cart:
	 ld de,mem_write_cart
	 jr memroutine_gen_write
	 
memroutine_gen_write_vram:
	 ld de,mem_write_vram
	 jr memroutine_gen_write
	 
memroutine_gen_not_cart_bank:
	 djnz memroutine_gen_not_vram
	 jr c,memroutine_gen_write_vram
	 
	 call memroutine_gen_index
	 ld de,vram_base
	 ld (hl),de
	 dec hl
	 ld (hl),$21
	 dec hl
	 ld (hl),$DD
	 dec hl
	 ld (hl),$5B	;LD.LIL IX,vram_base
	 ex de,hl
	 ld hl,-9
	 add hl,de
	 ex de,hl
	 dec hl
	 ld (hl),d
	 dec hl
	 ld (hl),e
	 dec hl
	 ld (hl),$E2	;JP PO,$-6
	 dec hl
	 ld (hl),$20
	 dec hl
	 ld (hl),$D6	;SUB $20
	 dec hl
	 ld a,c
	 add a,$7C	;LD A,B/D/H
	 ld (hl),a
	 jp memroutine_gen_end_swap
	 
memroutine_gen_not_ports:
	 djnz memroutine_gen_not_cart_bank
	 jr c,memroutine_gen_write_cart
	 
	 call memroutine_gen_index
	 ld de,rom_bank_base
	 ld (hl),de
	 dec hl
	 ld (hl),$2A
	 dec hl
	 ld (hl),$DD
	 dec hl
	 ld (hl),$5B	;LD.LIL IX,(rom_bank_base)
	 ex de,hl
	 ld hl,-9
	 add hl,de
	 ex de,hl
	 dec hl
	 ld (hl),d
	 dec hl
	 ld (hl),e
	 dec hl
	 ld (hl),$E2	;JP PO,$-6
	 dec hl
	 ld (hl),$40
	 dec hl
	 ld (hl),$C6	;ADD A,$40
	 dec hl
	 ld a,c
	 add a,$7C	;LD A,B/D/H
	 ld (hl),a
	 jp memroutine_gen_end_swap
	 
memroutine_gen_not_vram:
	 djnz memroutine_gen_not_cram
	
	 call memroutine_gen_index
	 ld de,cram_bank_base
	 ld (hl),de
	 dec hl
	 ld (hl),$2A
	 dec hl
	 ld (hl),$DD
	 dec hl
	 ld (hl),$5B	;LD.LIL IX,(cram_bank_base)
	 dec hl
	 ld (hl),-10
	 dec hl
	 ld (hl),$30	;JR NC,$-8
	 dec hl
	 ld (hl),$20
	 dec hl
	 ld (hl),$FE	;CP $20
	 dec hl
	 ld (hl),$A0
	 dec hl
	 ld (hl),$D6	;SUB $A0
	 dec hl
	 ld a,c
	 add a,$7C	;LD A,B/D/H
	 ld (hl),a
	 jp memroutine_gen_end_swap
	
memroutine_gen_not_cram:
	 ;We're in RAM, cool!
	 call memroutine_gen_index
	 ld de,wram_base
	 ld (hl),de
	 dec hl
	 ld (hl),$21
	 dec hl
	 ld (hl),$DD
	 dec hl
	 ld (hl),$5B	;LD.LIL IX,wram_base
	 dec hl
	 ld (hl),-10
	 dec hl
	 ld (hl),$30	;JR NC,$-8
	 dec hl
	 ld (hl),$3E
	 dec hl
	 ld (hl),$FE	;CP $3E
	 dec hl
	 ld (hl),$C0
	 dec hl
	 ld (hl),$D6	;SUB $C0
	 dec hl
	 ld a,c
	 add a,$7C	;LD A,B/D/H
	 ld (hl),a
	 jp memroutine_gen_end_swap
	
memroutine_gen_index:
	 ld (hl),0	;offset
	 dec hl
	 ld (hl),d	;opcode
	 dec hl
	 ld (hl),$DD	;IX prefix
	 dec hl
	 ld (hl),$5B	;.LIL prefix
	 dec hl
	 ld (hl),$08	;EX AF,AF'
	 dec hl
	 ld a,c
	 or a
	 jr nz,_
	 ld (hl),$EB	;EX DE,HL	(if accessing HL)
	 dec hl
	 ld a,-2
_
	 add a,a
	 add a,a
	 add a,a
	 add a,$29
	 ld (hl),a
	 dec hl
	 ld (hl),$DD
	 dec hl
	 ld (hl),$5B	;ADD.LIL IX,BC/DE/DE
	 dec hl
	 ld a,c
	 or a
	 jr nz,_
	 ld (hl),$EB
	 dec hl
_
	 dec hl
	 dec hl
	 ret
	
memroutine_gen_load_ix:
	 dec hl
	 ld (hl),$E1
	 dec hl
	 ld (hl),$DD	;POP IX
	 dec hl
	 ld a,c
	 add a,a
	 add a,a
	 add a,a
	 add a,$E5	;PUSH BC/DE/HL
	 ld (hl),a
	 ret