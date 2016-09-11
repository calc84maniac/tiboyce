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
	
identify_waitloop:
#ifdef 0
	push hl
	 push ix
	 pop af
	 push af
	  ld hl,WaitLoopSearchMessage
	  push hl
	   call debug_printf
	  pop hl
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
waitloop_try_next_target:
	; Consume 2 more bytes of recompiled code
	inc de
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
waitloop_find_data_op_again:
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
	inc de
	push hl
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
	
waitloop_identified:
#ifdef DEBUG
	push bc
	pop af
	push af
	 push ix
	 pop af
	 push af
	  ld hl,WaitLoopIdentifiedMessage
	  push hl
	   call debug_printf
	  pop hl
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
	ld de,-5
	add hl,de	;Sets C flag
	
	; Bail if not enough room for trampoline
	ex de,hl
	ld hl,(recompile_struct_end)
	ld hl,(hl)
	sbc hl,de	;Carry is set
	ret nc
	ex de,hl
	ld (z80codebase+memroutine_next),hl
	
	; Generate call based on variable accessed
	inc hl
	push hl
	 ld (hl),$CD
	 inc hl
	 inc b
	 jr nz,waitloop_variable
	 ld a,c
	 cp STAT & $FF
	 jr z,waitloop_stat
	 cp LY & $FF
	 jr nz,waitloop_variable
	 ld (hl),handle_waitloop_ly & $FF
	 inc hl
	 ld (hl),handle_waitloop_ly >> 8
	 jr waitloop_finish
waitloop_stat:
	 ld (hl),handle_waitloop_stat & $FF
	 inc hl
	 ld (hl),handle_waitloop_stat >> 8
	 jr waitloop_finish
waitloop_variable:
	 ld (hl),handle_waitloop_variable & $FF
	 inc hl
	 ld (hl),handle_waitloop_variable >> 8
waitloop_finish:
	 inc hl
	 ld.s (hl),ix
	pop ix
	ret
	
#ifdef DEBUG
WaitLoopSearchMessage:
	.db "Searching for waitloop at %04X.\n",0
WaitLoopIdentifiedMessage:
	.db "Setting waitloop at %04X, var %04X.\n",0
#endif
	
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
	
vblank_stuff:
	; Reset match bit
	ld (hl),2
	
	; Update FPS counter
vfps = $+1
	ld a,0
	add a,1
	daa
	ld (vfps),a
	
	; Finish rendering, if applicable
	push de
	 push ix
	  ld a,(z80codebase+render_this_frame)
	  or a
	  jr z,skip_this_frame
	
	  push bc
	   ; Finish rendering the frame
	   ld a,144
	   call render_scanlines
	  
	   ; Display sprites
	   ld a,(hram_base+LCDC)
	   rla
	   push iy
	    call c,draw_sprites
	   pop iy
	
	   ; Swap buffers
	   call prepare_next_frame
	   
#ifndef DBGNOSCALE
	   ; EXPAND DONG
	   ld a,144/3
	   ex de,hl
	   ld b,e	;B=0
_
	   push de
	   pop hl
	   ld c,160
	   add hl,bc
	   ex de,hl
	   ldir
	   ex de,hl
	   ld c,160
	   add hl,bc
	   push hl
	   pop de
	   add hl,bc
	   ex de,hl
	   ldir
	   dec a
	   jr nz,-_
#else
	   xor a
#endif
	   
fps_display_smc:
	   jr z,NoFPSDisplay
	   
fps = $+1
	   ld a,0
	   add a,1
	   daa
	   ld (fps),a
	   
	   ld a,(mpRtcSecondCount)
last_second = $+1
	   cp -1
	   call nz,update_fps
	   
	   ld de,0
vfps_display_tens = $+1
	   ld a,0
	   call display_digit
	   ld de,4
vfps_display_ones = $+1
	   ld a,0
	   call display_digit
	   ld de,12
fps_display_tens = $+1
	   ld a,0
	   call display_digit
	   ld de,16
fps_display_ones = $+1
	   ld a,0
	   call display_digit
	   
NoFPSDisplay:
	  pop bc
	  
	  ; Signify frame was rendered
	  scf
skip_this_frame:
	  
	  ; Handle frame synchronization
	  ld hl,z80codebase+frame_excess_count
	  dec (hl)
	  jp p,no_frame_sync
	  ; If we didn't render, save for later
	  jr nc,frame_sync_later
frame_sync_loop:
	  push hl
	   ld de,$000800
	   call wait_for_interrupt
	   ld hl,mpLcdIcr
	   ld (hl),4
	  pop hl
	  inc (hl)
	  jr nz,frame_sync_loop
frame_sync_later:
	  ; Set Z
	  xor a
no_frame_sync:
	  
	  ; Handle frameskip logic
	  ; At this point A=0, Z holds auto state
	  ex de,hl
	  ld hl,skippable_frames
frameskip_type_smc:
	  jr z,no_frameskip	;JR no_frameskip when off, JR $+2 when manual
	  dec (hl)
	  jr nz,frameskip_end
no_frameskip:
frameskip_value_smc = $+1
	  ld (hl),1
	  ex de,hl
	  bit 7,(hl)
	  jr nz,_
	  ld (hl),a
_
	  inc a
frameskip_end:
	  ld (z80codebase+render_this_frame),a
	  
	  ; Always update palettes because it's basically free!
	  call update_palettes
	  
	  ; Get keys
	  scf
	  sbc hl,hl
	  ld ix,mpKeypadGrp0

key_smc_right:
	  bit 2,(ix+7*2)	;Right
	  jr z,_
	  dec l
_
key_smc_left:
	  bit 1,(ix+7*2)	;Left
	  jr z,_
	  bit 0,l
	  set 0,l
	  jr z,_
	  res 1,l
_
key_smc_up:
	  bit 3,(ix+7*2)	;Up
	  jr z,_
	  res 2,l
_
key_smc_down:
	  bit 0,(ix+7*2)	;Down
	  jr z,_
	  bit 2,l
	  set 2,l
	  jr z,_
	  res 3,l
_
key_smc_a:
	  bit 5,(ix+1*2)	;2ND
	  jr z,_
	  dec h
_
key_smc_b:
	  bit 7,(ix+2*2)	;ALPHA
	  jr z,_
	  res 1,h
_
key_smc_select:
	  bit 7,(ix+3*2)	;X,T,0,n
	  jr z,_
	  res 2,h
_
key_smc_start:
	  bit 6,(ix+1*2)	;MODE
	  jr z,_
	  res 3,h
_
	  ld.sis (keys),hl

key_smc_menu:
	  bit 6,(ix+6*2)	;CLEAR
	  call nz,emulator_menu

	 pop ix
	pop de

	; Trigger VBLANK
	ld hl,hram_base+LCDC
	bit 7,(hl)
	jr z,_
	inc hl
	bit 4,(hl)
	ld l,IF & $FF
	set 0,(hl)
	jr z,_
	set 1,(hl)
_
	ld hl,mpTimerIntStatus
	ld a,(hl)
	ret.l
	
	; DE = interrupt source mask to wait on
	; Destroys: IX, DE, HL
	; Interrupt is acknowledged before waiting,
	; but not handled or acknowledged afterward
ack_and_wait_for_interrupt:
	ld (mpIntAcknowledge),de
	
	; DE = interrupt source mask to wait on
	; Destroys: IX, DE, HL
	; Interrupt is not actually handled or acknowledged
wait_for_interrupt:
	ld hl,mpIntEnable
	ld ix,(hl)
	ld (hl),de
	ex de,hl
	ld hl,z80codebase+rst38h
	ld (hl),$C9	;RET
	call.is wait_for_interrupt_stub
	ld (hl),$F5	;PUSH AF
	ex de,hl
	ld (hl),ix
	ret
	
prepare_next_frame:
	ld hl,scanlineLUT + (15*3)
	ld (scanlineLUT_ptr),hl
	ld hl,vram_tiles_start
	ld (window_tile_ptr),hl
	ld hl,(mpLcdBase)
	ld (current_buffer),hl
	dec hl
	ld (scanline_ptr),hl
	inc hl
	ld a,h
	xor (gb_frame_buffer_1 ^ gb_frame_buffer_2)>>8
	ld h,a
#ifndef DBGNOSCALE
	ld a,48 + $55
	ld (scanline_scale_accumulator),a
#endif
	xor a
	ld (window_tile_offset),a
	ld (myLY),a
	ld (mpLcdBase),hl
	ret
	
update_fps:
	ld (last_second),a
	
	ld a,(vfps)
	ld e,a
	and $0F
	ld (vfps_display_ones),a
	xor e
	rrca
	rrca
	rrca
	rrca
	ld (vfps_display_tens),a
	
	ld a,(fps)
	ld e,a
	and $0F
	ld (fps_display_ones),a
	xor e
	rrca
	rrca
	rrca
	rrca
	ld (fps_display_tens),a
	
	xor a
	ld (vfps),a
	ld (fps),a
	ret
	
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
	ret.l
	
lcdc_write:
	ld a,(z80codebase+render_this_frame)
	or a
	call.il nz,render_catchup
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
	xor (vram_pixels_start >> 16) ^ $FF
	ld (LCDC_0_smc),a
_
	bit 1,l
	jr z,_
	ld a,(LCDC_1_smc)
	xor $0F ^ $3E	;LD (HL),BC vs LD (HL),IY
	ld (LCDC_1_smc),a
_
	bit 2,l
	jr z,_
	ld a,(LCDC_2_smc_1)
	xor (7*3)^(15*3)
	ld (LCDC_2_smc_1),a
	ld a,(LCDC_2_smc_2)
	xor 8^16
	ld (LCDC_2_smc_2),a
	ld a,(LCDC_2_smc_3)
	xor $B1 ^ $B3	;RES 6,C vs RES 6,E
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
	ld a,(LCDC_4_smc_1)
	xor $80
	ld (LCDC_4_smc_1),a
	ld (LCDC_4_smc_2),a
_
	bit 5,l
	jr z,_
	ld a,(LCDC_5_smc)
	xor $08	;JR NC vs JR C
	ld (LCDC_5_smc),a
_
	bit 6,l
	jr z,_
	ld a,(LCDC_6_smc)
	xor $20	;$20 vs $00
	ld (LCDC_6_smc),a
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
	
skippable_frames:
	.db 1
current_buffer:
	.dl 0