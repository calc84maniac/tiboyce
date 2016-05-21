#define RST_BITS $C7+r_bits
#define RST_MEM $C7+r_mem
#define RST_PUSH $C7+r_push
#define RST_POP $C7+r_pop
#define RST_CALL $C7+r_call
#define RST_BRANCH $C7+r_branch
#define RST_INTERRUPT $C7+r_interrupt

recompile_struct_end:
	.dl 0
recompile_cache:
	.dl 0
base_address:
	.dl 0

flush_code:
#ifdef DEBUG
	ld hl,FlushMessage
	push hl
	 call printf
	pop hl
#endif
	ld hl,recompile_struct
	ld (recompile_struct_end),hl
	ld de,z80codebase+z80codesize
	ld (hl),de
	ld hl,z80codebase+myz80stack-256
	ld (z80codebase+memroutine_next),hl
	ld hl,recompile_cache_end
	ld (recompile_cache),hl
	ld hl,waitloop_target
	ld.sis (current_waitloop_1),hl
	ld.sis (current_waitloop_2),hl
	ld hl,memroutineLUT
	push hl
	pop de
	inc de
	ld (hl),l
	ld bc,$01FF
	ldir
	ld (int_cached_return),bc
	ret
	
FlushMessage:
	.db "Flushing recompiled code!\n",0
	
	; Input: DE = GB address
	; Output: HL = base pointer
get_base_address:
	ld a,d
	add a,a
	jr c,++_
	add a,a
	jr c,_
	ld hl,(rom_start)
	ret
_
	ld hl,(rom_bank_base)
	ret
_
	cp -2*2
	jr nc,_
	ld hl,vram_base
	add a,$40
	ret po
	ld hl,(cram_bank_base)
	ret
_
	ld hl,hram_base
	ret
	
	
identify_waitloop:
	ld hl,(int_cached_code)
	ld de,(int_cached_return)
	ld bc,opcodesizes
	
waitloop_find_jump:
	ld a,(de)
	ld c,a
	and $E7
	cp $20
	jr z,waitloop_found_jump
	cp $C2
	jr z,waitloop_found_jump
	ld a,(bc)
	or a
	ret.l z
	add a,e
	ld e,a
	jr nc,_
	inc d
_
	dec b
	ld a,(bc)
	inc b
	add a,l
	ld l,a
	jr nc,waitloop_find_jump
	inc h
	jr waitloop_find_jump
waitloop_found_jump:
	ld.s a,(hl)
	and $E7
	cp $C2
	ret.l nz
	inc hl
	ld.s bc,(hl)
	push bc
	 ld a,(de)
	 add a,a
	 inc de
	 jr c,_
	 ld a,(de)
	 inc de
	 rlca
	 sbc hl,hl
	 rrca
	 ld l,a
	 jr ++_
_
	 ex de,hl
	 ld e,(hl)
	 inc hl
	 ld d,(hl)
	 call get_base_address
_
	 add hl,de
	 ex de,hl
	pop hl
	 
	; Check for a read
	ld a,(de)
	cp $CB		;Bitwise ops
	inc de
	jr z,waitloop_found_read_bitwise
	; Consume 3 bytes of recompiled code
	inc hl
	inc hl
	inc hl
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
	ret.l
	
waitloop_found_read_1:
	; Use 8-bit immediate as read address
	ld ix,$00FF00
	ld a,(de)
	ld ixl,a
	; Consume immediate value
	inc de
	jr waitloop_find_data_op
	
waitloop_found_read_2:
	; Use 16-bit immediate as read address
	ex de,hl
	ld ix,(hl)
	lea.s ix,ix
	ex de,hl
waitloop_try_second_target:
	; Consume 2 more bytes of recompiled code
	inc hl
	inc hl
	; Consume immediate value
	inc de
	inc de
	jr waitloop_find_data_op
	
waitloop_found_read_bitwise:
	ld a,(de)
	and $C7
	cp $46		;BIT b,(HL)
	ret.l nz
	; Parse this opcode as a data op
	dec de
	exx
	
waitloop_found_read_hl:
	; Use HL as read address
	push hl
waitloop_found_read_rr:
	; Use stack value as read address
	exx
	pop ix
waitloop_find_data_op:
	ld a,(de)
	; Consume first byte of opcode and recompiled code
	inc de
	inc hl
	cp $CB		;Bitwise ops
	jr z,waitloop_found_data_op_bitwise
	and $C7
	cp $C6		;Immediate ALU ops
	jr z,waitloop_found_data_op_1
	and $C0
	cp $80		;Register ALU ops
	jr z,waitloop_found_data_op
	ret.l
	
waitloop_found_data_op_bitwise:
	ld a,(de)
	add a,$40	;BIT b,r
	ret.l po
waitloop_found_data_op_1:
	; Consume second byte of opcode and recompiled code
	inc de
	inc hl
waitloop_found_data_op:
	ld a,(de)
	and $E7
	cp $20		;JR cc
	jr z,waitloop_found_jr
	cp $C2		;JP cc
	jr nz,waitloop_find_data_op	;Allow multiple data operations
	inc de	; Consume extra JP byte
waitloop_found_jr:
	; Validate the recompiled target address
	inc hl
	push hl
	 ld.s hl,(hl)
	 sbc hl,bc
	 jr nz,waitloop_failure_pop
	
	 ; Check if read address is STAT
	 ld a,ixl
	 add a,$FFFF - STAT
	 and ixh
	 ; If high bit of IX was set, this was the second target
	 add ix,ix
	 jr c,waitloop_second_target
	 ; If STAT, then leave
	 inc a
	 jr z,waitloop_failure_pop
	
#ifdef DEBUG
	 push de
	  push bc
	   ld hl,WaitLoopFirstTargetMessage
	   push hl
	    call printf
	   pop hl
	  pop bc
	 pop de
#endif
	
	 ; Restore previous waitloop to its original target
	 ld.sis ix,(waitloop_target)
	 ld.sis hl,(current_waitloop_1)
	 ld.s (hl),ix
	 ld.sis hl,(current_waitloop_2)
	 ld.s (hl),ix
	pop hl
	
	; Set first target
	ld.sis (current_waitloop_1),hl
	ld.sis (waitloop_target),bc
	; Set high bit of IX to indicate second target
	ld ix,handle_waitloop+$800000
	ld.s (hl),ix
	; Set dummy second target
	lea ix,ix+waitloop_target-handle_waitloop
	ld.sis (current_waitloop_2),ix
#ifdef DEBUG
	jp waitloop_try_second_target
#else
	jr waitloop_try_second_target
#endif
	
waitloop_second_target:
	pop hl
	
	; Set second target
	ld.sis (current_waitloop_2),hl
	ld de,handle_waitloop
	ld.s (hl),de
#ifdef DEBUG
	ld hl,WaitLoopSecondTargetMessage
	push hl
	 call printf
	pop hl
#endif
	push hl
waitloop_failure_pop:
	pop hl
	ret.l
	
#ifdef DEBUG
WaitLoopCheckMessage:
	.db "Checking for waitloop at %04X (%04X).\n",0
WaitLoopFirstTargetMessage:
	.db "Setting waitloop at %04X.\n",0
	
WaitLoopSecondTargetMessage:
	.db "A double waitloop, even!\n",0
#endif
	
	; Get GB code address from recompiled code pointer (rounds up)
	; Input: DE = code pointer
	; Output: Z set if not found, or HL = 16-bit code pointer (rounded up), DE = GB address, IX = Literal GB address
lookup_gb_code_address:
#ifdef 0
	push de
	 ld hl,LookupGBMessage
	 push hl
	  call printf
	  ;call Wait
	 pop hl
	pop de
#endif
	ld bc,recompile_struct
	ld ix,(recompile_struct_end)
lookup_gb_loop:
	lea hl,ix
	or a
	sbc hl,bc
	jr z,lookup_gb_found_region
	srl h
	rr l
	res 2,l
	add hl,bc
	push hl
	 ld hl,(hl)
	 scf
	 sbc.s hl,de
	 jr nc,lookup_gb_lower
	 ex (sp),ix
	 lea bc,ix+8
lookup_gb_lower:
	pop ix
	jr lookup_gb_loop
	
lookup_gb_found_region:
	ld a,b
	or c
	ret.l z
	ld bc,opcodesizes
	ld hl,(ix-8)
	ld ix,(ix-6)
	sbc.s hl,de
	jr z,lookup_gb_found
lookup_gb_found_loop:
	ld c,(ix)
	ld a,(bc)
	or a
	jr z,lookup_gb_found
	add a,ixl
	ld ixl,a
	jr nc,_
	inc ixh
_
	dec b
	ld a,(bc)
	inc b
	add a,l
	ld l,a
	jr nc,lookup_gb_found_loop
	inc h
	jr nz,lookup_gb_found_loop
	ld a,c
	and %11100111
	cp $C4
	jr nz,lookup_gb_found
	lea ix,ix-3
	sbc hl,hl
lookup_gb_found:
	add hl,de
	push hl
	 or a
	 lea hl,ix
	 ld de,(rom_start)
	 sbc hl,de
	 ld de,$4000
	 sbc hl,de
	 jr c,lookup_gb_done
	 lea hl,ix
	 ld de,(rom_bank_base)
	 sbc hl,de
	 ld de,$8000
	 sbc hl,de
	 jr c,lookup_gb_done
	 lea hl,ix
	 ld de,vram_base
	 sbc hl,de
	 ld de,$FE00
	 sbc hl,de
	 jr c,lookup_gb_done
	 lea hl,ix
	 ld de,(cram_bank_base)
	 sbc hl,de
	 ld de,$C000
	 sbc hl,de
	 jr c,lookup_gb_done
	 lea hl,ix
	 ld de,-hram_base
lookup_gb_done:
	 add hl,de
	 ex de,hl
#ifdef 0
	 push ix
	  push de
	   ld hl,LookupGBFoundMessage
	   push hl
	    call printf
	    ;call Wait
	   pop hl
	  pop de
	 pop ix
#endif
	pop hl
	ret.l
	
LookupGBMessage:
	.db "Looking up GB address from %04X\n",0
	
LookupGBFoundMessage:
	.db "Found GB %04X @ %04X\n",0
	
int_cache_hit:
int_cached_code = $+2
	ld ix,0
	ret.l
	
pop_and_lookup_code_cached:
	ld de,(iy)
	inc de
	dec.s de
	lea iy,iy+2
lookup_code_cached:
	call get_base_address
	ld (base_address),hl
	add hl,de
	ex de,hl
	
int_cached_return = $+1
	ld hl,0
	sbc hl,de
	jr z,int_cache_hit
	
	ld bc,(recompile_cache)
	ld ix,recompile_cache_end
lookup_code_cached_loop:
	lea hl,ix
	or a
	sbc hl,bc
	jr z,lookup_code_cached_found
	srl h
	rr l
	jr nc,_
	dec hl
	dec hl
_
	add hl,bc
	push hl
	 ld hl,(hl)
	 sbc hl,de
	 jr nc,lookup_code_cached_lower
	 ex (sp),ix
	 lea bc,ix+5
lookup_code_cached_lower:
	pop ix
	jr lookup_code_cached_loop
	
lookup_code_cached_found:
	ld a,b
	sub recompile_cache_end>>8 & $FF
	or c
	jr z,lookup_code_cached_miss
	ld hl,(ix)
	sbc hl,de
	jr nz,lookup_code_cached_miss
	ld ix,(ix+3)
	ret.l
	
lookup_code_cached_miss:
	push de
	 push bc
#ifdef DEBUG
	  push de
	   ld hl,(base_address)
	   ex de,hl
	   xor a
	   sbc hl,de
	   push hl
	    ld h,a
	    ld a,(z80codebase+curr_rom_bank)
	    ld l,a
	    push hl
	     ld hl,CacheMissMessage
	     push hl
	      call printf
	     pop hl
	    pop hl
	   pop hl
	  pop de
#endif
	  call lookup_code_by_pointer
	  ld hl,(recompile_struct_end)
	  ld de,(recompile_cache)
	  ld bc,8
	  add hl,bc
	  sbc hl,de
	  jr c,_
	  ld de,recompile_cache_end
_
	 pop hl
	 or a
	 sbc hl,de
	 jr nc,_
	 xor a	;Set Z
_
	 ld b,h
	 ld c,l
	 ld hl,-5
	 add hl,de
	 ld (recompile_cache),hl
	 jr z,_
	 ex de,hl
	 ldir
	 ex de,hl
_
	pop de
	ld (hl),de
	inc hl
	inc hl
	inc hl
	lea de,ix
	ld (hl),e
	inc hl
	ld (hl),d
	ret.l
	
CacheMissMessage:
	.db "Cache miss at %02X:%04X\n",0
	
	; Input: DE = GB address to look up
	; Output: IX = recompiled code pointer
lookup_code:
#ifdef 0
	push de
	 ld a,(z80codebase+curr_rom_bank)
	 ld e,a
	 ld d,0
	 push de
	  ld hl,LookupMessage
	  push hl
	   call printf
	  pop hl
	 pop de
	pop de
#endif
	call get_base_address
	ld (base_address),hl
	add hl,de
	ex de,hl
lookup_code_by_pointer:
	ld hl,(recompile_struct_end)
	push hl
	 ld bc,(hl)
	 ld hl,(z80codebase+memroutine_next)
	 or a
	 sbc hl,bc
	 ld bc,opcodesizes
	 jr c,flush_and_recompile
lookuploop_restore:
	pop ix
lookuploop:
	ld a,ixh
	or ixl
	jr z,recompile
	lea ix,ix-8
	ld hl,(ix+5)
	sbc hl,de
	jr c,lookuploop
	ld hl,(ix+2)
	sbc hl,de
	jr z,lookupfoundstart
	jr nc,lookuploop
	push ix
	 ld ix,(ix)
foundloop:
	 add hl,de
	 ld c,(hl)
	 ld a,(bc)
	 or a
	 jr z,lookuploop_restore
	 add a,l
	 ld l,a
	 jr nc,_
	 inc h
_
	 dec b
	 ld a,(bc)
	 inc b
	 add a,ixl
	 ld ixl,a
	 jr nc,_
	 inc ixh
	 or a
_
	 sbc hl,de
	 jr nz,foundloop
	 
	pop hl
	ret
	
lookupfoundstart:
	ld ix,(ix)
	ret
	
flush_and_recompile:
	pop hl
	push bc
	 push de
	  call flush_code
	 pop de
	pop bc
	
	; Input: DE points to GB opcodes to recompile, DE-(base_address) is actual GB address
	; Output: IX = recompiled code pointer
recompile:
	push de
	 ld hl,(recompile_struct_end)
	 ld hl,(hl)
#ifdef DEBUG
	 push hl
	  inc hl
	  dec.s hl
	  push hl
	   push de
	    ld hl,(base_address)
	    ex de,hl
	    or a
	    sbc hl,de
	    push hl
	     or a,a
	     sbc hl,hl
	     ld a,(z80codebase+curr_rom_bank)
	     ld l,a
	     push hl
	      ld hl,RecompileMessage
	      push hl
	       call printf
	      pop hl
	     pop hl
	    pop hl
	   pop de
	  pop hl
	 pop hl
#endif
	 ex de,hl
	 ld ix,opgenroutines
	 ld bc,opgentable
	 ld c,(hl)
	 ld a,(bc)
	 ld ixl,a
	 jp (ix)
	
opgenEND:
	 inc de
	 ld ix,(recompile_struct_end)
	 ld (ix+5),hl
	 ld hl,(ix)
#ifdef 0
	 call print_recompiled_code
#endif
	 ex (sp),hl
	 ld (ix+2),hl
	 lea hl,ix+8
	 ld (hl),de
	 ld (recompile_struct_end),hl
	 ; Check for collision with cache
	 ld hl,(recompile_cache)
	 lea bc,ix+16
	 or a
	 sbc hl,bc
	 call c,flush_cache
	 ld hl,(z80codebase+memroutine_next)
	 sbc hl,de
	pop ix
	ret nc
	ld ix,(recompile_struct_end)
	ld hl,(ix-6)
	ld de,(base_address)
	or a
	sbc hl,de
	ld.sis (flush_address),hl
	ld ix,flush_handler
flush_cache:
	ld hl,recompile_cache_end
	ld (recompile_cache),hl
	ret
	
#ifdef DEBUG
#ifdef 0
print_recompiled_code:
	push ix
	 push hl
_
	  push de
	   push hl
	    ld a,(hl)
	    or a
	    sbc hl,hl
	    ld l,a
	    push hl
	     ld hl,ByteFormat
	     push hl
	      call printf
	     pop hl
	    pop hl
	   pop hl
	  pop de
	  inc hl
	  or a
	  sbc hl,de
	  add hl,de
	  jr nz,-_
	  call PutNewLine
	 pop hl
	pop ix
	ret
	;jp Wait
#endif
	
LookupMessage:
	.db "Looking up GB code at %02X:%04X\n",0
	
RecompileMessage:
	.db "Recompiling %02X:%04X (%06X) to %04X\n",0
	
ByteFormat:
	.db "%02X",0
#endif
	
	.block (-$)&255
opcoderecsizes:
	.db 0,3,3,1,1,1,2,1
	.db 5,1,3,1,1,1,2,1
	.db 0,3,3,1,1,1,2,1
	.db 0,1,3,1,1,1,2,1
	.db 3,3,3,1,1,1,2,1
	.db 3,1,3,1,1,1,2,1
	.db 3,5,3,3,3,3,4,1
	.db 3,3,3,3,1,1,2,1
	
	.db 0,1,1,1,1,1,3,1
	.db 1,0,1,1,1,1,3,1
	.db 1,1,0,1,1,1,3,1
	.db 1,1,1,0,1,1,3,1
	.db 1,1,1,1,0,1,3,1
	.db 1,1,1,1,1,0,3,1
	.db 3,3,3,3,3,3,3,3
	.db 1,1,1,1,1,1,3,0
	
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	
	.db 1,2,3,0,7,2,2,5
	.db 1,0,3,2,7,5,2,5
	.db 1,2,3,0,7,2,2,5
	.db 1,0,3,0,7,0,2,5
	.db 3,2,3,0,0,2,2,5
	.db 4,0,5,0,0,0,2,5
	.db 3,2,3,3,0,2,2,5
	.db 4,3,5,3,0,0,2,5
	
opcodesizes:
	.db 1,3,1,1,1,1,2,1
	.db 3,1,1,1,1,1,2,1
	.db 1,3,1,1,1,1,2,1
	.db 0,1,1,1,1,1,2,1
	.db 2,3,1,1,1,1,2,1
	.db 2,1,1,1,1,1,2,1
	.db 2,3,1,1,1,1,2,1
	.db 2,1,1,1,1,1,2,1
	
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	.db 1,1,1,1,1,1,1,1
	
	.db 1,1,3,0,3,1,2,1
	.db 1,0,3,2,3,3,2,1
	.db 1,1,3,0,3,1,2,1
	.db 1,0,3,0,3,0,2,1
	.db 2,1,1,0,0,1,2,1
	.db 2,0,3,0,0,0,2,1
	.db 2,1,1,1,0,1,2,1
	.db 2,1,3,1,0,0,2,1
	
opgentable:
;00
	.db opgen0byte & $FF
	.db opgen3byte_low & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen2byte & $FF
	.db opgen1byte & $FF
;08
	.db opgen08 & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen2byte & $FF
	.db opgen1byte & $FF
;10
	.db opgen0byte & $FF
	.db opgen3byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen2byte & $FF
	.db opgen1byte & $FF
;18
	.db opgenJR & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen2byte & $FF
	.db opgen1byte & $FF
;20
	.db opgenJRcond & $FF
	.db opgen3byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen2byte & $FF
	.db opgen1byte & $FF
;28
	.db opgenJRcond & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen2byte & $FF
	.db opgen1byte & $FF
;30
	.db opgenJRcond & $FF
	.db opgen31 & $FF
	.db opgenMEM & $FF
	.db opgen33 & $FF
	.db opgen34 & $FF
	.db opgen35 & $FF
	.db opgen36 & $FF
	.db opgen1byte & $FF
;38
	.db opgenJRcond & $FF
	.db opgen39 & $FF
	.db opgenMEM & $FF
	.db opgen3B & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen2byte & $FF
	.db opgen1byte & $FF
;40
	.db opgen0byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;48
	.db opgen1byte & $FF
	.db opgen0byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;50
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen0byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;58
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen0byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;60
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen0byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;68
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen0byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;70
	.db opgenMEM & $FF
	.db opgenMEM & $FF
	.db opgenMEM & $FF
	.db opgenMEM & $FF
	.db opgenMEM & $FF
	.db opgenMEM & $FF
	.db opgen76 & $FF
	.db opgenMEM & $FF
;78
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen0byte & $FF
;80
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;88
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;90
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;98
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;A0
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;A8
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;B0
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;B8
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgen1byte & $FF
	.db opgenMEM & $FF
	.db opgen1byte & $FF
;C0
	.db opgen1byte & $FF
	.db opgenPOP & $FF
	.db opgenJPcond & $FF
	.db opgenJP & $FF
	.db opgenCALLcond & $FF
	.db opgenPUSH & $FF
	.db opgen2byte & $FF
	.db opgenRST & $FF
;C8
	.db opgen1byte & $FF
	.db opgenRET & $FF
	.db opgenJPcond & $FF
	.db opgenCB & $FF
	.db opgenCALLcond & $FF
	.db opgenCALL & $FF
	.db opgen2byte & $FF
	.db opgenRST & $FF
;D0
	.db opgen1byte & $FF
	.db opgenPOP & $FF
	.db opgenJPcond & $FF
	.db opgenINVALID & $FF
	.db opgenCALLcond & $FF
	.db opgenPUSH & $FF
	.db opgen2byte & $FF
	.db opgenRST & $FF
;D8
	.db opgen1byte & $FF
	.db opgenRETI & $FF
	.db opgenJPcond & $FF
	.db opgenINVALID & $FF
	.db opgenCALLcond & $FF
	.db opgenINVALID & $FF
	.db opgen2byte & $FF
	.db opgenRST & $FF
;E0
	.db opgenE0 & $FF
	.db opgenPOP & $FF
	.db opgenE2 & $FF
	.db opgenINVALID & $FF
	.db opgenINVALID & $FF
	.db opgenPUSH & $FF
	.db opgen2byte & $FF
	.db opgenRST & $FF
;E8
	.db opgenE8 & $FF
	.db opgenE9 & $FF
	.db opgenEA & $FF
	.db opgenINVALID & $FF
	.db opgenINVALID & $FF
	.db opgenINVALID & $FF
	.db opgen2byte & $FF
	.db opgenRST & $FF
;F0
	.db opgenF0 & $FF
	.db opgenPOP & $FF
	.db opgenF2 & $FF
	.db opgenF3 & $FF
	.db opgenINVALID & $FF
	.db opgenPUSH & $FF
	.db opgen2byte & $FF
	.db opgenRST & $FF
;F8
	.db opgenF8 & $FF
	.db opgenF9 & $FF
	.db opgenFA & $FF
	.db opgenFB & $FF
	.db opgenINVALID & $FF
	.db opgenINVALID & $FF
	.db opgen2byte & $FF
	.db opgenINVALID & $FF
	
opgenCONSTwrite:
	ld bc,0
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	ld a,b
	rlca
	jr nc,opgencartwrite
	
	rlca
	jr nc,opgenVRAMwrite
	
	inc a
	jr z,opgenHMEMwrite
	
opgenWRAMwrite:
	push hl
	 ld hl,wram_base
	 add hl,bc
	 ex de,hl
	 ld (hl),$5B ;LD.LIL (addr),A
	 inc hl
	 ld (hl),$32
	 inc hl
	 ld (hl),de
	 inc hl
	 inc hl
	pop de
	jp opgen_next_swap_skip
	
opgencartwrite:
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),write_cart_handler & $FF
	inc hl
	ld (hl),write_cart_handler >> 8
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgenVRAMwrite:
	rlca
	jr c,opgenCRAMwrite
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),write_vram_handler & $FF
	inc hl
	ld (hl),write_vram_handler >> 8
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgenCRAMwrite:
	ld a,(ram_size+1)
	add a,a
	jr c,_
	push hl
	 ld hl,(cram_bank_base)
	 add hl,bc
	 ex de,hl
	 ld (hl),$5B ;LD.LIL (addr),A
	 inc hl
	 ld (hl),$32
	 inc hl
	 ld (hl),de
	 inc hl
	 inc hl
	pop de
	jp opgen_next_swap_skip
_
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),write_cram_bank_handler & $FF
	inc hl
	ld (hl),write_cram_bank_handler >> 8
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgenHMEMwrite:
	xor a
	ld (de),a
	inc de
	ld (de),a
	inc de
	jr _
	
opgenFFwrite:
	inc hl
	ld c,(hl)
	ld b,$FF
_
	ex de,hl
	ld a,c
	inc a
	jp m,opgenHRAMwrite
	jr nz,_
	ld bc,writeIEhandler
	jr opgenHMEMwriteroutine
_
	sub (IF & $FF)+1
	jr nz,_
	ld bc,writeIFhandler
	jr opgenHMEMwriteroutine
_
	sub LCDC - IF
	jr nz,_
	ld bc,writeLCDChandler
	jr opgenHMEMwriteroutine
_
	sub SCY - LCDC
	jr nz,_
	ld bc,writeSCYhandler
	jr opgenHMEMwriteroutine
_
	dec a
	jr nz,_
	ld bc,writeSCXhandler
	jr opgenHMEMwriteroutine
_
	sub WY - SCX
	jr nz,_
	ld bc,writeWYhandler
	jr opgenHMEMwriteroutine
_
	dec a
	jr nz,_
	ld bc,writeWXhandler
	jr opgenHMEMwriteroutine
_
	sub LYC - WX
	jr nz,_
	ld bc,writeLYChandler
	jr opgenHMEMwriteroutine
_
	dec a
	jr nz,opgenHRAMwrite
	ld bc,writeDMAhandler
opgenHMEMwriteroutine:
	ld (hl),$CD ;CALL addr
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgenHRAMwrite:
	ld (hl),$32 ;LD (addr),A
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgencartread:
	rlca
	jr c,opgencartbankread
	push hl
	 ld hl,(rom_start)
	 add hl,bc
	 ex de,hl
	 ld (hl),$5B ;LD.LIL A,(addr)
	 inc hl
	 ld (hl),$3A
	 inc hl
	 ld (hl),de
	 inc hl
	 inc hl
	pop de
	jp opgen_next_swap_skip
	
opgenCONSTread:
	ld bc,0
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	ld a,b
	rlca
	jr nc,opgencartread
	
	rlca
	jr nc,opgenVRAMread
	
	inc a
	jr z,opgenHMEMread
	
opgenWRAMread:
	push hl
	 ld hl,wram_base
	 add hl,bc
	 ex de,hl
	 ld (hl),$5B ;LD.LIL A,(addr)
	 inc hl
	 ld (hl),$3A
	 inc hl
	 ld (hl),de
	 inc hl
	 inc hl
	pop de
	jp opgen_next_swap_skip
	
opgencartbankread:
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),read_rom_bank_handler & $FF
	inc hl
	ld (hl),read_rom_bank_handler >> 8
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgenVRAMread:
	rlca
	jr c,opgenCRAMread
	
	push hl
	 ld hl,vram_base
	 add hl,bc
	 ex de,hl
	 ld (hl),$5B ;LD.LIL A,(addr)
	 inc hl
	 ld (hl),$3A
	 inc hl
	 ld (hl),de
	 inc hl
	 inc hl
	pop de
	jp opgen_next_swap_skip
	
opgenCRAMread:
	ld a,(ram_size+1)
	add a,a
	jr c,_
	push hl
	 ld hl,(cram_bank_base)
	 add hl,bc
	 ex de,hl
	 ld (hl),$5B ;LD.LIL A,(addr)
	 inc hl
	 ld (hl),$3A
	 inc hl
	 ld (hl),de
	 inc hl
	 inc hl
	pop de
	jp opgen_next_swap_skip
_
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),read_cram_bank_handler & $FF
	inc hl
	ld (hl),read_cram_bank_handler >> 8
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgenHMEMread:
	xor a
	ld (de),a
	inc de
	ld (de),a
	inc de
	jr _
	
opgenFFread:
	inc hl
	ld c,(hl)
	ld b,$FF
_
	ex de,hl
	ld a,c
	rlca
	jr c,opgenHRAMread
#comment
	cp $44<<1
	jr nz,_
	ld bc,readLY
	jr opgenHMEMreadroutine
_
#endcomment
	cp $04<<1
	jr nz,_
	ld bc,readDIVhandler
	jr opgenHMEMreadroutine
_
	cp $44<<1
	jr nz,_
	ld bc,readLYhandler
	jr opgenHMEMreadroutine
_
	cp $41<<1
	jr nz,_
	ld bc,readSTAThandler
	jr opgenHMEMreadroutine
_
	cp $00<<1
	jr nz,opgenHRAMread
	ld bc,readP1handler
opgenHMEMreadroutine:
	ld (hl),$CD
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgenHRAMread:
	ld (hl),$3A ;LD A,(addr)
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgen_emitbranch:
	ld a,RST_BRANCH
	ld (de),a
opgen_emitPC:
	scf
	ld a,(base_address)
	cpl
	adc a,l
	inc de
	ld (de),a
	ld a,(base_address+1)
	cpl
	adc a,h
	inc de
	ld (de),a
	ret