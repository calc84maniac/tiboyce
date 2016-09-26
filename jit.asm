#define RST_BITS $C7+r_bits
#define RST_MEM $C7+r_mem
#define RST_PUSH $C7+r_push
#define RST_POP $C7+r_pop
#define RST_CALL $C7+r_call
#define RST_CYCLE_CHECK $C7+r_cycle_check
#define RST_INTERRUPT $C7+r_interrupt

#define RAM_PREFIX_SIZE 5

recompile_struct_end:
	.dl 0
recompile_cache:
	.dl 0
base_address:
	.dl 0

; Do as in flush_code, but also reset RAM block padding amount.
; This is called only at startup, because block padding should 
; persist between flushes to reduce flush rates overall.
flush_code_reset_padding:
	; Set RAM padding to minimum (none)
	or a
	sbc hl,hl
	ld (ram_block_padding),hl
	
; Flushes the recompiled code, generated routines, and all caches.
; Inputs: None.
flush_code:
#ifdef DEBUG
	APRINTF(FlushMessage)
#endif
	; Empty recompiled code information struct
	ld hl,recompile_struct
	ld (recompile_struct_end),hl
	; Store first available block address to the first unused entry
	ld de,z80codebase+z80codesize
	ld (hl),de
	; Set the next memory access routine output below the Z80 stack
	ld hl,z80codebase+memroutine_end
	ld (z80codebase+memroutine_next),hl
	; Empty the recompiled code mapping cache
	ld hl,recompile_cache_end
	ld (recompile_cache),hl
	; Invalidate the memory routines
	ld hl,memroutineLUT
	push hl
	pop de
	inc de
	ld (hl),l
	ld bc,$01FF
	ldir
	ld hl,z80codebase+read_cycle_LUT
	ld (hl),e
	push hl
	pop de
	inc de
	ld c,READ_CYCLE_LUT_SIZE*3
	ldir
	; Set the cached interrupt return address to NULL
	ld (int_cached_return),bc
	ret
	
; Gets the 24-bit base pointer for a given Game Boy address.
; The base plus the address can be used to directly access GB memory.
;
; Inputs:  DE = GB address
; Outputs: HL = base pointer
; Destroys AF
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
	
; Gets the recompile struct entry for a given code pointer.
;
; Locating the code block is O(log N) in number of blocks.
;
; Inputs: DE = 16-bit Z80 code pointer
; Outputs: IX = BC = following struct entry
; Destroys F,BC,HL
lookup_code_block:
#ifdef 0
	push de
	 APRINTF(LookupGBMessage)
	pop de
#endif
	ld bc,recompile_struct
	ld ix,(recompile_struct_end)
lookup_code_block_loop:
	lea hl,ix
	or a
	sbc hl,bc
	ret z
	srl h
	rr l
	res 2,l
	add hl,bc
	push hl
	 ld hl,(hl)
	 scf
	 sbc.s hl,de
	 jr nc,lookup_code_block_lower
	 ex (sp),ix
	 lea bc,ix+8
lookup_code_block_lower:
	pop ix
	jr lookup_code_block_loop
	
; Gets the Game Boy opcode address from a recompiled code pointer, rounding up.
;
; Rounding up occurs if the code address happens to be inside a recompiled
; instruction - if you are guaranteed to be at the start of a recompiled
; instruction then you are guaranteed that no rounding occurs.
;
; Locating the code block is O(log N) in number of blocks.
; Locating the address within the block is O(N) in number of instructions.
;
; Inputs:  DE = 16-bit Z80 code pointer
; Outputs: NC if not found, or:
;          HL = 16-bit Z80 code pointer (rounded up)
;          DE = GB address
;          IX = Literal 24-bit pointer to GB address
;          A = NEGATIVE number of cycles until block end
; Destroys AF,BC
lookup_gb_code_address:
#ifdef 0
	push de
	 APRINTF(LookupGBMessage)
	pop de
#endif
	call lookup_code_block
	ld a,b
	or c
	jr z,$
	push iy
	 ld bc,opcodesizes
	 ld hl,(ix-8)
	 ld ix,(ix-6)
	 sbc.s hl,de
	 jr z,lookup_gb_found
	 lea iy,ix
	 add iy,iy
	 ld a,(ix+7)
	 ld iyl,a
	 jr nc,lookup_gb_found_loop
	 ld a,RAM_PREFIX_SIZE
	 jr lookup_gb_add
lookup_gb_found_loop:
	 ld c,(ix)
	 inc b
	 ld a,(bc)
	 dec b
	 neg
	 add a,iyl
	 ld iyl,a
	 ld a,(bc)
	 add a,ixl
	 jr nc,_
	 ld ixl,$FF
	 inc ix
_
	 ld ixl,a
	 dec b
	 ld a,(bc)
	 inc b
lookup_gb_add:
	 add a,l
	 ld l,a
	 jr nc,lookup_gb_found_loop
	 inc h
	 jr nz,lookup_gb_found_loop
lookup_gb_found:
	 add hl,de
	 push hl
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
	  push bc
	   push ix
	    push de
	     APRINTF(LookupGBFoundMessage)
	    pop de
	   pop ix
	  pop bc
#endif
	 pop hl
	 ld a,iyl
	pop iy
	scf
	ret.l
	
	
	; If a cached code lookup misses, resolve it and insert into the cache
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
	      APRINTF(CacheMissMessage)
	     pop hl
	    pop hl
	   pop de
#endif
	   call lookup_code_by_pointer
	   ld hl,(recompile_struct_end)
	   ld de,(recompile_cache)
	   ld bc,3+6
	   add hl,bc
	   sbc hl,de
	   jr c,_
	   ld de,recompile_cache_end
_
	  pop hl
	  or a
	  sbc hl,de
	  jr nc,_
	  cp a	;Set Z
_
	  ld b,h
	  ld c,l
	  ld hl,-6
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
	 inc hl
	 ld (hl),a
	pop hl
	ei
	ret.l
	
	
	 ; Fast return to the last interrupted code address
int_cache_hit:
int_cached_code = $+2
	 ld ix,0
	 xor a
int_cached_cycles = $+1
	 sub 0
	pop hl
	ei
	ret.l
	
	
	 ; When the binary search finishes, see if we found it
lookup_code_cached_found:
	 ; Past the end of the array means not found
	 ld a,b
	 sub recompile_cache_end>>8 & $FF
	 or c
	 jr z,lookup_code_cached_miss
	 ; Does the address match?
	 ld hl,(ix)
	 sbc hl,de
	 jr nz,lookup_code_cached_miss
	 ; We found it!
	 ld a,(ix+5)
	 ld ix,(ix+3)
	pop hl
	ei
	ret.l
	
	
; Pops an address from the Game Boy stack and does a cached lookup.
;
; Inputs:  HL = direct Game Boy stack pointer
; Outputs: IX = recompiled code address
;          HL = updated stack pointer
; Destroys AF,BC,DE,HL
pop_and_lookup_code_cached:
	inc.s de
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	
; Looks up a Game Boy address from the cached code mappings, and
; adds it to the cache upon miss.
;
; Note: Before touching the main cache, the last interrupt return address
;       is checked for a fast return. It is never added to the cache.
;
; Lookups are O(log N) in number of cache entries.
; Insertions are O(N) (with LDIR constant factor), plus code lookup overhead.
;
; Inputs:  DE = Game Boy address
; Outputs: IX = recompiled code address
;          A = number of cycles until block end
; Destroys AF,BC,DE
lookup_code_cached:
	push hl
	
	 ; Get the direct address in DE
	 call get_base_address
	 ld (base_address),hl
	 add hl,de
	 ex de,hl
	
	; Quickly check against the last interrupt return address
int_cached_return = $+1
	 ld hl,0
	 sbc hl,de
	 jr z,int_cache_hit
	
	 ; Binary search the cache for the pointer
	 ld bc,(recompile_cache)
	 ld ix,recompile_cache_end
lookup_code_cached_loop:
	 lea hl,ix
	 or a
	 sbc hl,bc
	 jr z,lookup_code_cached_found
	 srl h
	 rr l
	 bit 0,l
	 jr z,_
	 dec hl
	 dec hl
	 dec hl
_
	 add hl,bc
	 push hl
	  ld hl,(hl)
	  sbc hl,de
	  jr nc,lookup_code_cached_lower
	  ex (sp),ix
	  lea bc,ix+6
lookup_code_cached_lower:
	 pop ix
	 jr lookup_code_cached_loop
	
	
; Looks up a recompiled code pointer from a direct GB address,
; allowing a direct link within the currently executing RAM block.
; Normally, only the start of a RAM block is allowed for direct links.
; If executing from ROM or target is not the same block, proceed as normal.
;
; Inputs:  HL = direct 24-bit GB address to look up
;          IX = struct pointer of currently executing block
;          (base_address) = base address of pointer in HL
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
; Destroys AF,BC,DE,HL
lookup_code_link_internal:
	ex de,hl
	bit 7,(ix+4)
	jr z,lookup_code_by_pointer
	; We're running from RAM, check if destination is in running block
	ld hl,(ix+2)
	or a
	sbc hl,de
	jr z,internal_found_start
	jr nc,lookup_code_by_pointer
	ld bc,(ix+5)
	dec.s bc
	add hl,bc
	jr nc,lookup_code_by_pointer
internal_found_start:
	ld a,(ix+7)
	ld ix,(ix)
	lea ix,ix+RAM_PREFIX_SIZE
	ret z
	push iy
	 or a
	 sbc hl,bc
	 ld bc,opcodesizes
foundloop_internal:
	 ld iyl,a
	 add hl,de
	 ld c,(hl)
	 ; Add GB instruction size
	 ld a,(bc)
	 add a,l
	 ld l,a
	 jr nc,_
	 ld l,$FF
	 inc hl
_
	 ; Add recompiled instruction size
	 dec b
	 ld a,(bc)
	 inc b
	 add a,ixl
	 ld ixl,a
	 jr nc,_
	 inc ixh
_
	 ; Add cycles
	 inc b
	 ld a,(bc)
	 dec b
	 neg
	 add a,iyl
	 or a
	 sbc hl,de
	 jr c,foundloop_internal
	pop iy
	ret z
	jr lookup_code_by_pointer
	
; Looks up a recompiled code pointer from a GB address.
;
; Inputs:  DE = 16-bit GB address to look up
; Outputs: IX = recompiled code pointer
; Destroys AF,BC,DE,HL
lookup_code:
	call get_base_address
	ld (base_address),hl
	add hl,de
	ex de,hl
	
; Looks up a recompiled code pointer from a direct GB address.
;
; Inputs:  DE = direct 24-bit GB address to look up
;          (base_address) = base address of pointer in DE
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
; Destroys AF,BC,DE,HL
lookup_code_by_pointer:
	push iy
#ifdef 0
	 push de
	  ld hl,(base_address)
	  ex de,hl
	  or a
	  sbc hl,de
	  push hl
	   sbc hl,hl
	   ld a,(z80codebase+curr_rom_bank)
	   ld l,a
	   push hl
	    APRINTF(LookupMessage)
	   pop hl
	  pop hl
	 pop de
#endif
	 ld hl,(recompile_struct_end)
	 push hl
	  ld bc,(hl)
	  ld hl,(z80codebase+memroutine_next)
	  xor a
	  sbc hl,bc
	  jr c,flush_and_recompile_pop
lookuploop_restore:
	 pop ix
lookuploop:
	 ld a,ixh
	 or ixl
	 jr z,recompile
	 lea ix,ix-8
	 ld hl,(ix+2)
	 sbc hl,de
	 jr z,lookupfoundstart
	 jr nc,lookuploop
	 ld bc,(ix+5)
	 dec.s bc
	 add hl,bc
	 jr nc,lookuploop
	 ; Don't allow jumping to the middle of a RAM block
	 ld a,(ix+4)
	 rla
	 jr c,lookuploop
	 push ix
	  sbc hl,bc
	  ld bc,opcodesizes
	  ld a,(ix+7)
	  ld ix,(ix)
foundloop:
	  ld iyl,a
	  add hl,de
	  ld c,(hl)
	  ; Add GB instruction size
	  ld a,(bc)
	  add a,l
	  jr nc,_
	  ld l,$FF
	  inc hl
_
	  ld l,a
	  ; Add recompiled instruction size
	  dec b
	  ld a,(bc)
	  inc b
	  add a,ixl
	  ld ixl,a
	  jr nc,_
	  inc ixh
_
	  ; Add cycles
	  inc b
	  ld a,(bc)
	  dec b
	  neg
	  add a,iyl
	  or a
	  sbc hl,de
	  jr c,foundloop
	  jr nz,lookuploop_restore
	 pop hl
	pop iy
	ret
	
lookupfoundstart:
	 ld a,(ix+7)
	 ld ix,(ix)
	pop iy
	ret
	
flush_and_recompile_pop:
	 pop hl
	pop iy
flush_and_recompile:
	push iy
	 push de
	  call flush_code
	 pop de
	
	
; Recompiles a new code block starting from a direct GB address.
;
; Inputs:  DE = direct 24-bit GB address to recompile
;          A = 0
;          IY saved on stack
;          (base_address) = base address of pointer in DE
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
; Destroys AF,BC,DE,HL
recompile:
	 ld ix,(recompile_struct_end)
	 lea hl,ix+8
	 ld (recompile_struct_end),hl
	 
	 ; Check for collision with cache
	 ld hl,(recompile_cache)
	 lea bc,ix+16
	 or a
	 sbc hl,bc
	 call c,flush_cache
	 ld hl,(ix)
	 ld (ix+2),de
	 bit 7,(ix+4)
	 jr nz,recompile_ram
	
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
	      or a
	      sbc hl,hl
	      ld a,(z80codebase+curr_rom_bank)
	      ld l,a
	      push hl
	       APRINTF(RecompileMessage)
	      pop hl
	     pop hl
	    pop de
	   pop hl
	  pop hl
#endif
	 
	 call generate_opcodes
	
recompile_end_common:
	pop iy
	ld (ix+8),de
	ld hl,(z80codebase+memroutine_next)
	sbc hl,de
	ld ix,(ix)
	ret nc
	ld ix,(recompile_struct_end)
	ld hl,(ix-6)
	ld de,(base_address)
	xor a
	sbc hl,de
	ld.sis (flush_address),hl
	ld ix,flush_handler
flush_cache:
	ld hl,recompile_cache_end
	ld (recompile_cache),hl
	ret
	
recompile_ram:
#ifdef DEBUG
	  push ix
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
	       APRINTF(RecompileRamMessage)
	      pop hl
	     pop de
	    pop hl
	   pop hl
	  pop ix
#endif
	 
	 ld (hl),$CD
	 inc hl
	 ld (hl),coherency_handler & $FF
	 inc hl
	 ld (hl),coherency_handler >> 8
	 inc hl
	 ld (hl),ix
	 inc hl
	 inc hl
	 
	 call generate_opcodes
	
	 ; Copy the GB opcodes for coherency
	 push bc
	  ex (sp),hl
	  ; Add in padding to avoid flushes
	  ex de,hl
ram_block_padding = $+1
	  ld bc,0
	  add hl,bc
	  ex de,hl
	 pop bc
	 ldir
	 
#ifdef DEBUG
	 jp recompile_end_common
#else
	 jr recompile_end_common
#endif
	
; Recompiles an existing RAM code block in-place.
;
; If the new recompiled code overflows the allotted space, all code is flushed
; and the ram_block_padding variable is increased by the amount of overflow.
;
; Inputs:  IX = recompile struct entry for the block
; Outputs: None
; Destroys AF,BC,DE,HL
rerecompile:
#ifdef 0
	push ix
	 ld hl,(ix)
	 inc hl
	 dec.s hl
	 push hl
	  APRINTF(CoherencyFailedMessage)
	 pop hl
	pop ix
	or a
#endif
	
	ld hl,(ix+2)
	ld de,vram_base
	xor a
	sbc hl,de
	ld bc,$FE00
	sbc hl,bc
	jr c,rerecompile_found_base
	ld hl,(ix+2)
	ld de,hram_base
	sbc hl,de
	inc b
	dec c
	sbc hl,bc
	jr c,rerecompile_found_base
	ld de,(cram_bank_base)
rerecompile_found_base:
	ld (base_address),de
	
	ld hl,(ix)
	inc.s hl
	ld de,z80codebase + RAM_PREFIX_SIZE - 1
	add hl,de
	ld de,(ix+2)
	push iy
	 push ix
	  call generate_opcodes
	 pop ix
	pop iy
	
	push hl
	pop bc
	
	; Get the address to copy the opcodes to
	ld a,(ix+10)
	ld (ix+10),z80codebase >> 16
	ld hl,(ix+8)
	ld (ix+10),a
	sbc hl,bc	; Carry is reset
	ex de,hl
	
	; Make sure there is no overlap
	scf
	sbc hl,de
	jr nc,coherency_flush
	
	; Copy the new opcodes, from first to last
	
	ld hl,(ix+2)
	ldir
	ei
	jp.sis coherency_return
	
coherency_flush:
	; Get the number of bytes in the overflow
	inc hl
	; Add that many bytes to the RAM block padding to avoid future issues
	ld de,(ram_block_padding)
	add hl,de
	ld (ram_block_padding),hl
#ifdef DEBUG
	push ix
	 push hl
	  APRINTF(PaddingUpdateMessage)
	 pop hl
	pop ix
#endif
	; Start at the very beginning
	ld de,(ix+2)
	call flush_and_recompile
	exx
	ld bc,(CALL_STACK_DEPTH+1)*256
	exx
	pop.s bc
	pop.s de
	pop.s hl
	ld.sis sp,myz80stack-2
	ld sp,myADLstack
	ex af,af'
	ei
	jp.s (ix)
	
; Inputs:  IX = struct entry
;          DE = GB opcodes start
;          HL = block start
; Outputs: IX = struct entry
;          BC = GB opcodes start
;          DE = block end
;          HL = GB opcodes size
;          A = total block cycle count
;          Carry flag reset
; Destroys None
generate_opcodes:
	; Cycle count while recompiling is hl-iy. Start at zero.
	ld iyl,e
	ex de,hl
	ld bc,opgentable
	ld a,ixl
	ld (opgen_emit_jump_smc_1),a
	ld a,ixh
	ld (opgen_emit_jump_smc_2),a
	push ix
	 ld ix,opgenroutines
	 call opgen_next_fast
	 call m,opgen_cycle_overflow
	pop ix
	inc hl
	inc de
	; Get the cycle count
	ld a,l
	sub iyl
	; Get the size of the GB opcodes and save it
	ld bc,(ix+2)
	or a
	sbc hl,bc
	ld (ix+5),hl
	ld (ix+7),a
	; Check cycle count and reset carry
	or a
	ret p
	ld a,$7F
	ld (ix+7),a
	ret
	
#ifdef 0
print_recompiled_code:
	push ix
	 push hl
	  ld hl,(ix-8)
_
	  push de
	   push hl
	    ld.s a,(hl)
	    or a
	    sbc hl,hl
	    ld l,a
	    push hl
	     APRINTF(ByteFormat)
	    pop hl
	   pop hl
	  pop de
	  inc hl
	  or a
	  sbc.s hl,de
	  add hl,de
	  jr nz,-_
	  ACALL(PutNewLine)
	 pop hl
	pop ix
	ret
#endif
	
; A table of recompiled opcode sizes. Does not apply to block-ending opcodes.
	.block (-$)&255
opcoderecsizes:
	.db 0,3,3,1,1,1,2,1
	.db 5,1,3,1,1,1,2,1
	.db 0,3,3,1,1,1,2,1
	.db 0,1,3,1,1,1,2,1
	.db 10,3,3,1,1,1,2,1
	.db 10,1,3,1,1,1,2,1
	.db 10,5,3,4,3,3,4,1
	.db 10,3,3,4,1,1,2,1
	
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
	
	.db 6,2,10,0,9,2,2,7
	.db 6,0,10,2,9,7,2,7
	.db 6,2,10,0,9,2,2,7
	.db 6,0,10,0,9,0,2,7
	.db 3,2,3,0,0,2,2,7
	.db 4,0,5,0,0,0,2,7
	.db 3,2,3,3,0,2,2,7
	.db 4,3,5,3,0,0,2,7
	
; A table of Game Boy opcode sizes.
opcodesizes:
	.db 1,3,1,1,1,1,2,1
	.db 3,1,1,1,1,1,2,1
	.db 1,3,1,1,1,1,2,1
	.db 2,1,1,1,1,1,2,1
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
	
	.db 1,1,3,3,3,1,2,1
	.db 1,1,3,2,3,3,2,1
	.db 1,1,3,1,3,1,2,1
	.db 1,1,3,1,3,1,2,1
	.db 2,1,1,1,1,1,2,1
	.db 2,1,3,1,1,1,2,1
	.db 2,1,1,1,1,1,2,1
	.db 2,1,3,1,1,1,2,1
	
; A table of Game Boy opcode cycles. Block-ending opcodes are set to 0.
; Conditional branches are assumed not taken.
opcodecycles:
	.db 1,3,2,2,1,1,2,1
	.db 5,2,2,2,1,1,2,1
	.db 1,3,2,2,1,1,2,1
	.db 0,2,2,2,1,1,2,1
	.db 2,3,2,2,1,1,2,1
	.db 2,2,2,2,1,1,2,1
	.db 2,3,2,2,3,3,3,1
	.db 2,2,2,2,1,1,2,1
	
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 2,2,2,2,2,2,1,2
	.db 1,1,1,1,1,1,2,1
	
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	.db 1,1,1,1,1,1,2,1
	
	.db 2,3,3,0,3,4,2,1
	.db 2,0,3,2,3,3,2,1
	.db 2,3,3,0,3,4,2,1
	.db 2,0,3,0,3,0,2,1
	.db 3,3,2,0,0,4,2,1
	.db 4,0,4,0,0,0,2,1
	.db 3,3,2,1,0,4,2,1
	.db 3,2,4,1,0,0,2,1
	
; A table indexing opcode generation routines.
; All entry points live in a 256-byte space.
opgentable:
;00
	.db opgen0byte - opgenroutines
	.db opgen3byte_low - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgen1byte - opgenroutines
;08
	.db opgen08 - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgen1byte - opgenroutines
;10
	.db opgen0byte - opgenroutines
	.db opgen3byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgen1byte - opgenroutines
;18
	.db opgenJR - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgen1byte - opgenroutines
;20
	.db opgenJRcond - opgenroutines
	.db opgen3byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgen1byte - opgenroutines
;28
	.db opgenJRcond - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgen1byte - opgenroutines
;30
	.db opgenJRcond - opgenroutines
	.db opgen31 - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen33 - opgenroutines
	.db opgen34 - opgenroutines
	.db opgen35 - opgenroutines
	.db opgen36 - opgenroutines
	.db opgen1byte - opgenroutines
;38
	.db opgenJRcond - opgenroutines
	.db opgen39 - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen3B - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgen1byte - opgenroutines
;40
	.db opgen0byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;48
	.db opgen1byte - opgenroutines
	.db opgen0byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;50
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen0byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;58
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen0byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;60
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen0byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;68
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen0byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;70
	.db opgenMEM - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen76 - opgenroutines
	.db opgenMEM - opgenroutines
;78
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen0byte - opgenroutines
;80
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;88
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;90
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;98
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;A0
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;A8
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;B0
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;B8
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;C0
	.db opgenRETcond - opgenroutines
	.db opgenPOP - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenJP - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;C8
	.db opgenRETcond - opgenroutines
	.db opgenRET - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenCB - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenCALL - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;D0
	.db opgenRETcond - opgenroutines
	.db opgenPOP - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;D8
	.db opgenRETcond - opgenroutines
	.db opgenRETI - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;E0
	.db opgenE0 - opgenroutines
	.db opgenPOP - opgenroutines
	.db opgenE2 - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;E8
	.db opgenE8 - opgenroutines
	.db opgenE9 - opgenroutines
	.db opgenEA - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;F0
	.db opgenF0 - opgenroutines
	.db opgenPOP - opgenroutines
	.db opgenF2 - opgenroutines
	.db opgenF3 - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;F8
	.db opgenF8 - opgenroutines
	.db opgenF9 - opgenroutines
	.db opgenFA - opgenroutines
	.db opgenEI - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenINVALID - opgenroutines
	
opgen_cycle_overflow:
	ld a,(base_address)
	cpl
	add a,l
	ld c,a
	ld a,(base_address+1)
	cpl
	adc a,h
	ld b,a
	inc bc
	ld a,l
	sub iyl
	dec hl
	jp p,opgen_emit_jump
	ex de,hl
	ld (hl),$ED	;LEA IY,IY-128
	inc hl
	ld (hl),$33
	inc hl
	ld (hl),$80
	sub (hl)
	inc hl
	jr opgen_emit_jump_swapped
	
_opgenCALLcond:
	ld b,a
	add a,6
	ret m
	ld a,c
	xor $C4 ^ $28
	ld (de),a
	inc de
	ld a,7
	ld (de),a
	inc de
	ld a,b
_opgenCALL:
	add a,6
	ret m
	ld.sis bc,decode_call
	inc hl
	inc hl
opgen_emit_call:
	ex de,hl
	inc de
	ld (hl),$CD
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	inc hl
	; Store cycle count for call not taken, used by cached RET
	sub 3
	ld b,a
	scf
	ld a,(base_address)
	cpl
	adc a,e
	ld (hl),a
	inc hl
	ld a,(base_address+1)
	cpl
	adc a,d
	ld (hl),a
	inc hl
	ld (hl),b
	inc hl
	inc hl
	ex de,hl
	jp opgen_next
	
_opgenJR:
	add a,3
	ret m
	dec iy
opgen_emit_JR:
	push af
	 inc hl
	 ld a,(base_address)
	 cpl
	 add a,l
	 ld c,a
	 ld a,(base_address+1)
	 cpl
	 adc a,h
	 ld b,a
	 inc bc
	 inc bc
	 push hl
	  ld a,(hl)
	  rlca
	  sbc hl,hl
	  rrca
	  ld l,a
	  add hl,bc
	  ex (sp),hl
	 pop bc
	pop af
	jr opgen_emit_jump

_opgenJP:
	add a,4
	ret m
	dec iy
opgen_emit_JP:
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
opgen_emit_jump:
	ex de,hl
opgen_emit_jump_swapped:
	ld (hl),$CD
	inc hl
	ld (hl),decode_jump & $FF
	inc hl
	ld (hl),decode_jump >> 8
	inc hl
	; Save block struct pointer
opgen_emit_jump_smc_1 = $+1
	ld (hl),0
	inc hl
opgen_emit_jump_smc_2 = $+1
	ld (hl),0
	inc hl
	; Save target address
	ld (hl),c
	inc hl
	ld (hl),b
	inc hl
	; Save cycle count
	ld (hl),a
	ex de,hl
	ret
	
_opgenRET:
	add a,4
	ret m
	; Update total cycle count
	lea iy,iy-3
	ld a,c
	ld (de),a
	ret
	
_opgenRETcond:
	add a,5
	ret m
	dec iy
	ld b,a
	ex de,hl
	ld a,c
	xor $C0 ^ $28
	ld (hl),a
	inc hl
	ld (hl),4
	inc hl
	ld (hl),$CD
	inc hl
	ld (hl),decode_ret_cond & $FF
	inc hl
	ld (hl),decode_ret_cond >> 8
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgenroutinecall2byte_5cc:
	lea iy,iy-2
opgenroutinecall2byte_3cc:
	inc hl
	ld a,$CD
	ld (de),a
	inc de
	ex (sp),hl
	ldi
	ldi
	pop hl
	jp opgen2byte
	
opgenroutinecall1byte_4cc:
	dec iy
opgenroutinecall1byte_3cc:
	dec iy
	inc hl
	ld a,$CD
	ld (de),a
	inc de
	ex (sp),hl
	ldi
	ldi
	pop hl
	jp opgen1byte
	
opgenroutinecall_3cc:
	dec iy
opgenroutinecall_2cc:
	dec iy
opgenroutinecall_1cc:
	inc hl
	ld a,$CD
	ld (de),a
	inc de
	ex (sp),hl
	ldi
	ldi
	pop hl
	jp opgen_next_fast
	
opgenCONSTwrite:
	dec iy
	inc.s bc
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
	dec iy
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
	sub (TIMA & $FF)+1
	jr nz,_
	ld bc,writeTIMAhandler
	jr opgenHMEMwriteroutine
_
	dec a
	jr nz,_
	ld bc,writeTMAhandler
	jr opgenHMEMwriteroutine
_
	dec a
	jr nz,_
	ld bc,writeTAChandler
	jr opgenHMEMwriteroutine
_
	sub IF - TAC
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
	dec iy
	inc.s bc
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
	dec iy
	inc hl
	ld c,(hl)
	ld b,$FF
_
	ex de,hl
	ld a,c
	rlca
	jr c,opgenHRAMread
	cp DIV*2 & $FF
	jr nz,_
	ld bc,readDIVhandler
	jr opgenHMEMreadroutine
_
	cp TIMA*2 & $FF
	jr nz,_
	ld bc,readTIMAhandler
	jr opgenHMEMreadroutine
_
	cp LY*2 & $FF
	jr nz,_
	ld bc,readLYhandler
	jr opgenHMEMreadroutine
_
	cp STAT*2 & $FF
	jr nz,_
	ld bc,readSTAThandler
	jr opgenHMEMreadroutine
_
	cp P1*2 & $FF
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