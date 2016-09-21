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
	ld hl,1
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
	ld hl,z80codebase+myz80stack-256
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
; Destroys AF,BC
lookup_gb_code_address:
#ifdef 0
	push de
	 APRINTF(LookupGBMessage)
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
	push ix
	 add ix,ix
	pop ix
	jr nc,lookup_gb_found_loop
	ld a,RAM_PREFIX_SIZE
	jr lookup_gb_add
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
lookup_gb_add:
	add a,l
	ld l,a
	jr nc,lookup_gb_found_loop
	inc h
	jr nz,lookup_gb_found_loop
	ld a,c
	and %11100111
	sub $C4
	jr nz,lookup_gb_found
	cp l
	jr z,lookup_gb_found
	; In the middle of a conditional call, don't round up
	lea ix,ix-3
	ld l,a
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
	
	
	; Fast return to the last interrupted code address
int_cache_hit:
int_cached_code = $+2
	ld ix,0
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
	ld ix,(ix+3)
	ret.l
	
	
; Pops an address from the Game Boy stack and does a cached lookup.
;
; Inputs:  IY = direct Game Boy stack pointer
; Outputs: IX = recompiled code address
;          IY = updated stack pointer
; Destroys AF,BC,DE,HL
pop_and_lookup_code_cached:
	ld de,(iy)
	inc de
	dec.s de
	lea iy,iy+2
	
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
; Destroys AF,BC,DE,HL
lookup_code_cached:
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
	
	
; Looks up a recompiled code pointer from a direct GB address,
; allowing a direct link within the currently executing RAM block.
; Normally, only the start of a RAM block is allowed for direct links.
; If executing from ROM or target is not the same block, proceed as normal.
;
; Inputs:  HL = direct 24-bit GB address to look up
;          A = upper byte of currently executing direct GB address
;          (base_address) = base address of pointer in HL
; Outputs: IX = recompiled code pointer
; Destroys AF,BC,DE,HL
lookup_code_link_internal:
	ex de,hl
	rla
	jr nc,lookup_code_by_pointer
	; We're running from RAM, check if destination is in running block
current_ram_block = $+2
	ld ix,0
	ld hl,(ix+5)
	or a
	sbc hl,de
	jr c,lookup_code_by_pointer
	ld hl,(ix+2)
	sbc hl,de
	ld ix,(ix)
	lea ix,ix+RAM_PREFIX_SIZE
	ret z
	jr nc,lookup_code_by_pointer
	ld bc,opcodesizes
foundloop_internal:
	add hl,de
	ld c,(hl)
	ld a,(bc)
	or a
	jr z,lookup_code_by_pointer
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
	jr nz,foundloop_internal
	ret
	
	
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
; Destroys AF,BC,DE,HL
lookup_code_by_pointer:
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
	 or a
	 sbc hl,bc
	 jr c,flush_and_recompile_pop
	 ld bc,opcodesizes
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
	; Don't allow jumping to the middle of a RAM block
	bit 7,(ix+4)
	jr nz,lookuploop
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
	
flush_and_recompile_pop:
	pop hl
flush_and_recompile:
	push de
	 call flush_code
	pop de
	
	
; Recompiles a new code block starting from a direct GB address.
;
; Inputs:  DE = direct 24-bit GB address to recompile
;          (base_address) = base address of pointer in DE
; Outputs: IX = recompiled code pointer
; Destroys AF,BC,DE,HL
recompile:
	ld ix,(recompile_struct_end)
	lea hl,ix+8
	ld (recompile_struct_end),hl
	push hl
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
	     or a,a
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
	 
	 ex de,hl
	 ld ix,opgenroutines
	 ld bc,opgentable
	 call opgen_next_fast
	
	pop ix
	inc de
	ld (ix-3),hl
	or a
	
recompile_end_common:
	ld (ix),de
	ld hl,(z80codebase+memroutine_next)
	sbc hl,de
	ld ix,(ix-8)
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
	 
	 ex de,hl
	 ld ix,opgenroutines
	 ld bc,opgentable
	 call opgen_next_fast
	pop ix
	ld (ix-3),hl
	
	; Copy the GB opcodes for coherency
	ld bc,(ix-6)
	or a
	sbc hl,bc
	push hl
	pop bc
	inc bc
	
	; Add in padding to avoid flushes (must be at least 1)
ram_block_padding = $+1
	ld hl,1
	add hl,de
	ex de,hl
	
	ld hl,(ix-3)
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
	ex de,hl
	ld hl,(ix+2)
	
	push ix
	 ld ix,opgenroutines
	 ld bc,opgentable
	 call opgen_next_fast
	pop ix
	ld (ix+5),hl
	
	; Get the size of the GB opcodes
	ld bc,(ix+2)
	or a
	sbc hl,bc
	push hl
	pop bc
	inc bc
	
	; Add this size to the end of the generated code
	ex de,hl
	add hl,bc	; Resets carry
	
	; Get the address to copy the opcodes to
	ld a,(ix+10)
	ld (ix+10),z80codebase >> 16
	ld de,(ix+8)
	ld (ix+10),a
	
	; Make sure there is no overlap
	sbc hl,de	; Carry is reset
	jr nc,coherency_flush
	
	; Copy the new opcodes, from last to first
	ld hl,(ix+5)
	dec de
	lddr
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
	ld.sis sp,myz80stack-2
	ld bc,(CALL_STACK_DEPTH+1)*256
	push.s ix
	jp.sis coherency_return_popped
	
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
	
; A table of Game Boy opcode sizes. 0 means it ends the block.
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
	.db opgen1byte - opgenroutines
	.db opgenPOP - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenJP - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;C8
	.db opgen1byte - opgenroutines
	.db opgenRET - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenCB - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenCALL - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;D0
	.db opgen1byte - opgenroutines
	.db opgenPOP - opgenroutines
	.db opgenJPcond - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenCALLcond - opgenroutines
	.db opgenPUSH - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenRST - opgenroutines
;D8
	.db opgen1byte - opgenroutines
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
	
	
_opgenCALLcond:
	ld a,c
	xor $C4 ^ $28
	ld (de),a
	inc de
	ld a,7
	ld (de),a
	inc de
_opgenCALL:
	ld c,decode_call & $FF
	ld a,decode_call >> 8
	inc hl
	inc hl
opgen_emit_call:
	inc hl
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),c
	inc hl
	ld (hl),a
	inc hl
	ex de,hl
	scf
	ld a,(base_address)
	cpl
	adc a,l
	ld (de),a
	inc de
	ld a,(base_address+1)
	cpl
	adc a,h
	ld (de),a
	inc de
	ld a,l
	add a,iyh
	ld (de),a
	inc de
	inc de
	jp opgen_next_fast
	

_opgenJR:
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
	jr opgen_emit_jump

_opgenJP:
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
opgen_emit_jump:
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),decode_jump & $FF
	inc hl
	ld (hl),decode_jump >> 8
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	inc hl
	ex de,hl
	ld a,l
	add a,iyl
	ld (de),a
	inc de
	ret
	
opgenroutinecall2byte_5cc:
	lea iy,iy+2
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
	inc iy
opgenroutinecall1byte_3cc:
	inc iy
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
	inc iy
opgenroutinecall_2cc:
	inc iy
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