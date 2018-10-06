#define RST_BITS $C7+r_bits
#define RST_MEM $C7+r_mem
#define RST_CYCLE_CHECK $C7+r_cycle_check
#define RST_PUSH $C7+r_push
#define RST_POP $C7+r_pop
#define RST_CALL $C7+r_call
#define RST_EVENT $C7+r_event
#define RST_INVALID_OPCODE $C7+r_invalid_opcode

#define RAM_PREFIX_SIZE 5
#define MAX_CYCLES_PER_BLOCK 64
#define MAX_CACHE_FLUSHES_ALLOWED 2

cache_flushes_allowed:
	.db 0

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
; Destroys: AF, BC, HL, IX
flush_code:
	push de
#ifdef DEBUG
	 APRINTF(FlushMessage)
#endif
	 ; Empty recompiled code information struct
	 ld hl,recompile_struct
	 ld (hl),hl
	 ld l,8
	 ld (recompile_struct_end),hl
	 ; Store first available block address to the first unused entry
	 ld de,z80codebase+jit_start
	 ld (hl),de
	 ; Set the next memory access routine output below the Z80 stack
	 ld hl,z80codebase+memroutine_end
	 ld (z80codebase+memroutine_next),hl
	 ld de,ERROR_CATCHER
	 ld (hl),de
	 ; Empty the recompiled code mapping cache
	 ld hl,recompile_cache_end
	 ld (recompile_cache),hl
	 ; Fill unused memory with DI to catch bad execution
	 MEMSET_FAST(z80codebase+jit_start, memroutine_end - jit_start, $F3)
	 ; Invalidate the memory routines and recompile index LUT
	 MEMSET_FAST(memroutineLUT, $0400, 0)
	 ; Invalidate both the read and write cycle LUTs
	 ld hl,z80codebase+read_cycle_LUT
	 ld (hl),e
	 push hl
	 pop de
	 inc de
	 ld c,(MEM_CYCLE_LUT_SIZE + 1) * 2 + INT_RETURN_STACK_SIZE
	 ldir
	 inc (hl)
	 ld l,(z80codebase+int_return_stack) & $FF
	 ld (z80codebase+int_return_sp),hl
	 ; Set the cached interrupt return address to NULL
	 ld (int_cached_return),bc
	 ; Set the cached lookup address to NULL
	 ld (lookup_gb_cache_jit_address),bc
	 ; Reset the event address
	 ld hl,event_value
	 ld.sis (event_address),hl
	 ; Reset the interrupt target caches
	 ld ix,z80codebase+dispatch_joypad
	 ld hl,(decode_intcache << 8) | $CD	;CALL decode_intcache
	 ld a,$60
	 ld b,5
_
	 ld (ix+3),a
	 ld (ix+4),hl
	 lea ix,ix+9
	 sub 8
	 djnz -_
	 ld a,MAX_CACHE_FLUSHES_ALLOWED
	 ld (cache_flushes_allowed),a
	pop de
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
	ld hl,wram_base
	add a,a
	ret c
	ld hl,vram_base
	add a,a
	ret nc
	ld hl,(cram_bank_base)
	ret
_
	ld hl,hram_base
	ret
	
; Outputs: IX = 24-bit pointer
;          HL = GB address
; Destroys: F,DE
get_event_gb_address:
event_gb_address = $+2
	ld ix,0
	
; Gets the 16-bit Game Boy address from a 24-bit literal pointer.
;
; Inputs:  IX = 24-bit pointer
; Outputs: HL = GB address
; Destroys F,DE
get_gb_address:
	or a
	lea hl,ix
	ld de,(rom_start)
	sbc hl,de
	ld de,$4000
	sbc hl,de
	jr c,_
	lea hl,ix
	ld de,(rom_bank_base)
	sbc hl,de
	ld de,$8000
	sbc hl,de
	jr c,_
	lea hl,ix
	ld de,vram_base
	sbc hl,de
	ld de,$A000
	sbc hl,de
	jr c,_
	lea hl,ix
	ld de,wram_base
	sbc hl,de
	ld de,$E000
	sbc hl,de
	jr c,_
	lea hl,ix
	ld de,(cram_bank_base)
	sbc hl,de
	ld de,$C000
	sbc hl,de
	jr c,_
	lea hl,ix
	ld de,-hram_base
_
	add hl,de
	ret.l
	
	
; Gets the recompile struct entry for a given code pointer.
;
; Inputs: DE = 16-bit Z80 code pointer
; Outputs: IX = struct entry or $xx0000 if not found
; Destroys AF,BC,HL
lookup_code_block:
#ifdef 0
	push de
	 APRINTF(LookupGBMessage)
	pop de
#endif
	ld ix,recompile_struct
	ld hl,recompile_index_LUT
	ld l,d
	ld a,(hl)
	ld ixl,a
	inc h
	ld a,(hl)
	ld ixh,a
	ld a,e
	cpl
	ld c,a
	ld a,d
	cpl
	ld b,a
lookup_code_block_loop:
	ld hl,(ix)
	add.s hl,bc
	ret nc
	ld hl,(ix-8)
	add.s hl,bc
	jr nc,_
	ld hl,(ix-16)
	add.s hl,bc
	jr nc,++_
	ld hl,(ix-24)
	add.s hl,bc
	lea ix,ix-32
	jr c,lookup_code_block_loop
	lea ix,ix+8
	ret
_
	lea ix,ix-8
	ret
_
	lea ix,ix-16
	ret
	
Z80InvalidOpcode_helper:
	pop.s af
	exx
	pop.s de
#ifdef DEBUG
	; Open debugger on CEmu
	ld (exitReason),a
	ld a,2
	ld ($FFFFFF),a
	push de
	 APRINTF(InvalidOpcodeErrorMessage)
	pop de
#endif
	call.il lookup_gb_code_address
	call.il get_gb_address
	ld (errorArg),hl
	ld a,(z80codebase+curr_rom_bank)
	ld (errorArg+2),a
	ld a,(ERROR_INVALID_OPCODE << 2) + 1
	jr _
runtime_error:
#ifdef DEBUG
	; Open debugger on CEmu
	ld (exitReason),a
	ld a,2
	ld ($FFFFFF),a
	APRINTF(RuntimeErrorMessage)
#endif
	ld a,(ERROR_RUNTIME << 2) + 1
_
	ld (exitReason),a
	; Temporarily prevent auto state saving because state is unrecoverable
	ld hl,AutoSaveState
	set 1,(hl)
	AJUMP(ExitEmulationWithoutState)
	
; Gets the Game Boy opcode address from a recompiled code pointer.
;
; The pointer must point either to the start of a recompiled instruction
; or directly following the end of the last recompiled instruction in a block.
;
; Locating the code block is O(log N) in number of blocks.
; Locating the address within the block is O(N) in number of instructions.
;
; Inputs:  DE = 16-bit Z80 code pointer
; Outputs: IX = Literal 24-bit pointer to GB opcode
;          DE = 16-bit Z80 code pointer
;          A = NEGATIVE number of cycles until block end, or 0 if at start of RAM block
; Destroys F,BC,HL
lookup_gb_code_address:
	or a
lookup_gb_cache_jit_address = $+1
	ld hl,0
	sbc hl,de
	jr z,lookup_gb_cache_found
	ld (lookup_gb_cache_jit_address),de
#ifdef 0
	push de
	 APRINTF(LookupGBMessage)
	pop de
#endif
	call lookup_code_block
	ld a,ixh
	or ixl
	jr z,runtime_error
	ld hl,(ix)
	xor a
	sub (ix+7)	; Should set carry flag
	ASSERT_C
	ld ix,(ix+2)
	inc.s hl
	sbc hl,de
	jr z,lookup_gb_found_start
	push hl
	 ex (sp),iy
	 lea hl,ix
	 add hl,hl
	 ld hl,opcodesizes
	 ld bc,RAM_PREFIX_SIZE
	 jr c,lookup_gb_add
lookup_gb_found_loop:
	 ld l,(ix)
	 inc h
	 add a,(hl)
	 dec h
	 ld c,(hl)
	 add ix,bc
	 dec h
	 ld c,(hl)
	 inc h
lookup_gb_add:
	 add iy,bc
	 jr nc,lookup_gb_found_loop
	 dec iy
	 add iy,iy
#ifdef DEBUG
	 jp nc,runtime_error
#else
	 jr nc,runtime_error
#endif
	pop iy
#ifdef 0
	push af
	 push ix
	  push de
	   APRINTF(LookupGBFoundMessage)
	  pop de
	 pop ix
	pop af
#endif
	ld (lookup_gb_cache_gb_address),ix
	ld (lookup_gb_cache_cycles),a
	ret.l
	
lookup_gb_cache_found:
lookup_gb_cache_gb_address = $+2
	ld ix,0
lookup_gb_cache_cycles = $+1
	ld a,0
	ret.l
	
lookup_gb_found_start:
	lea hl,ix
	ld (lookup_gb_cache_gb_address),hl
	add hl,hl
_
	ld (lookup_gb_cache_cycles),a
	ret.l nc
	xor a
	jr -_
	
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
	   ; Check if the cache needs to be flushed
	   ld hl,(recompile_struct_end)
	   ld de,(recompile_cache)
	   ld bc,3+6
	   add hl,bc
	   sbc hl,de
	   jr c,_
	   ; Allow a certain number of cache flushes before flushing the entire JIT space
	   ld hl,cache_flushes_allowed
	   dec (hl)
	   ; Get the GB code pointer in HL
	   pop de
	   pop hl
	   push hl
	   push de
	   ; If no cache flushes remaining, prepare a JIT flush (replacing IX and A)
	   call z,prepare_flush_by_pointer
	   ; Flush the cache
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
#ifdef DEBUG
	 jp nz,lookup_code_cached_miss
#else
	 jr nz,lookup_code_cached_miss
#endif
	 ; We found it!
	 ld a,(ix+5)
	 ld ix,(ix+3)
	pop hl
	ret.l
	

	
	 ; Fast return to the last interrupted code address
int_cache_hit:
	 ld hl,(z80codebase+int_return_sp)
	 dec hl
	 dec hl
	 ld ix,(hl)
	 dec hl
	 dec hl
	 dec hl
	 dec hl
	 xor a
	 sub (hl)
	 ld (z80codebase+int_return_sp),hl
	 inc hl
	 ld hl,(hl)
	 ld (int_cached_return),hl
	pop hl
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
	
lookup_code_cached_by_pointer_pushed:
	; Quickly check against the last interrupt return address
int_cached_return = $+1
	 ld hl,0
	 sbc hl,de
	 jr z,int_cache_hit
	
	 ; Binary search the cache for the pointer
recompile_cache = $+1
	 ld bc,0
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
	 
lookup_code_cached_by_pointer:
	call.il _
	ret
_
	push hl
	 jr lookup_code_cached_by_pointer_pushed
	
lookup_code_link_internal:
	call get_base_address
lookup_code_link_internal_with_base:
	ld (base_address),hl
	add hl,de
	
; Looks up a recompiled code pointer from a direct GB address,
; allowing a direct link within the currently executing RAM block.
; Normally, only the start of a RAM block is allowed for direct links.
; If executing from ROM or target is not the same block, proceed as normal.
;
; Inputs:  HL = direct 24-bit GB address to look up
;          IX = struct pointer of the currently executing block
;          (base_address) = base address of pointer in HL
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
; Destroys AF,BC,DE,HL
lookup_code_link_internal_by_pointer:
	ex de,hl
	bit 7,(ix+4)
	jr z,lookup_code_by_pointer
	; We're running from RAM, check if destination is in running block
	ld hl,(ix+2)
	or a
	sbc hl,de
	jr z,internal_found_start
	jr nc,lookup_code_cached_by_pointer
	ld bc,(ix+5)
	dec.s bc
	add hl,bc
	jr nc,lookup_code_cached_by_pointer
internal_found_start:
	ld a,(ix+7)
	ld ix,(ix)
	lea ix,ix+RAM_PREFIX_SIZE
	ret z
	or a
	sbc hl,bc
	push hl
	 ex (sp),iy
	 add hl,de
	 push de
	  ld de,opcodesizes
	  ld b,e
foundloop_internal:
	  ; Get current opcode
	  ld e,(hl)
	  ex de,hl
	  ; Add recompiled instruction size
	  dec h
	  ld c,(hl)
	  inc h
	  add ix,bc
	  ; Add cycles
	  inc h
	  sub (hl)
	  dec h
	  ; Add GB instruction size
	  ld c,(hl)
	  ex de,hl
	  add hl,bc
	  add iy,bc
	  jr nc,foundloop_internal
	 pop de
	pop iy
	or a
	sbc hl,de
	ret z
	jr lookup_code_by_pointer
	
; Looks up a recompiled code pointer from a GB address.
;
; Inputs:  DE = 16-bit GB address to look up
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
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
lookuploop_restore:
	 pop ix
lookuploop:
	 lea ix,ix-8
	 ld a,ixh
	 or ixl
	 jr z,recompile
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
	  push hl
	  pop iy
	  add hl,de
	  push de
	   ld de,opcodesizes
	   ld b,e
	   ld a,(ix+7)
	   ld ix,(ix)
foundloop:
	   ; Get current opcode
	   ld e,(hl)
	   ex de,hl
	   ; Add recompiled instruction size
	   dec h
	   ld c,(hl)
	   inc h
	   add ix,bc
	   ; Add cycles
	   inc h
	   sub (hl)
	   dec h
	   ; Add GB instruction size
	   ld c,(hl)
	   ex de,hl
	   add hl,bc
	   add iy,bc
	   jr nc,foundloop
	  pop de
	  or a
	  sbc hl,de
	  jr nz,lookuploop_restore
	 pop hl
	pop iy
	ret
	
lookupfoundstart:
	 ld a,(ix+7)
	 bit 7,(ix+4)
	 ld ix,(ix)
	pop iy
	ret z
	; Report a cycle length of 0 for RAM blocks
	xor a
	ret
	
	
; Recompiles a new code block starting from a direct GB address.
;
; Inputs:  DE = direct 24-bit GB address to recompile
;          IY saved on stack
;          (base_address) = base address of pointer in DE
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
; Destroys AF,BC,DE,HL
recompile:
recompile_struct_end = $+2
	 ld ix,0
	 
	 ld hl,(ix)
	 ld (ix+2),de
	 bit 7,(ix+4)
#ifdef DEBUG
	 jp nz,recompile_ram
#else
	 jr nz,recompile_ram
#endif
	
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
	 pop ix
#endif
	 
	 call generate_opcodes
	
recompile_end_common:
	pop iy
	lea hl,ix+8
	ld (recompile_struct_end),hl
	ld (hl),de
	; Update the index LUT
	ld b,h
	ld c,l
	ld hl,recompile_index_LUT
	ld l,(ix+1)
	push af
	 ld a,d
_
	 ld (hl),c
	 inc h
	 ld (hl),b
	 dec h
	 inc l
	 cp l
	 jr nc,-_
	pop af
	
	; Check for collision with memroutines
	ld hl,(z80codebase+memroutine_next)
	sbc hl,de
	jr c,_
	; Check for collision with cache
	ld hl,(recompile_cache)
	lea de,ix+(MAX_CACHE_FLUSHES_ALLOWED*8)+3+6
	sbc hl,de
	ld ix,(ix)
	ret nc
	; Allow a certain number of cache flushes before flushing the entire JIT space
	ld hl,cache_flushes_allowed
	dec (hl)
	jr nz,flush_cache
_
	; Calculate the Game Boy start address from the generated code block
	ld ix,(recompile_struct_end)
	ld hl,(ix-6)
prepare_flush_by_pointer:
	ld de,(base_address)
	xor a
	sbc hl,de
prepare_flush:
	; Save off the Game Boy start address
	ld.sis (flush_address),hl
	; Prevent the cycle overflow code path from being taken
	ld hl,$E9DD08	;EX AF,AF' \ JP (IX)
	ld (z80codebase+cycle_overflow_flush_smc),hl
	; Dispatch to the flush handler instead of the recompiled code
	ld ix,flush_handler
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
	 
	 ; Report block cycle length of 0
	 xor a
#ifdef DEBUG
	 jp recompile_end_common
#else
	 jr recompile_end_common
#endif
	 
flush_cache:
	ld hl,recompile_cache_end
	ld (recompile_cache),hl
	ret
	
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
	
	; Remove any outstanding scheduled event
	ld.sis hl,(event_address)
	ld de,z80codebase + event_value
	ld a,(de)
	ld.s (hl),a
	ld.sis (event_address),de
	
	ld hl,(ix+2)
	ld de,vram_base
	xor a
	sbc hl,de
	ld bc,$A000
	sbc hl,bc
	jr c,rerecompile_found_base
	ld hl,(ix+2)
	ld de,wram_base
	sbc hl,de
	ld bc,$E000
	sbc hl,bc
	jr c,rerecompile_found_base
	ld hl,(ix+2)
	ld de,hram_base
	sbc hl,de
	dec c
	ld b,c
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
	
	; Invalidate both the read and write cycle LUTs
	ld hl,z80codebase+read_cycle_LUT
	ld (hl),c
	push hl
	pop de
	inc de
	ld c,(MEM_CYCLE_LUT_SIZE + 1) * 2
	ldir
	; Empty the interrupt return stack
	ld (z80codebase+int_return_sp),hl
	
	; Set the cached interrupt return address to NULL
	ld (int_cached_return),bc
	; Set the cached lookup address to NULL
	ld (lookup_gb_cache_jit_address),bc
	
	ei
	jp.sis coherency_return
	
coherency_cycle_overflow:
	xor a
	sub (ix+7)
	ld hl,(ix)
	ld de,RAM_PREFIX_SIZE
	add hl,de
	ex de,hl
	ld ix,(ix+2)
	jp schedule_event_helper_post_lookup
	
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
	call flush_code
	call lookup_code_by_pointer
	exx
	ld bc,(CALL_STACK_DEPTH+1)*256
	exx
	pop.s bc
	pop.s de
	pop.s hl
	ld.sis sp,myz80stack-2
	ld sp,myADLstack
	ei
	jp.sis dispatch_cycles
	
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
	; Cycle count while recompiling is hl-iy. Start at negative maximum.
	ld a,e
	add a,MAX_CYCLES_PER_BLOCK
	ld iyl,a
	push hl
base_address = $+1
	 ld hl,0
	 or a
	 sbc hl,de
	 ld (opgenCONSTwrite_smc),hl
	pop hl
	ex de,hl
	ld bc,opgentable
	ld a,ixl
	ld (opgen_emit_jump_smc_1),a
	ld a,ixh
	ld (opgen_emit_jump_smc_2),a
	push ix
	 ld ix,opgenroutines
	 call opgen_next_fast
	 call p,opgen_cycle_overflow
	pop ix
	inc hl
	inc de
	; Get the cycle count
	ld a,l
	sub iyl
	add a,MAX_CYCLES_PER_BLOCK
	; Get the size of the GB opcodes and save it
	ld bc,(ix+2)
	or a
	sbc hl,bc
	ld (ix+5),hl
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
	.db 1,3,3,1,1,1,2,1
	.db 5,1,3,1,1,1,2,1
	.db 1,3,3,1,1,1,2,1
	.db 0,1,3,1,1,1,2,1
	.db 10,3,3,1,1,1,2,3
	.db 10,1,3,1,1,1,2,1
	.db 10,5,3,4,3,3,4,1
	.db 10,3,3,4,1,1,2,1
	
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 3,3,3,3,3,3,3,3
	.db 1,1,1,1,1,1,3,1
	
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	
	.db 6,2,10,0,10,2,2,8
	.db 6,0,10,2,10,8,2,8
	.db 6,2,10,0,10,2,2,8
	.db 6,0,10,0,10,0,2,8
	.db 3,2,3,0,0,2,2,8
	.db 4,0,5,0,0,0,2,8
	.db 3,3,3,3,0,3,2,8
	.db 4,3,5,3,0,0,2,8
	
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
	.db -1,2,2,2,1,1,2,1
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
	
	.db 2,3,3,-1,3,4,2,1
	.db 2,-1,3,2,3,3,2,1
	.db 2,3,3,-1,3,4,2,1
	.db 2,-1,3,-1,3,-1,2,1
	.db 3,3,2,-1,-1,4,2,1
	.db 4,-1,4,-1,-1,-1,2,1
	.db 3,3,2,1,-1,4,2,1
	.db 3,2,4,1,-1,-1,2,1
	
; A table indexing opcode generation routines.
; All entry points live in a 256-byte space.
opgentable:
;00
	.db opgenNOP - opgenroutines
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
	.db opgenNOP - opgenroutines
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
	.db opgen27 - opgenroutines
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
	.db opgenNOP - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;48
	.db opgen1byte - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;50
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;58
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;60
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte - opgenroutines
;68
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgenNOP - opgenroutines
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
	.db opgenNOP - opgenroutines
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
	.db opgenF1 - opgenroutines
	.db opgenF2 - opgenroutines
	.db opgenF3 - opgenroutines
	.db opgenINVALID - opgenroutines
	.db opgenF5 - opgenroutines
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
	.db opgenRST - opgenroutines
	
; A table indexing port write handlers.
; All entry points live in a 256-byte space.
#define WRITE_PORT_NO_HANDLER_DIRECT 0
#define WRITE_PORT_NO_HANDLER_IGNORE 1
mem_write_port_handler_table:
	.db writeIEhandler - mem_write_port_handler_base
;00
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db writeSChandler - mem_write_port_handler_base
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db writeDIVhandler - mem_write_port_handler_base
	.db writeTIMAhandler - mem_write_port_handler_base
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db writeTAChandler - mem_write_port_handler_base
;08
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db writeIFhandler - mem_write_port_handler_base
;10
	.db writeNR10handler - mem_write_port_handler_base
	.db writeNR11handler - mem_write_port_handler_base
	.db writeNR12handler - mem_write_port_handler_base
	.db writeNR13handler - mem_write_port_handler_base
	.db writeNR14handler - mem_write_port_handler_base
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db writeNR21handler - mem_write_port_handler_base
	.db writeNR22handler - mem_write_port_handler_base
;18
	.db writeNR23handler - mem_write_port_handler_base
	.db writeNR24handler - mem_write_port_handler_base
	.db writeNR30handler - mem_write_port_handler_base
	.db writeNR31handler - mem_write_port_handler_base
	.db writeNR32handler - mem_write_port_handler_base
	.db writeNR33handler - mem_write_port_handler_base
	.db writeNR34handler - mem_write_port_handler_base
	.db WRITE_PORT_NO_HANDLER_IGNORE
;20
	.db writeNR41handler - mem_write_port_handler_base
	.db writeNR42handler - mem_write_port_handler_base
	.db writeNR43handler - mem_write_port_handler_base
	.db writeNR44handler - mem_write_port_handler_base
	.db writeNR50handler - mem_write_port_handler_base
	.db writeNR51handler - mem_write_port_handler_base
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_IGNORE
;28
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
;30
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
;38
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
;40
	.db writeLCDChandler - mem_write_port_handler_base
	.db writeSTAThandler - mem_write_port_handler_base
	.db writeSCYhandler - mem_write_port_handler_base
	.db writeSCXhandler - mem_write_port_handler_base
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db writeLYChandler - mem_write_port_handler_base
	.db writeDMAhandler - mem_write_port_handler_base
	.db writeBGPhandler - mem_write_port_handler_base
;48
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db writeWYhandler - mem_write_port_handler_base
	.db writeWXhandler - mem_write_port_handler_base
mem_write_port_handler_table_end:
	
	
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
	add a,MAX_CYCLES_PER_BLOCK
	dec hl
	dec iy
	jr opgen_emit_jump
	
_opgenRST:
	ld.sis bc,decode_rst
	sub 2
	jr opgen_emit_call
	
_opgenCALLcond:
	ld b,a
	ld a,c
	xor $C4 ^ $28
	ld (de),a
	inc de
	ld a,8
	ld (de),a
	inc de
	ld a,b
_opgenCALL:
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
	add a,MAX_CYCLES_PER_BLOCK + 3
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
	; Store the current ROM bank if in the correct range, else 0
	sub $40
	cp $40
	sbc a,a
	jr z,_
	ld a,(z80codebase+curr_rom_bank)
_
	ld (hl),a
	inc hl
	ld (hl),b
	inc hl
	inc hl
	ex de,hl
	jp opgen_next
	
_opgenJR:
	dec iy
opgen_emit_JR:
	add a,MAX_CYCLES_PER_BLOCK + 3
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
	dec iy
opgen_emit_JP:
	add a,MAX_CYCLES_PER_BLOCK + 4
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
	sbc a,a	; Carry was set, so set sign flag
	ret
	
_opgenRET:
	; Update total cycle count
	lea iy,iy-3
	ld a,c
	ld (de),a
	ret
	
_opgenRETcond:
	add a,MAX_CYCLES_PER_BLOCK + 5
	ld b,a
	dec iy
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
	
opgenroutinecall_4cc:
	dec iy
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
	add a,a
	jr nc,opgencartwrite
	
	cp $FC
	jr nc,opgenHMEMwrite
	
	push hl
opgenCONSTwrite_smc = $+1
	 ld hl,0
	 add hl,bc
	 ex (sp),hl
	
	 add a,a
	 jr nc,opgenVRAMwrite
	
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
_
	 ex de,hl
	 inc hl
	 inc de
	pop af
	or a
	jp nz,opgen_next
	ret
	
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
	 add a,a
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
	 jr -_
	
opgenCRAMwrite:
	 ld a,(cram_size)
	 or a
	 jr nz,_
	 ld a,(cram_size+1)
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
	 jr -_
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
	 jr --_
	
opgenHMEMwrite:
	xor a
	ld (de),a
	inc de
	ld (de),a
	inc de
	bit 0,b
	jr nz,_
	jr ++_
	
opgenFFwrite:
	dec iy
	inc hl
	ld c,(hl)
	ld b,$FF
_
	ld a,c
	cp $7F
_
	ex de,hl
	jp pe,opgenHRAMwrite
	inc a
	push hl
	 ld hl,mem_write_port_handler_table_end
	 cp l
	 ld l,a
	 ld a,(hl)
	pop hl
	jr nc,opgenHRAMignore
	add a,b
	jr nc,opgenHRAMwrite
	jr z,opgenHRAMignore0
	ld (hl),$CD ;CALL addr
	inc hl
	add a,(mem_write_port_handler_base + 1) & $FF
	ld (hl),a
	ld a,(mem_write_port_handler_base + 257) >> 8
	adc a,b
_
	inc hl
	ld (hl),a
	jp opgen_next_swap_skip
	
opgenHRAMignore:
	xor a
opgenHRAMignore0:
	ld (hl),a	;NOP
	inc hl
	ld (hl),a
	jr -_
	
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
	add a,a
	jr nc,opgencartread
	
	add a,a
	jr nc,opgenVRAMread
	
	add a,2*4
	jr c,opgenHMEMread
	
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
	ld a,(cram_size)
	or a
	jr nz,_
	ld a,(cram_size+1)
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
	ld a,b
	inc a
	jr z,_
	jr ++_
	
opgenFFread:
	dec iy
	inc hl
	ld c,(hl)
	ld b,$FF
_
	ld a,c
_
	rlca
	ex de,hl
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
	jr nz,_
	ld bc,readP1handler
	jr opgenHMEMreadroutine
_
	cp NR52*2 & $FF
	jr nz,opgenHRAMread
	ld bc,readNR52handler
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