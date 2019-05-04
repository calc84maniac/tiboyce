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
	 ld (int_return_sp),hl
	 ; Reset the event address
	 ld hl,event_value
	 ld.sis (event_address),hl
	 ; Reset the interrupt target caches
	 ld hl,z80codebase+dispatch_vblank
	 ld de,(decode_intcache << 8) | $C3	;CALL decode_intcache
	 ld b,5
_
	 ld (hl),de
	 inc hl \ inc hl \ inc hl
	 ld (hl),c
	 inc hl
	 djnz -_
	 ; Reset the cached RST targets
	 xor a
	 ld b,8
_
	 ld (hl),a
	 inc hl
	 ld (hl),c
	 inc hl
	 ld (hl),c
	 inc hl
	 add a,8
	 djnz -_
	 ; Set the cached interrupt return address to -1
	 dec bc
	 ld (int_cached_return),bc
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
#ifdef DEBUG
	dec sp
	push de
	inc sp
	pop af
	or a
	jr nz,$
#endif
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
	
; Gets a 24-bit unique identifier for a given Game Boy address, using the current GB memory map.
;
; Inputs:  DE = GB address
; Outputs: DE = GB address plus (bank << 24), depending on region
; Destroys AF
get_banked_address:
	ld a,d
	add a,$40
	ret po
	push hl
	 ld hl,(z80codebase+curr_rom_bank-2)
	 ld h,d
	 ld l,e
	 ex de,hl
	pop hl
	ret
	
; Gets the recompile struct entry for a given code pointer.
;
; Inputs: BC = 16-bit Z80 code pointer
; Outputs: IX = struct entry or $xx0000 if not found
; Destroys AF,DE,HL
lookup_code_block:
#ifdef 0
	push bc
	 APRINTF(LookupGBMessage)
	pop bc
#endif
	ld ix,recompile_struct
	ld hl,recompile_index_LUT
	ld l,b
	ld a,(hl)
	ld ixl,a
	inc h
	ld a,(hl)
	ld ixh,a
	ld a,c
	cpl
	ld e,a
	ld a,b
	cpl
	ld d,a
lookup_code_block_loop:
	ld hl,(ix)
	add.s hl,de
	ret nc
	ld hl,(ix-8)
	add.s hl,de
	jr nc,_
	ld hl,(ix-16)
	add.s hl,de
	jr nc,++_
	ld hl,(ix-24)
	add.s hl,de
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
	pop.s bc
#ifdef DEBUG
	; Open debugger on CEmu
	ld (exitReason),a
	ld a,2
	ld ($FFFFFF),a
	push bc
	 APRINTF(InvalidOpcodeErrorMessage)
	pop bc
#endif
	call.il lookup_gb_code_address
	ld (errorArg),de
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
; Inputs:  BC = 16-bit Z80 code pointer
; Outputs: DE = 16-bit GB code address
;          A = Number of cycles until sub-block end
; Destroys F,HL,IX
lookup_gb_code_address:
#ifdef 0
	push bc
	 APRINTF(LookupGBMessage)
	pop bc
#endif
	call lookup_code_block
	ld a,ixh
	or ixl
	jr z,runtime_error
	ld de,(ix)
	ld hl,(ix+2)
	ex.s de,hl
	sbc hl,bc
	jr z,lookup_gb_found_start
	push hl
	 call get_base_address
#ifdef DEBUG
	 ld a,d
	 sub $40
	 cp $40
	 jr nc,_
	 ld a,(z80codebase+curr_rom_bank)
	 cp (ix+4)
	 jr nz,$
_
#endif
	 ld a,(ix+7)
	pop ix
	push hl
	 push bc
	  add hl,de
	  bit 7,d	; Check whether the GB address is in ROM
	  ld de,opcodesizes
	  ld bc,RAM_PREFIX_SIZE
	  jr nz,lookup_gb_add
lookup_gb_found_loop:
	  ld e,(hl)
	  ex de,hl
	  ld c,(hl)
	  ex de,hl
	  add hl,bc
	  ex de,hl
	  inc h
	  sub (hl)
	  dec h
	  dec h
	  ld c,(hl)
	  inc h
	  ex de,hl
	  jr c,lookup_gb_new_sub_block
lookup_gb_add:
	  add ix,bc
	  jr nc,lookup_gb_found_loop
	  dec ix
	  add ix,ix
runtime_error_trampoline:
#ifdef DEBUG
	  jp nc,runtime_error
#else
	  jr nc,runtime_error
#endif
	 pop bc
lookup_gb_finish:
	pop de
	or a
	sbc hl,de
	ex de,hl
#ifdef 0
	push af
	 push de
	  push bc
	   APRINTF(LookupGBFoundMessage)
	  pop bc
	 pop de
	pop af
#endif
	ret.l
	
lookup_gb_found_start:
	ld a,(ix+7)
	ret.l

lookup_gb_new_sub_block_end:
	 dec ix
	 add ix,bc
	 jr nc,runtime_error_trampoline
	 add.s a,(ix-2)
	 ASSERT_C
	 jr lookup_gb_finish
	
lookup_gb_new_sub_block:
	  bit 7,a
	  jr z,lookup_gb_prefix
	  add ix,bc
	 pop bc
	 jr c,lookup_gb_new_sub_block_end
	 push hl
	  lea hl,ix-3
	  add hl,bc
	  add.s a,(hl)
	  ASSERT_C
	 pop hl
	 push bc
	  ld bc,0
	  jr lookup_gb_found_loop
	
lookup_gb_prefix:
	  ; Count CB-prefixed opcode cycles
	  sbc a,c
	  ASSERT_NC
	  ld c,a
	  dec hl
	  ld a,(hl)
	  inc hl
	  xor $46
	  and $C7
	  jr z,_
	  and 7
	  jr nz,++_
	  dec c
_
	  dec c
#ifdef DEBUG
	  jp m,$
#endif
_
	  ld a,c
	  ld c,2
	  jr lookup_gb_add
	
	
	; If a cached code lookup misses, resolve it and insert into the cache
lookup_code_cached_miss:
	 push de
	  push bc
#ifdef DEBUG
	   push de
	    APRINTF(CacheMissMessage)
	   pop de
#endif
	   call lookup_code_with_bank
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
	   ; Get the GB banked pointer in HL
	   pop de
	   pop hl
	   push hl
	   push de
	   ; If no cache flushes remaining, prepare a JIT flush (replacing IX and A)
	   call z,prepare_flush
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
	 ld (hl),ix
	 inc hl
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
	 ld hl,(int_return_sp)
	 dec hl
	 dec hl
	 ld ix,(hl)
	 dec hl
	 dec hl
	 dec hl
	 dec hl
	 xor a
	 sub (hl)
	 ld (int_return_sp),hl
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
;          DE = popped Game Boy address (banked)
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
;          DE = Game Boy address (banked)
;          A = number of cycles until block end
; Destroys AF,BC,DE
lookup_code_cached:
	; Get the banked address in DE
	call get_banked_address

lookup_code_cached_with_bank:
	push hl
	 ; Quickly check against the last interrupt return address
int_cached_return = $+1
	 ld hl,0
	 or a
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
	
	
lookup_code_link_internal_with_bank_cached:
	call.il lookup_code_cached_with_bank
	ret
	
internal_found_start:
	ld a,(ix+7)
	ld ix,(ix)
	lea ix,ix+RAM_PREFIX_SIZE
	ret
	
internal_found_new_subblock:
	  bit 7,a
	  jr z,internal_found_prefix
	  add.s a,(ix-3)
	  ASSERT_C
	  add hl,bc
	  add iy,bc
	  jr nc,foundloop_internal
	  jr foundloop_internal_finish
	  
internal_found_prefix:
	  ; Count CB-prefixed opcode cycles
	  sbc a,c
	  bit.s 2,(ix-2)
	  jr z,foundloop_internal_continue
	  sub c
	  bit.s 7,(ix-1)
	  jr nz,foundloop_internal_continue
	  bit.s 6,(ix-1)
	  jr z,foundloop_internal_continue
	  inc a
	  jr foundloop_internal_continue
	
lookup_code_link_internal:
	call get_banked_address
	
; Looks up a recompiled code pointer from a banked GB address,
; allowing a direct link within the currently executing RAM block.
; Normally, only the start of a RAM block is allowed for direct links.
; If executing from ROM or target is not the same block, proceed as normal.
;
; Inputs:  DE = banked GB address to look up
;          IX = struct pointer of the currently executing block
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
; Destroys AF,BC,DE,HL
lookup_code_link_internal_with_bank:
	ld hl,(ix+2)
	bit 7,h
	jr z,lookup_code_with_bank
	; We're running from RAM, check if destination is in running block
	or a
	sbc hl,de
	jr z,internal_found_start
	jr nc,lookup_code_link_internal_with_bank_cached
	ld bc,(ix+5)
	dec.s bc
	add hl,bc
	jr nc,lookup_code_link_internal_with_bank_cached
	or a
	sbc hl,bc
	push hl
	 ex (sp),iy
	 add.s hl,de
	 push de
	  ex de,hl
	  call get_base_address
	  add hl,de
	  ld de,opcodesizes
	  ld b,e
	  ld a,(ix+7)
	  ld ix,(ix)
	  lea ix,ix+RAM_PREFIX_SIZE
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
	  jr c,internal_found_new_subblock
foundloop_internal_continue:
	  add hl,bc
	  add iy,bc
	  jr nc,foundloop_internal
foundloop_internal_finish:
	 pop de
	 dec iy
	 add iy,iy
	pop iy
	ret c
	jp lookup_code_link_internal_with_bank_cached
	
lookupfoundstart:
	 ld a,(ix+7)
	 bit 7,(ix+3)
	 ld ix,(ix)
	pop iy
	ret z
	; Report a cycle length of 0 for RAM blocks
	xor a
	ret
	
; Looks up a recompiled code pointer from a GB address.
;
; Inputs:  DE = 16-bit GB address to look up
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
; Destroys AF,BC,DE,HL
lookup_code:
	call get_banked_address
	
; Looks up a recompiled code pointer from a banked GB address.
;
; Inputs:  DE = banked GB address to look up
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
; Destroys AF,BC,DE,HL
lookup_code_with_bank:
	push iy
#ifdef 0
	 push de
	  APRINTF(LookupMessage)
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
	 ld a,(ix+3)
	 rla
	 jr c,lookuploop
	 push ix
	  sbc hl,bc
	  push hl
	  pop iy
	  add.s hl,de
	  push de
	   ex de,hl
	   call get_base_address
	   add hl,de
	   ld de,opcodesizes
	   ld b,e
	   ld a,(ix+7)
	   ld ix,(ix)
lookup_found_loop:
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
	   jr c,lookup_found_new_subblock
lookup_found_continue:
	   add hl,bc
	   add iy,bc
	   jr nc,lookup_found_loop
lookup_found_loop_finish:
	  pop de
	  dec iy
	  add iy,iy
	  jr nc,lookuploop_restore
	 pop hl
	pop iy
	ret
	
lookup_found_new_subblock:
	   bit 7,a
	   jr z,lookup_found_prefix
	   add.s a,(ix-3)
	   ASSERT_C
	   add hl,bc
	   add iy,bc
	   jr nc,lookup_found_loop
	   jr lookup_found_loop_finish
	
lookup_found_prefix:
	   ; Count CB-prefixed opcode cycles
	   sbc a,c
	   bit.s 2,(ix-2)
	   jr z,lookup_found_continue
	   sub c
	   bit.s 7,(ix-1)
	   jr nz,lookup_found_continue
	   bit.s 6,(ix-1)
	   jr z,lookup_found_continue
	   inc a
	   jr lookup_found_continue
	
	
; Recompiles a new code block starting from a banked GB address.
;
; Inputs:  DE = banked GB address to recompile
;          IY saved on stack
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
; Destroys AF,BC,DE,HL
recompile:
recompile_struct_end = $+2
	 ld ix,0
	 
	 ld hl,(ix)
	 ld (ix+2),de
	 bit 7,d
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
	     APRINTF(RecompileMessage)
	    pop de
	   pop hl
	  pop hl
	 pop ix
#endif
	 
	 call generate_opcodes
	 ld a,(ix+7)
	
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
	; Retrieve the Game Boy start address from the generated code block
	ld ix,(recompile_struct_end)
	ld hl,(ix-6)
prepare_flush:
	; Save off the Game Boy start address
	ld.sis (flush_address),hl
	; Prevent an event from being scheduled on the flush handler
	ld hl,$C3 | (schedule_event_finish_no_schedule << 8)	;JP schedule_event_finish_no_schedule
	ld (flush_event_smc),hl
	; Dispatch to the flush handler instead of the recompiled code
	ld ix,flush_handler
	; Don't consume any cycles during dispatch
	xor a
	ret
	
recompile_ram:
#ifdef DEBUG
	 push ix
	  push hl
	   inc hl
	   dec.s hl
	   push hl
	    push de
	     APRINTF(RecompileRamMessage)
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
	 push hl
	  ld hl,(ix+2)
	  inc hl
	  dec.s hl
	  add hl,bc
	  ; Add in padding to avoid flushes
	  ex de,hl
ram_block_padding = $+1
	  ld bc,0
	  add hl,bc
	  ex de,hl
	 pop bc
	 ldir
	 ; Complement the final byte to force the match to end here
	 dec de
	 ld a,(de)
	 cpl
	 ld (de),a
	 inc de
	 
	 ; Report block cycle length of 0
	 xor a
	 jp recompile_end_common
	 
flush_cache:
	ld hl,recompile_cache_end
	ld (recompile_cache),hl
	ret
	
	
check_coherency_helper:
	ex af,af'
check_coherency_helper_swapped:
	ld de,recompile_struct
	add ix,de
	ld hl,(ix+2)
	ex.s de,hl
	call get_base_address
	add hl,de
	ex de,hl
	ld bc,(ix+5)
	ld hl,(ix+8)
	sbc hl,bc
check_coherency_loop:
	ld a,(de)
	inc de
	cpi.s
	jr nz,_
	ld a,(de)
	inc de
	cpi.s
	jr nz,_
	ld a,(de)
	inc de
	cpi.s
	jr nz,_
	ld a,(de)
	inc de
	cpi.s
	jr z,check_coherency_loop
_
	jp pe,rerecompile
	; Make sure the last (complemented) byte matches
	cpl
	dec hl
	xor.s (hl)
	jr nz,rerecompile
	
check_coherency_cycles:
	ld a,(ix+7)
	ld (_+2),a
_
	lea iy,iy+0
	ld a,iyh
	or a
	jr z,check_coherency_cycle_overflow
	ex af,af'
	ei
	jp.sis coherency_return
	
	
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
	  ld hl,(ix+2)
	  push hl
	   APRINTF(CoherencyFailedMessage)
	  pop hl
	 pop hl
	pop ix
	or a
#endif
	
	ld hl,(ix)
	inc.s hl
	ld de,z80codebase + RAM_PREFIX_SIZE - 1
	add hl,de
	ld de,(ix+2)
	push iy
	 call generate_opcodes
	pop iy
	
#ifdef DEBUG
	ld a,h
	cp 1
	jr nc,$
#endif
	
	push hl
	 ; Get the address to copy the opcodes from
	 ld l,(ix+2)
	 ld h,(ix+3)
	 add hl,bc
	pop bc
	
	push hl
	 ; Get the address to copy the opcodes to
	 ld hl,z80codebase
	 ld l,(ix+8)
	 ld h,(ix+9)
	 sbc hl,bc	; Carry is reset
	 ASSERT_NC
	 ex de,hl
	
	 ; Make sure there is no overlap
	 scf
	 sbc hl,de
	 jr nc,coherency_flush
	
	; Copy the new opcodes, from first to last
	pop hl
	ldir
	dec de
	ld a,(de)
	cpl
	ld (de),a
	
	; Invalidate both the read and write cycle LUTs
	ld hl,z80codebase+read_cycle_LUT
	ld (hl),c
	push hl
	pop de
	inc de
	ld c,(MEM_CYCLE_LUT_SIZE + 1) * 2
	ldir
	; Empty the interrupt return stack
	ld (int_return_sp),hl
	
	; Set the cached interrupt return address to -1
	dec bc
	ld (int_cached_return),bc
	jr check_coherency_cycles
	
check_coherency_cycle_overflow:
	ld c,(ix+7)
	ld hl,(ix+2)
	ex.s de,hl
	pop.s ix
	jp schedule_event_helper
	
coherency_flush:
	pop de
	pop.s de
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
	call lookup_code_with_bank
	exx
	ld bc,(CALL_STACK_DEPTH+1)*256
	exx
	pop.s bc
	pop.s de
	pop.s hl
	ld.sis sp,myz80stack-2
	ld sp,myADLstack
	; No need to count cycles, that is handled by the RAM block prefix
	ex af,af'
	ei
	jp.s (ix)
	
	
; Inputs:  IX = struct entry
;          DE = GB opcodes start address (banked)
;          HL = block start
; Outputs: IX = struct entry
;          BC = GB opcodes base address
;          DE = block end
;          HL = GB opcodes size
;          A = total block cycle count
;          Carry flag reset
; Destroys None
generate_opcodes:
	push hl
	 lea hl,ix+7
	 ld (opgen_last_cycle_count_smc),hl
	 or a
	 sbc hl,hl
	 sbc hl,de
	 ld (opgenCONSTwrite_smc),hl
	 dec de
	 inc.s de
	 call get_base_address
	 ex de,hl
	 add hl,de
	 ex de,hl
	 ld a,l
	 ld (opgen_base_address_smc_1),a
	 ld a,h
	 ld (opgen_base_address_smc_2),a
	 ex (sp),hl
	 ex de,hl
	 ld bc,opgentable
	 ld a,ixl
	 ld (opgen_emit_jump_smc_1),a
	 ld a,ixh
	 ld (opgen_emit_jump_smc_2),a
	 ; Cycle count while recompiling is hl-iy. Start at negative maximum.
	 ld a,l
	 add a,MAX_CYCLES_PER_BLOCK + $80
	 ld iyl,a
	 push ix
	  ld ix,opgenroutines
	  call opgen_next_fast
	  call m,opgen_cycle_overflow
	 pop ix
	 ex de,hl
	 inc hl
	 ; Get the size of the GB opcodes and save it
	 ld bc,(ix+2)
	 inc bc
	 dec.s bc
	 or a
	 sbc hl,bc
	pop bc
	sbc hl,bc
	ld (ix+5),l
	ld (ix+6),h
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
	.db 17,3,3,1,1,1,2,3
	.db 17,1,3,1,1,1,2,1
	.db 17,5,3,4,3,3,4,1
	.db 17,3,3,4,1,1,2,1
	
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 3,3,3,3,3,3,6,3
	.db 1,1,1,1,1,1,3,1
	
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	
	.db 12,2,17,0,10,2,2,7
	.db 12,0,17,2,10,8,2,7
	.db 12,2,17,0,10,2,2,7
	.db 12,0,17,0,10,0,2,7
	.db 3,2,3,0,0,2,2,7
	.db 4,0,5,0,0,0,2,7
	.db 3,3,3,3,0,3,2,7
	.db 4,3,5,6,0,0,2,7
	
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
; Prefix opcodes (i.e. CB) are set to -1, for efficient detection.
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
	
	.db 2,3,3,0,4,4,2,4
	.db 2,0,3,-1,4,4,2,4
	.db 2,3,3,0,4,4,2,4
	.db 2,0,3,0,4,0,2,4
	.db 3,3,2,0,0,4,2,4
	.db 4,0,4,0,0,0,2,4
	.db 3,3,2,1,0,4,2,4
	.db 3,2,4,1,0,0,2,4
	
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
	push hl
	 call opgen_emit_unconditional_jump
	pop de
	dec de
	ret
	
_opgenRST:
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),do_rst & $FF
	inc hl
	ld (hl),do_rst >> 8
	call opgen_reset_cycle_count
	lea iy,iy-3
	jr opgen_finish_rst
	
_opgenCALLcond:
	ex de,hl
	ld a,c
	xor $C4 ^ $CC
	ld (hl),a
	inc hl
	bit 4,c
	jr nz,++_
	bit 3,c
	jr nz,_
	ld (hl),do_call_nz & $FF
	inc hl
	ld (hl),do_call_nz >> 8
	jr opgen_finish_cond_call
_
	ld (hl),do_call_z & $FF
	inc hl
	ld (hl),do_call_z >> 8
	jr opgen_finish_cond_call
_
	bit 3,c
	jr nz,_
	ld (hl),do_call_nc & $FF
	inc hl
	ld (hl),do_call_nc >> 8
	jr opgen_finish_cond_call
_
	ld (hl),do_call_c & $FF
	inc hl
	ld (hl),do_call_c >> 8
opgen_finish_cond_call:
	inc hl
	ld (hl),$CD
	inc hl
	ld (hl),decode_call_cond & $FF
	inc hl
	ld (hl),decode_call_cond >> 8
	jr opgen_finish_call

_opgenCALL:
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),decode_call & $FF
	inc hl
	ld (hl),decode_call >> 8
	inc hl
opgen_finish_call:
	call opgen_reset_cycle_count
	dec iy
	inc de
	inc de
opgen_finish_rst:
	inc de
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
	ld (opgen_last_cycle_count_smc),hl
	call opgen_emit_gb_address
	ex de,hl
	jp opgen_next_fast

opgen_emit_unconditional_jump:
	ex de,hl
opgen_emit_unconditional_jump_swapped:
	ld a,$C3
opgen_emit_jump_swapped:
	ld (hl),$CD
	inc hl
	ld (hl),decode_jump & $FF
	inc hl
	ld (hl),decode_jump >> 8
	inc hl
	ld (hl),RST_CYCLE_CHECK
	inc hl
	ld (hl),$08	;EX AF,AF'
	inc hl
	; Emit jump instruction
	ld (hl),a
	inc hl
	; Emit block struct pointer
opgen_emit_jump_smc_1 = $+1
	ld (hl),0
	inc hl
opgen_emit_jump_smc_2 = $+1
	ld (hl),0
	call opgen_emit_gb_address
	call opgen_reset_cycle_count
	inc de
	ld a,c
	rla
	ret nc
	inc de
	ret
	
_opgenRET:
	ex de,hl
	call opgen_reset_cycle_count
	ld (hl),c
	inc hl
	ret
	
_opgenRETcond:
	ex de,hl
	call opgen_reset_cycle_count
	dec iy
	ld a,c
	xor $C0 ^ $28
	ld (hl),a
	inc hl
	ld (hl),5
	inc hl
	ld (hl),$FD	;INC IY
	inc hl
	ld (hl),$23
	inc hl
	ld (hl),$C9	;RET
	call opgen_emit_gb_address
	
opgen_emit_block_bridge:
	ld (hl),$ED	;LEA IY,IY+offset
	inc hl
	ld (hl),$33
	inc hl
	ld (opgen_last_cycle_count_smc),hl
	inc hl
	ld (hl),RST_CYCLE_CHECK
	inc hl
	ld (hl),$08	;EX AF,AF'
	inc hl
	inc de
	ex de,hl
	jp opgen_next_fast
	
opgen_emit_gb_address:
	inc hl
	ld a,e
opgen_base_address_smc_1 = $+1
	sub 0
	ld (hl),a
	inc hl
	ld a,d
opgen_base_address_smc_2 = $+1
	sbc a,0
	ld (hl),a
	inc hl
	ret
	
opgenblockend:
	ex (sp),hl
	ld hl,(hl)
	ex de,hl
	ld (hl),0	;NOP
	inc hl
	ld (hl),$C3	;JP
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	pop de
	
opgen_reset_cycle_count:
	ld a,e
	sub iyl
	add a,MAX_CYCLES_PER_BLOCK + $80
opgen_last_cycle_count_smc = $+1
	ld (0),a
	add a,iyl
	ld iyl,a
	xor a
	ret
	
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
	
opgenroutinecallsplit_1cc:
	ld a,$CD
	ld (de),a
	inc de
	ex (sp),hl
	ldi
	ldi
	pop hl
	ex de,hl
	call opgen_reset_cycle_count
	ld (opgen_last_cycle_count_smc),hl
	inc de
	call opgen_emit_gb_address
	ex de,hl
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
	dec a	; Set sign flag
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