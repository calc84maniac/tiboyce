#define RST_BITS $C7+r_bits
#define RST_MEM $C7+r_mem
#define RST_POP $C7+r_pop
#define RST_CALL $C7+r_call
#define RST_EVENT $C7+r_event
#define RST_CYCLE_CHECK $C7+r_cycle_check

#define RAM_PREFIX_SIZE 5
; Define such that cycles per block (at most 4x the bytes) is at most 249.
; This limitation is for two reasons:
; 1) Up to 6 cycles may be combined with the block cycle count for a taken CALL
; 2) Sub-block overflow and prefixed instruction detection when stepping through
;    a block can be differentiated by range. Sub-block overflow gives a result
;    no lower than -5 (aka 251), and a prefixed instruction (subtracting -1
;    from the current cycle count gives a value no higher than 250.
#define MAX_OPCODE_BYTES_PER_BLOCK 62
#define MAX_BYTES_PER_OPCODE 3
#define MAX_CACHE_FLUSHES_ALLOWED 2

; The default value for no cycle offset available. Any non-negative offset
; is considered invalid, but the offset may be decremented by 1 during
; instructions, so it must stay non-negative in those cases.
#define NO_CYCLE_INFO 1
; This bit specifies that a memory access cannot cause a reschedule,
; so the Game Boy address will not be included in the cycle cache.
#define NO_RESCHEDULE 2

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
	 ; Invalidate the memory routines, recompile index, and recompile cache LUT
	 MEMSET_FAST(memroutineLUT, $0500, 0)
	 MEMSET_FAST(recompile_cache_LUT+256, 256, (recompile_cache_end>>8)&$FF)
	 ; Reset the event address
	 ld hl,event_value
	 ld.sis (event_address),hl
	 ; Reset the RST target caches
	 ld hl,z80codebase+do_rst_08-1
	 ld de,do_rst_08 - do_rst_00
	 ld a,decode_rst - do_rst_08
	 ld b,8
_
	 ld (hl),a
	 add hl,de
	 sub e
	 djnz -_
	 ld a,MAX_CACHE_FLUSHES_ALLOWED
	 ld (cache_flushes_allowed),a
	 ; Resolve the interrupt target caches
	 ld hl,z80codebase+dispatch_vblank+3
	 ld e,$40-8
_
	 ld a,e
	 add a,8
	 ld e,a
	 push de
	  push hl
	   call lookup_code
	  pop hl
	 pop de
	 ; Add 4 cycles to mimic RST target caches
	 sub -4 ; Sets carry
	 ld (hl),a
	 dec hl
	 dec hl
	 ld.s (hl),ix
	 rl l
	 jp p,-_
	pop de
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
	exx
	pop.s bc
	dec bc
	dec bc
	dec bc
#ifdef DEBUG
	; Open debugger on CEmu
	ld (exitReason),a
	ld a,2
	ld ($FFFFFF),a
	push bc
	 APRINTF(InvalidOpcodeErrorMessage)
	pop bc
#endif
	call lookup_gb_code_address
	call get_banked_address
	ld (errorArg),de
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
	
lookup_gb_found_start:
	ld a,(ix+7)
	bit 7,d
	ret z
	xor a
	ret
	
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
	 GET_BASE_ADDR_FAST
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
	 ; Get the LSB of the block end for overlap detection
	 ld a,l
	 add a,e
	 add a,(ix+5)
	 inc a
	 ld (lookup_gb_overlap_smc),a
	 ; Get the cycle count of the first sub-block
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
	  dec h
	  sub (hl)
	  dec h
	  ld c,(hl)
	  inc h
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
lookup_gb_finish_overlapped:
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
	ret

lookup_gb_new_sub_block_end:
	 dec ix
	 add ix,bc
	 jr nc,runtime_error_trampoline
	 jr z,_
	 inc ix  ; For CALL/RST/HALT, count is at -3 bytes
_
	 add.s a,(ix-3)  ; For RET/JR/JP, count is at -4 bytes
	 ASSERT_C
	 jr lookup_gb_finish
	
lookup_gb_new_sub_block:
	  cp -5
	  jr c,lookup_gb_prefix
	  ; Note: for an overlapped instruction, the following transformations
	  ; may end up incorrect, but they will be discarded after the bounds check
	  bit 2,e
	  jr nz,_
	  inc bc ; For RET/JR/JP, offsets are stored -1
_
	  add ix,bc
	 pop bc
	 ; This jump will not be taken for an overlapped instruction
	 jr c,lookup_gb_new_sub_block_end
	 push hl
	  lea hl,ix-4 ; For RET/JR/JP, count is at -4 bytes
	  jr z,_
	  inc hl  ; For CALL/RST, count is at -3 bytes
_
	  add hl,bc
	  add.s a,(hl)
	 pop hl
	 push bc
lookup_gb_overlap_smc = $+1
	  ld bc,0
	  ; Check if the end of the block was exceeded
	  ld e,a
	  ld a,c
	  sub l
	  rla
	  ld a,e
	  jr nc,lookup_gb_found_loop
lookup_gb_found_overlapped:
	  ; TODO: error checking?
	  ; Cycle offset after instruction should always be 0
	  xor a
	  jr lookup_gb_finish_overlapped
	
lookup_gb_prefix:
	  ; Count CB-prefixed opcode cycles
	  sbc a,c
	  ; If the cycle count overflowed from a CB-prefix instruction,
	  ; this means it must have been an overlapped instruction
	  ; This path is also taken if we reached a HALT
	  jr c,lookup_gb_found_overlapped_or_halt
	  ld c,a
	  dec hl
	  ld a,(hl)
	  inc hl
	  xor $36
	  ; Check for (HL) access
	  tst a,7
	  jr z,_
	  ; Check for SWAP to adjust JIT code size appropriately
	  cp 8
	  ld a,c
	  ld c,1
	  rl c
	  jr lookup_gb_add
_
	  ; Check for BIT to adjust cycle count appropriately
	  add a,$80
	  cp $C0
	  sbc a,a
	  dec a
	  add a,c
	  ASSERT_C
	  ld c,4
	  jr lookup_gb_add
	  
lookup_gb_found_overlapped_or_halt:
	  ; Check if the opcode was a HALT or not
	  inc a
	  jr nz,lookup_gb_found_overlapped
	  ; If it was a HALT, get the bugged instruction byte
	  dec hl
	  dec hl
	  ld e,(hl)
	  dec hl
	  ; Check if at the start of the bugged instruction
	  ld c,9
	  add ix,bc
	  jr nc,_
	 ; Get the cycle count of the bugged instruction
	 pop bc
	 dec bc
	 ld.s a,(bc)
	 inc bc
	 dec a
	 jr lookup_gb_finish
_
	  ; If not, determine the address after the bugged instruction
	  ld a,(de)
	  ld c,a
	  add hl,bc
	  ; Cycle count will be 0
	  jr lookup_gb_found_overlapped
	
	 ; When a match is found, load it from the cache
lookup_code_cached_found:
	 ld a,(ix-3)
	 ld ix,(ix-5)
	pop hl
	ret.l
	
	
; Looks up a Game Boy address from the cached code mappings, and
; adds it to the cache upon miss.
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
	 ; Search the cache for the pointer, indexing by the LSB
	 ld ix,recompile_cache_end
	 ld hl,recompile_cache_LUT
	 ld l,e
	 ld a,(hl)
	 dec l
	 ld c,(hl)
	 inc h
	 ld b,(hl)
	 inc l
	 jr z,_
	 ld ixl,c
	 ld ixh,b
_
	 sub ixl
	 jr z,lookup_code_cached_miss
	 ld bc,-5
lookup_code_cached_loop:
	 ld hl,(ix-3)
	 ld l,e
	 or a
	 sbc hl,de
	 jr z,lookup_code_cached_found
	 add ix,bc
	 sub c
	 jr nz,lookup_code_cached_loop
	
	; If a cached code lookup misses, resolve it and insert into the cache
lookup_code_cached_miss:
	 push de
	  push ix
#ifdef DEBUG
	   push de
	    APRINTF(CacheMissMessage)
	   pop de
#endif
	   call lookup_code_with_bank
	   ; Check if the cache needs to be flushed
	   ld hl,(recompile_struct_end)
	   ld de,(recompile_cache)
	   ld bc,3+5
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
	   call flush_cache
	   ex de,hl
_
	  pop hl
	  ; Copy back the entries below the insert point to make room
	  or a
	  sbc hl,de
	  jr nc,_
	  cp a	;Set Z
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
	 ; Assign to the new entry
	 ld (hl),ix
	 inc hl
	 inc hl
	 ld (hl),de
	 ld (hl),a
	 ; Update range bounds for each LSB range copied back
	 ld hl,recompile_cache_LUT + 5
	 ld b,l
	 ld l,e
	 ld c,a
_
	 ld a,(hl)
	 sub b
	 ld (hl),a
	 jr c,_
	 inc l
	 jr nz,-_
	 ld a,c
	pop hl
	ret.l
	
_
	 inc h
	 dec (hl)
	 dec h
	 inc l
	 jr nz,--_
	 ld a,c
	pop hl
	ret.l
	
	
lookup_code_link_internal_with_bank_cached:
	call.il lookup_code_cached_with_bank
	or a
	ret
	
internal_found_start:
	ld a,(ix+7)
	ld ix,(ix)
	lea ix,ix+RAM_PREFIX_SIZE
	scf
	ret
	
internal_found_new_subblock:
	  cp -5
	  jr c,internal_found_prefix
	  add.s a,(ix-3)
	  ;ASSERT_C
	  bit 2,e
	  jr nz,_ ; For CALL/RST, offsets are normal
	  inc ix  ; For RET/JR/JP, offsets are stored -1
_
	  add hl,bc
	  add iy,bc
	  jr nc,foundloop_internal
	  jr foundloop_internal_finish
	  
internal_found_prefix:
	  ; Count CB-prefixed opcode cycles
	  sbc a,c
	  ; If we're stepping past a HALT, then continue
	  jr c,foundloop_internal_continue
	  ; Analyze the first byte of JIT code
	  ld.s e,(ix-2)
	  ; Check for CB prefix, as opposed to CALL, LD IXH, or LD A,A
	  bit 2,e
	  jr z,foundloop_internal_continue
	  inc ix
	  ; Check for CALL, as opposed to LD IXH or LD A,A
	  bit 4,e
	  jr z,foundloop_internal_continue
	  ld e,a
	  inc ix
	  inc hl
	  ld a,(hl)
	  dec hl
	  ; Check for BIT to adjust cycle count
	  add a,$80
	  cp $C0
	  sbc a,a
	  dec a
	  add a,e
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
;          Carry is set if it is valid to use waitloop detection on the result
; Destroys AF,BC,DE,HL
lookup_code_link_internal_with_bank:
	ld hl,(ix+2)
	bit 7,h
	jr z,lookup_code_with_bank
	xor a
	; We're running from RAM, check if destination is in running block
	sbc hl,de
	jr z,internal_found_start
	jr nc,lookup_code_link_internal_with_bank_cached
	ld b,a
	mlt bc
	ld c,(ix+5)
	add hl,bc
	jr nc,lookup_code_link_internal_with_bank_cached
	or a
	sbc hl,bc
	push hl
	 ex (sp),iy
	 add.s hl,de
	 push de
	  ex de,hl
	  GET_BASE_ADDR_NO_ASSERT
	  add hl,de
	  ld de,opcodesizes
	  ld a,(ix+7)
	  ld ix,(ix)
	  lea ix,ix+RAM_PREFIX_SIZE
foundloop_internal:
	  ; Get current opcode
	  ld e,(hl)
	  ex de,hl
	  ; Add recompiled instruction size
	  dec h
	  dec h
	  ld c,(hl)
	  add ix,bc
	  ; Add cycles
	  inc h
	  sub (hl)
	  inc h
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
	 ld ix,(ix)
	pop iy
	bit 7,d
	scf
	ret z
	; Report a cycle length of 0 for RAM blocks
	xor a
	ret
	
; Looks up a recompiled code pointer from a GB address.
;
; Inputs:  DE = 16-bit GB address to look up
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
;          Carry is reset if at the start of a newly recompiled or RAM block
; Destroys AF,BC,DE,HL
lookup_code:
	call get_banked_address
	
; Looks up a recompiled code pointer from a banked GB address.
;
; Inputs:  DE = banked GB address to look up
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
;          Carry is reset if at the start of a newly recompiled or RAM block
; Destroys AF,BC,DE,HL
lookup_code_with_bank:
	ld bc,0
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
	 jr z,recompile_trampoline
	 ld hl,(ix+2)
	 sbc hl,de
	 jr z,lookupfoundstart
	 jr nc,lookuploop
	 ld c,(ix+5)
	 add hl,bc
	 jr nc,lookuploop
	 ; Don't allow jumping to the middle of a RAM block
	 ld a,d
	 rla
	 jr c,lookuploop
	 push ix
	  sbc hl,bc
	  push hl
	  pop iy
	  add.s hl,de
	  push de
	   ex de,hl
	   GET_BASE_ADDR_FAST
	   add hl,de
	   ld de,opcodesizes
	   ld a,(ix+7)
	   ld ix,(ix)
lookup_found_loop:
	   ; Get current opcode
	   ld e,(hl)
	   ex de,hl
	   ; Add recompiled instruction size
	   dec h
	   dec h
	   ld c,(hl)
	   add ix,bc
	   ; Add cycles
	   inc h
	   sub (hl)
	   inc h
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
	   cp -5
	   jr c,lookup_found_prefix
	   add.s a,(ix-3)
	   ASSERT_C
	   bit 2,e
	   jr nz,_ ; For CALL/RST, offsets are normal
	   inc ix  ; For RET/JR/JP, offsets are stored -1
_
	   add hl,bc
	   add iy,bc
	   jr nc,lookup_found_loop
	   jr lookup_found_loop_finish
	
recompile_trampoline:
	jr recompile
	
lookup_found_prefix:
	   ; Count CB-prefixed opcode cycles
	   sbc a,c
	   ; If we're stepping past a HALT, then continue
	   jr c,lookup_found_continue
	   ; Analyze the first byte of JIT code
	   ld.s e,(ix-2)
	   ; Check for CB prefix, as opposed to CALL, LD IXH, or LD A,A
	   bit 2,e
	   jr z,lookup_found_continue
	   inc ix
	   ; Check for CALL, as opposed to LD IXH or LD A,A
	   bit 4,e
	   jr z,lookup_found_continue
	   ld e,a
	   inc ix
	   inc hl
	   ld a,(hl)
	   dec hl
	   ; Check for BIT to adjust cycle count
	   add a,$80
	   cp $C0
	   sbc a,a
	   dec a
	   add a,e
	   jr lookup_found_continue
	
	
; Recompiles a new code block starting from a banked GB address.
;
; Inputs:  DE = banked GB address to recompile
;          IY saved on stack
; Outputs: IX = recompiled code pointer
;          A = number of cycles until block end
;          Carry is reset
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
	 or a
	
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
recompile_cache = $+1
	ld hl,0
	lea de,ix+(MAX_CACHE_FLUSHES_ALLOWED*8)+3+5
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
	ld (flush_event_smc_1),hl
	ld (flush_event_smc_2),hl
	ld hl,$D2 | (schedule_event_finish_for_call_no_schedule << 8)	;JP NC,schedule_event_finish_for_call_no_schedule
	ld (flush_event_for_call_smc),hl
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
	 
	 ; Add in padding to avoid flushes
	 ex de,hl
ram_block_padding = $+1
	 ld bc,0
	 add hl,bc
	 ex de,hl
	 ; Copy the GB opcodes for coherency
	 ld c,a
	 xor a
	 ld b,a
	 sbc hl,bc
	 inc bc
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
	push af
	 MEMSET_FAST(recompile_cache_LUT, 256, 0)
	 MEMSET_FAST(recompile_cache_LUT+256, 256, (recompile_cache_end>>8)&$FF)
	pop af
	ld hl,recompile_cache_end
	ld (recompile_cache),hl
	ret
	
	
check_coherency_helper:
	exx
	push hl
	ex af,af'
	ld c,a
	push bc
	ld de,recompile_struct
	add ix,de
	ld b,e
	ld c,(ix+5)
	inc c
	ld hl,(ix+2)
	ex.s de,hl
	GET_BASE_ADDR_FAST
	add hl,de
	ex de,hl
	ld hl,i
	ld l,(ix+8)
	ld h,(ix+9)
	sbc hl,bc
check_coherency_loop:
	ld a,(de)
	inc de
	cpi
	jr nz,_
	ld a,(de)
	inc de
	cpi
	jr nz,_
	ld a,(de)
	inc de
	cpi
	jr nz,_
	ld a,(de)
	inc de
	cpi
	jr z,check_coherency_loop
_
	jp pe,rerecompile
	; Make sure the last (complemented) byte matches
	cpl
	dec hl
	xor.s (hl)
	jr nz,rerecompile
	
check_coherency_cycles:
	pop bc
	ld a,(ix+7)
	add a,c
	jp.sis nc,coherency_return
	inc iyh
	jp.sis nz,coherency_return
	ld iyl,a
	sub c
	ld c,a
	ld hl,(ix+2)
	ex.s de,hl
	pop.s ix
	push.s ix
#ifdef VALIDATE_SCHEDULE
	call schedule_event_helper
#else
	jp schedule_event_helper
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
	
	; Get the address to copy the opcodes from
	ld c,a
	xor a
	ld b,a
	inc.s bc
	inc hl
	sbc hl,bc
	
	push hl
	 ; Get the address to copy the opcodes to
	 ld hl,i
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
	jr check_coherency_cycles
	
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
	pop bc
	pop hl
	ld a,c
	ex af,af'
	exx
	; Flush entire call stack, including interrupt returns
	ld sp,myADLstack
	ld.sis sp,myz80stack-4
	jp.s (ix)
	
	
; Inputs:  IX = struct entry
;          DE = GB opcodes start address (banked)
;          HL = block start
; Outputs: IX = struct entry
;          BC = GB opcodes base address
;          DE = JIT block end
;          HL = pointer to last opcode byte
;          A = GB opcodes size minus 1
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
	 ld bc,opgentable + MAX_OPCODE_BYTES_PER_BLOCK
	 ; Get memory region base address and check if region changes in next 256 bytes
	 ld hl,mem_region_lut
	 ld l,d
	 inc l
	 ld a,(hl)
	 dec l
	 ld l,(hl)
	 dec h
	 cp l
	 ld hl,(hl)
	 ld a,c  ;MAX_OPCODE_BYTES_PER_BLOCK
	 jr z,++_
	 ; On region change, clamp input boundary to 256-byte range
	 add a,e
	 jr nc,_
	 xor a
_
	 sub e
_
	 ld (opgen_byte_limit_smc),a
	 ex de,hl
	 add hl,de
	 ex de,hl
	 ; Define input boundary
	 add a,e
	 ; Prevent crossing memory region until bound is initially reached
	 sub MAX_BYTES_PER_OPCODE
	 ld iyh,a
	 ld a,l
	 ld (opgen_base_address_smc_1),a
	 ld a,h
	 ld (opgen_base_address_smc_2),a
	 ex (sp),hl
	 ; Cycle count while recompiling is l-iyl. Start at 0.
	 ld iyl,e
	 ex de,hl
	 ld (jump_template_struct_smc),ix
	 push ix
	  ld ix,opgenroutines
	  call opgen_next_fast
	  call m,opgen_region_overflow
	 pop ix
	 ex de,hl
	 ; Get the size of the GB opcodes (minus 1) and save it
	 ld a,l
	 sub (ix+2)
	pop bc
	sub c
	ld (ix+5),a
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
	
#macro OPCYCLE_NEXT
	ret c
	inc d
	ld e,(hl)
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)
#endmacro
	
	.block (-$)&255	
opcycleroutines:
	
	; 1b op, 3b rec, 1cc
opcycle27:
opcycle3F:
opcycleF3:
opcycleEI:
	dec ix
	; 1b op, 4b rec, 1cc
opcycleROT:
	add ix,bc
	; 1b op, 1b rec, 1cc
opcycle1byte:
	inc ix
	; 1b op, 0b rec, 1cc
opcycleNOP:
	inc de
	ex de,hl
	inc a
	ret z
	inc d
opcycle_first:
	ld e,(hl)
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)
	
opcycleCB_maybe_swap:
	; Differentiate between CB prefix and CALL
	bit 1,l
	jr nz,opcycleCB_normal
	; 1b op, 3b rec, 2cc
opcycleMEM:
opcycle33:
opcycle39:
opcycle3B:
opcycleE2:
opcycleF2:
opcycleF9:
	add ix,bc
	inc de
	ex de,hl
	add a,2
	OPCYCLE_NEXT
	
	; 1b op, 1b rec, 2cc
opcycle1byte_2cc:
	inc ix
	inc de
	ex de,hl
	add a,2
	OPCYCLE_NEXT
	
	; 2b op, 2b rec, 2cc
opcycle2byte:
	inc de
opcycleCB_normal:
	lea ix,ix+2
	inc de
	ex de,hl
	add a,2
	OPCYCLE_NEXT

	; 3b op, 5b rec, 3cc
opcycle31:
	lea ix,ix+(5-3)
	; 3b op, 3b rec, 3cc
opcycle3byte:
	add ix,bc
	ex de,hl
	add hl,bc
	add a,c
	OPCYCLE_NEXT
	
	; 2b op, 4b rec, 3cc
opcycleF8:
	lea ix,ix+(4-6)
	; 2b op, 6b rec, 3cc
opcycle36:
	add ix,bc
	; 2b op, 3b rec, 3cc
opcycleE0:
opcycleF0:
	inc de
	; 1b op, 3b rec, 3cc
opcycle34:
opcycle35:
opcycleF1:
opcyclePOP:
	add ix,bc
	inc de
	ex de,hl
	add a,c
	OPCYCLE_NEXT

opcycleCB:
	; Check for CB prefix or CALL (as opposed to LD IXH or LD A,A)
	ld.s l,(ix)
	bit 4,l
	inc de
	jr z,opcycleCB_maybe_swap
	; 2b op, 4b rec,
	lea ix,ix+4
	ld l,a
	ld a,(de)
	; 3cc if second opcode byte is between $40-$7F, 4cc otherwise
	sub $40
	add a,$C0
	ld a,l
	inc de
	ex de,hl
	adc a,c
	OPCYCLE_NEXT

	; 3b op, 5b rec, 4cc
opcycleEA:
opcycleFA:
	lea ix,ix+5
	ex de,hl
	add hl,bc
	add a,4
	OPCYCLE_NEXT

	; 2b op, 4b rec, 4cc
opcycleE8:
	inc de
	inc ix
	; 1b op, 3b rec, 4cc
opcycleC5:
opcycleD5:
opcycleE5:
opcycleF5:
	inc de
	add ix,bc
	ex de,hl
	add a,4
	OPCYCLE_NEXT
	
	; 3b op, 7b rec, 5cc
opcycle08:
	lea ix,ix+7
	ex de,hl
	add hl,bc
	add a,5
	OPCYCLE_NEXT
	
	; Bugged halt instruction
opcycleHALT:
	; Dispatch to the bugged instruction without incrementing the pointer
	ex de,hl
	inc d
	inc hl
	ld e,(hl)
	dec hl
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)
	
	; Invalid opcodes within a sub-block
opcycleINVALID:
opcycleJR:
opcycleJRcond:
opcycleJP:
opcycleJPcond:
opcycleCALL:
opcycleCALLcond:
opcycleRET:
opcycleRETcond:
opcycleRETI:
opcycleRST:
opcycleE9:
	jp runtime_error

	.echo "Opcycle routine size: ", $ - opcycleroutines
	.block 256 - ($ - opcycleroutines)

; A table indexing opcode cycle counting routines.
; All entry points live in a 256-byte space.
opcounttable:
;00
	.db opcycleNOP - opcycleroutines
	.db opcycle3byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleROT - opcycleroutines
;08
	.db opcycle08 - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleROT - opcycleroutines
;10
	.db opcycleNOP - opcycleroutines
	.db opcycle3byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleROT - opcycleroutines
;18
	.db opcycleJR - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleROT - opcycleroutines
;20
	.db opcycleJRcond - opcycleroutines
	.db opcycle3byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycle27 - opcycleroutines
;28
	.db opcycleJRcond - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
;30
	.db opcycleJRcond - opcycleroutines
	.db opcycle31 - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle33 - opcycleroutines
	.db opcycle34 - opcycleroutines
	.db opcycle35 - opcycleroutines
	.db opcycle36 - opcycleroutines
	.db opcycle1byte - opcycleroutines
;38
	.db opcycleJRcond - opcycleroutines
	.db opcycle39 - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle3B - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycle3F - opcycleroutines
;40
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;48
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;50
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;58
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;60
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;68
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;70
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleHALT - opcycleroutines
	.db opcycleMEM - opcycleroutines
;78
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleNOP - opcycleroutines
;80
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;88
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;90
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;98
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;A0
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;A8
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;B0
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;B8
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;C0
	.db opcycleRETcond - opcycleroutines
	.db opcyclePOP - opcycleroutines
	.db opcycleJPcond - opcycleroutines
	.db opcycleJP - opcycleroutines
	.db opcycleCALLcond - opcycleroutines
	.db opcycleC5 - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;C8
	.db opcycleRETcond - opcycleroutines
	.db opcycleRET - opcycleroutines
	.db opcycleJPcond - opcycleroutines
	.db opcycleCB - opcycleroutines
	.db opcycleCALLcond - opcycleroutines
	.db opcycleCALL - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;D0
	.db opcycleRETcond - opcycleroutines
	.db opcyclePOP - opcycleroutines
	.db opcycleJPcond - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleCALLcond - opcycleroutines
	.db opcycleD5 - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;D8
	.db opcycleRETcond - opcycleroutines
	.db opcycleRETI - opcycleroutines
	.db opcycleJPcond - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleCALLcond - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;E0
	.db opcycleE0 - opcycleroutines
	.db opcyclePOP - opcycleroutines
	.db opcycleE2 - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleE5 - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;E8
	.db opcycleE8 - opcycleroutines
	.db opcycleE9 - opcycleroutines
	.db opcycleEA - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;F0
	.db opcycleF0 - opcycleroutines
	.db opcycleF1 - opcycleroutines
	.db opcycleF2 - opcycleroutines
	.db opcycleF3 - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleF5 - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;F8
	.db opcycleF8 - opcycleroutines
	.db opcycleF9 - opcycleroutines
	.db opcycleFA - opcycleroutines
	.db opcycleEI - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
	
; A table of recompiled opcode sizes. Does not apply to block-ending opcodes.
opcoderecsizes:
	.db 0,3,3,1,1,1,2,4
	.db 7,1,3,1,1,1,2,4
	.db 0,3,3,1,1,1,2,4
	.db 0,1,3,1,1,1,2,4
	.db 19-1,3,3,1,1,1,2,3
	.db 19-1,1,3,1,1,1,2,1
	.db 19-1,5,3,3,3,3,6,1
	.db 19-1,3,3,3,1,1,2,3
	
	.db 0,1,1,1,1,1,3,1
	.db 1,0,1,1,1,1,3,1
	.db 1,1,0,1,1,1,3,1
	.db 1,1,1,0,1,1,3,1
	.db 1,1,1,1,0,1,3,1
	.db 1,1,1,1,1,0,3,1
	.db 3,3,3,3,3,3,1,3
	.db 1,1,1,1,1,1,3,0
	
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	.db 1,1,1,1,1,1,3,1
	
	.db 12-1,3,19-1,0,10,3,2,6
	.db 12-1,0,19-1,2,10,8,2,6
	.db 12-1,3,19-1,0,10,3,2,6
	.db 12-1,0,19-1,0,10,0,2,6
	.db 3,3,3,0,0,3,2,6
	.db 4,0,5,0,0,0,2,6
	.db 3,3,3,3,0,3,2,6
	.db 4,3,5,3,0,0,2,6
	
; A table of Game Boy opcode cycles. Block-ending opcodes are set to 0.
; Conditional branches are assumed not taken.
; Prefix opcodes (i.e. CB) and HALT are set to -1, for efficient detection.
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
	.db 2,2,2,2,2,2,-1,2
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
	
; A table of Game Boy opcode sizes.
; The HALT opcode defaults to 3 bytes, to include the following opcode bytes
; for any instruction that could be affected by the HALT bug.
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
	.db 1,1,1,1,1,1,3,1
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
	.db opgenROT - opgenroutines
;08
	.db opgen08 - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenROT - opgenroutines
;10
	.db opgenNOP - opgenroutines
	.db opgen3byte - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenROT - opgenroutines
;18
	.db opgenJR - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgenMEM - opgenroutines
	.db opgen1byte_2cc - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgenROT - opgenroutines
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
	.db opgen3F - opgenroutines
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
	.db opgenC5 - opgenroutines
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
	.db opgenD5 - opgenroutines
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
	.db opgenE5 - opgenroutines
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
	.db writeIEhandler - mem_write_port_routines
;00
	.db writeP1handler - mem_write_port_routines
	.db WRITE_PORT_NO_HANDLER_DIRECT
	.db writeSChandler - mem_write_port_routines
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db writeDIVhandler - mem_write_port_routines
	.db writeTIMAhandler - mem_write_port_routines
	.db writeTMAhandler - mem_write_port_routines
	.db writeTAChandler - mem_write_port_routines
;08
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db writeIFhandler - mem_write_port_routines
;10
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
;18
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db WRITE_PORT_NO_HANDLER_IGNORE
;20
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db write_audio_handler - mem_write_port_routines
	.db writeNR52handler - mem_write_port_routines
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
	.db writeLCDChandler - mem_write_port_routines
	.db writeSTAThandler - mem_write_port_routines
	.db write_scroll_handler - mem_write_port_routines
	.db write_scroll_handler - mem_write_port_routines
	.db WRITE_PORT_NO_HANDLER_IGNORE
	.db writeLYChandler - mem_write_port_routines
	.db writeDMAhandler - mem_write_port_routines
	.db writeBGPhandler - mem_write_port_routines
;48
	.db write_scroll_handler - mem_write_port_routines
	.db write_scroll_handler - mem_write_port_routines
	.db write_scroll_handler - mem_write_port_routines
	.db write_scroll_handler - mem_write_port_routines
mem_write_port_handler_table_end:
	
	.block (128-11-$)&255
; Information for stack jump targets when RTC is mapped
; The base address and bound information are omitted,
; those are taken from the main CRAM stack info
	; push jump targets
	.db do_push_rtc - (do_push_jump_smc_1 + 1)
	.db do_push_for_call_rtc - (do_push_for_call_jump_smc_1 + 1)
	; pop jump targets
	.db do_pop_rtc - (do_pop_jump_smc_1 + 1)
	.db do_pop_for_ret_overflow - (do_pop_for_ret_jump_smc_1 + 1)
	.db ophandlerF1_pop_rtc - (ophandlerF1_jump_smc_1 + 1)
	; callstack pop overflow check
	.db $00
	jr $+(callstack_ret_overflow - (callstack_ret_check_overflow_smc+1))
	.block 3

; A table of information for stack memory areas
stack_bank_info_table:
hram_unbanked_base:
	.dl hram_base	;$ff80-$ffff
	.db ($ff+1)&$ff,($ff+1)&$ff
	; push jump targets
	.db do_push_z80 - (do_push_jump_smc_1 + 1)
	.db do_push_for_call_z80 - (do_push_for_call_jump_smc_1 + 1)
	; pop jump targets
	.db do_pop_z80 - (do_pop_jump_smc_1 + 1)
	.db do_pop_for_ret_z80 - (do_pop_for_ret_jump_smc_1 + 1)
	.db ophandlerF1_pop_z80 - (ophandlerF1_jump_smc_1 + 1)
	; callstack pop overflow check
	.db $40
	jr nz,$+(callstack_ret_overflow - (callstack_ret_check_overflow_smc+1))
	.block 3

; The start address of Game Boy ROM page 0.
rom_start:
	.dl 0	;$0000-$3fff
	.db $00+1,$3f+1
	; push jump targets
	.db do_push_cart - (do_push_jump_smc_1 + 1)
	.db do_push_for_call_cart - (do_push_for_call_jump_smc_1 + 1)
	; pop jump targets
	.db do_pop_adl - (do_pop_jump_smc_1 + 1)
	.db do_pop_for_ret_adl - (do_pop_for_ret_jump_smc_1 + 1)
	.db ophandlerF1_pop_adl - (ophandlerF1_jump_smc_1 + 1)
	; callstack pop overflow check
	.db $FF
	jr z,$+(callstack_ret_bound - (callstack_ret_check_overflow_smc+1))
	.block 3

hmem_unbanked_base:
	.dl hram_base	;$fe00-$ffff
	.db $fe+1,($ff+1)&$ff
	; push jump targets
	.db do_push_hmem - (do_push_jump_smc_1 + 1)
	.db do_push_for_call_hmem - (do_push_for_call_jump_smc_1 + 1)
	; pop jump targets
	.db do_pop_hmem - (do_pop_jump_smc_1 + 1)
	.db do_pop_for_ret_overflow - (do_pop_for_ret_jump_smc_1 + 1)
	.db ophandlerF1_pop_hmem - (ophandlerF1_jump_smc_1 + 1)
	; callstack pop overflow check
	.db $00
	jr $+(callstack_ret_overflow - (callstack_ret_check_overflow_smc+1))
	.block 3

; The address of the currently banked ROM page, minus $4000.
; Can be indexed directly by the Game Boy address.
rom_bank_base:
	.dl 0	;$4000-$7fff
	.db $40+1,$7f+1
	; push jump targets
	.db do_push_cart - (do_push_jump_smc_1 + 1)
	.db do_push_for_call_cart - (do_push_for_call_jump_smc_1 + 1)
	; pop jump targets
	.db do_pop_adl - (do_pop_jump_smc_1 + 1)
	.db do_pop_for_ret_adl - (do_pop_for_ret_jump_smc_1 + 1)
	.db ophandlerF1_pop_adl - (ophandlerF1_jump_smc_1 + 1)
	; callstack pop overflow check
	.db $FF
	jr z,$+(callstack_ret_bound - (callstack_ret_check_overflow_smc+1))
	.block 3

vram_bank_base:
	.dl vram_base	;$8000-$9fff
	.db $80+1,$9f+1
	; push jump targets
	.db do_push_vram - (do_push_jump_smc_1 + 1)
	.db do_push_for_call_vram - (do_push_for_call_jump_smc_1 + 1)
	; pop jump targets
	.db do_pop_adl - (do_pop_jump_smc_1 + 1)
	.db do_pop_for_ret_adl - (do_pop_for_ret_jump_smc_1 + 1)
	.db ophandlerF1_pop_adl - (ophandlerF1_jump_smc_1 + 1)
	; callstack pop overflow check
	.db $FF
	jr z,$+(callstack_ret_bound - (callstack_ret_check_overflow_smc+1))
	.block 3

; The address of the currently banked RAM page, minus $A000.
; Can be indexed directly by the Game Boy address.
cram_bank_base:
	.dl 0	;$a000-$bfff
	.db $a0+1,$bf+1
	; push jump targets
	.db do_push_adl - (do_push_jump_smc_1 + 1)
	.db do_push_for_call_adl - (do_push_for_call_jump_smc_1 + 1)
	; pop jump targets
	.db do_pop_adl - (do_pop_jump_smc_1 + 1)
	.db do_pop_for_ret_adl - (do_pop_for_ret_jump_smc_1 + 1)
	.db ophandlerF1_pop_adl - (ophandlerF1_jump_smc_1 + 1)
	; callstack pop overflow check
	.db $FF
	jr z,$+(callstack_ret_bound - (callstack_ret_check_overflow_smc+1))
	.block 3

wram_unbanked_base:
	.dl wram_base	;$c000-$dfff
	.db $c0+1,$df+1
	; push jump targets
	.db do_push_adl - (do_push_jump_smc_1 + 1)
	.db do_push_for_call_adl - (do_push_for_call_jump_smc_1 + 1)
	; pop jump targets
	.db do_pop_adl - (do_pop_jump_smc_1 + 1)
	.db do_pop_for_ret_adl - (do_pop_for_ret_jump_smc_1 + 1)
	.db ophandlerF1_pop_adl - (ophandlerF1_jump_smc_1 + 1)
	; callstack pop overflow check
	.db $FF
	jr z,$+(callstack_ret_bound - (callstack_ret_check_overflow_smc+1))
	.block 3

wram_mirror_unbanked_base:
	.dl wram_base-$2000	;$e000-$fdff
	.db $e0+1,$fd+1
	; push jump targets
	.db do_push_adl - (do_push_jump_smc_1 + 1)
	.db do_push_for_call_adl - (do_push_for_call_jump_smc_1 + 1)
	; pop jump targets
	.db do_pop_adl - (do_pop_jump_smc_1 + 1)
	.db do_pop_for_ret_adl - (do_pop_for_ret_jump_smc_1 + 1)
	.db ophandlerF1_pop_adl - (ophandlerF1_jump_smc_1 + 1)
	; callstack pop overflow check
	.db $FF
	jr z,$+(callstack_ret_bound - (callstack_ret_check_overflow_smc+1))
	.block 3

mem_region_lut:
	.fill $40, rom_start & $FF
	.fill $40, rom_bank_base & $FF
	.fill $20, vram_bank_base & $FF
	.fill $20, cram_bank_base & $FF
	.fill $20, wram_unbanked_base & $FF
	.fill $1E, wram_mirror_unbanked_base & $FF
	.fill $02, hmem_unbanked_base & $FF
	
block_bridge_template:
	.assume adl=0
	nop
	call decode_block_bridge
	.dw cycle_overflow_for_bridge
	.assume adl=1
block_bridge_template_size = $-block_bridge_template
	
jump_template:
	.assume adl=0
	call decode_jump
	jr nc,$+1 ;RST_CYCLE_CHECK
	ex af,af'
jump_template_struct_smc = $
	.dw 0
	.assume adl=1
jump_template_size = $-jump_template
	.db 0 ;padding for struct pointer write
	
opgen_region_overflow:
	; Check if a memory region crossing should be handled
opgen_byte_limit_smc = $+1
	ld a,0
	cp MAX_OPCODE_BYTES_PER_BLOCK
	jr nz,opgen_cross_mem_region
	; Special case for RET/RETI/JP HL; emit directly to prevent redundant jumps
	ld a,c
	xor $F9
	jr z,_	; Exclude LD SP,HL
	and $CF
	jr z,opgen_region_overflow_direct
_
opgen_emit_block_bridge:
	; Emit a block bridge
	push hl
opgen_emit_block_bridge_pushed:
	 ld hl,block_bridge_template
	 ld bc,block_bridge_template_size
	 jr nc,_
	 ; Memory region bridge is one byte smaller
	 inc hl
	 dec c
_
	 ldir
	 ld c,5
	 add hl,bc
	 ld c,jump_template_size - 5
	 ldir
	pop hl
	ex de,hl
	ld (hl),c ; Default to 0 cycles, if not overwritten by the cycle count
	call opgen_emit_gb_address
	; Don't include the next opcode in the block,
	; but still count cycles up to the next opcode
	ld a,e
	dec de
	jp opgen_reset_cycle_count_any
opgen_region_overflow_direct:
	ld a,(bc)
	ld ixl,a
	jp (ix)
	
opgen_cross_mem_region_loop:
	inc b
	; Emit exactly one instruction by skipping the initial bound check
	call opgen_next_no_bound_check
	; Return if it was a block-ending instruction
	ret p
opgen_cross_mem_region:
	; Get the low byte of PC (minus 1)
	ld a,(opgen_base_address_smc_1)
	cpl
	add a,l
	ld ixl,a
	; If the crossing point was reached exactly, emit a bridge
	add a,1
	jr c,opgen_emit_block_bridge
	; Get size of next instruction
	dec b
	ld a,(bc)
	; Check if next instruction overlaps the boundary
	add a,ixl
	jr nc,opgen_cross_mem_region_loop
	; Emit a memory-region-overlapping instruction
	; Note that such an instruction is *always* 2 or 3 bytes in size
	; Put the instruction size in BC
	ld a,(bc)
	ld bc,$CD
	ex de,hl
	ld (hl),c ;CALL
	ld c,a
	inc hl
	; Check the number of bytes in the new memory region (1 or 2 bytes)
	jr nz,++_
	; One byte is in the new memory region
	rra
	jr c,_
	; One byte is in the old memory region
	ld (hl),handle_overlapped_op_1_1 & $FF
	inc hl
	ld (hl),handle_overlapped_op_1_1 >> 8
	jr +++_
_
	; Two bytes are in the old memory region
	ld (hl),handle_overlapped_op_2_1 & $FF
	inc hl
	ld (hl),handle_overlapped_op_2_1 >> 8
	jr ++_
_
	; Two bytes are in the new memory region
	; One byte is always in the old memory region
	ld (hl),handle_overlapped_op_1_2 & $FF
	inc hl
	ld (hl),handle_overlapped_op_1_2 >> 8
_
	inc hl
	; Reset the cycle count to prevent the scheduler from crossing this point
	call opgen_reset_cycle_count
	ex de,hl
	; Save the current output pointer to use as the opcode pointer
	push de
	 ; Check the number of bytes in the old memory region
	 ; IXL is -3 if 2 bytes, or -2 if 1 byte
	 ld a,ixl
	 rra
	 ; Copy those 1 or 2 bytes
	 jr nc,_
	 ldi
_
	 ; Save the pointer to the last byte to use as the effective block end
	 ; This is essential to include all bytes from the old memory region
	 ; for SMC checks in RAM code, but also not cross the end of the region
	 push hl
	  ldi
	  ; Now HL points precisely to the end of the old memory region,
	  ; and DE points to the corresponding address in the copied opcode
	  ; Update the base address to correspond to the copied opcode
	  ld a,e
	  ld (opgen_base_address_smc_1),a
	  ld a,(opgen_base_address_smc_2)
	  sub h
	  add a,d
	  ld (opgen_base_address_smc_2),a
	  ; Get the MSB of the raw GB address
	  sub d
	  neg
	  ; Get the base address of the new memory region
	  ld hl,mem_region_lut
	  ld l,a
	  ld l,(hl)
	  dec h
	  ld hl,(hl)
	  ; Get the pointer to the start of the region
	  push bc
	   ld c,b ;C=0
	   ld b,a
	   add hl,bc
	  pop bc
	  ; Copy the remaining opcode bytes from the new memory region
	  ldir
	  ; Store the address MSB after the opcode bytes
	  ld (de),a
	 ; Restore the pointer to the start of the copied opcode
	 pop hl
	 ex (sp),hl
	 ex de,hl
	 call opgen_emit_overlapped_opcode
	 ; If the opcode did not end the block, emit a bridge to the next region
	 scf
	 call m,opgen_emit_block_bridge
	; Restore the pointer to the end of the old memory region
	pop de
	ret
	
opgen_emit_overlapped_opcode:
	inc hl
	; Update the bound checker to allow a single opcode
	ld iyh,e
	; Start the cycle count from this opcode
	ld iyl,e
	; Save the cycle count immediately before the recompiled code
	ld (opgen_last_cycle_count_smc),hl
	inc hl
	ex de,hl
	; Check for CB-prefix opcode
	ld a,(hl)
	cp $CB
	jr nz,_
	; Ensure a consistent generated size for CB opcodes
	; This allows recompilation in-place in all cases
	inc hl
	ld a,(hl)
	dec hl
	and $07
	cp $06
	; No adjustment for three-byte operations
	jr z,_
	; Emit NOP before two-byte operations
	xor a
	ld (de),a
	inc de
_
	; Generate the opcode
	jp opgen_next
	
_opgenRST:
	ex de,hl
	ld (hl),$CD
	inc hl
	; Translate the opcode into a routine entry offset
	ld a,c
	rrca
	rrca
	cpl
	add a,c
	add a,(do_rst_38+1) & $FF
	ld (hl),a
	adc a,((do_rst_38+1) >> 8) - 1
	sub (hl)
	inc hl
	ld (hl),a
	call opgen_reset_cycle_count
	dec iyl
	dec iyl
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
	inc hl
	call opgen_reset_cycle_count
	inc de
	inc de
opgen_finish_rst:
	dec iyl
	inc de
	inc hl
	ld (opgen_last_cycle_count_smc),hl
	call opgen_emit_gb_address
	ex de,hl
	jp opgen_next_fast

opgen_emit_jump:
	ld a,c
	push hl
	 ld hl,jump_template
	 ld bc,jump_template_size
	 ldir
	pop hl
	ld c,a
	ex de,hl
	ld (hl),c
	call opgen_emit_gb_address
	call opgen_reset_cycle_count
	inc de
	ld a,c
	rla
	ret nc
	inc de
	ret
	
opgen_emit_ret:
	ex de,hl
	call opgen_reset_cycle_count
	bit 0,c
	jr z,opgen_finish_cond_ret
	ld (hl),$D9	;EXX
	inc hl
	ld (hl),$D1	;POP DE
	inc hl
	ld (hl),c	;RET
	inc hl
	ret
	
opgen_finish_cond_ret:
	dec iyl
	ld a,c
	xor $C0 ^ $C2
	ld (hl),a	;JP cc,ophandlerRETcond
	inc hl
	ld (hl),ophandlerRETcond & $FF
	inc hl
	ld (hl),ophandlerRETcond >> 8
	inc hl
	ld (hl),a	;JP cc,gb_address
	; If block is ending, combine the subblock and end-of-block bridges
	; Also avoid emitting the redundant Game Boy address
	ld a,e
	sub iyh
	rla
	jr nc,opgen_emit_subblock_combined_bridge
	call opgen_emit_gb_address
	
opgen_emit_subblock_bridge:
	ld (hl),$08	;EX AF,AF'
	inc hl
	ld (hl),$C6	;ADD A,
	inc hl
	ld (opgen_last_cycle_count_smc),hl
	inc hl
	ld (hl),$38	;JR C,
	inc hl
	ld (hl),RST_CYCLE_CHECK
	inc hl
	ld (hl),$08	;EX AF,AF'
	jp opgen_next_swap_skip
	
opgen_emit_subblock_combined_bridge:
	inc de
	; Subtract 1 from the cycle count, will be readjusted at decode time
	inc iyl
	ex de,hl
	push hl
	 ; Place the cycle count of the untaken branch in the associated info
	 ld hl,8
	 add hl,de
	 ld (opgen_last_cycle_count_smc),hl
	 ; Emit the slower bridge that preserves the associated info
	 scf
	 jp opgen_emit_block_bridge_pushed
	
opgen_emit_gb_address:
	inc hl
opgen_emit_gb_address_noinc:
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
	; No JIT opcode can begin with an absolute jump;
	; this is to prevent the memory cycle offset resolver from
	; confusing the end of the preceding instruction with a
	; dispatch for a CALL/RST/interrupt
	ld (hl),$08 ;EX AF,AF'
	inc hl
	ld (hl),$C3	;JP
opgenblockend_invalid_finish:
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	pop de
	
opgen_reset_cycle_count:
	ld a,e
opgen_reset_cycle_count_any:
	sub iyl
opgen_last_cycle_count_smc = $+1
	ld (0),a
	ld iyl,e
port_access_trampoline_count_smc = $+1
	ld a,0
	or a
	ret z
	push bc
	 push de
	  push hl
	   ld b,a
	   sbc hl,hl
	   ex de,hl
	   ld hl,(opgen_last_cycle_count_smc)
	   ld c,(hl)
	   ld hl,(z80codebase+memroutine_next)
_
	   ld a,(hl)
	   ld (hl),$C3
	   cpl
	   ld e,a
	   res 0,e
	   add hl,de
	   inc hl
	   xor e
	   ld e,a
	   ld a,(hl)
	   sub c
	   ld (hl),a
	   add hl,de
	   inc hl
	   djnz -_
	  pop hl
	 pop de
	pop bc
	xor a
	ld (port_access_trampoline_count_smc),a
	ret
	
opgenblockend_invalid:
	push hl
	ex de,hl
	ld (hl),$CD	;CALL
	ld de,Z80InvalidOpcode
	jr opgenblockend_invalid_finish
	
_opgen76:
	ex de,hl
	ld (hl),$CD
	inc hl
	ld (hl),decode_halt & $FF
	inc hl
	ld (hl),decode_halt >> 8
	inc hl
	; The next three bytes will be decoded to the cycle count and JIT address
	; of the non-bugged continuation block
	inc hl
	inc hl
	; Emit the address following the HALT instruction
	inc de
	call opgen_emit_gb_address
	; Read the first byte of the bugged instruction
	ld a,(de)
	ld c,a
	; Reset the cycle count at the start of the HALT
	dec de
	call opgen_reset_cycle_count
	ld (opgen_last_cycle_count_smc),hl
	inc hl
	; Update the bound checker to allow a single opcode
	ld iyh,e
	; Disable any memory region crossing logic after the opcode is emitted
	ld a,MAX_OPCODE_BYTES_PER_BLOCK
	ld (opgen_byte_limit_smc),a
	dec iyl ; Include one extra cycle for the HALT instruction
	ex de,hl
	; Generate the opcode
	ld a,(bc)
	ld ixl,a
	; Check if we need to special-case a copy of the first opcode byte
	sub opgen_next_fast & $FF
	add a,opgen_next_fast - opgen76
	jr nc,_
	; If the bugged opcode is a HALT, it will bug eternally
	; To avoid infinite code generation, instead we emit an invalid opcode
	jr z,opgenblockend_invalid
	; For opgen3byte_low, increment the B register
	and 1
	add a,b
	ld b,a
	; Write the first byte manually
	ld a,c
	ld (de),a
	inc de
	inc hl
	; Adjust the entry point
	; For opgen3byte_low, this causes an AND B to be executed, which is harmless
	lea ix,ix+2
_
	jp (ix)
	
opgenroutinecall2byte_5cc:
	dec iyl
	dec iyl
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
	dec iyl
opgenroutinecall1byte_3cc:
	dec iyl
	inc hl
	ld a,$CD
	ld (de),a
	inc de
	ex (sp),hl
	ldi
	ldi
	pop hl
	jp opgen1byte
	
_opgen08:
	ld a,$D9	;EXX
	ld (de),a
	inc de
	ld a,$11	;LD DE,nnnn
	ld (de),a
	inc de
	inc hl
	ldi
	ldi
	dec hl
	call opgenroutinecall_3cc
	.dw ophandler08
	
_opgen36:
	ld a,$FD	;LD IYL,nn
	ld (de),a
	inc de
	ld a,$2E
	ld (de),a
	inc de
	inc hl
	ldi
	ld c,$76
	jp opgen36_finish
	
_opgenCB_mem:
	ex de,hl
	dec hl
	ld (hl),$DD ;LD IXH,
	inc hl
	ld (hl),$26
	inc hl
	dec iyl
	ld a,c
	; Check for RES/SET
	add a,$87 ; Use A register in implementation
	jr c,_
	add a,2-7 ; Use D register in implementation
	; Check for BIT
	cp $C0
	jr nc,++_
_
	dec iyl
_
	rra ; Reset top bit only for BIT instructions
	xor ($80 ^ $30) >> 1 ; Revert to original instruction type
	ld (hl),a
	inc hl
	ld (hl),RST_BITS
	jp opgen_next_swap_skip
	
_opgenCB_swap:
	ex de,hl
	dec hl
	or c
	jr nz,_
	ld (hl),$7F ;LD A,A
	inc hl
	dec iyl
	dec iyl
_
	ld (hl),$CD
	inc hl
	ld b,do_swap_b - do_swap_c
	mlt bc
	ld a,do_swap_hl & $FF
	sub c
	ld (hl),a
	inc hl
	ld a,do_swap_hl >> 8
	sbc a,b
	ld (hl),a
	jp opgen_next_swap_skip
	
opgenroutinecall_4cc:
	dec iyl
opgenroutinecall_3cc:
	dec iyl
opgenroutinecall_2cc:
	dec iyl
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
	dec iyl
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
	  res 5,b ; Handle mirroring
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
	dec iyl
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
	cp 1
	jr c,opgenHRAMwrite
	jr z,opgenHRAMignore
	ld b,mem_write_port_routines >> 8
	; Set Z flag and reset C flag for scroll write
	cp write_scroll_handler - mem_write_port_routines
	jr z,emit_port_handler_trampoline
	; Set Z flag for audio write, set C flag for reschedulable write
	cp write_audio_handler - mem_write_port_routines
emit_port_handler_trampoline:
	push hl
	 push bc
	  push de
	   ex de,hl
	   ld hl,(z80codebase+memroutine_next)
	   ; If Z is set, routine size is 7 bytes
	   ; If C is set, routine size is 8 bytes, else 6 bytes
	   ; Subtract the routine size minus 1, preserve the Z flag in bit 0 of C,
	   ; and preserve the C flag in bit 1 of C
	   ld bc,-6
	   jr z,_
	   dec c
	   jr c,_
	   inc c
	   inc c
_
	   add hl,bc ; Sets carry
	   ; Compare to current JIT output pointer to ensure no overflow
	   ; can happen into the previous block
	   ; Also subtracts one extra byte
	   sbc hl,de
	   jr c,emit_port_handler_trampoline_overflow
	   add hl,de
	   ld (z80codebase+memroutine_next),hl
	   ; Temporarily save the byte count instead of the JP,
	   ; so the cycles can be fixed up at the end of the block
	   ld (hl),c
	   inc hl
	   ld (hl),(ERROR_CATCHER >> 8) & $FF
	   inc hl
	   ld (hl),ERROR_CATCHER >> 16
	   inc hl
	  pop de
	  ; Emit the address following the instruction
	  ld ixl,a
	  inc de
	  bit 1,c
	  call z,opgen_emit_gb_address_noinc
	  dec de
	  ; Get the number of cycles from the start of the sub-block
	  ld a,e
	  sub iyl
	  ; Restore the Z flag input
	  bit 0,c
	 pop bc
	 push hl
	  ld (hl),$DD
	  inc hl
	  ld (hl),$2E ;LD IXL,
	  jr nz,_
	  ld (hl),$21 ;LD IX,
	  inc hl
	  ld (hl),a ; Cycles
	  ld a,c ; addr
_
	  inc hl
	  ld (hl),a ; Cycles or addr
	  inc hl
	  ; JP routine (preserve byte count to be overwritten with JP)
	  inc hl
	  ld a,ixl
	  ld (hl),a
	  inc hl
	  ld (hl),b
	  ld hl,port_access_trampoline_count_smc
	  inc (hl)
	 pop bc
	pop hl
	ld (hl),$CD ;CALL addr
	jr _
	
opgenHRAMwrite:
	ld (hl),$32 ;LD (addr),A
_
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
emit_port_handler_trampoline_overflow:
	  pop de
	 pop bc
	pop hl
	; Make sure the overflow is detected because the code is invalid
	ld (z80codebase+memroutine_next),hl
	; Also disable fixups at block end
	xor a
	ld (port_access_trampoline_count_smc),a
opgenHRAMignore:
	xor a
	ld (hl),a	;NOP
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
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
	dec iyl
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
	 res 5,b ; Handle mirroring
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
	dec iyl
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
	cp IF*2 & $FF
	jr nz,_
	ld bc,readIFhandler
	jr opgenHMEMreadroutine
_
	cp LY*2 & $FF
	jr nz,_
	ld bc,readLYhandler
	jr opgenHMEMreadroutine
_
	cp STAT*2 & $FF
	jr nz,opgenHRAMread
	ld bc,readSTAThandler
opgenHMEMreadroutine:
	or a ; Reset Z flag and C flag
	ld a,c
	jp emit_port_handler_trampoline
	
opgenHRAMread:
	ld (hl),$3A ;LD A,(addr)
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip