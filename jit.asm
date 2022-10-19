#define RST_GET_HL_READ_PTR $C7+r_get_hl_read_ptr
#define RST_GET_HL_READWRITE_PTR $C7+r_get_hl_readwrite_ptr
#define RST_GET_HL_HRAM_PTR $C7+r_get_hl_hram_ptr
#define RST_EVENT $C7+r_event
#define RST_CALL $C7+r_call
#define RST_CYCLE_CHECK $C7+r_cycle_check

#define RAM_PREFIX_SIZE 7
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
#ifdef FASTLOG
	 FASTLOG_EVENT(JIT_FLUSH, 0)
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
	 ld hl,z80codebase+trampoline_end
	 ld (z80codebase+trampoline_next),hl
	 ld de,ERROR_CATCHER
	 ld (hl),de
	 ; Empty the recompiled code mapping cache
	 ld hl,recompile_cache_end
	 ld (recompile_cache),hl
	 ; Fill unused memory with DI to catch bad execution
	 MEMSET_FAST(z80codebase+jit_start, trampoline_end - jit_start, $F3)
	 ; Invalidate the memory routines, recompile index, and recompile cache LUT
	 MEMSET_FAST(recompile_index_LUT, $0300, 0)
	 MEMSET_FAST(recompile_cache_LUT+256, 256, (recompile_cache_end>>8)&$FF)
	 ; Reset the event address
	 ld hl,event_value
	 ld.sis (event_address),hl
	 ; Reset the max allowed cache flushes
	 ld a,MAX_CACHE_FLUSHES_ALLOWED
	 ld (cache_flushes_allowed),a
	 ; Reset the RST target caches
	 ld ix,z80codebase+do_rst_00
	 ld de,decode_rst
	 ld b,8
	 xor a
_
	 ld hl,(ix+2)
	 ld.s (hl),a ;0-cycle dispatch
	 inc hl
	 inc hl
	 ld.s (hl),de ;decode_rst
	 lea ix,ix+do_rst_08-do_rst_00
	 djnz -_
	 ; Resolve the interrupt target caches
	 ld hl,z80codebase+dispatch_vblank+2
	 ld d,a
	 ld e,$40
	 dec a
	 ld (recompile_cycle_offset_sp),a
_
	 push de
	  push hl
	   call lookup_code
	  pop hl
	 pop de
	 ld.s (hl),ix
	 dec hl
	 dec hl
	 ; Add 5 cycles for taken interrupt
	 add a,5
	 ld (hl),a
	 inc hl
	 ld a,e
	 add a,8
	 ld e,a
	 sla l
	 jp p,-_
	pop de
	ret
	
; Gets a 24-bit unique identifier for a given Game Boy address, using the current GB memory map.
;
; Inputs:  DE = GB address
; Outputs: DE = GB address plus (bank << 24), depending on region
; Destroys AF, HL
get_banked_address:
	ld a,d
	add a,$40
	ret po
	ld hl,(z80codebase+curr_rom_bank-2)
	ld h,d
	ld l,e
	ex de,hl
	ret
	
; Looks up the containing code block for a given code pointer and determines
; whether it is executing on external or internal bus. This is used for
; emulating open bus reads.
;
; Inputs: HL = 16-bit Z80 code pointer
; Outputs: C flag set if on internal bus
;          A = $FF if C flag is set, for convenience
; Destroys: A
lookup_code_bus:
	push ix
	 push de
	  push hl
	   ld ix,recompile_struct
	   ld de,recompile_index_LUT
	   ld e,h
	   ld a,(de)
	   ld ixl,a
	   inc d
	   ld a,(de)
	   ld ixh,a
	   ld a,l
	   cpl
	   ld e,a
	   ld a,h
	   cpl
	   ld d,a
	   call lookup_code_block_loop
	  pop hl
	 pop de
	 ; For now, just treat OAM/HRAM as internal
	 ld a,(ix+3)
	 add a,2
	 sbc a,a
	pop ix
	ret.l
	
do_pop_instr_get_cycle_offset_helper:
	push ix
	 push de
	  call lookup_gb_code_address
	 pop de
	 ; Get the cycle offset of the first pop read
	 cpl
	 dec a
	 ld e,a
	pop ix
	pop.s bc
	jp.sis do_pop_instr_get_cycle_offset_return
	
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
	ld a,(exitReason)
#endif
#ifdef FASTLOG
	exx
	ex af,af'
	push af
	push bc
	push de
	push hl
	exx
	ex af,af'
	push af
	push bc
	push de
	push hl
	push ix
	push iy
#endif
	call lookup_gb_code_address
	call get_banked_address
	ld (errorArg),de
#ifdef FASTLOG
	push de
	FASTLOG_EVENT(INVALID_OPCODE, 33)
#endif
#ifdef DEBUG
	push bc
	 APRINTF(InvalidOpcodeErrorMessage)
	pop bc
#endif
	ld a,(ERROR_INVALID_OPCODE << 2) + 5
	jr runtime_error_finish
runtime_error:
#ifdef DEBUG
	; Open debugger on CEmu
	ld (exitReason),a
	ld a,2
	ld ($FFFFFF),a
	ld a,(exitReason)
#endif
#ifdef FASTLOG
	exx
	ex af,af'
	push af
	push bc
	push de
	push hl
	exx
	ex af,af'
	push af
	push bc
	push de
	push hl
	push ix
	push iy
	FASTLOG_EVENT(RUNTIME_ERROR, 30)
#endif
#ifdef DEBUG
	APRINTF(RuntimeErrorMessage)
#endif
	ld a,(ERROR_RUNTIME << 2) + 5
runtime_error_finish:
	ld (exitReason),a
	; Temporarily prevent auto state saving because state is unrecoverable
	xor a
#ifdef FASTLOG
	call fastlog_dump_to_save
#endif
	ld (should_auto_save),a
	AJUMP(ExitEmulationWithoutState)
	
lookup_gb_found_start:
	ld a,(ix+7)
	bit 7,d
	ret z
	xor a
	ret
	
lookup_gb_found_abs_read_write:
	  ; Add a cycle for the memory access
	  dec a
	  ; Get the real JIT pointer in HL
	 pop bc
	 push bc
	  push hl
	   lea hl,ix
	   add hl,bc
	   ld bc,3
	   ; Check for LDH
	   bit 1,e
	   jr z,lookup_gb_found_abs_read_write_high
	   ; Add another cycle
	   dec a
	   ; Check the first JIT byte for short read/write
	   ; This checks for a LD (nn),A or JR instruction,
	   ; as opposed to .LIL, LD L,A, or EXX
	   bit.s 0,(hl)
	   jr z,lookup_gb_pop_add
	   ; Check the second JIT byte for long read/write
	   ; This checks for a LD (nnn),A or LD A,(nnn) instruction,
	   ; as opposed to CALL, EXX, LD C,n, or LD BC,nnnn
	   inc hl
	   bit.s 5,(hl)
	  pop hl
	  ld c,5
#ifdef FASTLOG
	  jp nz,lookup_gb_add
#else
	  jr nz,lookup_gb_add
#endif
	  ; Default to an 8-byte implementation
	  ld c,8
	  ; Decrement to the MSB of the accessed address
	  dec hl
	  ; Check for read vs. write
	  bit 4,e
	  jr z,lookup_gb_found_abs_write
	  ; Check for a port read
	  ld e,(hl)
	  inc hl
	  inc e
	  jr nz,lookup_gb_add
	  ; Port reads are 6 bytes
	  ld c,6
	  jr lookup_gb_add
	 
lookup_gb_found_abs_write:
	  ; Check for an MBC write
	  bit 7,(hl)
	  inc hl
	  jr nz,lookup_gb_add
	  ; MBC writes are 4 bytes
	  ld c,4
	  jr lookup_gb_add
	
runtime_error_trampoline:
	jr runtime_error
	
lookup_gb_found_abs_read_write_high:
	   ; Check the first JIT byte for short read/write
	   ; This checks for a LD (nn),A or JR instruction,
	   ; as opposed to .LIL, LD L,A, or EXX
	   bit.s 0,(hl)
lookup_gb_pop_add:
	  pop hl
	  jr z,lookup_gb_add
	  ; Check for read vs. write
	  bit 4,e
	  ; Port reads are 6 bytes
	  ld c,6
	  jr nz,lookup_gb_add
	  ; Port writes are 8 bytes
	  ld c,8 ;8-6
	  jr lookup_gb_add
	
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
#ifdef FASTLOG
	push bc
	FASTLOG_EVENT(LOOKUP_GB, 2)
	inc sp
#endif
	call lookup_code_block
	ld a,ixh
	or ixl
	jr z,runtime_error_trampoline
	ld de,(ix)
	ld hl,(ix+2)
	ex.s de,hl
	sbc hl,bc
	jr z,lookup_gb_found_start
	push hl
	 GET_BASE_ADDR_NO_ASSERT
#ifdef 0
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
	  ld de,opcoderecsizes
	  ld bc,RAM_PREFIX_SIZE
	  jr nz,lookup_gb_add
lookup_gb_found_loop:
	  ld e,(hl)
	  ex de,hl
	  inc h
	  inc h
	  ld c,(hl)
	  ex de,hl
	  add hl,bc
	  ex de,hl
	  dec h
	  sub (hl)
	  dec h
	  ld c,(hl)
	  ex de,hl
	  jr c,lookup_gb_new_sub_block
lookup_gb_add:
	  add ix,bc
	  jr nc,lookup_gb_found_loop
	  dec ix
	  add ix,ix
	  jr nc,runtime_error_trampoline
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
#ifdef FASTLOG
	push de
	FASTLOG_EVENT(LOOKUP_GB_FOUND, 2)
	inc sp
#endif
	ret

lookup_gb_new_sub_block_end:
	 dec ix
	 add ix,bc
#ifdef FASTLOG
	 jp nc,runtime_error
#else
	 jr nc,runtime_error_trampoline
#endif
	 jr nz,_
	 inc ix  ; For RET/JR/JP/RST, count is at -4 bytes
_
	 add.s a,(ix-4)  ; For CALL, count is at -5 bytes
	 ASSERT_C
	 jr lookup_gb_finish
	
lookup_gb_new_sub_block:
	  cp -5
	  jr c,lookup_gb_variable_length
	  ; Note: for an overlapped instruction, the following transformations
	  ; may end up incorrect, but they will be discarded after the bounds check
	  inc e
	  bit 2,e
	  jr z,_
	  inc bc ; For CALL, offsets are stored -1
_
	  add ix,bc
	 pop bc
	 ; This jump will not be taken for an overlapped instruction
	 jr c,lookup_gb_new_sub_block_end
	 push hl
	  lea hl,ix-4 ; For RET/JR/JP/RST, count is at -4 bytes
	  jr z,_
	  dec hl  ; For CALL, count is at -5 bytes
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
	
lookup_gb_variable_length:
	  ; Count CB-prefixed opcode cycles
	  sub 1+2
	  ; If the cycle count overflowed from a CB-prefix instruction,
	  ; this means it must have been an overlapped instruction
	  ; This path is also taken if we reached a HALT
	  jr c,lookup_gb_found_overlapped_or_halt
	  ; Differentiate CB prefix and absolute reads/writes
	  bit 0,e
	  jp z,lookup_gb_found_abs_read_write
	  ; Look up the second byte in the CB opcode table
	  dec hl
	  ld e,(hl)
	  inc hl
	  ex de,hl
	  dec h
	  ld c,(hl)
	  inc h
	  ex de,hl
	  ; Check for (HL) access
	  srl c
	  jr nc,lookup_gb_add
	  ; Check for BIT b,(HL)
	  jr nz,_
	  ld c,4
	  dec a ; Adjust cycles for read
	  jr lookup_gb_add
_
	  sub 2 ; Adjust cycles for read/write
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
#ifdef FASTLOG
	 jp lookup_gb_finish
#else
	 jr lookup_gb_finish
#endif
_
	  ; If not, determine the address after the bugged instruction
	  ld a,(de)
	  ld c,a
	  add hl,bc
	  ; Cycle count will be 0
	  jr lookup_gb_found_overlapped
	
dynamic_jp_mismatch:
	ld.s de,(ix+2)
	pop hl
	lea hl,ix-1
	ld ix,recompile_struct
	add ix,de
	ld de,(ix)
	ld bc,RAM_PREFIX_SIZE
	ld.s (hl),$CD ;CALL
	ldir.s
	push de
	 exx
	pop hl
	exx
	jp rerecompile_popped
	
	; When a match is found, load it from the cache
lookup_code_cached_found:
	ld a,(ix-3)
	ld ix,(ix-5)
	ret.l
	
lookup_code_cached_for_dynamic_jp:
	ld a,(hl)
	cp $C3
	jr nz,dynamic_jp_mismatch
	inc hl
	ld hl,(hl)
	ex.s de,hl
	
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
; Destroys AF,BC,DE,HL
lookup_code_cached:
	; Get the banked address in DE
	call get_banked_address

lookup_code_cached_with_bank:
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
#ifdef FASTLOG
	  push de
	  FASTLOG_EVENT(CACHE_MISS, 3)
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
	ret.l
	
_
	inc h
	dec (hl)
	dec h
	inc l
	jr nz,--_
	ld a,c
	ret.l
	

lookup_found_special:
	 ; Check whether this was a cycle overflow or a variable-length impl
	 cp -5
	 jr c,lookup_found_variable_length
	 ; Found a new sub-block
	 ; Add cycles for the next sub-block
	 add.s a,(ix-4)
	 ASSERT_C
	 inc e
	 bit 2,e
	 jr z,_  ; For RET/JR/JP/RST, offsets are normal
	 inc ix  ; For CALL, offsets are stored -1
_
	 add hl,bc
	 add iy,bc
	 jr nc,lookup_found_loop
	 jr lookup_found_loop_finish
	
lookup_found_variable_length:
	 ; Count variable-length opcode cycles (assume cycles=opcode bytes)
	 sbc a,c
	 ; If we're stepping past a HALT, then continue
	 jr c,lookup_found_continue
	 ; Differentiate CB prefix and absolute reads/writes
	 bit 0,e
	 jr z,lookup_found_abs_read_write
	 ; Look up the second byte in the CB opcode table
	 inc hl
	 ld e,(hl)
	 inc hl
	 ex de,hl
	 dec h
	 ld c,(hl)
	 inc h
	 ex de,hl
	 ; Check for (HL) access
	 srl c
	 jr nc,++_
	 ; Check for BIT b,(HL)
	 jr nz,_
	 ; Set actual recompiled code size
	 ld c,4
	 ; Adjust cycle count for read
	 inc a
_
	 ; Adjust cycle count for read/write
	 sub 2
_
	 ; Add actual recompiled code size
	 add ix,bc
	 ; Restore GB instruction size
	 ld c,2
	 jr lookup_found_continue_2
	
lookup_code_in_block:
	add.s hl,de
	push de
	 ex de,hl
	 GET_BASE_ADDR_FAST
	 add hl,de
	 ld de,opcoderecsizes
lookup_found_loop:
	 ; Get current opcode
	 ld e,(hl)
	 ex de,hl
	 ; Add recompiled instruction size
	 ld c,(hl)
	 add ix,bc
	 ; Add cycles
	 inc h
	 sub (hl)
	 inc h
	 ; Add GB instruction size
	 ld c,(hl)
	 dec h
	 dec h
	 ex de,hl
	 jr c,lookup_found_special
lookup_found_continue:
	 add hl,bc
lookup_found_continue_2:
	 add iy,bc
	 jr nc,lookup_found_loop
lookup_found_loop_finish:
	pop de
	dec iy
	add iy,iy
	ret
	
lookup_found_abs_read_write:
	 ; Add a cycle for the memory access
	 dec a
	 ; Check the first JIT byte for short read/write
	 ; This checks for a LD (nn),A instruction,
	 ; as opposed to .LIL, LD L,A, or EXX
	 bit.s 0,(ix)
	 lea ix,ix+3
	 jr z,lookup_found_continue
	 ; Check for LDH
	 bit 0,c
	 jr z,lookup_found_abs_read_write_high
	 ; Check the second JIT byte for long read/write
	 ; This checks for a LD (nnn),A or LD A,(nnn) instruction,
	 ; as opposed to CALL, EXX, LD C,n, or LD BC,nnnn
	 bit.s 5,(ix-3+1)
	 lea ix,ix+5-3
	 jr nz,lookup_found_continue
	 ; Default to an 8-byte implementation
	 add ix,bc ;8-5
	 ; Advance to the MSB of the accessed address
	 inc hl
	 inc hl
	 ; Check for read vs. write
	 bit 4,e
	 jr z,lookup_found_abs_write
	 ; Check for a port read
	 ld e,(hl)
	 inc hl
	 inc e
	 jr nz,lookup_found_continue_2
	 ; Port reads are 6 bytes
	 lea ix,ix-2
	 jr lookup_found_continue_2
	 
lookup_found_abs_write:
	 ; Check for an MBC write
	 bit 7,(hl)
	 inc hl
	 jr nz,lookup_found_continue_2
	 ; MBC writes are 4 bytes
	 lea ix,ix-4
	 jr lookup_found_continue_2
	 
lookup_found_abs_read_write_high:
	 ; Check for read vs. write
	 bit 4,e
	 ; Port reads are 6 bytes
	 lea ix,ix+6-3
	 jr nz,lookup_found_continue
	 ; Port writes are 8 bytes
	 add ix,bc ;8-6
	 jr lookup_found_continue
	
	
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
#ifdef FASTLOG
	push hl
	push hl
	push de
	FASTLOG_EVENT(LOOKUP_JIT_INTERNAL, 6)
	pop hl
#endif
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
	 ld a,(ix+7)
	 ld ix,(ix)
	 lea ix,ix+RAM_PREFIX_SIZE
	 call lookup_code_in_block
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
#ifdef FASTLOG
	push de
	FASTLOG_EVENT(LOOKUP_JIT, 3)
#endif
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
	 jr z,recompile
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
	  ld a,(ix+7)
	  ld ix,(ix)
	  call lookup_code_in_block
	  jr nc,lookuploop_restore
	 pop hl
	pop iy
	ret
	
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
#ifdef FASTLOG
	 push hl
	 push de
	 FASTLOG_EVENT(RECOMPILE, 5)
	 dec sp
	 dec sp
	 pop hl
#endif
	 bit 7,d
	 jp nz,recompile_ram
	
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
	 ; Check for approaching the end of the JIT code space
	 ld a,d
	 cp (flags_lut >> 8) - 2
	 jr nc,prepare_flush_from_buffer_overflow
	pop af
	
	; Check for collision with trampolines
	ld hl,(z80codebase+trampoline_next)
	sbc hl,de
	jr c,prepare_flush_from_recompile
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
prepare_flush_from_recompile:
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
	; Dispatch to the flush handler instead of the recompiled code
	ld ix,flush_handler
	; Don't consume any cycles during dispatch
	xor a
	ret
	
prepare_flush_from_buffer_overflow:
	pop hl
	; Check if flags LUT was actually overflowed into
	cp flags_lut >> 8
	jr c,prepare_flush_from_recompile
	APTR(flags_lut_init)
	ld de,z80codebase+flags_lut
	ld bc,$0100
	ldir
	jr prepare_flush_from_recompile
	
flush_cache:
	push af
	 MEMSET_FAST(recompile_cache_LUT, 256, 0)
	 MEMSET_FAST(recompile_cache_LUT+256, 256, (recompile_cache_end>>8)&$FF)
	pop af
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
	     APRINTF(RecompileRamMessage)
	    pop de
	   pop hl
	  pop hl
	 pop ix
#endif
	 
	 ld (hl),$CD ;CALL
	 push hl
	  ld bc,RAM_PREFIX_SIZE
	  add hl,bc
	  
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
	  ; Check for a single JP opcode
	  ld a,(hl)
	  cp $C3 ;JP
	  jr z,recompile_ram_dynamic_jp
recompile_ram_dynamic_jp_continue:
	  ldir
	  ; Decrement the final byte to force the match to end here
	  dec de
	  ld a,(de)
	  dec a
	  ld (de),a
	  inc de
	 
	 pop hl
recompile_ram_dynamic_jp_skip:
	 ; Determine the SMC handler to use based on the RAM region
	 inc hl
	 ld a,(ix+3)
	 inc a
	 jr nz,_
	 ld (hl),coherency_handler_hram & $FF
	 inc hl
	 ld (hl),coherency_handler_hram >> 8
	 jr ++_
_
	 ld (hl),coherency_handler_wram & $FF
	 inc hl
	 ld (hl),coherency_handler_wram >> 8
	 sub $C0+1
recompile_ram_unbanked_range_smc = $+1
	 cp $20
	 jr c,_
	 dec hl
	 ld (hl),coherency_handler_generic & $FF
	 inc hl
	 ld (hl),coherency_handler_generic >> 8
_
	 inc hl
	 ; Save the block struct pointer in the block prefix
	 ld (hl),ix
	 inc hl
	 inc hl
	 ; Save the block end pointer in the block prefix, in case the next
	 ; block pointer is advanced by a low-pool trampoline allocation
	 ld (hl),e
	 inc hl
	 ld (hl),d
	 
	 ; Report block cycle length of 0
	 xor a
	 jp recompile_end_common
	
recompile_ram_dynamic_jp:
	  ; Only handle a fully contiguous JP (not overlapped)
	  ld a,c
	  cp 3
	  jr nz,recompile_ram_dynamic_jp_continue
	  ; Save the block end address and get the block start address
	  ex de,hl
	  ex (sp),hl
	  ld (hl),$D9 ;EXX
	  inc hl
	  ex de,hl
	  ; Look up the memory reads
	  ld hl,z80codebase+mem_read_lut+$08 ;EX AF,AF'
	  ld a,l
	  ld l,(ix+3)
	  ld l,(hl)
	  dec h ;mem_read_any_routines
	  ; Check if there's a banked access routine
	  dec l
	  cp (hl)
	  ; Get the pointer to the base address variable
	  ld b,2
	  add hl,bc
	  ; Get the opcode GB address (note: bank is 0 for RAM addresses)
	  ld bc,(ix+2)
	  jr nz,recompile_ram_dynamic_jp_unbanked
	  ex de,hl
	  ld (hl),$01 ;LD BC,opcode_gb_addr
	  inc hl
	  ld (hl),c
	  inc hl
	  ld (hl),b
	  ld a,$2A ;LD HL,(*_banked_base)
	  ld bc,do_dynamic_jp_banked
	  jr recompile_ram_dynamic_jp_finish
	  
recompile_ram_dynamic_jp_unbanked:
	  ; Get the direct opcode address
	  ld hl,(hl)
	  add hl,bc
	  ex de,hl
	  ld (hl),a   ;EX AF,AF'
	  ld bc,do_dynamic_jp
	  ld a,$21 ;LD HL,opcode_addr
recompile_ram_dynamic_jp_finish:
	  inc hl
	  ld (hl),$5B ;.LIL
	  inc hl
	  ld (hl),a
	  inc hl
	  ld (hl),de
	  inc hl
	  inc hl
	  inc hl
	  ld (hl),$CD ;CALL
	  inc hl
	  ld (hl),c
	  inc hl
	  ld (hl),b
	 ; Restore the block end address
	 pop de
	 ; Dump the rest of the normal prefix after this point
	 jr recompile_ram_dynamic_jp_skip
	
check_coherency_helper_generic:
	ld hl,recompile_struct+2
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	inc hl
	ld a,(hl)
	inc hl
	inc hl
	push hl
	 ld hl,z80codebase+mem_read_lut
	 ld l,d
	 ld l,(hl)
	 inc h ;mem_get_ptr_routines
	 inc l \ inc l
	 ld ix,(hl)
	 jr check_coherency_helper_any
	
check_coherency_helper_hram:
	ld ix,hram_base
check_coherency_helper:
	ld hl,recompile_struct+2
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	inc hl
	ld a,(hl)
	inc hl
	inc hl
	push hl
check_coherency_helper_any:
	 add ix,de
	 sbc hl,hl
	 add hl,sp
	 ex de,hl
	 ld hl,z80codebase-1
	 cpl
	 ld l,a
	 sub ixl
	 add hl,bc ; Resets carry
	 ld sp,hl
check_coherency_loop:
	 pop hl
	 ld bc,(ix)
	 sbc hl,bc
	 jr nz,check_coherency_loop_finish_3
	 pop hl
	 ld bc,(ix+3)
	 sbc hl,bc
	 jr nz,check_coherency_loop_finish_6
	 pop hl
	 ld bc,(ix+6)
	 sbc hl,bc
	 jr nz,check_coherency_loop_finish_9
	 pop hl
	 ld bc,(ix+9)
	 sbc hl,bc
	 jr nz,check_coherency_loop_finish_12
	 pop hl
	 ld bc,(ix+12)
	 sbc hl,bc
	 lea ix,ix+15
	 jr z,check_coherency_loop
	 jr check_coherency_loop_finish
	 
check_coherency_loop_finish_12:
	 add a,3
check_coherency_loop_finish_9:
	 add a,9
	 jr check_coherency_loop_finish
	 
check_coherency_loop_finish_6:
	 add a,3
check_coherency_loop_finish_3:
	 add a,3
check_coherency_loop_finish:
	 ex de,hl
	 ld sp,hl
	 add a,ixl
	 jr z,check_coherency_3byte
	 dec a
	 jr z,check_coherency_2byte
	 dec a
	 jr nz,rerecompile
	 ; Make sure the last (decremented) byte matched
	 inc e
	 jr z,check_coherency_cycles
	 jr rerecompile
check_coherency_2byte:
	 ; Make sure the last (decremented) byte and second-to-last byte matched
	 inc d
	 or d
	 or e
	 jr z,check_coherency_cycles
	 jr rerecompile
check_coherency_3byte:
	 ; Make sure the last (decremented) byte and previous two bytes matched
	 scf
	 sbc.s hl,hl
	 adc hl,de
	 jr nz,rerecompile
	 
check_coherency_cycles:
	pop hl
	pop.s de
check_coherency_cycles_popped:
	ld a,e
	add a,(hl)
	jr c,++_
_
	pop.s ix
	exx
	ex af,af'
	jp.s (hl)	
_
	inc d
	jr nz,--_
	inc.s bc ;BCU=0
	ld c,a
	sub e
	ld b,a
	dec hl \ dec hl \ dec hl \ dec hl
	ld d,(hl)
	dec hl
	ld e,(hl)
	exx
	push hl
	pop ix
	exx
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
; Inputs:  (SP) = recompile struct entry for the block, plus 7
; Outputs: None
; Destroys AF,BC,DE,HL
rerecompile:
	pop ix
	lea ix,ix-7
rerecompile_popped:
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
#endif
#ifdef FASTLOG
	ld hl,(ix)
	push hl
	ld hl,(ix+2)
	push hl
	FASTLOG_EVENT(RERECOMPILE, 5)
	inc sp
#endif
	
	ld hl,(ix)
	inc.s hl
	ld de,z80codebase + RAM_PREFIX_SIZE - 1
	add hl,de
	push hl
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
	
	 ; Get the address to copy the opcodes to
	 ex (sp),hl
	 ; Load the lower two bytes individually to preserve the upper byte
	 dec hl
	 ld a,(hl)
	 dec hl
	 ld l,(hl)
	 ld h,a
	 sbc hl,bc	; Carry is reset
	 ASSERT_NC
	 ex de,hl
	
	 ; Make sure there is no overlap
	 scf
	 sbc hl,de
	 jr nc,coherency_flush
	
	 ; Check for collision of end of recompiled code with trampolines
	 push de
	  ld hl,(recompile_struct_end)
	  ld de,(hl)
	  ld hl,(z80codebase+trampoline_next)
	  or a
	  sbc hl,de
	 pop de
	 jr c,coherency_flush_no_padding
	
	; Copy the new opcodes, from first to last
	pop hl
	ldir
	dec de
	ld a,(de)
	dec a
	ld (de),a
	
	; Check for entries on the callstack containing this region
	pea ix+7
	 sbc hl,hl
	 add.s hl,sp
	 ; Get the number of used callstack entries
	 ld a,(myz80stack - 4 - 4) & $FF
	 sub l
#ifdef FASTLOG
	 jp z,check_coherency_cycles
#else
	 jr z,check_coherency_cycles
#endif
	 rrca
	 rrca
	 push hl
	  ld hl,(ix)
	  ex.s de,hl
	  sbc hl,de
	  ld b,h
	  ld c,l
	  add hl,de
	  ex de,hl
	  sbc hl,hl
	  sbc hl,de
	  ex de,hl
	  pop.s hl
	  pop.s hl
_
	  pop.s hl
	  pop.s hl
	  add hl,de
	  add hl,bc
	  jr c,rerecompile_found_callstack_entry
	  dec a
	  jr nz,-_
	 pop hl
	 ld.s sp,hl
	 jp check_coherency_cycles
	
rerecompile_found_callstack_entry:
	  ; For now, just flush the whole callstack
	 pop hl
	 ld.s sp,hl
	 pop.s de
	 pop.s hl
	 ld.sis sp,myz80stack - 4
	 push.s hl
	pop hl
	ld sp,myADLstack
	jp check_coherency_cycles_popped
	
coherency_flush_no_padding:
	sbc hl,hl
coherency_flush:
	pop de
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
#ifdef FASTLOG
	push hl
	FASTLOG_EVENT(PADDING_UPDATE, 3)
#endif
	; Start at the very beginning
	ld de,(ix+2)
	call flush_code
	call lookup_code_with_bank
	pop.s de
	ld a,e
	ex af,af'
	exx
	lea hl,ix
	pop.s ix
	; Flush entire call stack, including interrupt returns
	ld sp,myADLstack
	ld.sis sp,myz80stack-4
	jp.s (hl)
	
	
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
	 ld hl,z80codebase+mem_read_lut
	 ld l,d
	 inc l
	 ld a,(hl)
	 dec l
	 ld l,(hl)
	 cp l
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
	 inc h ;mem_get_ptr_routines
	 inc l \ inc l
	 ld hl,(hl)
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
	
	.block (203-$)&255	

opcycleCB_hl:
	; Check for BIT
	jr z,opcycleCB_bit_hl
	; 2b op, variable rec, 4cc
	add ix,bc
	ld c,3
	add a,4
	ret c
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)
opcycleCB_bit_hl:
	; 2b op, 4b rec, 3cc
	lea ix,ix+4
	ld c,3
	add a,c
	ret c
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)
	
opcycle_abs_read_write_special:
	add ix,bc
	ex de,hl
	; Check read vs. write
	bit 4,(hl)
	inc hl
	inc hl
	jr z,opcycle_abs_write_special
	; Check for port read
	ld e,(hl)
	inc hl
	inc e
	jr nz,opcycle_abs_read_write_special_finish
	; Port reads are 6 bytes
	lea ix,ix-2
	jr opcycle_abs_read_write_special_finish
	
opcycle_abs_write_special:
	; Check for MBC write
	bit 7,(hl)
	inc hl
	jr nz,opcycle_abs_read_write_special_finish
	; MBC writes are 4 bytes
	lea ix,ix-4
	jr opcycle_abs_read_write_special_finish
	
	.block (-$)&255
opcycleroutines:
opcycleCB:
	; Look up second opcode byte in opcoderecsizes_CB
	ex de,hl
	inc hl
	ld e,(hl)
	inc hl
	ex de,hl
	inc h
	inc h
	ld c,(hl)
	dec h
	ex de,hl
	ld e,(hl)
	; Check for (HL) access
	srl c
	jr c,opcycleCB_hl
	; 2b op, variable rec, 2cc
	add ix,bc
	ld c,3
	add a,2
	ret c
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)
	
	; 3b op, variable rec, 4cc
opcycle_abs_read_write:
	bit.s 0,(ix)
	add ix,bc
	jr z,opcycle_abs_read_write_finish
	bit.s 5,(ix-3+1)
	lea ix,ix+5-3
	jr z,opcycle_abs_read_write_special
opcycle_abs_read_write_finish:
	ex de,hl
	add hl,bc
opcycle_abs_read_write_special_finish:
	add a,4
	OPCYCLE_NEXT
	
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
	
	; 1b op, 2b rec, 1cc
opcycle1byte_ix:
	dec ix
	; 1b op, 3b rec, 1cc
opcycleROTC:
opcycle27:
opcycle3F:
opcycleF3:
opcycleEI:
	add ix,bc
	inc de
	ex de,hl
	inc a
	ret z
	inc d
	ld e,(hl)
	ex de,hl
	ld l,(hl)
	dec h
	jp (hl)
	
	; 3b op, 7b rec, 5cc
opcycle08:
	lea ix,ix+7
	ex de,hl
	add hl,bc
	add a,5
	jr opcycle_next
	
	; 2b op, 3b rec, 2cc
opcycle2byte_ix:
	inc de
	; 1b op, 3b rec, 2cc
opcycleMEM:
opcycle19:
opcycle29:
opcycle39:
opcycleF9:
	add ix,bc
	inc de
	ex de,hl
	add a,2
opcycle_next:
	OPCYCLE_NEXT
	
	; 1b op, 6b rec, 2cc
opcycleE2:
opcycleF2:
	inc ix
	; 1b op, 5b rec, 2cc
opcycle09:
	lea ix,ix+4
	; 1b op, 1b rec, 2cc
opcycle1byte_2cc:
	inc ix
	inc de
	ex de,hl
	add a,2
	OPCYCLE_NEXT
	
	; 1b op, 7b rec, 2cc
opcycleMEM_7b:
	add ix,bc
	; 1b op, 4b rec, 2cc
opcycle33:
opcycle3B:
opcycleMEM_4b:
	lea ix,ix+4
	inc de
	ex de,hl
	add a,2
	OPCYCLE_NEXT
	
	; 2b op, 2b rec, 2cc
opcycle2byte:
	inc de
	; 1b op, 2b rec, 2cc
opcycle1byte_2cc_ix:
	lea ix,ix+2
	inc de
	ex de,hl
	add a,2
	OPCYCLE_NEXT
	
	; 3b op, 7b rec, 3cc
opcycle31:
	add ix,bc
	; 3b op, 4b rec, 3cc
opcycle3byte_ix:
	inc ix
	; 3b op, 3b rec, 3cc
opcycle3byte:
	add ix,bc
	ex de,hl
	add hl,bc
	add a,c
	OPCYCLE_NEXT
	
	; 2b op, variable rec, 3cc
opcycleE0:
	bit.s 0,(ix)
	add ix,bc
	jr nz,opcycleE0_port
	inc de
	inc de
	ex de,hl
	add a,c
	OPCYCLE_NEXT
	
	; 2b op, variable rec, 3cc
opcycleF0:
	bit.s 0,(ix)
	add ix,bc
	inc de
	jr z,opcycleF0_normal
	jr opcycleF0_port
	
	; 2b op, 5b rec, 3cc
opcycleE0_port:
opcycleF8:
	inc ix
	; 2b op, 4b rec, 3cc
opcycle36:
	inc de
	; 1b op, 4b rec, 3cc
opcyclePOP:
	inc ix
	; 1b op, 3b rec, 3cc
opcycleF0_port:
opcycle34:
opcycle35:
opcycleF1:
	add ix,bc
opcycleF0_normal:
	inc de
	ex de,hl
	add a,c
	OPCYCLE_NEXT

	; 2b op, 5b rec, 4cc
opcycleE8:
	inc de
	lea ix,ix+2
	; 1b op, 3b rec, 4cc
opcyclePUSH:
	inc de
	add ix,bc
	ex de,hl
	add a,4
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
opcycleSTOP:
	jp runtime_error

	.echo "Opcycle routine size: ", $ - opcycleroutines
	.block 256 - ($ - opcycleroutines)

; A table indexing opcode cycle counting routines.
; All entry points live in a 256-byte space.
opcounttable:
;00
	.db opcycleNOP - opcycleroutines
	.db opcycle3byte_ix - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle1byte_2cc_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle2byte_ix - opcycleroutines
	.db opcycleROTC - opcycleroutines
;08
	.db opcycle08 - opcycleroutines
	.db opcycle09 - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle1byte_2cc_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle2byte_ix - opcycleroutines
	.db opcycleROTC - opcycleroutines
;10
	.db opcycleSTOP - opcycleroutines
	.db opcycle3byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleROT - opcycleroutines
;18
	.db opcycleJR - opcycleroutines
	.db opcycle19 - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleROT - opcycleroutines
;20
	.db opcycleJRcond - opcycleroutines
	.db opcycle3byte - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycle27 - opcycleroutines
;28
	.db opcycleJRcond - opcycleroutines
	.db opcycle29 - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle1byte_2cc - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
;30
	.db opcycleJRcond - opcycleroutines
	.db opcycle31 - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle33 - opcycleroutines
	.db opcycle34 - opcycleroutines
	.db opcycle35 - opcycleroutines
	.db opcycle36 - opcycleroutines
	.db opcycle1byte - opcycleroutines
;38
	.db opcycleJRcond - opcycleroutines
	.db opcycle39 - opcycleroutines
	.db opcycleMEM_4b - opcycleroutines
	.db opcycle3B - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycle3F - opcycleroutines
;40
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycleMEM_7b - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
;48
	.db opcycle1byte_ix - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycleMEM_7b - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
;50
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;58
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;60
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;68
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleNOP - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;70
	.db opcycleMEM_7b - opcycleroutines
	.db opcycleMEM_7b - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleHALT - opcycleroutines
	.db opcycleMEM - opcycleroutines
;78
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycleNOP - opcycleroutines
;80
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;88
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;90
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;98
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;A0
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;A8
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;B0
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycle1byte - opcycleroutines
	.db opcycleMEM - opcycleroutines
	.db opcycle1byte - opcycleroutines
;B8
	.db opcycle1byte_ix - opcycleroutines
	.db opcycle1byte_ix - opcycleroutines
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
	.db opcyclePUSH - opcycleroutines
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
	.db opcyclePUSH - opcycleroutines
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
	.db opcyclePUSH - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;E8
	.db opcycleE8 - opcycleroutines
	.db opcycleE9 - opcycleroutines
	.db opcycle_abs_read_write - opcycleroutines
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
	.db opcyclePUSH - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
;F8
	.db opcycleF8 - opcycleroutines
	.db opcycleF9 - opcycleroutines
	.db opcycle_abs_read_write - opcycleroutines
	.db opcycleEI - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycleINVALID - opcycleroutines
	.db opcycle2byte - opcycleroutines
	.db opcycleRST - opcycleroutines
	
; A table of recompiled bitwise opcode sizes.
; The value is scaled by 2, and bit 0 is set for (HL) accesses.
; BIT b,(HL) opcodes report a size of 0 for easy detection,
; but their actual size is 4 bytes.
opcoderecsizes_CB:
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 6,6,6,6,6,6,7,10
	.db 16,16,4,4,4,4,9,4
	
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	.db 10,10,4,4,4,4,1,4
	
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	.db 16,16,4,4,4,4,9,4
	
; A table of recompiled opcode sizes.
; Does not apply to block-ending opcodes or variable-length implementations.
; Currently CB, E0, EA, F0, and EA are variable-length.
; CALL opcodes are offset by 1 to make it easier to find sub-block cycles.
opcoderecsizes:
	.db 0,4,4,2,2,2,3,3
	.db 7,5,4,2,2,2,3,3
	.db 0,3,3,1,1,1,2,4
	.db 0,3,3,1,1,1,2,4
	.db 19,3,4,1,1,1,2,3
	.db 19,3,4,1,1,1,2,1
	.db 19,7,4,4,3,3,4,1
	.db 19,3,4,4,1,1,2,3
	
	.db 0,2,2,2,2,2,7,2
	.db 2,0,2,2,2,2,7,2
	.db 2,2,0,1,1,1,3,1
	.db 2,2,1,0,1,1,3,1
	.db 2,2,1,1,0,1,3,1
	.db 2,2,1,1,1,0,3,1
	.db 7,7,3,3,3,3,1,3
	.db 2,2,1,1,1,1,3,0
	
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	.db 2,2,1,1,1,1,3,1
	
	.db 12,4,19,0,11-1,3,2,9
	.db 12,0,19,0,11-1,11-1,2,9
	.db 12,4,19,0,11-1,3,2,9
	.db 12,0,19,0,11-1,0,2,9
	.db 0,4,6,0,0,3,2,9
	.db 5,0,0,0,0,0,2,9
	.db 0,3,6,3,0,3,2,9
	.db 5,3,0,3,0,0,2,9
	
; A table of Game Boy opcode cycles. Block-ending opcodes are set to 0.
; Conditional branches are assumed not taken.
; Opcodes with variable-length implementations (e.g. CB) and HALT are set to -1,
; for efficient detection.
opcodecycles:
	.db 1,3,2,2,1,1,2,1
	.db 5,2,2,2,1,1,2,1
	.db 0,3,2,2,1,1,2,1
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
	.db 2,2,2,2,2,2,-2,2
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
	.db -1,3,2,0,0,4,2,4
	.db 4,0,-1,0,0,0,2,4
	.db -1,3,2,1,0,4,2,4
	.db 3,2,-1,1,0,0,2,4
	
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
	.db opgen01 - opgenroutines
	.db opgenBCwrite - opgenroutines
	.db opgen1byte_2cc_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen2byte_remap_ix - opgenroutines
	.db opgenROT - opgenroutines
;08
	.db opgen08 - opgenroutines
	.db opgen09 - opgenroutines
	.db opgenBCread - opgenroutines
	.db opgen1byte_2cc_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen2byte_remap_ix - opgenroutines
	.db opgenROT - opgenroutines
;10
	.db opgenHALT_STOP - opgenroutines
	.db opgen3byte_remap - opgenroutines
	.db opgenDEwrite - opgenroutines
	.db opgen1byte_2cc_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen2byte_remap - opgenroutines
	.db opgenROT - opgenroutines
;18
	.db opgenJR - opgenroutines
	.db opgen19 - opgenroutines
	.db opgenDEread - opgenroutines
	.db opgen1byte_2cc_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen2byte_remap - opgenroutines
	.db opgenROT - opgenroutines
;20
	.db opgenJRcond - opgenroutines
	.db opgen3byte_remap - opgenroutines
	.db opgenHLwrite_post - opgenroutines
	.db opgen1byte_2cc_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen2byte_remap - opgenroutines
	.db opgen27 - opgenroutines
;28
	.db opgenJRcond - opgenroutines
	.db opgen29 - opgenroutines
	.db opgenHLread_post - opgenroutines
	.db opgen1byte_2cc_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen2byte_remap - opgenroutines
	.db opgen1byte - opgenroutines
;30
	.db opgenJRcond - opgenroutines
	.db opgen31 - opgenroutines
	.db opgenHLwrite_post - opgenroutines
	.db opgen33 - opgenroutines
	.db opgenHLreadwrite - opgenroutines
	.db opgenHLreadwrite - opgenroutines
	.db opgen36 - opgenroutines
	.db opgen1byte - opgenroutines
;38
	.db opgenJRcond - opgenroutines
	.db opgen39 - opgenroutines
	.db opgenHLread_post - opgenroutines
	.db opgen3B - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen1byte - opgenroutines
	.db opgen2byte - opgenroutines
	.db opgen3F - opgenroutines
;40
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgenHLread_bc - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
;48
	.db opgen1byte_remap_ix - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgenHLread_bc - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
;50
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte_remap - opgenroutines
;58
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte_remap - opgenroutines
;60
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte_remap - opgenroutines
;68
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenNOP - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte_remap - opgenroutines
;70
	.db opgenHLwrite_bc - opgenroutines
	.db opgenHLwrite_bc - opgenroutines
	.db opgenHLwrite - opgenroutines
	.db opgenHLwrite - opgenroutines
	.db opgenHLwrite - opgenroutines
	.db opgenHLwrite - opgenroutines
	.db opgenHALT_STOP - opgenroutines
	.db opgenHLwrite - opgenroutines
;78
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgenNOP - opgenroutines
;80
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;88
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;90
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;98
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;A0
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;A8
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;B0
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
	.db opgen1byte - opgenroutines
;B8
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap_ix - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgen1byte_remap - opgenroutines
	.db opgenHLread - opgenroutines
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
	.db opgenRST - opgenroutines
	
; A supplemental table for opcode generation.
; Determines translations or routine addresses for certain opcodes.
	;00
	.db 0,  0,0,$23,$24,$25,$26,0
	.db 0,  0,0,$2B,$2C,$2D,$2E,0
	.db 0,$01,0,$03,$04,$05,$06,0
	.db 0,  0,0,$0B,$0C,$0D,$0E,0
	.db ophandler31 >> 8
	.db   $11,$13,$13,$14,$15,$16,ophandler27 & $FF
	.db ophandler27 >> 8
	.db     0,$13,$1B,$1C,$1D,$1E,ophandler31 & $FF
	;30
	.db 0
	.db 0
	.db $1B
	.db ophandler33 & $FF
	.db ophandler3B >> 8
	.db 0
	.db ophandler39 >> 8
	.db 0
	;38
	.db 0
	.db ophandler39 & $FF
	.db $1B
	.db ophandler3B & $FF
	.db ophandler33 >> 8
	.db 0
	.db 0
	.db 0
	
	;40
	.db   0,$65,$60,$61,$62,$63,$63,$67
	.db $6C,  0,$68,$69,$6A,$6B,$6B,$6F
	.db $44,$45,  0,$41,$42,$43,$46,$47
	.db $4C,$4D,$48,  0,$4A,$4B,$4E,$4F
	.db $54,$55,$50,$51,  0,$53,$56,$57
	.db $5C,$5D,$58,$59,$5A,  0,$5E,$5F
	.db $7C,$7D,$70,$71,$72,$73,  0,$77
	.db $7C,$7D,$78,$79,$7A,$7B,$7E,  0
	
	;80
	.db $84,$85,$80,$81,$82,$83,$86,0
	.db $8C,$8D,$88,$89,$8A,$8B,$8E,0
	.db $94,$95,$90,$91,$92,$93,$96,0
	.db $9C,$9D,$98,$99,$9A,$9B,$9E,0
	.db $A4,$A5,$A0,$A1,$A2,$A3,$A6,0
	.db $AC,$AD,$A8,$A9,$AA,$AB,$AE,0
	.db $B4,$B5,$B0,$B1,$B2,$B3,$B6,0
	.db $BC,$BD,$B8,$B9,$BA,$BB,$BE,0
	
	;C0
	.db 0
	.db ophandlerC1 & $FF
	.db 0
	.db 0
	.db 0
	.db ophandlerC5 & $FF
	.db ophandlerRET >> 8
	.db 0
	;C8
	.db 0
	.db ophandlerRET & $FF
	.db ophandlerC5 >> 8
	.db 0
	.db 0
	.db 0
	.db ophandlerC1 >> 8
	.db 0
	;D0
	.db ophandlerF8_zero & $FF
	.db ophandlerD1 & $FF
	.db 0
	.db 0
	.db 0
	.db ophandlerD5 & $FF
	.db ophandlerRETI >> 8
	.db 0
	;D8
	.db 0
	.db ophandlerRETI & $FF
	.db ophandlerD5 >> 8
	.db 0
	.db 0
	.db 0
	.db ophandlerD1 >> 8
	.db ophandlerF8_zero >> 8
	;E0
	.db ophandlerE8_non_negative & $FF
	.db ophandlerE1 & $FF
	.db op_write_c_hmem & $FF
	.db 0
	.db 0
	.db ophandlerE5 & $FF
	.db ophandlerE9 >> 8
	.db ophandlerE8_negative >> 8
	;E8
	.db ophandlerE8_negative & $FF
	.db ophandlerE9 & $FF
	.db ophandlerE5 >> 8
	.db 0
	.db 0
	.db op_write_c_hmem >> 8
	.db ophandlerE1 >> 8
	.db ophandlerE8_non_negative >> 8
	;F0
	.db ophandlerF8_positive & $FF
	.db ophandlerF1 & $FF
	.db op_read_c_hmem & $FF
	.db ophandlerF3 & $FF
	.db ophandlerEI >> 8
	.db ophandlerF5 & $FF
	.db ophandlerF9 >> 8
	.db ophandlerF8_negative >> 8
	;F8
	.db ophandlerF8_negative & $FF
	.db ophandlerF9 & $FF
	.db ophandlerF5 >> 8
	.db ophandlerEI & $FF
	.db ophandlerF3 >> 8
	.db op_read_c_hmem >> 8
	.db ophandlerF1 >> 8
	.db ophandlerF8_positive >> 8
	
rom_bank_fill_routines:
	.db 0
	.db rom_bank_fill_rem_0 - rom_bank_fill_routines
	.db rom_bank_fill_exactly_1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-21 - rom_bank_fill_routines
	
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
rom_bank_fill_rem_0:
	ld sp,(mbc_change_rom_bank_smc)
	jp.sis nz,mbc_no_fix_sp
	jr rom_bank_fill_fix_sp
	
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
rom_bank_fill_rem_1:
	inc sp \ inc sp
	push bc
	ld sp,(mbc_change_rom_bank_smc)
	jp.sis nz,mbc_no_fix_sp
rom_bank_fill_fix_sp:
	ld hl,(rom_bank_base)
	jp.sis mbc_fix_sp
	
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
	push bc
rom_bank_fill_rem_2:
	inc sp
	push bc
	ld sp,(mbc_change_rom_bank_smc)
	jp.sis nz,mbc_no_fix_sp
	jr rom_bank_fill_fix_sp
	
rom_bank_fill_exactly_1:
	ld hl,-1
	add hl,sp
	ld (hl),c
	ld sp,(mbc_change_rom_bank_smc)
	jp.sis nz,mbc_no_fix_sp
	jr rom_bank_fill_fix_sp
	
	.block (rom_bank_fill_routines+192) - $
	.db rom_bank_fill_rem_0-21 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-20 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-19 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-18 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-17 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-16 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-15 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-14 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-13 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-12 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-11 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-10 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-9 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-8 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-7 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-6 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-5 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-4 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-3 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-2 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_1-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0-1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_2 - rom_bank_fill_routines
	.db rom_bank_fill_exactly_1 - rom_bank_fill_routines
	.db rom_bank_fill_rem_0 - rom_bank_fill_routines
	
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
	; Check if block was forced to end
	or a
	jr z,_
	; Check if a memory region crossing should be handled
opgen_byte_limit_smc = $+1
	ld a,0
	cp MAX_OPCODE_BYTES_PER_BLOCK
	jr nz,opgen_cross_mem_region
_
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
	  ld hl,z80codebase+mem_read_lut
	  ld l,a
	  ld l,(hl)
	  inc h ;mem_get_ptr_routines
	  inc l \ inc l
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
	ld bc,opcoderecsizes_CB
	inc hl
	ld c,(hl)
	dec hl
	ld a,(bc)
	srl a
	jr nz,_
	ld a,4
_
	; Subtract the size from the maximum size of 8
	cpl
	add a,9
	jr z,++_
	; Emit NOPs before the operation
	ld b,a
	xor a
_
	ld (de),a
	inc de
	djnz -_	
_
	; Generate the opcode
	jp opgen_next
	
	; RST instruction format:
	;  EXX
	;  LD HL,gb_ret_addr
	;  LD B,ret_cycles
	;  CALL do_rst_NN
_opgenRST:
	ex de,hl
	ld (hl),$D9 ;EXX
	inc hl
	ld (hl),$21 ;LD HL,gb_ret_addr
	call opgen_reset_cycle_count
	inc de
	call opgen_emit_gb_address
	dec de
	ld (hl),$06 ;LD B,ret_cycles
	inc hl
	ld (opgen_last_cycle_count_smc),hl
	inc hl
	ld (hl),$CD ;CALL do_rst_NN
	inc hl
	; Translate the opcode into a routine entry offset
	ld a,c
	rrca
	adc a,c
	add a,(do_rst_38+1) & $FF
	ld (hl),a
	adc a,((do_rst_38+1) >> 8) - 1
	sub (hl)
	inc hl
	ld (hl),a
	dec iyl
	dec iyl
	jp opgen_next_swap_skip_1cc
	
	; CALL CC instruction format:
	;  LD HL,jit_target
	;  CALL do_call_CC (or do_*_bank_call_CC)
	;  .DB ret_cycles
	;  .DB call_cycles
	;  .DW gb_ret_addr
	;  .DB target_bank (if applicable)
	; For non-decoded calls, jit_target=decode_call and call_cycles=0
_opgenCALLcond:
	; Use a difference of (8*$C0) == (6<<8) between each CALL opcode type
	ld b,$C0
	mlt bc
	push hl
	 ld hl,((do_call_nz << 8) | $CD) - ($C4*$C0) ;CALL do_call_*
	 add hl,bc
	 ex (sp),hl
	pop bc
	xor a ;call_cycles=0
	jr opgen_finish_cond_call

	; CALL instruction format:
	;  LD HL,jit_target
	;  EXX
	;  LD BC,(ret_cycles << 8) | call_cycles
	;  LD HL,gb_ret_addr
	;  RST r_call
	; For non-decoded calls, jit_target=decode_call and call_cycles=0
	; Decoded calls which target a banked address use the following format:
	;  LD HL,jit_target
	;  CALL do_*_bank_call
	;  .DB ret_cycles
	;  .DB call_cycles
	;  .DW gb_ret_addr
	;  .DB target_bank
_opgenCALL:
	ld a,$21 ;LD HL,
	ld bc,$0001D9 ;EXX / LD BC,$XX00
opgen_finish_cond_call:
	ex de,hl
	ld (hl),$21 ;LD HL,decode_call
	inc hl
	ld (hl),decode_call & $FF
	inc hl
	ld (hl),decode_call >> 8
	inc hl
	ld (hl),bc
	inc hl
	inc hl
	inc hl
	inc hl
	ld (hl),a
	dec hl
	call opgen_reset_cycle_count
	inc de
	inc de
opgen_finish_rst:
	dec iyl
	ld (opgen_last_cycle_count_smc),hl
	inc hl
	inc de
	call opgen_emit_gb_address
	dec de
	ld (hl),RST_CALL
	jp opgen_next_swap_skip

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
	
opgen_emit_cond_ret:
	ex de,hl
	call opgen_reset_cycle_count
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
	push hl
	ex de,hl
	inc b
	ld a,(bc)
	ld e,a
	ld a,c
	xor $0F
	ld c,a
	ld a,(bc)
	ld d,a
#ifdef DEBUG
	ld (hl),$CD	;CALL
#else
	ld (hl),$C3	;JP
#endif
	djnz opgenblockend_finish
	
_opgenSTOP:
	ld (hl),ophandlerSTOP & $FF
	inc hl
	ld (hl),ophandlerSTOP >> 8
	; Emit the address following the STOP instruction
	inc de
	call opgen_emit_gb_address
	dec de
	jr opgen_reset_cycle_count
	
opgenblockend_invalid:
	push hl
	ex de,hl
	ld (hl),$CD	;CALL
	ld de,Z80InvalidOpcode
opgenblockend_finish:
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
	ld a,(recompile_cycle_offset_sp)
	inc a
	ret z
	push bc
	 push de
	  push hl
	   ex de,hl ;DEU=z80codebase>>16
	   ; Check for trampoline overlap and avoid modifying data to be flushed
	   ld hl,(z80codebase+trampoline_next)
	   sbc hl,de
	   jr c,++_
	   ld hl,(opgen_last_cycle_count_smc)
	   ld c,(hl)
	   ld hl,recompile_cycle_offset_stack - 1
	   ld l,a
_
	   ld e,(hl)
	   inc l
	   ld d,(hl)
	   ld a,(de)
	   sub c
	   ld (de),a
	   inc l
	   jr nz,-_
_
	   ASSERT_C
	   sbc a,a
	   ld (recompile_cycle_offset_sp),a
	  pop hl
	 pop de
	pop bc
	xor a
	ret
	
_opgenHALT_STOP:
	ex de,hl
	ld (hl),$CD
	inc hl
	bit 6,c
	jr z,_opgenSTOP
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
	add a,opgen_next_fast - opgenHALT_STOP
	jr nc,_
	; If the bugged opcode is a HALT, it will bug eternally
	; To avoid infinite code generation, instead we emit an invalid opcode
	jr z,opgenblockend_invalid
	; Write the first byte manually
	ld a,c
	ld (de),a
	inc de
	inc hl
	; Adjust the entry point
	lea ix,ix+2
_
	jp (ix)
	
opgenroutinecall_displacement:
	dec iyl
	ld a,($2E+$E8) & $FF ;LD L/E,displacement
	sub c
	ld (de),a
	inc de
	inc hl
	ld a,(hl)
	ld (de),a
	inc de
	; Check for negative displacement
	add a,a
	jr c,opgenroutinecall
	res 3,c
	; Check for positive displacement
	jr nz,opgenroutinecall
	res 5,c
	; Only generate a routine call for LD HL,SP+0
	bit 4,c
	jr nz,opgenroutinecall
	; For ADD SP,0 just clear all flags
	ex de,hl
	ld (hl),$ED ;LD HL,I
	inc hl
	ld (hl),$D7
	inc hl
	ld (hl),a ;NOP
	inc hl
	inc de
	ex de,hl
	inc b
	ret
	
opgenroutinecall_2cc:
	dec iyl
opgenroutinecall:
	inc hl
opgenroutinecall_noinc:
	ld a,$CD
	ld (de),a
	inc de
	inc b
	ld a,(bc)
	ld (de),a
	inc de
	ld a,c
	xor $0F
	ld c,a
	sub $FB ^ $0F ; Check for EI
	ld a,(bc)
	ld (de),a
	inc de
	ret nz
	inc sp \ inc sp \ inc sp
	ld c,(hl) ; Read next opcode
	dec b ; Sets sign flag
	ret
	
opgen_emit_load_cycle_offset_swap_1cc:
	dec iyl
opgen_emit_load_cycle_offset_swap:
	ex de,hl
opgen_emit_load_cycle_offset:
	ld (hl),$D9 ;EXX
	inc hl
	ld (hl),$0E ;LD C,cycle_offset
opgen_emit_cycle_offset:
	inc hl
	ld a,e
	sub iyl
	ld (hl),a
	ex de,hl
	push hl
recompile_cycle_offset_sp = $+1
	 ld hl,recompile_cycle_offset_stack - 1
	 ld (hl),d
	 dec hl
	 ld (hl),e
	 dec hl
	 ld (recompile_cycle_offset_sp),hl
	pop hl
	ex de,hl
	inc hl
	ret
	
_opgen31:
	ld a,$D9 ;EXX
	ld (de),a
	inc de
	ld a,$01 ;LD BC,nnnn
	ld (de),a
	inc de
	inc hl
	ldi
	ldi
	jr opgenroutinecall_noinc
	
_opgen08:
	inc hl
	ld c,(hl)
	inc hl
	ld a,(hl)
	ex de,hl
	ld (hl),$D9 ;EXX
	inc hl
	; Check for $FFxx
	inc a
	jr nz,_
	; Check for HRAM
	bit 7,c
	jr nz,opgen08_hram
_
	; Do fast implementation for WRAM, OAM, or HRAM
	cp $C0+1
	jr c,opgen08_slow
opgen08_hram:
	ld (hl),$21 ;LD HL,nnnn
	inc hl
	ld (hl),c
	ld bc,ophandler08_fast
	or a
	jr nz,opgen08_finish
	ld bc,ophandler08_hram
opgen08_finish:
	inc hl
	dec a
	ld (hl),a
	inc hl
	ld (hl),$CD ;CALL opgen08_*
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	dec iyl
	jp opgen_next_swap_skip_1cc	
opgen08_slow:
	ld (hl),$01 ;LD BC,nnnn
	inc hl
	ld (hl),c
	ld bc,ophandler08_slow
	jr opgen08_finish
	
_opgen36:
	inc hl
opgenCB_readwrite_finish:
	dec iyl
	ex de,hl
	ld (hl),RST_GET_HL_READWRITE_PTR
opgenCB_read_finish:
	inc hl
	ld (hl),$5B ;.LIL
	inc hl
	ld (hl),c
	inc hl
	ex de,hl
	jp opgen1byte
	
opgenCB_bc:
	ld c,$54 ;LD D,H
	jr z,_
	ld c,$5D ;LD E,L
_
	ex de,hl
	dec hl
	; Check for BIT
	cp $C0
	jp pe,opgenCB_bc_bit
	ld (hl),$EB ;EX DE,HL
	inc hl
	ld (hl),$DD
	inc hl
	ld (hl),c ;LD D,IXH or LD E,IXL
	inc hl
	ld (hl),$CB
	inc hl
	; Translate from B/C to D/E
	add a,2
	ld (hl),a
	inc hl
	ld (hl),$DD
	inc hl
	; Translate to LD IXH,D or LD IXL,E
	ld a,c
	add a,$62-$54
	ld (hl),a
	inc hl
	ld (hl),$EB ;EX DE,HL
	jp opgen_next_swap_skip
	
opgenCB_bc_bit:
	ld (hl),$ED ;LEA HL,IX
	inc hl
	ld (hl),$22
	inc hl
	ld (hl),$00
	inc hl
	ld (hl),$CB
	inc hl
	; Translate from B/C to H/L
	add a,4
	ld (hl),a
	jp opgen_next_swap_skip
	
_opgenCB:
	ld a,c
	ld (de),a
	inc hl
	ld a,(hl)
	xor $36
	ld c,a
	and $F8
	jr z,opgenCB_swap
	xor c
	jr z,opgenCB_mem
	inc de
	dec a
	jp z,opgen1byte
	cp 5
	ld a,(hl)
	jr nc,opgenCB_bc
	; Translate from D/E/H/L to B/C/D/E
	sub 2
	ld (de),a
	inc hl
	inc de
	jp opgen_next_fast
	
opgenCB_mem:
	dec iyl
	; Check for BIT
	ld a,c
	cp $C0
	ld c,$CB
	jp po,opgenCB_readwrite_finish
	ex de,hl
	ld (hl),RST_GET_HL_READ_PTR
	jp opgenCB_read_finish
	
opgenCB_swap:
	ex de,hl
	inc a
	xor c
	jr nz,_
	ld a,$0F ;RRCA
	ld (hl),a \ inc hl
	ld (hl),a \ inc hl
	ld (hl),a \ inc hl
	ld (hl),a \ inc hl
	ld (hl),$B7 ;OR A
	jp opgen_next_swap_skip
_
	dec a
	jr nz,_
	dec iyl
	dec iyl
_
	ld (hl),$CD
	inc hl
	ld c,a
	ld b,do_swap_c - do_swap_b
	mlt bc
	ld a,do_swap_hl_normal & $FF
	sub c
	ld (hl),a
	inc hl
	ld a,do_swap_hl_normal >> 8
	sbc a,b
	ld (hl),a
	jp opgen_next_swap_skip
	
opgen_emit_hl_read:
	ex de,hl
	ld (hl),RST_GET_HL_READ_PTR
	inc hl
	ld (hl),$49 ;.LIS
	ex de,hl
	jp opgen1byte_2cc_remap_inc
	
opgen_emit_hl_write:
	ex de,hl
	ld (hl),RST_GET_HL_READWRITE_PTR
	inc hl
	ld (hl),$49 ;.LIS
	ex de,hl
	jp opgen1byte_2cc_remap_inc
	
opgen_emit_hl_readwrite:
	ex de,hl
	ld (hl),RST_GET_HL_READWRITE_PTR
	inc hl
	ld (hl),$49 ;.LIS
	inc hl
	ex de,hl
	dec iyl
	dec iyl
	jp opgen1byte
	
opgen_emit_hl_read_post:
	ex de,hl
	ld (hl),RST_GET_HL_READ_PTR
	inc hl
	ld (hl),$5B ;.LIL
	inc hl
	ld (hl),$7E ;LD A,(HL)
	ex de,hl
	jp opgen1byte_2cc_remap_inc
	
opgen_emit_hl_write_post:
	ex de,hl
	ld (hl),RST_GET_HL_READWRITE_PTR
	inc hl
	ld (hl),$5B ;.LIL
	inc hl
	ld (hl),$77 ;LD (HL),A
	ex de,hl
	jp opgen1byte_2cc_remap_inc
	
opgen_emit_hl_read_bc:
	ex de,hl
	ld (hl),RST_GET_HL_READ_PTR
	inc hl
	ld (hl),$5B ;.LIL
	inc hl
	ld (hl),$6E ;LD L,(HL)
	inc hl
	ld (hl),$EB ;EX DE,HL
	inc hl
	ld (hl),$DD ;LD IXH/IXL,E
	inc hl
	ld a,c
	add a,$63-$46 ; Convert from LD B/C,(HL)
	ld (hl),a
	jp opgen_next_ex_swap_skip_1cc ;EX DE,HL
	
opgen_emit_hl_write_bc:
	ex de,hl
	ld (hl),$F5 ;PUSH AF
	inc hl
	ld (hl),$DD ;LD A,IXH/IXL
	inc hl
	ld a,c
	add a,$7C-$70 ; Convert from LD (HL),B/C
	ld (hl),a
	inc hl
	ld (hl),RST_GET_HL_READWRITE_PTR
	inc hl
	ld (hl),$5B ;.LIL
	inc hl
	ld (hl),$77 ;LD (HL),A
	inc hl
	ld (hl),$F1 ;POP AF
	jp opgen_next_swap_skip_1cc
	
opgen_emit_de_read:
	ex de,hl
	ld (hl),$CD ;CALL op_read_de_normal
	inc hl
	ld (hl),op_read_de_normal & $FF
	inc hl
	ld (hl),op_read_de_normal >> 8
	jp opgen_next_swap_skip_1cc
	
opgen_emit_de_write:
	ex de,hl
	ld (hl),$CD ;CALL op_write_de_normal
	inc hl
	ld (hl),op_write_de_normal & $FF
	inc hl
	ld (hl),op_write_de_normal >> 8
	jp opgen_next_swap_skip_1cc
	
opgen_emit_bc_read:
	ex de,hl
	ld (hl),$CD ;CALL op_read_bc_normal
	inc hl
	ld (hl),op_read_bc_normal & $FF
	inc hl
	ld (hl),op_read_bc_normal >> 8
	inc hl
	ld (hl),$D9 ;EXX
	jp opgen_next_swap_skip_1cc

opgen_emit_bc_write:
	ex de,hl
	ld (hl),$CD ;CALL op_write_bc_normal
	inc hl
	ld (hl),op_write_bc_normal & $FF
	inc hl
	ld (hl),op_write_bc_normal >> 8
	inc hl
	ld (hl),$D9 ;EXX
	jp opgen_next_swap_skip_1cc
	
opgenMBCorVRAMwrite:
	 add a,a
	 jr c,opgenVRAMwrite
	 ; Get the routine address
	 inc l
	 ld hl,(hl)
	 ex de,hl
	 ld (hl),$6F ;LD L,A
	 inc hl
	 ld (hl),$CD ;CALL mbc_write_*_handler
	 inc hl
	 ld (hl),e
	 inc hl
	 ld (hl),d
	pop de
	jp opgen_next_swap_skip
	
opgenVRAMwrite:
	pop hl
	push hl
	 call opgen_emit_load_cycle_offset_swap
	 ld (hl),$CD ;CALL vram_banked_write_handler
	 inc hl
	 ld (hl),vram_banked_write_handler & $FF
	 inc hl
	 ld (hl),vram_banked_write_handler >> 8
	 inc hl
	 ld (hl),c ;.DW addr
	 inc hl
	 ld (hl),b
	 jr opgenRAMwrite_finish
	
opgenCONSTwrite:
	dec iyl
	inc.s bc
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	ld a,b
	inc a
	jr z,opgenHMEMwrite
	inc a
	cp $E0+2
	jr c,_
	res 5,b ;Handle WRAM mirroring
_
	
	push hl
	 ld hl,z80codebase+mem_write_lut
	 ld l,b
	 ld l,(hl)
	 inc h ;mem_write_any_routines
	 
	 ld a,b
	 cp $A0
	 jr c,opgenMBCorVRAMwrite
	 
	 ; Check if there's a banked access routine
	 dec l
	 ld a,(hl)
	 cp $08 ;EX AF,AF'
	 jr z,opgen_banked_write
	 ; Get the raw write address
	 dec h \ dec h ;mem_get_ptr_routines
	 inc l \ inc l \ inc l
	 ld hl,(hl)
	 add hl,bc
	 ex de,hl
	 ld (hl),$5B ;LD.LIL (addr),A
	 inc hl
	 ld (hl),$32
	 inc hl
	 ld (hl),de
	 inc hl
	 inc hl
opgenRAMwrite_finish:
	 inc hl
	 ex de,hl
	 ; Check if the write is within 64 bytes of the block start,
	 ; potentially causing SMC
opgenCONSTwrite_smc = $+1
	 ld hl,0
	 add hl,bc
	 ld a,l
	 and $C0
	 or h
	pop hl
	inc hl
	jp nz,opgen_next
	; Set sign flag
	dec a
	ret
	
opgen_banked_write:
	 ex de,hl
	 ld (hl),$D9 ;EXX
	 inc hl
	 ld (hl),$01 ;LD BC,addr
	 inc hl
	 ld (hl),c
	 inc hl
	 ld (hl),b
	 inc hl
	 ld (hl),$CD ;CALL *_banked_write_handler
	 inc hl
	 ld (hl),e
	 inc hl
	 ld (hl),d
	 inc hl
	 ld (hl),$D9 ;EXX
	 jr opgenRAMwrite_finish
	
opgenFFwrite:
	dec iyl
	inc hl
	ld c,(hl)
opgenHMEMwrite:
	ld b,mem_write_port_lut >> 8
	ld.s a,(bc)
	; Check for direct write (no handler call)
	cp write_port_direct - mem_write_port_routines
	jr z,opgenHMEMwrite_direct
	; Check for ignored write (no handler call)
	cp write_port_ignore - mem_write_port_routines
	jr z,opgenHMEMwrite_ignored
	; Set bit 0 of B if a trampoline should be emitted
	rl b
	push hl
	 ; Resolve the real handler address
	 ld hl,z80codebase+mem_write_port_routines
	 ld l,a
	 ; Check the first byte of the handler to see if it's a trampoline
	 ld a,(hl)
	 cp $18 ;JR
	 jr z,_
	 cp $10 ;DJNZ
	 jr nz,++_
	 dec c ; Adjust the passed port LSB
_
	 ; Get the JR offset
	 inc hl
	 ld a,(hl)
	 dec hl
	 ; Check for a backwards jump, which corresponds to write_audio_disable SMC
	 rlca
	 jr c,opgen_port_write_resolved_jp
	 rrca
	 ; This is always a forward jump to the next code MSB
	 add a,l
	 ld l,a
	 inc h
	 jr opgen_port_write_resolved_jr
_
	 cp $C3 ;JP
	 jr nz,opgen_port_write_resolved_jp
	 inc hl
	 ld hl,(hl)
opgen_port_write_resolved_jp:
	 dec hl ; Move to preceding LD E,A (possibly the handler entry point)
	 dec hl ; Move to possible preceding EX AF,AF'
opgen_port_write_resolved_jr:
	 ; Check whether the routine needs to pass the port LSB
	 ld.s a,(hl)
	 sub $08 ;EX AF,AF'
	 jr z,_
	 ; If not, emit the EX AF,AF' directly into the JIT code
	 inc hl
	 ld c,$08 ;EX AF,AF'
	 ld a,$0E-1 ;LD C,
_
	 inc a ;LD BC,
	 ex (sp),hl
	 ex de,hl
	 ; Emit first part of port read impl
	 ld (hl),$6F ;LD L,A
	 inc hl
	 ld (hl),$D9 ;EXX
	 inc hl
	 ld (hl),a ;LD C, or LD BC,
	 call opgen_emit_cycle_offset
	 ld (hl),c ;EX AF,AF' or port LSB
	 inc hl
	 ; Check if a trampoline should be emitted
	 bit 0,b
	 ; Pop the handler address
	pop bc
	jr z,opgen_port_write_no_trampoline
opgenE2_finish:
	push hl
	 ld a,5
	 cp a ; Set Z flag
	 call allocate_high_trampoline_for_jit
	 ; Avoid emitting a trampoline if a flush is imminent,
	 ; to avoid overwriting code prior to this JIT block
	 jr c,_
	 ; Emit the GB address before the jump
	 inc de
	 call opgen_emit_gb_address_noinc
	 dec de
	 ; Emit the jump to the handler
	 push hl
	  ld (hl),$C3 ;JP write_*_handler
	  inc hl
	  ld (hl),c
	  inc hl
	  ld (hl),b
	 ; Use the trampoline as the handler
	 pop bc
_
	pop hl
opgen_port_write_no_trampoline:
	; Emit handler call
	ld (hl),$CD ;CALL write_*_handler
	inc hl
	ld (hl),c
	inc hl
	ld (hl),b
	jp opgen_next_swap_skip
	
opgenHMEMwrite_direct:
	ex de,hl
	ld (hl),$32 ;LD ($FF00+n),A
	inc hl
	ld (hl),c
	inc hl
	ld (hl),$FF
	jp opgen_next_swap_skip
	
opgenHMEMwrite_ignored:
	ex de,hl
	ld (hl),$18 ;JR $+3
	inc hl
	ld (hl),$01
	inc hl
	jp opgen_next_swap_skip
	
_opgenE2:
	call opgen_emit_load_cycle_offset_swap_1cc
	ld bc,op_write_c_hmem
	jr opgenE2_finish
	
opgenCONSTread:
	dec iyl
	inc.s bc
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	ld a,b
	inc a
	jr z,opgenHMEMread
	push hl
	 ; Check for a ROM read, needs a special handler because of trimming
	 bit 7,b
	 jr z,opgen_rom_read
	 inc a
	 cp $E2
	 jr c,_
	 res 5,b ; Handle WRAM mirroring
_
	 ; Look up the memory reads
	 ld hl,z80codebase+mem_read_lut
	 ld l,b
	 ld l,(hl)
	 dec h ;mem_read_any_routines
	 ; Check if there's a banked access routine
	 dec l
	 ld a,(hl)
	 cp $08 ;EX AF,AF'
	 jr z,opgen_banked_read
	 ; Get the raw read address
	 inc h \ inc h ;mem_get_ptr_routines
	 inc l \ inc l \ inc l
	 ld hl,(hl)
	 add hl,bc
	 ex de,hl
	 ld (hl),$5B ;LD.LIL A,(addr)
	 inc hl
	 ld (hl),$3A
	 inc hl
	 ld (hl),de
	 inc hl \ inc hl
	pop de
	jp opgen_next_swap_skip
	
opgen_rom_read:
	 ld hl,rom_any_read_handler
opgen_banked_read:
	 ex de,hl
	 ld (hl),$D9 ;EXX
	 inc hl
	 ld (hl),$01 ;LD BC,addr
	 inc hl
	 ld (hl),c
	 inc hl
	 ld (hl),b
	 inc hl
	 ld (hl),$CD ;CALL *_banked_read_handler
	 inc hl
	 ld (hl),e
	 inc hl
	 ld (hl),d
	 inc hl
	 ld (hl),$D9 ;EXX
	pop de
	jp opgen_next_swap_skip
	
opgenFFread:
	dec iyl
	inc hl
	ld c,(hl)
opgenHMEMread:
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
	call opgen_emit_load_cycle_offset
	ld (hl),$CD ;CALL read_*_handler
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
	ld (hl),$FF
	jp opgen_next_swap_skip
	
#ifdef SCHEDULER_LOG
scheduler_log:
	push ix
	 ld.sis hl,(stack_window_base)
	 ld e,iyl
	 ld d,0
	 add.s hl,de
	 ex af,af'
	 push af
	  push hl
	   exx
	   push de
	    push bc
	     push ix
	      push af
	      pop de
	      ld hl,z80codebase+flags_lut
	      ld l,e
	      ld e,(hl)
	      push de
	       ld.sis de,(event_gb_address)
	       call get_banked_address
	       push de
	        APRINTF(SchedulerLogMessage)
	       pop hl
	      pop de
	     pop ix
	    pop bc
	   pop de
	   exx
	  pop hl
	 pop af
	 ex af,af'
	pop ix
	ret.l
#endif
