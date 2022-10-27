; Routines to handle trampolines for special memory accesses.

; Trampolines are allocated into two pools, one growing forward (low pool)
; and the other growing backward (high pool). The low pool is shared with the
; rest of the JIT code.
; Being in the high pool indicates the trampoline call ends the instruction.
; Being in the low pool indicates the trampoline call is followed by at least
; one byte of code (e.g. INC DE, DEC DE, POP AF, NOP).
; If the byte of code is EX DE,HL this corresponds to a LD B/C,(HL) opcode
; and indicates a four-byte suffix (only relevant to the patcher, since read
; handlers never need to access the trampoline).
;
; Writes to I/O may require the Game Boy and JIT addresses of the following
; instruction. The Game Boy address is located behind the trampoline entry
; point, and the JIT address can be inferred from the trampoline pool and the
; return address. 
;
; Types of trampolines:
;   Read from I/O:
;     LD A,(rr):
;       EXX \ LD C,cycle_offset \ JP op_read_rr_port
;     LD B/C,(HL):
;       EXX \ LD C,cycle_offset \ JP op_read_hl_port_l
;     op r,(HL):
;       EXX \ LD C,cycle_offset \ CALL op_read_hl_port_l \ op r,L \ RET
;     BIT b,(HL):
;       EXX \ LD C,cycle_offset \ CALL op_read_hl_port_l \ BIT b,L \ RET
;   Write to I/O:
;     LD (rr),r:
;       .DW gb_addr \ LD L,r \ EXX \ LD C,cycle_offset \ JP op_write_rr_port
;     LD (HL),n:
;       .DW gb_addr \ LD L,n \ EXX \ LD C,cycle_offset \ JP op_write_hl_port
;     LD/LDH ($FF00+n),A (only when it may cause an interrupt):
;       .DW gb_addr \ JP op_write_port_*
;   Read-modify-write to I/O:
;     INC/DEC (HL)
;       .DW gb_addr \ EXX \ LD BC,cycle_offset | (opcode_L << 8) \ JP op_readwrite_hl_port_incdec
;     bitop (HL)
;       .DW gb_addr \ EXX \ LD BC,cycle_offset | (opcode_L << 8) \ JP op_readwrite_hl_port_bitwise
;     SWAP (HL)
;       .DW gb_addr \ EXX \ LD C,cycle_offset \ JP op_readwrite_hl_port_swap
;
;   Write to VRAM/OAM:
;     LD (rr),r:
;       LD L,r \ EXX \ LD C,cycle_offset \ JP op_write_rr_vram
;     LD (HL),n:
;       LD L,n \ EXX \ LD C,cycle_offset \ JP op_write_hl_vram
;   Read-modify-write to VRAM:
;     INC/DEC (HL)
;       EXX \ LD BC,cycle_offset | (opcode_L << 8) \ JP op_readwrite_hl_vram_incdec
;     bitop (HL)
;       EXX \ LD BC,cycle_offset | (opcode_L << 8) \ JP op_readwrite_hl_vram_bitwise
;     SWAP (HL)
;       EXX \ LD C,cycle_offset \ JP op_readwrite_hl_vram_swap
;
;   Write to MBC:
;     LD (HL),r:
;       LD L,r \ LD H,addr>>8 \ JP mbc_write_*_handler - 4
;     LD (HL),n:
;       LD HL,n | (addr&$FF00) \ JP mbc_write_*_handler - 4

	; Set Z flag and always allow allocation
	or a
	sbc hl,hl
	jr _
	; Input: A=trampoline size, Z set to allocate in high pool
	; Output: Carry set on failure, or HL=trampoline start
	; Destroys: F
allocate_trampoline:
	; Get the low pool pointer
	ld hl,(recompile_struct_end)
	ld hl,(hl)
	; Input: HL=current JIT output address to use as the allocation limit
	;        Z flag set
allocate_high_trampoline_for_jit:
	push bc
	 push de
	  ; Zero-extend the allocation size
	  ld bc,0
	  ld c,a
	  ; Advance the low pool pointer by the allocation size
	  add hl,bc
	  ex de,hl
	  ld hl,(z80codebase+trampoline_next)
	  jr nz,allocate_trampoline_low_pool
	  ; Check for overlap with the high pool pointer
	  sbc hl,de
	  jr c,allocate_trampoline_failed
	  ; Restore the high pool pointer and subtract the allocation size
	  add hl,de
	  sbc hl,bc
	  ; Write back the high pool pointer
	  ld (z80codebase+trampoline_next),hl
	  ; Prepend the error catcher to the new trampoline
	  ld de,ERROR_CATCHER
	  ld (hl),de
	  inc hl
	  inc hl
	  inc hl
allocate_trampoline_failed:
	 pop de
	pop bc
	ret
	
allocate_trampoline_low_pool:
	  ; Check for overlap with the high pool pointer
	  sbc hl,de
	  jr c,allocate_trampoline_failed
	  ; Write back the low pool pointer
	  ld hl,(recompile_struct_end)
	  ld (hl),de
	  ; Subtract the allocation size
	  ex de,hl
	  sbc hl,bc
	 pop de
	pop bc
	ret
	
	; Input: A=trampoline size (not including GB address)
	;        IX=location to emit CALL opcode in JIT implementation
	;        L=flags
	;          Bit 0: Reset if GB address should prefix the trampoline
	;          Bit 1: Set if CALL opcode is not at the end of the JIT implementation
	;          Bit 2: Set if GB opcode is 2 bytes, reset if 1 byte
	;          Bit 3: Reset if GB opcode has 2 memory cycles, set if only 1
	;        (SPS)=cycle count
	;        (SPS+2)=Game Boy BC
	;        (SPS+4)=Game Boy AF for some memory accesses
	;        BCDEHL' are swapped
	; Output: On success:
	;           A=cycle offset of last memory access
	;           BC=old HL
	;           HL=trampoline start (after emitted GB address if applicable)
	;           CALL to trampoline is emitted to JIT implementation
	;         On failure:
	;           Function does not return, JIT code is flushed
	; Destroys: F
lookup_gb_code_and_allocate_trampoline:
	lea.s bc,ix+3
lookup_gb_code_and_allocate_trampoline_any:
	push de
	 push ix
	  push hl
	   ; Account for GB address prefix
	   bit 0,l
	   jr nz,_
	   add a,2
_
	   ; Advance BC to the end of the JIT implementation
	   ; Also sets the Z flag input for trampoline allocation
	   bit 1,l
	   push af
	    jr z,_
	    inc bc
_
	    call lookup_gb_code_address
	    ; Negate and move back one cycle to the final memory access
	    cpl
	    ld c,a
	   pop af
	   call allocate_trampoline
	   ld a,c
	  pop bc
	  jr c,lookup_gb_code_and_allocate_trampoline_failed
	  ; Emit GB address prefix if needed
	  bit 0,c
	  jr nz,_
	  ld (hl),e
	  inc hl
	  ld (hl),d
	  inc hl
_
	 pop ix
	 ld.s (ix),$CD ;CALL trampoline
	 ld.s (ix+1),hl
	pop de
	ret
	
lookup_gb_code_and_allocate_trampoline_failed:
	; Check if the instruction is two bytes
	bit 2,c
	jr z,_
	; Move back an extra byte and cycle
	dec de
	dec a
_
	; Check if an additional cycle must be subtracted
	bit 3,c
	jr nz,_
	dec a
_
	; Move back one byte and one more cycle
	dec.s de
	dec a
	; ADL call stack is getting wiped so don't bother popping from it
	; Pop important information from the Z80 stack
	pop.s bc ; Cycle counter
	pop.s ix ; Game Boy BC
	; Check the Z80 stack offset; if it's odd, Game Boy AF was pushed
	; prior to the memory access, so it must be restored
	add hl,hl
	add hl,hl
	add.s hl,sp
	bit 1,l
	jr z,_
	ex af,af'
	pop.s af
	ex af,af'
_
	; Add cycle offset to the cycle count
	add a,c
	jr c,flush_normal
	dec b
	
; Flushes the JIT code and recompiles anew.
; Does not use a traditional call/return, must be jumped to directly.
;
; When the recompiler overflows, it returns a pointer to flush_handler,
; which provides this routine with the GB address.
;
; Inputs:  DE = GB address to recompile
;          BCDEHL' have been swapped
; Outputs: JIT is flushed and execution begins at the new recompiled block
;          BCDEHL' have been unswapped
flush_normal:
	ld c,a
flush_for_halt:
	push ix
	 push de
	  push bc
	   ld hl,$C3 | (do_event_pushed << 8)	;JP do_event_pushed
	   ld (flush_event_smc_1),hl
	   ld (flush_event_smc_2),hl
	   call flush_code
	   call lookup_code
	  pop hl
	 pop de
	 ex.s de,hl
	 exx
	 ex (sp),ix
	pop hl
	exx
	; Flush entire call stack, including interrupt returns
	ld sp,myADLstack
	ld.sis sp,myz80stack-4
	add a,e
	jr c,_
	dec d
_
	inc d
	exx
	jr z,_
	ex af,af'
	jp.s (hl)
_
	push.s hl
	exx
	ex.s (sp),ix
	inc.s bc
	ld c,a
	sub e
	ld b,a
	ex de,hl
#ifdef VALIDATE_SCHEDULE
	call schedule_event_helper
#else
	jp schedule_event_helper
#endif
	
	; Input: HL=pointer to opcode of the old JIT implementation
	;        HL'=memory access routine for write
	;        DE'=Game Boy HL
	;        A=opcode byte minus $36
	;        C=flags
	;          Bit 0: Always 1 (no GB address should prefix the trampoline)
	;          Bit 1: Set if IX+3 is not at the end of the JIT implementation
	;          Bit 2: Always 0 (for one opcode byte)
	;          Bit 3: Always 1 (for one memory cycle)
	;        Z flag set for LD (HL),n
patch_hl_mbc_write_helper:
	; We now have a LD (HL),r or LD (HL),n instruction
	dec hl
	dec hl
	push.s hl
	ex.s (sp),ix
	push.s de
	ld l,c
	ld d,$26 ;LD H,
	jr nz,patch_hl_mbc_write_reg_helper
	; Move second byte of opcode to trampoline and replace with NOP
	ld.s d,(ix+3)
	ld.s (ix+3),a ;0
	ld a,$21+($70-$68-$36) ;LD HL,nn
patch_hl_mbc_write_reg_helper:
	; Convert LD (HL),r to LD L,r
	sub $70-$68-$36
	ld e,a
	ld a,6
	call lookup_gb_code_and_allocate_trampoline
	ld (hl),e ;LD L,r or LD HL,nn
	inc hl
	ld (hl),d ;LD H,n or n
	exx
	; Get MBC routine address
	push hl
	 ld a,d ; Get MSB of write address
	 exx
	pop de
	; Move to the HL access entry point
	dec de \ dec de \ dec de \ dec de
	jr patch_hl_readwrite_helper_finish	
	
	; Input: IX=pointer after the RST of the old JIT implementation
	;        DE=memory access routine for write
	;        H=non-prefixed opcode byte from the old JIT implementation
	;        L=flags
	;          Bit 0: Reset if GB address should prefix the trampoline
	;          Bit 1: Set if IX+3 is not at the end of the JIT implementation
	;          Bit 2: Always 0 (for one opcode byte)
	;          Bit 3: Always 1 (for one memory cycle)
patch_hl_write_helper:
	dec ix
	; Here we have a write or read-modify-write
	; Convert (HL) access to L access
	ld a,h
	sub $08
	ld h,a
	; Check for INC (HL), DEC (HL), or CB-prefix
	sub $2E ;LD L,n
	rla
	jr c,patch_hl_readwrite_helper
	jr nz,patch_hl_write_reg_helper
	ld a,8
	call lookup_gb_code_and_allocate_trampoline
	ld (hl),b ;LD L,n
	inc hl
	; Move write value to trampoline and replace with NOP
	ld.s b,(ix+3)
	ld.s (ix+3),0 ;NOP
	ld (hl),b
	jr patch_hl_write_helper_finish
	
	; Input: IX=pointer after CALL of the old JIT implementation
	;        HL=memory access routine for write
	;        A=flags rotated right by 2, except bit 1
	;        Carry=bit 1 of flags
patch_bc_de_write_helper:
	ex de,hl
	lea ix,ix-3
	ld h,$6F ;LD L,A
	rla
	rlca
	ld l,a
patch_hl_write_reg_helper:
	ld a,7
	call lookup_gb_code_and_allocate_trampoline
	ld (hl),b ;LD L,r
patch_hl_write_helper_finish:
	inc hl
patch_port_direct_read_helper_finish:
	ld (hl),$D9 ;EXX
	inc hl
	ld (hl),$0E ;LD C,cycle_offset
patch_hl_readwrite_helper_finish:
	inc hl
	ld (hl),a
	inc hl
	ld (hl),$C3 ;JP op_write_*
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	jp.sis patch_memory_access_finish
	
patch_hl_readwrite_helper:
	; Retrieve op_readwrite_hl_*_incdec from before op_write_hl_*
	ex de,hl
	dec hl
	dec hl
	ld.s hl,(hl)
	ex de,hl
	; Check for CB prefix
	rra
	jr c,_
	; Advance to op_readwrite_hl_*_bitwise
	inc de \ inc de \ inc de \ inc de
	; Move second byte of opcode to trampoline and replace with NOP
	ld.s h,(ix+3)
	ld.s (ix+3),0
	; Switch from (HL) access to L access
	dec h
	; Indicate two opcode bytes
	set 2,l
_
	; Indicate two memory cycles
	res 3,l
	ld a,7
	call lookup_gb_code_and_allocate_trampoline
	ld (hl),$D9 ;EXX
	inc hl
	ld (hl),$01 ;LD BC,cycle_offset | (opcode << 8)
	inc hl
	ld (hl),a
	ld a,b
	jr patch_hl_readwrite_helper_finish
	
	; Input: IX=pointer after CALL of the old JIT implementation
	;        HL=memory access routine for write
	;        Carry=bit 0 of flags
patch_hl_readwrite_swap_helper:
	ex de,hl
	lea ix,ix-3
	; Indicate a two-byte, four-cycle instruction
	ld a,$04>>1
	rla
	ld l,a
	ld a,6
	call lookup_gb_code_and_allocate_trampoline
	jr patch_port_direct_read_helper_finish
	
	; Input: IX=pointer after CALL of the old JIT implementation
	;        HL=memory access routine for write
	;        Carry=bit 1 of flags
patch_bc_de_port_read_helper:
	ex de,hl
	lea ix,ix-3
	; Indicate a one-byte, two-cycle instruction and request no GB address
	ld a,$42
	rla
	rlca
	ld l,a
	.db $D2 ;JP NC,
patch_hl_port_direct_read_a_helper:
	ld de,op_read_hl_port
	lea.s bc,ix+3
patch_port_direct_read_helper:
	ld a,6
	call lookup_gb_code_and_allocate_trampoline_any
	jr patch_port_direct_read_helper_finish
	
	; Input: IX=pointer after the RST of the old JIT implementation
	;        DE=memory access routine for write
	;        H=non-prefixed opcode byte from the old JIT implementation
	;        L=flags
	;          Bit 0: Reset if GB address should prefix the trampoline
	;          Bit 1: Set if IX+3 is not at the end of the JIT implementation
	;          Bit 2: Always 0 (for one opcode byte)
	;          Bit 3: Always 1 (for one memory cycle)
patch_hl_port_read_helper:
	dec ix
	ld de,op_read_hl_port_l
	; Check for bitwise operation
	ld a,h
	cp $CB
	jr z,patch_hl_port_read_bitwise_helper
	cp $7E ;LD A,(HL)
	jr z,patch_hl_port_direct_read_a_helper
	cp $6E ;LD L,(HL)
	; Get the byte after the JIT implementation (minus 1, bit 1 of L is set)
	lea.s bc,ix+7-1
	jr z,patch_port_direct_read_helper
patch_hl_port_indirect_read_helper:
	ld a,8
	call patch_hl_port_indirect_read_common
patch_hl_port_indirect_read_finish:
	; Switch from (HL) access to L access
	dec b
	ld (hl),b ;Opcode reading L
	inc hl
	ld (hl),$C9 ;RET
	jp.sis patch_memory_access_finish
	
patch_hl_port_read_bitwise_helper:
	; Indicate a two-byte, three-cycle instruction
	set 2,l
	ld a,9
	call patch_hl_port_indirect_read_common
	ld (hl),b ;CB prefix
	inc hl
	; Move second byte of opcode to trampoline and replace with NOP
	ld.s b,(ix+3)
	ld.s (ix+3),0
	jr patch_hl_port_indirect_read_finish
	
patch_hl_port_indirect_read_common:
	call lookup_gb_code_and_allocate_trampoline
	ld (hl),$D9 ;EXX
	inc hl
	ld (hl),$0E ;LD C,cycle_offset
	inc hl
	ld (hl),a
	inc hl
	ld (hl),$CD ;CALL op_read_hl_port_l
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	ret
	