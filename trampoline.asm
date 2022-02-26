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
;       .DW gb_addr \ EXX \ LD C,cycle_offset \ jp op_readwrite_hl_port_swap
;
;   Write to VRAM:
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

allocate_high_trampoline:
	cp a
	; Input: A=trampoline size, Z set to allocate in high pool
	; Output: Carry set on failure, or HL=trampoline start
	; Destroys: F
allocate_trampoline:
	push bc
	 push de
	  ; Zero-extend the allocation size
	  ld bc,0
	  ld c,a
	  ; Advance the low pool pointer by the allocation size
	  ld hl,(recompile_struct_end)
	  ld hl,(hl)
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
	ret.l
	
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
	ret.l
	