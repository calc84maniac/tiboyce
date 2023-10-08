	.assume adl=0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Routine tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	.block (push_routines_start+256)-$
mem_read_any_routines:
	; This set of routines is for direct reads into the A register.
	; Reads from (DE) and (BC) use these routines, and some absolute reads.
	; The LSBs of these routines must match the get_ptr routine addresses.
	
	; For (DE) and (BC) reads, the Z flag is reset on entry, and the JIT
	; implementation may be patched for special read types.
	; May also be used as generic read routines, if the Z flag is set.
	; In the generic read case, a valid cycle offset must be passed.
	
	; Inputs: AF' is swapped, BC=Game Boy address, Z=1 for generic read
	;         If generic read, BCDEHL' are swapped, and E=cycle offset
	; Outputs: AF' is unswapped, A=read value
	;          If (DE) or (BC) read, JIT implementation may be patched
	; Destroys: HL, F' (and for generic read, possibly E and BC)
	
rtc_latched = $
	.db 0	;seconds
	.db 0	;minutes
	.db 0	;hours
	.dw 0	;days
	.fill 3,$FF	;non-registers
rtc_current = rtc_latched+8
	;.db 0	;seconds
	;.db 0	;minutes
	;.db 0	;hours
	;.dw 0	;days
	;.block 3	;non-registers
rtc_last = rtc_current+8
	;.db 0   ;seconds
	;.db 0   ;minutes
	;.db 0   ;hours
	;.dw 0   ;days
cram_open_bus_addr = rtc_last+5
	;.dw 0
	
try_unlock_sha:
	; 12 bytes of code, will be overwritten by RTC init
	in0 a,($06)
	ret.l z
	set 2,a
	out0 ($06),a
	ret.l
	
	; Reserved space for MBC writes
	.block (mem_read_any_routines+(6*4)-1)-$
	
	; Read-only regions come first for detection by stack handling
rom_banked_read_any:
rom_bank_base_for_read = $+2+z80codebase
	ld.lil hl,0
	add.l hl,bc
	ex af,af'
	ld.l a,(hl)
	ret
	
rom_unbanked_read_any:
rom_unbanked_base_for_read = $+2+z80codebase
	ld.lil hl,0
	add.l hl,bc
	ex af,af'
	ld.l a,(hl)
	ret
	nop
	
rom_trimmed_read_any:
	jp.lil rom_trimmed_read_any_helper
	
vram_banked_read_handler:
	ex af,af'
vram_banked_read_any:
vram_bank_base_for_read = $+2+z80codebase
	ld.lil hl,0
	add.l hl,bc
	ex af,af'
	ld.l a,(hl)
	ret
	
	; Read-write regions via long pointers start here
cram_banked_read_handler:
	ex af,af'
cram_banked_read_any:
cram_banked_read_any_protect_smc = $
cram_bank_base_for_read = $+2+z80codebase
	ld.lil hl,0
cram_banked_read_any_rtc_smc = $
cram_banked_read_any_mbc2_smc = $
	add.l hl,bc
	ex af,af'
	ld.l a,(hl)
	ret
	nop
	
wram_banked_read_handler:
	ex af,af'
wram_banked_read_any:
wram_bank_base_for_read = $+2+z80codebase
	ld.lil hl,0
_
	add.l hl,bc
	ex af,af'
	ld.l a,(hl)
	ret
	
wram_mirror_banked_read_any:
	ld.lil hl,(wram_mirror_bank_base)
	jr -_
	
wram_unbanked_read_any:
wram_unbanked_base_for_read = $+2+z80codebase
	ld.lil hl,0 ;wram_base
_
	add.l hl,bc
	ex af,af'
	ld.l a,(hl)
	ret

wram_mirror_unbanked_read_any:
	ld.lil hl,(wram_mirror_unbanked_base)
	jr -_
	
	; Read-write regions via short pointers start here
shadow_stack_read_any:
	ld hl,(shadow_stack_base-z80codebase)
	add hl,bc
	ex af,af'
	ld a,(hl)
	ret
	nop
	nop
	
hmem_read_any:
	jr nz,patch_bc_de_hmem_read
	bit 7,c
	jp z,port_read_any
oam_read_any:
	ex af,af'
	ld a,(bc)
	ret
	
patch_bc_de_hmem_read:
	ld hl,$FFFF-LY
	add hl,bc
	jp nc,patch_bc_de_port_read
	; Patch to to a HRAM BC or DE read
	pop hl
	dec hl
	dec hl
	push af
	ld a,(hl)
	add a,op_read_de_hram-op_read_de_normal
	ld (hl),a
	inc hl
	jr nc,_
	inc (hl)
_
	pop af
	inc hl
	ex af,af'
	ld a,(bc)
	jp (hl)
	
cram_open_bus_read_any:
	jp do_cram_open_bus_read_any
cram_rtc_read_any:
	; Patched in during setup if needed
	;ex af,af'
	;ld a,(hl)
	;ret
cram_mbc2_read_any:
	exx
	pop hl
	push hl
	call do_cram_open_bus_read_any_with_return
	ld l,a
	push af
	 exx
	 ld a,b
	 srl b
	 ld b,$A0>>1
	 rl b
	 add.l hl,bc
	 ld b,a
	 ld.l a,(hl)
	 exx
	 xor l
	 and $0F
	 xor l
	 exx
	 ld l,a
	pop af
	ld a,l
	ret
	
	; GBC BG palette data
	.block (mem_read_any_routines+256-64)-$
gbc_bg_palette_data:
	.block 64
	
	; LUT to index read routines.
	; Additionally, direct memory region base pointers may be retrieved
	; by adding 2 to the index and accessing from the get_ptr routine range.
	.block (mem_read_any_routines+256)-$
mem_read_lut:
	.fill $40, rom_unbanked_read_any & $FF
	.fill $40, rom_banked_read_any & $FF
	.fill $20, vram_banked_read_any & $FF
	.fill $20, cram_banked_read_any & $FF
	.fill $10, wram_unbanked_read_any & $FF
	.fill $10, wram_banked_read_any & $FF
	.fill $10, wram_mirror_unbanked_read_any & $FF
	.fill $0E, wram_mirror_banked_read_any & $FF
	.fill $01, oam_read_any & $FF
	.fill $01, hmem_read_any & $FF
	
mem_get_ptr_routines:
	; This set of routines is for direct pointer retrieval.
	; Reads, writes, and modifies through (HL) use these routines.
	; The LSBs of these routines must match the read_any/write_any addresses.
	
	; For (HL) accesses, the Z flag is reset on entry, and the JIT
	; implementation may be patched for special access types.
	; May also be used as generic pointer getters, if the Z flag is set.
	; Pointer retrieval may fail, in which case the caller must fall back on
	; the direct read and/or write routines.
	
	; Inputs: AF' is swapped, DE=Game Boy address, Z=1/C=0 for generic access
	; Outputs: AF' is unswapped, UHL=direct pointer
	;          If (HL) access, JIT implementation may be patched.
	;          If generic access, the shadow C flag is set on failure.
	; Destroys: HL, HL', F'
	
	; This is used to identify I/O as a unique region (e.g. for stack accesses)
io_region_base = $+z80codebase
	
mbc_cram_protect_get_write_ptr:
	jp nz,patch_hl_mbc_access
	scf
	ex af,af'
	ret
	
mbc_rom_bank_get_write_ptr:
	jp nz,patch_hl_mbc_access
	scf
	ex af,af'
	ret
	
mbc_cram_bank_get_write_ptr:
	jp nz,patch_hl_mbc_access
_
	scf
	ex af,af'
	ret
	
mbc_specific_get_write_ptr:
	jp nz,patch_hl_mbc_access
	jr -_
	
rom_banked_get_ptr:
	MATCH_LSB(rom_banked_get_ptr, rom_banked_read_any)
rom_bank_base = $+2+z80codebase
	ld.lil hl,0
	add.l hl,de
	ex af,af'
	ret
	nop
	nop
	
rom_unbanked_get_ptr:
	MATCH_LSB(rom_unbanked_get_ptr, rom_unbanked_read_any)
rom_unbanked_base = $+2+z80codebase
	ld.lil hl,0
	add.l hl,de
	ex af,af'
	ret
rom_trimmed_get_ptr_impl:
	jp do_rom_trimmed_get_ptr
	
rom_trimmed_get_ptr:
	MATCH_LSB(rom_trimmed_get_ptr, rom_trimmed_read_any)
rom_trimmed_base = $+2+z80codebase
	jr rom_trimmed_get_ptr_impl
	.dl $FFFFFF
	nop
	
vram_banked_get_read_ptr:
	MATCH_LSB(vram_banked_get_ptr, vram_banked_read_any)
vram_bank_base = $+2+z80codebase
	ld.lil hl,0
	add.l hl,de
	ex af,af'
	ret
	nop
	nop
	
	nop
cram_banked_get_ptr:
	MATCH_LSB(cram_banked_get_ptr, cram_banked_read_any)
cram_banked_get_ptr_protect_smc = $
cram_bank_base = $+2+z80codebase
	ld.lil hl,0
cram_banked_get_ptr_rtc_smc = $
cram_banked_get_ptr_mbc2_smc = $
	add.l hl,de
	ex af,af'
	ret
	
cram_rtc_get_read_ptr_finish:
	 exx
	pop af
	ex af,af'
	ret
	
wram_banked_get_ptr:
	MATCH_LSB(wram_banked_get_ptr, wram_banked_read_any)
wram_bank_base = $+2+z80codebase
	ld.lil hl,0
_
	add.l hl,de
	ex af,af'
	ret
	nop
	nop
	
wram_mirror_banked_get_ptr:
	MATCH_LSB(wram_mirror_banked_get_ptr, wram_mirror_banked_read_any)
wram_mirror_bank_base = $+2+z80codebase
	ld.lil hl,0
	jr -_
	
wram_unbanked_get_ptr:
	MATCH_LSB(wram_unbanked_get_ptr, wram_unbanked_read_any)
wram_unbanked_base = $+2+z80codebase
	ld.lil hl,0 ;wram_base
_
	add.l hl,de
	ex af,af'
	ret
	nop
	nop

wram_mirror_unbanked_get_ptr:
	MATCH_LSB(wram_mirror_unbanked_get_ptr, wram_mirror_unbanked_read_any)
wram_mirror_unbanked_base = $+2+z80codebase
	ld.lil hl,0 ;wram_base - $2000
	jr -_
	
shadow_stack_get_ptr:
	MATCH_LSB(shadow_stack_get_ptr, shadow_stack_read_any)
shadow_stack_base = $+2+z80codebase
	ld.lil hl,z80codebase
	add.l hl,de
	ex af,af'
	ret
	
hmem_get_ptr:
	MATCH_LSB(hmem_get_ptr, hmem_read_any)
	; This is used for pointer lookups by the JIT engine, and only for reads,
	; to more simply support HRAM execution.
hmem_unbanked_base = $+2+z80codebase
	ld.lil hl,hram_base
	jr hmem_get_pointer_impl
	
oam_get_ptr:
	MATCH_LSB(oam_get_ptr, oam_read_any)
oam_unbanked_base = $+2+z80codebase
	ld.lil hl,hram_base
	add.l hl,de
	ex af,af'
	ret
	
vram_oam_get_write_ptr:
	jp nz,patch_hl_vram_access
try_get_pointer_failed:
	scf
	ex af,af'
	ret
	
hmem_get_pointer_impl:
	jp nz,patch_hl_hmem_access
	bit 7,e
	jr z,try_get_pointer_failed
	add.l hl,de
	ex af,af'
	ret
	nop
	
cram_open_bus_get_ptr:
	MATCH_LSB(cram_open_bus_get_ptr, cram_open_bus_read_any)
	jp do_cram_open_bus_get_ptr
	; This is treated as a union because MBC1 and RTC cannot coexist
mbc1_preserved_cram_bank_base = $+z80codebase
cram_rtc_get_ptr:
	MATCH_LSB(cram_rtc_get_ptr, cram_rtc_read_any)
cram_mbc2_get_ptr:
	MATCH_LSB(cram_mbc2_get_ptr, cram_mbc2_read_any)
	; Check whether this is a read or write
	exx
	pop hl
	push hl
	push af
	 dec hl
	 ld a,(hl)
	 cp RST_GET_HL_READ_PTR
	 jr z,cram_rtc_get_read_ptr_finish
	 ; Check if the RTC has changed since the last update
	 ld.lil a,(mpRtcIntStatus)
	 rra
	 call c,update_rtc
	 ; Force a latch the next time one is requested
	 xor a
	 ld (mbc_rtc_latch_smc),a
	 exx
	 ; Copy the read value to the write value in case of read-modify-write
	 ld a,(hl)
	 set 3,l
	 ld (hl),a
	pop af
	ex af,af'
	ret
	nop
	
	; GBC OBJ palette data
	.block (mem_get_ptr_routines+256-64)-$
gbc_obj_palette_data:
	MATCH_LSB(gbc_obj_palette_data, gbc_bg_palette_data)
	.block 64
	
	; LUT to index write routines.
	.block (mem_get_ptr_routines+256)-$
mem_write_lut:
	.fill $20, mbc_cram_protect_get_write_ptr & $FF
	.fill $20, mbc_rom_bank_get_write_ptr & $FF
	.fill $20, mbc_cram_bank_get_write_ptr & $FF
	.fill $20, mbc_specific_get_write_ptr & $FF
	.fill $20, vram_oam_get_write_ptr & $FF
	.fill $20, cram_banked_get_ptr & $FF
	.fill $10, wram_unbanked_get_ptr & $FF
	.fill $10, wram_banked_get_ptr & $FF
	.fill $10, wram_mirror_unbanked_get_ptr & $FF
	.fill $0E, wram_mirror_banked_get_ptr & $FF
	.fill $01, vram_oam_get_write_ptr & $FF
	.fill $01, hmem_get_ptr & $FF
	
mem_write_any_routines:
	; This set of routines is for direct writes from the A register.
	; Writes to (DE) and (BC) use these routines, and some absolute writes.
	; The LSBs of these routines must match the get_ptr routine addresses.
	
	; For (DE) and (BC) writes, the Z flag is reset on entry, and the JIT
	; implementation may be patched for special write types.
	; May also be used as generic write routines, if the Z flag is set.
	; In the generic write case, a valid cycle offset must be passed.
	
	; Inputs: AF' is swapped, BC=Game Boy address, Z=1 for generic write
	;         If generic write, BCDEHL' are swapped, and E=cycle offset
	; Outputs: AF' is unswapped, A is written to address
	;          If (DE) or (BC) write, JIT implementation may be patched
	; Destroys: HL, HL', F' (and for generic write, possibly E and BC)
	
	; MBC write handlers come first for easy detection by absolute writes
mbc_cram_protect_write_any:
	MATCH_LSB(mbc_cram_protect_write_any, mbc_cram_protect_get_write_ptr)
mbc_cram_protect_handler_smc = $+1
	ld hl,mbc_write_cram_protect_handler
	ex af,af'
	jr handle_mbc_write_any
	
mbc_rom_bank_write_any:
	MATCH_LSB(mbc_rom_bank_write_any, mbc_rom_bank_get_write_ptr)
	ld hl,mbc_write_rom_bank_handler
	ex af,af'
	jr handle_mbc_write_any
	
mbc_cram_bank_write_any:
	MATCH_LSB(mbc_cram_bank_write_any, mbc_cram_bank_get_write_ptr)
mbc1_write_large_rom_handler_smc = $+1
	ld hl,mbc_write_cram_bank_handler
	ex af,af'
	jr handle_mbc_write_any
	
mbc_specific_write_any:
	MATCH_LSB(mbc_specific_write_any, mbc_specific_get_write_ptr)
	ld hl,mbc_write_denied_handler

	; Reserved space for ROM/VRAM reads in other routine tables.
	; Use this space for the MBC generic write handler.
	; Since no additional information (e.g. cycles) is needed, just call
	; the routine directly. We save BC' and DE' since the swap state of the
	; shadow registers is arbitrary (unswapped for DE write, swapped otherwise)
handle_mbc_write_any:
	ld (handle_mbc_write_any_smc),hl
	push bc
	 push de
	  exx
	  ld l,a
handle_mbc_write_any_smc = $+1
	  call 0
	  exx
	 pop de
	pop bc
	ret
	
	.block (mem_write_any_routines + (cram_banked_read_handler - mem_read_any_routines)) - $
cram_banked_write_handler:
	ex af,af'
cram_banked_write_any:
	MATCH_LSB(cram_banked_write_any, cram_banked_get_ptr)
cram_banked_write_any_protect_smc = $
cram_bank_base_for_write = $+2+z80codebase
	ld.lil hl,0
cram_banked_write_any_rtc_smc = $
cram_banked_write_any_mbc2_smc = $
	add.l hl,bc
	ex af,af'
	ld.l (hl),a
	ret
	nop
	
wram_banked_write_handler:
	ex af,af'
wram_banked_write_any:
	MATCH_LSB(wram_banked_write_any, wram_banked_get_ptr)
wram_bank_base_for_write = $+2+z80codebase
	ld.lil hl,0
_
	add.l hl,bc
	ex af,af'
	ld.l (hl),a
	ret
	
wram_mirror_banked_write_any:
	MATCH_LSB(wram_mirror_banked_write_any, wram_mirror_banked_get_ptr)
	ld.lil hl,(wram_mirror_bank_base)
	jr -_
	
wram_unbanked_write_any:
	MATCH_LSB(wram_unbanked_write_any, wram_unbanked_get_ptr)
wram_unbanked_base_for_write = $+2+z80codebase
	ld.lil hl,0 ;wram_base
_
	add.l hl,bc
	ex af,af'
	ld.l (hl),a
	ret

wram_mirror_unbanked_write_any:
	MATCH_LSB(wram_mirror_unbanked_write_any, wram_mirror_unbanked_get_ptr)
	ld.lil hl,(wram_mirror_unbanked_base)
	jr -_
	
shadow_stack_write_any:
	MATCH_LSB(shadow_stack_write_any, shadow_stack_get_ptr)
	ld hl,(shadow_stack_base-z80codebase)
	add hl,bc
	ex af,af'
	ld (hl),a
	ret
	nop
	nop
	
hmem_write_any:
	MATCH_LSB(hmem_write_any, hmem_get_ptr)
	jr nz,patch_bc_de_hmem_write
	; This unswaps shadow registers, so swap them back before returning
	call do_hmem_write_any
	exx
	ret
	
oam_write_any:
	MATCH_LSB(oam_write_any, oam_get_ptr)
	ex af,af'
	ld (bc),a
	ret
	
	.block 6
	
vram_oam_write_any:
	MATCH_LSB(vram_oam_write_any, vram_oam_get_write_ptr)
	jp nz,patch_bc_de_vram_write
	push bc
	 ld c,e
	 ld e,a
	 call updateSTAT
	pop bc
	; This unswaps shadow registers, so swap them back before returning
	call do_vram_oam_write_any
	exx
	ret
	nop
	nop
	nop
	
cram_open_bus_write_any:
	MATCH_LSB(cram_open_bus_write_any, cram_open_bus_get_ptr)
	ex af,af'
	ret
	nop
cram_rtc_write_any:
	MATCH_LSB(cram_rtc_write_any, cram_rtc_get_ptr)
cram_mbc2_write_any:
	MATCH_LSB(cram_mbc2_write_any, cram_mbc2_get_ptr)
	exx
	; Check if the RTC has changed since the last update
	ld.lil hl,mpRtcIntStatus
	bit.l 0,(hl)
	call nz,update_rtc
	; Force a latch the next time one is requested
	ld hl,mbc_rtc_latch_smc
	ld (hl),0
	exx
	set 3,l
	ex af,af'
	ld (hl),a
	ret
	
patch_bc_de_hmem_write:
	inc bc
	ld hl,$007F
	add hl,bc
	dec bc
	jp nc,patch_bc_de_port_write
	; Patch to to a HRAM BC or DE write
	pop hl
	dec hl
	dec hl
	push af
	ld a,(hl)
	add a,op_write_de_hram-op_write_de_normal
	ld (hl),a
	inc hl
	jr nc,_
	inc (hl)
_
	pop af
	inc hl
	ex af,af'
	ld (bc),a
	jp (hl)
	
	; LUT for the middle byte of WRAM (mirror) bank bases
	.block (mem_write_any_routines+256-8)-$
wram_bank_base_lut:
	.db $00-$20 ;((wram_gbc_base + $0000 - $2000) >> 8) & $FF
	.db $00-$20 ;((wram_gbc_base + $0000 - $2000) >> 8) & $FF
	.db $10-$20 ;((wram_gbc_base + $1000 - $2000) >> 8) & $FF
	.db $20-$20 ;((wram_gbc_base + $2000 - $2000) >> 8) & $FF
	.db $30-$20 ;((wram_gbc_base + $3000 - $2000) >> 8) & $FF
	.db $40-$20 ;((wram_gbc_base + $4000 - $2000) >> 8) & $FF
	.db $50-$20 ;((wram_gbc_base + $5000 - $2000) >> 8) & $FF
	.db $60-$20 ;((wram_gbc_base + $6000 - $2000) >> 8) & $FF
	
	#include "ports.asm"
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generic memory access routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Input: DE=Game Boy address
	; Output: Shadow carry set on failure
	;         On success, UHL=direct read address
	; Destroys: HL', F'
try_get_mem_read_ptr:
	ex af,af'
try_get_mem_read_ptr_swapped:
	ld h,mem_read_lut >> 8
	ld l,d
	ld l,(hl)
	inc h ;mem_get_ptr_routines
	; Indicate a non-patchable caller
	cp a ; Set Z flag, reset C flag
	jp (hl)
	
	; Input: DE=Game Boy address
	; Output: Shadow carry set on failure
	;         On success, UHL=direct read/write address
	; Destroys: F'
try_get_mem_readwrite_ptr:
	ex af,af'
try_get_mem_readwrite_ptr_swapped:
	ld h,mem_write_lut >> 8
	ld l,d
	ld l,(hl)
	dec h ;mem_get_ptr_routines
	; Indicate a non-patchable caller
	cp a ; Set Z flag, reset C flag
	jp (hl)
	
read_mem_any_stale_bus_next:
	inc bc
	inc e
	ld a,$FF
	; Input: BC'=Game Boy address, D',A'=cycle counter, E'=cycle offset, A=open bus value, BCDEHL' are swapped
	; Output: A=read value, BCDEHL' are swapped
	; Destroys: HL, E', HL', F'
read_mem_any:
	ex af,af'
	ld h,mem_read_lut >> 8
	ld l,b
	ld l,(hl)
	dec h ;mem_read_any_routines
	; Indicate a non-patchable caller
	cp a ; Set Z flag
	jp (hl)
	
	; Input: BC'=Game Boy address, A=value to write, E'=cycle offset, BCDEHL' are swapped
	; Output: Value is written to Game Boy address, BCDEHL' are swapped
	; Destroys: HL, F', HL', E', BC'
write_mem_any:
	ex af,af'
	ld h,mem_write_lut >> 8
	ld l,b
	ld l,(hl)
	inc h ;mem_write_any_routines
	; Indicate a non-patchable caller
	cp a ; Set Z flag
	jp (hl)
	
generic_write_gb_address = $
	.dw 0
do_hmem_write_any:
	ex af,af'
	exx
	ld l,a
	exx
	ex af,af'
	ld b,c
	ld c,e
	ld e,a
	ld h,mem_write_port_lut >> 8
	ld l,b
	ld l,(hl)
	inc h
	jp (hl)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Slowmem patching routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
unpatch_hl_hram_access:
	pop hl
	ex af,af'
	push af
	 ; Convert .SIS to .LIS or .SIL to .LIL
	 ld a,(hl)
	 or $09
	 ld (hl),a
	 inc hl
	 ld a,(hl)
	 cp $CB
	 jr z,unpatch_hl_hram_access_bitwise
	 xor $36 ;LD (HL),n
	 jr z,unpatch_hl_hram_access_readwrite
	 and $07 ;LD r,(HL) or op A,(HL)
	 jr nz,unpatch_hl_hram_access_readwrite
unpatch_hl_hram_access_readonly:
	 dec hl
	 dec hl
	 ld (hl),RST_GET_HL_READ_PTR
	pop af
	jp (hl)
unpatch_hl_hram_access_bitwise:
	 inc hl
	 ld a,(hl)
	 dec hl
	 add a,$40
	 jp pe,unpatch_hl_hram_access_readonly
unpatch_hl_hram_access_readwrite:
	 dec hl
	 dec hl
	 ld (hl),RST_GET_HL_READWRITE_PTR
	pop af
	jp (hl)
	
patch_hl_hmem_access:
	inc de
	ld hl,$007F
	add hl,de
	dec de
	jr nc,patch_hl_port_access
	pop hl
	dec hl
	; Check for RST vs. (get_hl_readwrite_ptr_swapped >> 8) == 0
	sla (hl)
	jr nc,_
	ex af,af'
	ld (hl),RST_GET_HL_HRAM_PTR
	inc hl
	; Convert .LIS to .SIS or .LIL to .SIL
	res 3,(hl)
	res 0,(hl)
	push de
	 ex (sp),hl
	ret
_
	pop hl
	push hl
	dec hl
	ld (hl),do_swap_hl_hram >> 8
	dec hl
	ld (hl),do_swap_hl_hram & $FF
	; Carry is reset, and AF is already unswapped
	ld h,a
	jp do_swap_hl_hram_finish
	
patch_hl_port_access:
	exx
	ld e,a
	ex (sp),ix
	ld a,(ix-1)
	cp RST_GET_HL_READ_PTR
	; Check for (get_hl_readwrite_ptr_swapped >> 8) == 0
	jr c,patch_hl_port_readwrite_swap
	push de
	 ld hl,(ix)
	 ; Check for read
	 jp.lil z,patch_hl_port_read_helper
	 dec l ; Reset bit 0 to request a GB address
	 ld de,op_write_hl_port
	 jp.lil patch_hl_write_helper
	
patch_hl_vram_access:
	exx
	ld e,a
	ex (sp),ix
	ld a,(ix-1)
	; Check for (get_hl_readwrite_ptr_swapped >> 8) == 0
	or a
	jr z,patch_hl_vram_readwrite_swap
	push de
	 ld hl,(ix)
	 ld de,op_write_hl_vram
	 jp.lil patch_hl_write_helper
	
patch_hl_port_readwrite_swap:
	ld hl,op_readwrite_hl_port_swap
	ex af,af'
	or a ; Request GB address
	jr _
	
patch_hl_vram_readwrite_swap:
	ld hl,op_readwrite_hl_vram_swap
	ex af,af'
	scf ; Request no GB address
_
	pop ix
	ex (sp),ix
	ld e,a
	push de
	 jp.lil patch_hl_readwrite_swap_helper
	
patch_bc_de_port_read:
	; Get the address of the routine being called
	ex (sp),ix
	ld hl,(ix-2)
	; Check EXX vs. EX AF,AF'
	bit 7,(hl)
	; Swap or unswap shadow registers
	exx
	ld hl,op_read_de_port
	jr z,_
	; For a BC write, shadow registers are now unswapped, so reswap
	exx
	ld hl,op_read_bc_port
	; Remove the EXX following the call
	ld (ix),0 ;NOP
	; Indicate a following byte
	scf
_
	ld e,a
	push de
	 jp.lil patch_bc_de_port_read_helper
	
patch_bc_de_port_write:
	; Get the address of the routine being called
	ex (sp),ix
	ld hl,(ix-2)
	; Check EXX vs. EX AF,AF'
	bit 7,(hl)
	; Swap or unswap shadow registers
	exx
	ld hl,op_write_de_port
	jr z,_
	; For a BC write, shadow registers are now unswapped, so reswap
	exx
	ld hl,op_write_bc_port
	; Remove the EXX following the call
	ld (ix),0 ;NOP
	; Indicate a following byte
	scf
_
	ld e,a
	push de
	 ; Indicate a one-byte, two-cycle instruction and request GB address
	 ld a,$02
	 jp.lil patch_bc_de_write_helper
	
patch_bc_de_vram_write:
	; Get the address of the routine being called
	ex (sp),ix
	ld hl,(ix-2)
	; Clear carry
	or a
	; Check EXX vs. EX AF,AF'
	bit 7,(hl)
	; Swap or unswap shadow registers
	exx
	ld hl,op_write_de_vram
	jr z,_
	; For a BC write, shadow registers are now unswapped, so reswap
	exx
	ld hl,op_write_bc_vram
	; Remove the EXX following the call
	ld (ix),0 ;NOP
	; Indicate a following byte
	scf
_
	ld e,a
	push de
	 ; Indicate a one-byte, two-cycle instruction and request no GB address
	 ld a,$42
	 jp.lil patch_bc_de_write_helper
	
patch_memory_access_finish:
	pop de
	ex (sp),ix
	ld a,e
	exx
	ex af,af'
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Read-only routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Input: IX=Game Boy BC
	; Output: A=read value, BCDEHL' are swapped
	;         If not possible, JIT instruction is patched
	; Destroys: BC', HL', F'
op_read_bc_normal:
	exx
	lea bc,ix
rom_any_read_handler:
	; Input: BC=Game Boy DE
	; Output: A=read value
	;         If not possible, JIT instruction is patched
	; Destroys: HL, F'
op_read_de_normal:
	ex af,af'
op_read_de_normal_swapped:
	ld h,mem_read_lut >> 8
	ld l,b
	ld l,(hl)
	; Implicitly reset Z flag to indicate a patchable caller
	dec h ;mem_read_any_routines
	jp (hl)
	
op_read_bc_hram:
	exx
	lea bc,ix
op_read_de_hram:
	ex af,af'
	ld hl,$FFFF-LY
	add hl,bc
	jr nc,_
	ex af,af'
	ld a,(bc)
	ret
_
	; Patch back to a normal BC or DE read
	pop hl
	push hl
	dec hl
	dec hl
	push af
	ld a,(hl)
	sub op_read_de_hram-op_read_de_normal
	ld (hl),a
	jr nc,_
	inc hl
	dec (hl)
_
	pop af
	jr op_read_de_normal_swapped
	
do_rom_trimmed_get_ptr:
	jp.lil rom_trimmed_get_ptr_helper
	
do_cram_open_bus_read_any:
	; Get the return address
	pop hl
	push hl
do_cram_open_bus_read_any_with_return:
	push af
	 call.il lookup_code_bus
	 jr c,do_cram_open_bus_stale_read
	pop af
	; Get the address of the routine call
	dec hl
	dec hl
	ld hl,(hl)
	; If the Z flag is reset, it may be a BC/DE read
	; If the Z flag is set, it may be a generic read
	; For LD A,(nnnn) the Z flag input is arbitrary
	jr z,do_cram_open_bus_read_not_bcde
	bit 7,(hl) ; Check for EXX
	jr nz,do_cram_open_bus_read_bc
	inc hl
	bit 2,(hl) ; Check for LD H,n or JR
	jr nz,do_cram_open_bus_read_de
do_cram_open_bus_read_absolute:
	; For LD A,(nnnn) read the MSB of the address
	ex af,af'
	ld a,b
	ret
	
do_cram_open_bus_read_not_bcde:
	inc hl
	bit 2,(hl) ; Check for LD H,n/INC E or JR
	jr z,do_cram_open_bus_read_absolute
	; For generic reads, preserve the input open bus value
	ex af,af'
	ret
	
do_cram_open_bus_stale_read:
	pop af
	ex af,af'
	ld a,$FF
	ret
	
do_cram_open_bus_read_bc:
	ex af,af'
	ld a,$0A ;LD A,(BC)
	ret
	
do_cram_open_bus_read_de:
	ex af,af'
	ld a,$1A ;LD A,(DE)
	ret
	
do_cram_open_bus_get_ptr:
	pop hl
	push hl
do_cram_open_bus_get_ptr_with_return:
	push af
	 ; Currently, try_get_mem_readwrite_ptr is used only by LD (nnnn),SP or PUSH
	 ; This is write-only, so just return a pointer to two bytes of scratch space
	 jr z,do_cram_open_bus_get_ptr_finish
	 call.il lookup_code_bus
	 jr c,do_cram_open_bus_get_ptr_finish
	 ; Check for RST vs. (get_hl_readwrite_ptr_swapped >> 8) == 0
	 dec hl
	 ld a,(hl)
	 inc hl
	 cp RST_GET_HL_READ_PTR
	 jr z,do_cram_open_bus_get_read_ptr
	 cp RST_GET_HL_READWRITE_PTR
	 jr z,do_cram_open_bus_get_readwrite_ptr
	 ; If neither RST, then this is SWAP (HL)
	 ld a,$36 ;Second byte of SWAP (HL)
do_cram_open_bus_get_ptr_finish:
	 ld.lil hl,z80codebase+cram_open_bus_addr
	 ld (hl),a
	pop af
	ex af,af'
	ret
	
do_cram_open_bus_get_read_ptr:
	 bit 2,(hl) ;Check for .LIS or .LIL
	 inc hl
	 ld a,(hl)
	 jr nz,do_cram_open_bus_get_read_ptr_long
	 ; Check for op A,(HL)
	 cp $7E
	 jr nc,do_cram_open_bus_get_ptr_finish
	 ; Convert LD B/C/D/E,(HL) back to LD D/E/H/L,(HL)
	 add a,$50-$40
	 jr do_cram_open_bus_get_ptr_finish
	 
do_cram_open_bus_get_read_ptr_long:
	 cp $7E ;LD A,(HL)
	 inc hl
	 ld a,(hl)
	 jr z,do_cram_open_bus_get_read_ptr_prepost
	 ; Higher means CB prefix, and we already loaded the second byte
	 jr nc,do_cram_open_bus_get_ptr_finish
	 ; Lower means LD L,(HL) so resolve the load target
	 inc hl ; Skip EX DE,HL
	 inc hl ; Skip DD prefix
	 ld a,(hl) ; LD H/L,E
	 sub $63-$46 ; Convert to LD B/C,(HL)
	 jr do_cram_open_bus_get_ptr_finish
	 
do_cram_open_bus_get_read_ptr_prepost:
	 ; Convert from INC/DEC DE to LDI/LDD A,(HL)
	 add a,a
	 sub ($23<<1)-$22
	 jr do_cram_open_bus_get_ptr_finish
	
do_cram_open_bus_get_readwrite_ptr:
	 ; We only care about read-modify-writes
	 inc hl ; Skip over mode prefix
	 ld a,(hl)
	 cp $CB
	 ; Non-bitwise read-modify-writes can only be INC/DEC (HL)
	 jr nz,do_cram_open_bus_get_ptr_finish
	 ; Read the second CB prefix byte
	 inc hl
	 ld a,(hl)
	 jr do_cram_open_bus_get_ptr_finish
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Write and read-modify-write routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Input: IX=Game Boy BC, A=write value
	; Output: Value is written to memory, BCDEHL' are swapped
	;         If not possible, JIT instruction is patched
	; Destroys: BC', HL', F'
op_write_bc_normal:
	exx
	lea bc,ix
	; Input: BC=Game Boy DE, A=write value
	; Output: Value is written to memory
	;         If not possible, JIT instruction is patched
	; Destroys: HL, F'
op_write_de_normal:
	ex af,af'
op_write_de_normal_swapped:
	ld h,mem_write_lut >> 8
	ld l,b
	ld l,(hl)
	; Implicitly reset Z flag to indicate a patchable caller
	inc h ;mem_write_any_routines
	jp (hl)
	
op_write_bc_hram:
	exx
	lea bc,ix
op_write_de_hram:
	ex af,af'
	inc bc
	ld hl,$007F
	add hl,bc
	dec bc
	jr nc,_
	ex af,af'
	ld (bc),a
	ret
_
	; Patch back to a normal BC or DE write
	pop hl
	push hl
	dec hl
	dec hl
	push af
	ld a,(hl)
	sub op_write_de_hram-op_write_de_normal
	ld (hl),a
	jr nc,_
	inc hl
	dec (hl)
_
	pop af
	jr op_write_de_normal_swapped
	
patch_hl_mbc_access:
	; Get the access routine address
	ld h,mem_write_any_routines >> 8
	inc hl
	ld hl,(hl)
	; Get the access instruction type
	exx
	ld e,a
	pop hl
	ld c,(hl)
	inc hl
	ld a,(hl)
	sub $36 ;LD (HL),n
	jp.lil p,patch_hl_mbc_write_helper
	; We now have a read-modify-write instruction
	ld bc,$0118 ;JR $+1 (skip over the RET)
	jr c,do_hl_mbc_access_incdec
	; Check for SWAP (HL), which has PUSH AF as the following instruction
	cp $CB-$36
	jr nz,do_hl_mbc_swap
	; Get the second byte of the CB prefix instruction
	inc hl
	ld b,(hl)
	.db $C2 ;JP NZ,
do_hl_mbc_access_incdec:
	; Switch from INC/DEC (HL) to INC/DEC L
	sub $34-$2C-$36
	; Push the advanced return address
	inc hl
	push hl
	ld c,a ; CB prefix or INC/DEC instruction
	; Switch from (HL) access to L access, or $01 to NOP
	dec b
do_hl_mbc_swap:
	; Write the read/modify/write instruction
	ld (do_hl_mbc_read_modify_write_smc),bc
	; Restore the cycle counter
	ld a,e
	exx
	; Push the MBC write routine address
	push hl
	; Get the ROM read pointer (this cannot fail)
	call get_hl_read_ptr_swapped
	; Read the ROM value into L
	ld.l l,(hl)
	; Modify L and affect flags
do_hl_mbc_read_modify_write_smc = $
	rlc l
	; Execute the MBC write
	ret
	; Unswap AF for real this time since SWAP (HL) doesn't swap it
	ex af,af'
do_swap_for_memory:
	; Perform a SWAP
	ld h,a
	ld a,l
	rrca
	rrca 
	rrca
	rrca
	ld l,a
	or a
	ld a,h
	; Execute the MBC write
	ret
	
vram_oam_write_handler:
	ex af,af'
	ld e,a
	call updateSTAT
	pop hl
	ld bc,(hl)
	inc hl
	inc hl
	push hl
do_vram_oam_write_any:
	bit 6,b
	jr nz,do_oam_write_any
	push de
	 ex af,af'
	 ld e,a
	 ex af,af'
	 ld a,r
	 ld a,e
gbc_write_vram_and_expand_smc_1 = z80codebase+$+2
	 jp.lil p,write_vram_and_expand
gbc_write_vram_and_expand_catchup_smc_1 = z80codebase+$+2
	 jp.lil write_vram_and_expand_catchup
	
	; Input: IX=Game Boy BC, L=write value, C'=cycle offset, BCDEHL' are swapped
	; Output: Value written, or write instruction is unpatched
	; Destroys: HL, BC', E', HL', F'
op_write_bc_vram:
	ex af,af'
	ld e,a
	call updateSTAT
	lea bc,ix
	ld a,b
	cp $20
	jp po,unpatch_op_write_bc_vram
	exx
	ld a,r
	ld a,l
	exx
	push de
	 ld e,a
gbc_write_vram_and_expand_smc_2 = z80codebase+$+2
	 jp.lil p,write_vram_and_expand
gbc_write_vram_and_expand_catchup_smc_2 = z80codebase+$+2
	 jp.lil write_vram_and_expand_catchup
	
	; Input: BC=Game Boy DE, L=write value, C'=cycle offset, BCDEHL' are swapped
	; Output: Value written, or write instruction is unpatched
	; Destroys: HL, BC', E', HL', F'
op_write_de_vram:
	ex af,af'
	ld e,a
	call updateSTAT
	exx
	ld a,b
	cp $20
	jp po,unpatch_op_write_de_vram
	ld a,r
	ld a,l
	push bc
	 exx
	pop bc
	push de
	 ld e,a
gbc_write_vram_and_expand_smc_3 = z80codebase+$+2
	 jp.lil p,write_vram_and_expand
gbc_write_vram_and_expand_catchup_smc_3 = z80codebase+$+2
	 jp.lil write_vram_and_expand_catchup
	
op_readwrite_hl_vram_swap:
	ld hl,$0318 ;JR $+5
	jr op_readwrite_hl_vram_2
op_readwrite_hl_vram_incdec:
	ld l,$00 ;NOP
	jr op_readwrite_hl_vram
op_readwrite_hl_vram_bitwise:
	ld l,$CB
	; Input: DE=Game Boy HL, LB'=modify opcode, C'=cycle offset, BCDEHL' are swapped
	; Output: Value read and written, or read-write instruction is unpatched
	; Destroys: HL, BC', E', HL', F'
op_readwrite_hl_vram:
	ld h,b
op_readwrite_hl_vram_2:
	ld (op_readwrite_hl_vram_smc),hl
	ex af,af'
	ld e,a
	call updateSTAT
	exx
	ld a,d
	cp $20
	jp po,unpatch_op_readwrite_hl_vram
	ld.lil hl,(vram_bank_base)
	add.l hl,de
	ld.l l,(hl)
	ex af,af'
op_readwrite_hl_vram_smc = $
	rlc l
	ex af,af'
	jr op_write_hl_vram_finish
	call do_swap_for_memory
	ex af,af'
	jr op_write_hl_vram_finish
	
do_oam_write_any:
	ld a,c
	exx
	ld h,a
	ex af,af'
	ld l,a
	ex af,af'
	jr op_write_any_oam
	
	; Input: DE=Game Boy HL, L=write value, C'=cycle offset, BCDEHL' are swapped
	; Output: Value written, or write instruction is unpatched
	; Destroys: HL, BC', E', HL', F'
	.dw op_readwrite_hl_vram_incdec
op_write_hl_vram:
	ex af,af'
	ld e,a
	call updateSTAT
	exx
	ld a,d
	cp $20
	jp po,unpatch_op_write_hl_vram
op_write_hl_vram_finish:
	ld a,r
	ld a,l
	push de
	 exx
	pop bc
	push de
	 ld e,a
gbc_write_vram_and_expand_smc_4 = z80codebase+$+2
	 jp.lil p,write_vram_and_expand
gbc_write_vram_and_expand_catchup_smc_4 = z80codebase+$+2
	 jp.lil write_vram_and_expand_catchup

	; Input: IX=Game Boy BC, L=write value, C'=cycle offset, BCDEHL' are swapped
	; Output: Value written to port at BC
	;         If not possible, JIT instruction is patched
	; Destroys: HL, BC', E', HL', F'
op_write_bc_port:
	ex af,af'
	ld e,a
	ld a,ixh
	inc a
	jr nz,unpatch_op_write_bc_port
	ld b,ixl
	ld h,mem_write_port_lut >> 8
	ld l,b
	ld l,(hl)
	inc h
	jp (hl)
	
unpatch_op_write_bc_vram:
	; Check for OAM write
	cp $FE
	jr z,op_write_bc_oam
unpatch_op_write_bc_port:
	ld a,e
	ex af,af'
	exx
	pop hl
	ld (hl),$D9 ;EXX
	dec hl
	ld (hl),op_write_bc_normal >> 8
	dec hl
	ld (hl),op_write_bc_normal & $FF
	dec hl
	jp (hl)
	
	; Input: BC=Game Boy DE, L=write value, C'=cycle offset, BCDEHL' are swapped
	; Output: Value written to port at DE
	;         If not possible, JIT instruction is patched
	; Destroys: HL, BC', E', HL', F'
op_write_de_port:
	ex af,af'
	ld e,a
	exx
	ld a,b
	inc a
	jr nz,unpatch_op_write_de_port
	ld a,c
	exx
	ld b,a
	ld h,mem_write_port_lut >> 8
	ld l,b
	ld l,(hl)
	inc h
	jp (hl)

unpatch_op_write_de_vram:
	; Check for OAM write
	cp $FE
	ld h,c
	jr z,op_write_any_oam
unpatch_op_write_de_port:
	exx
	ld a,e
	ex af,af'
	exx
	pop hl
	dec hl
	ld (hl),op_write_de_normal >> 8
	dec hl
	ld (hl),op_write_de_normal & $FF
	dec hl
	jp (hl)

op_write_bc_oam:
	ld a,ixl
	ld h,a
	; Input: H=OAM write address, L=write value, DE'=cycle counter, AF' is swapped
	;        LY/STAT have been updated
op_write_any_oam:
	; Check for rendering catchup
	ld a,r
sprite_catchup_available = $+1
	or 0 ; Bit 7 is set if BG has been rendered ahead of sprites
	call.il m,write_oam_catchup
	; Write value to OAM
	ld a,l
	ld l,h
	ld h,$FE
	ld (hl),a
	; Restore cycle counter
	exx
	ld a,e
	exx
	ex af,af'
	ret

	; Input: DE=Game Boy HL, L=write value, C'=cycle offset, BCDEHL' are swapped
	; Output: Value is written to the port at Game Boy HL
	;         If not possible, JIT instruction is patched
	; Destroys: HL, BC', E', HL', F'
	.dw op_readwrite_hl_port_incdec
op_write_hl_port:
	ex af,af'
	ld e,a
	exx
	ld a,d
	inc a
	jr nz,unpatch_op_write_hl_port
	ld a,e
	exx
	ld b,a
	ld h,mem_write_port_lut >> 8
	ld l,b
	ld l,(hl)
	inc h
	jp (hl)
	
	; Input: IXL=Game Boy C, A=write value, C'=cycle offset, BCDEHL' is swapped
	; Output: A is written to ($FF00+C)
	; Destroys: HL, BC', E', HL', F'
op_write_c_hmem:
	exx
	ld l,a
	exx
	ex af,af'
	ld e,a
	ld b,ixl
	ld l,b
	ld h,mem_write_port_lut >> 8
	ld l,(hl)
	inc h
	jp (hl)
	
unpatch_op_write_hl_vram:
	; Check for OAM write
	cp $FE
	ld h,e
	jr z,op_write_any_oam
unpatch_op_write_hl_port:
	; Restore cycle counter in A'
	exx
	ld a,e
	exx
	; Get the trampoline address
	pop hl
	push hl
	dec hl
	dec hl
	ld hl,(hl)
unpatch_op_write_hl_any:
	ex af,af'
	; Put trampoline address in DE
	ex de,hl
	; Save old value of DE and get pointer following CALL in HL
	ex (sp),hl
	 push af
	  ; Get the load opcode
	  ld a,(de)
	  ; Check if it's LD L,r
	  cp $68
	  jr nc,_
	  ; If not, it's LD L,n or LD HL,nn
	  ; Get the immediate value and place it after the CALL
	  inc de
	  ld a,(de)
	  ld (hl),a
	  ld a,$2E ;LD L,n
_
	  dec hl
	  ; Translate from LD L, to LD (HL),
	  sub -8 ; Sets carry
	  ld (hl),a
	  dec hl
	  ; Check which pool the trampoline is in
	  ; Carry set if high pool
	  push hl
	   ld hl,(trampoline_next)
	   sbc hl,de
	  pop hl
	  sbc a,a
	  and $49^$5B ;.LIS/.LIL
	  xor $5B ;.LIL
	  ld (hl),a
	  dec hl
	  ld (hl),RST_GET_HL_READWRITE_PTR
	 pop af
	pop de
	jp (hl)
	
op_readwrite_hl_oam:
	ld (op_readwrite_hl_oam_smc),hl
	exx
	ld a,(de)
	ld l,a
	ld h,e
	ex af,af'
op_readwrite_hl_oam_smc = $
	rlc l
	ex af,af'
	jr op_write_any_oam
	call do_swap_for_memory
	ex af,af'
	ld h,e
	jr op_write_any_oam
	
unpatch_op_write_hl_mbc:
	; Get the trampoline address
	dec hl
	dec hl
	ld hl,(hl)
	ex af,af'
	bit 7,d
	jr nz,unpatch_op_write_hl_any
	push hl
	 ; Update the new MSB
	 inc hl
	 inc hl
	 ld (hl),d
	 inc hl
	 inc hl
	 push de
	  ex de,hl
	  ; Update the routine address
	  ld l,h
	  ld h,mem_write_lut >> 8
	  ld l,(hl)
	  inc h ;mem_write_any_routines
	  inc hl
	  ld hl,(hl)
	  dec hl \ dec hl \ dec hl \ dec hl
	  ex de,hl
	  ld (hl),de
	 pop de
	 ex af,af'
	 ; Re-execute the trampoline to initialize the HL input
	 ret
	
unpatch_op_readwrite_hl_vram:
	; Check for OAM write
	cp $FE
	exx
	ld hl,(op_readwrite_hl_vram_smc)
	jr z,op_readwrite_hl_oam
unpatch_op_readwrite_hl_port:
	; Get the address following the CALL
	ex (sp),hl
	; Put the opcode SMC bytes in BC
	pop bc
	dec hl
	ld a,c
	or a ;NOP
	jr nz,_
	ld a,b
	add a,8 ; Switch opcode byte from L back to (HL)
	ld (hl),a
	dec hl
	ld (hl),$49 ;.LIS
	jr unpatch_op_readwrite_hl_finish
_
	cp $CB ;bitwise prefix
	jr nz,_
	inc b ; Switch second opcode byte from L back to (HL)
	ld (hl),bc
	dec hl
	ld (hl),$5B ;.LIL
unpatch_op_readwrite_hl_finish:
	dec hl
	ld (hl),RST_GET_HL_READWRITE_PTR
unpatch_op_readwrite_hl_finish_2:
	push hl
	 ld a,e
	 ex af,af'
	 exx
	ret
_
	ld (hl),do_swap_hl_normal >> 8
	dec hl
	ld (hl),do_swap_hl_normal & $FF
	dec hl
	jr unpatch_op_readwrite_hl_finish_2
	
op_readwrite_hl_port_swap:
	;JR op_readwrite_hl_port_swap_finish
	ld hl,$18 | ((op_readwrite_hl_port_swap_finish - (op_readwrite_hl_port_smc+2)) << 8)
	jr op_readwrite_hl_port_2
op_readwrite_hl_port_incdec:
	ld l,$00 ;NOP
	jr op_readwrite_hl_port
op_readwrite_hl_port_bitwise:
	ld l,$CB
	; Input: DE=Game Boy HL, LB'=modify opcode, C'=cycle offset, BCDEHL' are swapped
	; Output: Value read and written, or read-write instruction is unpatched
	; Destroys: HL, BC', E', HL', F'
op_readwrite_hl_port:
	ld h,b
op_readwrite_hl_port_2:
	ld (op_readwrite_hl_port_smc),hl
	ex af,af'
	ld e,a
	exx
	ld a,d
	inc a
	ld a,e
	exx
	jr nz,unpatch_op_readwrite_hl_port
	ld b,a
	push bc
	 dec c ; Read happens one cycle before write
	 call op_read_hl_port_l_unchecked
op_readwrite_hl_port_smc = $
	 rlc l
op_readwrite_hl_port_finish:
	 ex af,af'
	 exx
	pop bc
	ld h,mem_write_port_lut >> 8
	ld l,b
	ld l,(hl)
	inc h
	jp (hl)
op_readwrite_hl_port_swap_finish:
	 call do_swap_for_memory
	 jr op_readwrite_hl_port_finish
	