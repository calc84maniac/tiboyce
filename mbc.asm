	.assume adl=0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MBC write handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; Prefix for MBC write handlers allowing direct-call for HL write ops
#macro OP_WRITE_HL_MBC
	pop af
	pop hl
	push hl
	jp unpatch_op_write_hl_mbc
	; Inputs: H = byte to write to MBC register at Game Boy HL
	;         L = expected MSB of written address
	;         If not possible, JIT instruction is patched
	; Output: Cartridge state is updated, IY (Game Boy SP) may be modified
	; Destroys: HL, DE', HL', F'
	push af
	 ld a,d
	 cp h
	 .db $20 ;JR NZ,$-9
	; Following code must be PUSH AF
#endmacro

; For each of the following routines:
;  Input: A=byte to write to MBC register
;  Output: Cartridge state is updated, IY (Game Boy SP) may be modified
;  Destroys: HL, DE', HL', F'
	OP_WRITE_HL_MBC
mbc1_write_large_rom_handler:
	push af
	 ld a,l
mbc1_large_rom_ram_banking_smc = $
	 ld hl,mbc_write_cram_bank_handler ; Replaced with CALL if needed
	 rrca
	 rrca
	 rrca
mbc1_rom_size_smc = $+1
	 and 0
	 ld (mbc1_upper_bits_smc),a
	 ld a,(curr_rom_bank)
	 jr mbc1_large_rom_continue
	
	
	OP_WRITE_HL_MBC
mbc_write_rom_bank_handler:
	push af
	 ld a,l
mbc1_large_rom_continue:
mbc5_rom_bank_optimize_more_dst = $ ; Copy to here to eliminate masking
	; Adjust value to physical page based on ROM size and check if 0-page
	; might need to be overridden
rom_bank_mask_smc = $+1
	 and 0
mbc5_rom_bank_optimize_dst = $ ; Copy to here to eliminate zero mask check
mbc_zero_page_optimize_smc = $+1 ; Update jump when zero mask equals bank mask
	 jr z,mbc_zero_page_override_fast
mbc_zero_page_continue:
mbc1_upper_bits_smc = $+1 ; Combine with upper bits of bank
mbc1_upper_bits_dst = $+2 ; Copy to here to enable upper bit combine
	 ;or 0
mbc_optimize_start = $ ; Copy this sequence of code forward or backward
	 exx
	 ld b,a
	 ld (rom_bank_check_smc_1),a
	 ld hl,curr_rom_bank
	 xor (hl)
	 ld (hl),b
	 jp.lil nz,mbc_change_rom_bank_helper
	 exx
	pop af
	ret
mbc_optimize_size = $ - mbc_optimize_start
	
	 ; Switch to page 0 or 1 depending on the written value
mbc_zero_page_override_mbc2:
	 add hl,hl
mbc_zero_page_override_mbc1:
	 add hl,hl
	 add hl,hl
mbc_zero_page_override_mbc3:
	 add hl,hl
	 cp l
	 sbc a,a
	 ; Fast entry point if the zero mask is the same as the ROM size mask
mbc_zero_page_override_fast:
	 inc a
	 jr mbc_zero_page_continue
	
	OP_WRITE_HL_MBC
mbc_write_rtc_cram_bank_handler:
	push af
	 bit 3,l
	 jr nz,mbc_rtc_switch_to_rtc
	 ; Check if an RTC register is currently mapped in
	 ld a,(cram_banked_get_ptr_rtc_smc)
	 rra ;JR vs. ADD.L
	 jr c,mbc_write_cram_common
	 ; See if SP is pointing into the swapped bank
	 ld a,(curr_gb_stack_region)
	 sub cram_bank_base & $FF
	 ld a,l
	 exx
	 ; Switch back to fastmem
	 ld hl,$1949 ;ADD.L HL,DE
	 ld (cram_banked_get_ptr_rtc_smc),hl
	 ld h,$09 ;ADD.L HL,BC
	 ld (cram_banked_read_any_rtc_smc),hl
	 ld (cram_banked_write_any_rtc_smc),hl
	 jr nz,mbc_write_cram_common_swapped
	 ; Force fixup of the stack access routines
	 ld hl,$28 | ((mbc_fix_sp_switch_cram - (mbc_fix_sp_switch_cram_smc+2)) << 8)
	 ld (mbc_fix_sp_switch_cram_smc),hl
	 jr mbc_write_cram_common_swapped
	 
mbc_rtc_switch_to_rtc:
	 ld a,l
	 exx
	 ; Switch to RTC accesses
	 ld hl,$18 | ((cram_rtc_read_any - (cram_banked_read_any_rtc_smc+2)) << 8)
	 ld (cram_banked_get_ptr_rtc_smc),hl
	 ld (cram_banked_read_any_rtc_smc),hl
	 ld (cram_banked_write_any_rtc_smc),hl
	 ; Set the read register address as the bank base
	 ld.lil hl,z80codebase+rtc_latched+7
	 and l
	 ld l,a
	 ; See if SP is pointing into the swapped bank
	 ld a,(curr_gb_stack_region)
	 sub cram_bank_base & $FF
	 jr nz,mbc_write_cram_any
	 ; Force fixup of the stack access routines
	 ld a,mbc_fix_sp_switch_cram - (mbc_fix_sp_switch_cram_smc+2)
	 ld (mbc_fix_sp_switch_cram_smc+1),a
	 xor a
	 jr mbc_write_cram_any
	 
	OP_WRITE_HL_MBC
mbc_write_cram_bank_handler:
	push af
mbc_write_cram_common:
	 ld a,l
	 exx
mbc_write_cram_common_swapped:
cram_size_smc = $+1
	 and 0
	 sbc hl,hl
	 rrca
	 rrca
	 rrca
	 ld h,a
cram_base_0 = $+2
	 ld.lil bc,0
	 add.l hl,bc
mbc_write_cram_any_check_stack:
	 ; See if SP is pointing into the swapped bank
	 ld a,(curr_gb_stack_region)
	 sub cram_bank_base & $FF
mbc_write_cram_any:
mbc1_cram_smc_1 = $+2
	 ld.lil (cram_bank_base),hl
mbc1_cram_smc_for_read = $+2
	 ld.lil (cram_bank_base_for_read),hl
mbc1_cram_smc_for_write = $+2
	 ld.lil (cram_bank_base_for_write),hl
mbc_fix_sp_switch_cram_smc = $
	 jr z,mbc_fix_sp
mbc_no_fix_sp:
mbc_finish:
	 exx
	pop af
	ret
	
	OP_WRITE_HL_MBC
mbc1_write_mode_handler:
	push af
	 bit 0,l
	 exx
mbc1_cram_smc_2 = $+2
	 ld.lil hl,(cram_bank_base)
	 ld a,cram_bank_base & $FF
	 jr nz,_
cram_base_1 = $+2
	 ld.lil bc,0
	 ld.lil (cram_bank_base),bc
	 ld.lil (cram_bank_base_for_read),bc
	 ld.lil (cram_bank_base_for_write),bc
	 ld a,mbc1_preserved_cram_bank_base & $FF
_
	 ld (mbc1_cram_smc_1),a
	 ld (mbc1_cram_smc_2),a
	 ld (mbc1_cram_smc_for_read),a
	 ld (mbc1_cram_smc_for_write),a
	 jr mbc_write_cram_any_check_stack
	
mbc_fix_sp_switch_cram:
	 or mbc_fix_sp - (mbc_fix_sp_switch_cram_smc+2) ; Resets Z
	 ld (mbc_fix_sp_switch_cram_smc+1),a
mbc_fix_sp:
	 ; If so, update it
	 ld e,iyl
	 ld iy,(stack_window_base)
	 ld bc,$0080
	 add iy,bc
	 ex.l de,hl
	 add.l iy,de
	 ex de,hl
	 jr nz,set_gb_stack_region_cram_trampoline
	 ; Get the new offset from the window base
	 ld a,iyl
	 xor c
	 ; Set the low byte of the stack index
	 ld iyl,e
	 ; Apply offset SMC if the offset has changed
	 ld hl,do_pop_any_ptr_offset_smc
	 cp (hl)
	 jr z,mbc_finish
	 ld (hl),a
	 jp pop_apply_stack_offset_smc
	
	OP_WRITE_HL_MBC
mbc_write_cram_protect_handler:
	push af
	 ld a,l
mbc_cram_protect_mask_smc = $+1
	 and $0F
	 cp $0A
	 ld hl,$215B ;LD.LIL HL,
	 jr z,_
	 ld hl,$18 | ((cram_open_bus_read_any - (cram_banked_read_any_protect_smc+2)) << 8)
_
	 ld (cram_banked_get_ptr_protect_smc),hl
	 ld (cram_banked_read_any_protect_smc),hl
	 ld (cram_banked_write_any_protect_smc),hl
	 ; See if SP is pointing into the protected bank
	 ld a,(curr_gb_stack_region)
	 cp cram_bank_base & $FF
	 jr z,_
mbc_finish_unswapped:
	pop af
	ret
_
	 ; If an RTC register is not selected, fix stack routines
	 ld a,(cram_banked_get_ptr_rtc_smc)
	 rra ;JR vs. ADD.L
	 jr nc,mbc_finish_unswapped
	 ; Prepare the offset based on the current stack pointer
	 exx
	 ld e,iyl
	 ld a,(do_pop_any_ptr_offset_smc)
	 xor $80
	 ld iyl,a
set_gb_stack_region_cram_trampoline:
	 ld bc,$2525 ; DEC H \ DEC H
	 jp set_gb_stack_region_cram
	
	; Size-optimized HL write op
	push af
	 ld a,d
	 cp h
	 .db $3E ;LD A,
mbc_write_denied_handler:
	ret
	 jr z,mbc_finish_unswapped
	 ; Fallthrough to operator patching call
	OP_WRITE_HL_MBC
mbc_write_rtc_latch_handler:
	push af
	 ; Check if the RTC has changed since the last latch
	 ld.lil a,(mpRtcIntStatus)
	 rra
mbc_rtc_latch_smc = $+1
	 jr nc,$+2 ;mbc_rtc_latch_finish
	 call c,update_rtc
	 ld hl,(rtc_current)
	 ld a,l
	 and $3F
	 ld l,a
	 ld a,h
	 and $3F
	 ld h,a
	 ld (rtc_latched),hl
	 ld hl,(rtc_current+2)
	 ld a,l
	 and $1F
	 ld l,a
	 ld (rtc_latched+2),hl
	 ld a,(rtc_current+4)
	 and $C1
	 ld (rtc_latched+4),a
	 ; Disable any forced latch
	 ld a,mbc_rtc_latch_finish - (mbc_rtc_latch_smc+1)
	 ld (mbc_rtc_latch_smc),a
mbc_rtc_latch_finish:
	pop af
	ret
	
update_rtc:
	; This can be called from multiple write contexts, but HL can be destroyed.
	; This is called at most once per second, so it's fine to take our time.
	push af
	 push bc
	  push de
	   push ix
	    call.il update_rtc_helper
	   pop ix
	  pop de
	 pop bc
	pop af
	ret
	