z80code:
	.assume adl=0
	.org 0
	CPU_SPEED_START()

active_ints:
	.db 0
hdma_line_counter:
	.db 0
waitloop_sentinel:
	.db 0

	.block $08-$
	; Input: DE=Game Boy HL
	; Output: UHL=direct read pointer, or implementation is patched
	; Destroys: F'
r_get_hl_read_ptr:
	ex af,af'
get_hl_read_ptr_swapped:
	ld h,mem_read_lut >> 8
	ld l,d
	ld l,(hl)
	; Implicitly reset Z flag to indicate a patchable caller
	inc h ;mem_get_ptr_routines
	jp (hl)

	.block $10-$
	; Input: DE=Game Boy HL
	; Output: UHL=direct read/write pointer, or implementation is patched
	; Destroys: F'
r_get_hl_readwrite_ptr:
	ex af,af'
get_hl_readwrite_ptr_swapped:
	ld h,mem_write_lut >> 8
	ld l,d
	ld l,(hl)
	; Implicitly reset Z flag to indicate a patchable caller
	dec h ;mem_get_ptr_routines
	jp (hl)

	.block $18-$
r_event:
	pop hl
	dec hl
event_value = $+1
	ld (hl),0
	jp do_event

	.block $20-$
r_get_hl_hram_ptr:
	ex af,af'
	inc de
	ld hl,$007F
	add hl,de
	dec de
	jr nc,_
	ld h,d
	ld l,e
	ex af,af'
	ret
_
	jp unpatch_hl_hram_access

	;.block $28-$
	; Taken by previous routine

	.block $30-$
r_call:
	ex af,af'
	dec iyl
	jp m,do_call
	jr do_push_overflow_for_call

	.block $38-$
r_cycle_check:
rst38h:
	exx
	inc d
	exx
	ret nz
	ex (sp),ix
	jr c,cycle_overflow_for_subblock
	ld hl,(ix+2)
	push hl
	 exx
	 inc bc ;BCU=0
	 ld c,a
	 ld a,-3
	 sub (ix-3)
	 ld b,a
	 ld de,(ix+4)
	pop ix
#ifdef VALIDATE_SCHEDULE
	call.il schedule_jump_event_helper_adjusted
#else
	jp.lil schedule_jump_event_helper_adjusted
#endif

#ifndef NO_PORTS
push_z80_de_safe:
	pop hl
	push de
	push hl
	ret.l

push_z80_hl_safe:
	ex (sp),hl
	push hl
	ret.l

z80_retn:
	retn

	.block $66-$
z80_nmi:
	jp.lil jit_stack_overflow_helper
#endif

cycle_overflow_for_subblock:
	inc ix
	lea hl,ix
	exx
	inc bc ;BCU=0
	ld c,a
	ld b,(ix-4)
	ld de,(ix-8)
#ifdef VALIDATE_SCHEDULE
	call.il schedule_subblock_event_helper
#else
	jp.lil schedule_subblock_event_helper
#endif

	#include "branch.asm"
	#include "ophandler.asm"
	#include "scheduler.asm"
	#include "events.asm"
	#include "mbc.asm"
	#include "stack.asm"
	#include "memory.asm"

z80_pop_restore_swap_ret:
	 pop bc
write_vram_and_expand_finish:
	pop de
z80_restore_swap_ret:
	ld a,e
	exx
z80_swap_af_ret:
	ex af,af'
z80_ret:
	ret

wait_for_interrupt_stub:
	ei
	halt
	ret.l

	; Cached RST and interrupt handlers are combined in this space
	; Handlers consist of a jump followed by a cycle count
	; Interrupt handlers are indexed by 1, 2, 4, 8, 16
	; Address info is stored in halves of empty handler slots
	; Handler to address info mapping: add 10 slots and divide by 2
	; Unused slot halves: 7.5, 9.5, 10.0, 12.0, 12.5
	.echo "Wasted space before dispatchers: ", (-$)&$FF
	.block (-$)&$FF
dispatch_rst_00: ;0 -> 5.0
	.db 0 \ jp 0
dispatch_vblank: ;1 -> 5.5
	.db 0 \ jp 0
dispatch_stat:   ;2 -> 6.0
	.db 0 \ jp 0
dispatch_rst_08: ;3 -> 6.5
	.db 0 \ jp 0
dispatch_timer:  ;4 -> 7.0
	.db 0 \ jp 0
	; Address info for RST 00h, VBLANK, STAT, RST 08h, TIMER
	.dw $0000, $0040, $0048, $0008, $0050, 0
dispatch_serial: ;8 -> 9.0
	.db 0 \ jp 0
	; Address info for SERIAL, RST 10h
	.dw $0058, 0, 0, $0010
dispatch_rst_10: ;11 -> 10.5
	.db 0 \ jp 0
	; Address info for JOYPAD, RST 18h - 38h
	.dw 0, 0, $0060, $0018, $0020, $0028, $0030, $0038
dispatch_joypad: ;16 -> 13.0
	.db 0 \ jp 0
dispatch_rst_18: ;17 -> 13.5
	.db 0 \ jp 0
dispatch_rst_20: ;18 -> 14.0
	.db 0 \ jp 0
dispatch_rst_28: ;19 -> 14.5
	.db 0 \ jp 0
dispatch_rst_30: ;20 -> 15.0
	.db 0 \ jp 0
dispatch_rst_38: ;21 -> 15.5
	.db 0 \ jp 0

flush_handler:
	exx
	ld b,d
flush_address = $+1
	ld de,0
	ex af,af'
	jp.lil flush_normal

coherency_handler_generic:
	ex (sp),ix
	 lea hl,ix+RAM_PREFIX_SIZE-3
	 exx
	 ex af,af'
	 ld e,a
	 push de
	  ld de,(ix)
	  ld bc,(ix+2)
	  jp.lil check_coherency_helper_generic

coherency_handler_wram:
	ex (sp),ix
	 lea hl,ix+RAM_PREFIX_SIZE-3
	 exx
	 ex af,af'
	 ld e,a
	 push de
	  ld de,(ix)
	  ld bc,(ix+2)
coherency_handler_wram_smc = $+3
	  ld.lil ix,0 ;wram_base
	  jp.lil check_coherency_helper

coherency_handler_hram:
	ex (sp),ix
	 lea hl,ix+RAM_PREFIX_SIZE-3
	 exx
	 ex af,af'
	 ld e,a
	 push de
	  ld de,(ix)
	  ld bc,(ix+2)
	  jp.lil check_coherency_helper_hram

do_dynamic_jp_banked:
	ex af,af'
	add.l hl,bc
do_dynamic_jp:
	ex (sp),ix
	 ld e,a
	 push de
	  call.il lookup_code_cached_for_dynamic_jp
	  ex de,hl
	  add a,4 ; Add cycles for taken JP
	 pop de
	 add a,e
	 jr c,++_
_
	 exx
	 ex af,af'
	 ex (sp),ix
	 ret
_
	 inc d
	 jr nz,--_
	 inc bc ;BCU=0
	 ld c,a
	 sub e
	 sub 4
	 ld b,a
	 ex de,hl
	 exx
	 lea hl,ix
	 exx
#ifdef VALIDATE_SCHEDULE
	 call.il schedule_event_helper
#else
	 jp.lil schedule_event_helper
#endif

Z80InvalidOpcode:
	jp.lil Z80InvalidOpcode_helper

Z80Error:
	jp.lil runtime_error

keys:
	.dw $FFFF

trampoline_next:
	.dl 0
persistent_vblank_counter:
	.dw 0
render_save_sps:
	.dw 0

	; One word of stack space for sprite rendering during vblank
lcd_on_ppu_event_checker:
	.dw 0
event_counter_checkers:
event_counter_checker_slot_PPU:
	.dw ppu_expired_vblank
event_counter_checker_slot_timer:
	.dw disabled_counter_checker
event_counter_checker_slot_serial:
	.dw disabled_counter_checker
event_counter_checker_slot_HDMA:
event_counter_checkers_ei_delay:
	.dw event_counter_checkers_done
event_counter_checkers_ei_delay_2:
	.dw event_counter_checkers_done

	CPU_SPEED_END()

#ifdef FASTLOG
fastlog_z80:
	jp.lil fastlog_z80_helper
#endif

	.assume adl=1
z80codesize = $-0
	.org z80code+z80codesize

	.echo "Z80 mode code size: ", z80codesize

jit_start = z80codesize
