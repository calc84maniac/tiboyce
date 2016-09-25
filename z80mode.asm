#define CALL_STACK_DEPTH 32
#define READ_CYCLE_LUT_SIZE 8

z80code:
	.assume adl=0
	.org 0
r_bits:
	ex af,af'
	ex (sp),hl
	ld a,(hl)
	inc hl
	ex (sp),hl
	jp do_bits
	
	.block $08-$
r_mem:
	di
	pop ix
	push af
	 jp decode_mem
last_read_cycle:
	.db 0
	
	.block $10-$
r_cycle_check:
	ex af,af'
	ld a,iyh
	add a,a
	ret nc
	jp cycle_overflow_for_jump
	
	.block $18-$
r_push:
	pop ix
	exx
	pop de
	jr do_push
	
	.block $20-$
r_pop:
	exx
	ld.l ix,(hl)
	ex (sp),ix
	jr do_pop
	
	.block $28-$
r_call:
	pop ix
	exx
	djnz do_call
	jr do_call_reset_callstack
	
	.block $30-$
r_interrupt:
	ret
	
	.block $38-$
rst38h:
	push af
	 ld a,pLcdMis >> 8
	 in a,(pLcdMis & $FF)
	 or a
	 jr z,_
	 ld.lil (mpLcdIcr),a
frame_excess_count = $+1
	 ld a,0
	 inc a
	 ld (frame_excess_count),a
_
	 ld a,pIntMaskedStatus >> 8
	 in a,(pIntMaskedStatus & $FF)
	 rra
	 jr c,on_interrupt
	pop af
	ei
	ret
	
on_interrupt:
	ld a,1
	ld.lil (mpIntAcknowledge),a
CmdExitSMC = $+2
	jp.lil 0
	
do_push:
	dec.l hl
do_push_smc_1 = $+1
	ld.l (hl),d
	dec.l hl
do_push_smc_2 = $+1
	ld.l (hl),e
	exx
	jp (ix)
	
do_pop:
	inc.l hl
	inc.l hl
	exx
	jp (ix)
	
do_call_reset_callstack:
	ld b,CALL_STACK_DEPTH
	ld.lil sp,myADLstack
	ld sp,myz80stack-2
do_call:
	pea ix+6
	ld de,(ix+2)
	dec.l hl
do_push_smc_3 = $+1
	ld.l (hl),d
	dec.l hl
do_push_smc_4 = $+1
	ld.l (hl),e
	push.l hl
	ex af,af'
	ld a,(ix+5)
	ld ix,(ix)
do_call_write_smc = $+1
	call dispatch_cycles_exx
call_stack_ret:
	ex af,af'
	exx
	pop.l de
	or a
	sbc.l hl,de
	add.l hl,de
	jr nz,ophandlerRETskip
	pop ix
	inc b
	ld c,b	;C is now at least 2
	ld a,(ix-4)
	cpi.l
	jr nz,ophandlerRETnomatch_dec
	ld de,(ix-3)
	ld a,e
	cpi.l
	jr nz,ophandlerRETnomatch_dec2
	ld a,d
dispatch_cycles_exx:
	exx
dispatch_cycles:
	ld (_+2),a
_
	lea iy,iy+0
	ld a,iyh
	add a,a
	jr c,cycle_overflow
	ex af,af'
	jp (ix)
	
ophandlerRETskip:
	jr c,ophandlerRETsave
	pop de
	inc b
	exx
	ex af,af'
	ret
	
ophandlerRETsave:
	push.l de
	ld de,call_stack_ret
	push de
	jr ophandlerRETnomatch
	
ophandlerRETnomatch_dec2:
	dec.l hl
ophandlerRETnomatch_dec:
	dec.l hl
ophandlerRETnomatch:
	push bc
	 di
	 call.il pop_and_lookup_code_cached
	pop bc
	jr dispatch_cycles_exx
	
cycle_overflow_for_jump:
	pop ix
	ld ix,(ix+2)
cycle_overflow:
	push hl
cycle_overflow_loop:
	 ld hl,LY
	 ld a,(hl)
	 inc a
	 cp 154
	 jr c,_
	 xor a
_
	 ld (hl),a
	 inc hl
	 cp (hl)
	 call z,LYCmatch
	 cp 144
	 jr z,vblank_handler
vblank_handler_ret:
	 lea iy,iy+112
	 ld a,iyh
	 inc a
	 jr z,cycle_overflow_loop
	 ld l,h
	 ld a,(hl)
	 ld l,IF & $FF
	 and (hl)
intstate = $+1
	 and $00
	 jr nz,trigger_interrupt
	pop hl
	ex af,af'
	jp (ix)
	
vblank_handler:
	di
	jp.lil vblank_helper
	
trigger_interrupt:
	 rrca
	 jr nc,_
	 res 0,(hl)
	 ld hl,$0040
	 jr dispatch_int
_
	 rrca
	 jr nc,_
	 res 1,(hl)
	 ld hl,$0048
	 jr dispatch_int
_
	 rrca
	 jr nc,_
	 res 2,(hl)
	 ld hl,$0050
	 jr dispatch_int
_
	 res 3,(hl)
	 ld hl,$0058
dispatch_int:
	 push bc
	  push de
	   push hl
	    lea de,ix
	    di
	    call.il lookup_gb_code_address
	    ; If we're on a HALT, exit it
	    ld.l a,(ix)
	    cp $76
	    jr nz,_
	    inc.l ix
	    inc de
	    inc hl
	    inc hl
	    inc hl
_
	    ld.lil (int_cached_return),ix
	    ld.lil (int_cached_code),hl
	   pop hl
	   ex de,hl
	   call.il lookup_code_cached
	  pop de
	 pop bc
	 ex (sp),hl
	 xor a
	 ld (intstate),a
	 ex af,af'
	 exx
	pop de
	jp do_push
	
LYCmatch:
	ld l,LCDC - ioregs
	bit 7,(hl)
	ret z
	inc hl
	bit 6,(hl)
	ret z
	ld l,IF - ioregs
	set 1,(hl)
	ret
	
decode_mem:
	 ld a,(memroutine_next)
	 sub ixl
	 ld a,(memroutine_next+1)
	 sbc a,ixh
	 jr nc,_
	 ld a,(ix)
	pop ix
	pop ix
	lea ix,ix-2
	ld (ix),a
	ex af,af'
	push af
_
	 push hl
	  push de
	   call.il decode_mem_helper
	   ld (ix+1),de
	   ld (ix),$CD
	  pop de
	 pop hl
	pop af
	jp (ix)
	
decode_jump:
	ex af,af'
	ex (sp),hl
	push bc
	 push de
	  inc hl
	  ld ix,(hl)
	  inc hl
	  inc hl
	  ld de,(hl)
	  push hl
	   di
	   call.il decode_jump_helper
	  pop hl
	  ld (hl),ix
	  dec hl
	  ld (hl),$C3	;JP
	  dec hl
	  ld (hl),$08	;EX AF,AF'
	  dec hl
	  sub (hl)	;calc cycle count
	  ld (hl),RST_CYCLE_CHECK
	  dec hl
	  ld (hl),a
	  dec hl
	  ld (hl),$33
	  dec hl
	  ld (hl),$ED	;LEA IY,IY+offset
	 pop de
	pop bc
	ex (sp),hl
	ex af,af'
	ret
	
decode_call:
	ex af,af'
	ex (sp),hl
	push bc
	 push de
	  push hl
	   ld de,(hl)
	   di
	   call.il decode_call_helper
_
	  pop hl
	  inc hl
	  inc hl
	  sub (hl)
	  inc hl
	  ld (hl),a
	  ld de,-5
	  add hl,de
	  ld (hl),ix
	  dec hl
	  ld (hl),RST_CALL
	 pop de
	pop bc
	ex (sp),hl
	ex af,af'
	ret
	
decode_rst:
	ex af,af'
	ex (sp),hl
	push bc
	 push de
	  push hl
	   ld de,(hl)
	   di
	   call.il decode_rst_helper
	   jr -_
	
wait_for_interrupt_stub:
	ei
	halt
	ret.l
	
flush_handler:
	exx
flush_address = $+1
	ld de,0
	di
	jp.lil flush_normal
	
flush_mem_handler:
	exx
	pop de
	di
	jp.lil flush_mem
	
coherency_handler:
	ex af,af'
	ex (sp),hl
	ld ix,(hl)
	inc hl
	inc hl
	ex (sp),hl
	push hl
	 push de
	  push bc
	   ld.lil de,recompile_struct
	   add.l ix,de
	   ld.l de,(ix+2)
	   ld.l bc,(ix+5)
	   ld.l hl,(ix+8)
	   sbc hl,bc
check_coherency_loop:
	   ld.l a,(de)
	   inc.l de
	   cpi
	   jr nz,check_coherency_failed
	   jp pe,check_coherency_loop
coherency_return:
	  pop bc
	 pop de
	pop hl
coherency_return_popped:
	ex af,af'
	ret
	
check_coherency_failed:
	   di
	   jp.lil rerecompile
	
do_swap:
	inc a
	jr nz,do_swap_generic
	ex af,af'
	rrca
	rrca
	rrca
	rrca
	ret
do_swap_generic:
	inc a
	jr z,do_swap_hl
	add a,$7E	;LD A,r
	ld (_),a
	add a,a
	add a,a
	add a,a
	sub $79		;LD r,A
	ld (++_),a
_
	ld a,b
	rrca
	rrca
	rrca
	rrca
_
	ld b,a
	ex af,af'
	ret
do_swap_hl:
	lea iy,iy-2	;Consume 2 extra cycles
	call mem_read_any
	rrca
	rrca
	rrca
	rrca
	ld ixl,a
	ex af,af'
	push af
	 call mem_write_any_ixl
	pop af
	ret
	
do_bits:
	sub $30
	sub 8
	jr c,do_swap
	add a,$38-1	;Use L instead of (HL)
	cp $C0
	jp pe,do_bits_readonly
	lea iy,iy-2	;Consume 2 extra cycles
	ld (do_bits_smc),a
	call mem_read_any
	; Use L because we have to affect flags, bleh
	push hl
	 ld l,a
	 ex af,af'
	 ld h,a
do_bits_smc = $+1
	 rlc l
	 ld a,l
	 ex (sp),hl
	 call mem_write_any
	pop ix
	ld a,ixh
	ret
do_bits_readonly:
	dec iy	;Consume 1 extra cycle
	ld (do_bits_readonly_smc),a
	call mem_read_any
	; Use L because we have to affect flags, bleh
	push hl
	 ld l,a
	 ex af,af'
do_bits_readonly_smc = $+1
	 bit 0,l
	pop hl
	ret
	
ophandler08:
	pop ix
	pea ix+2
	push af
	 push de
	  exx
	  push hl
	   exx
	   ex (sp),hl
	   ld de,(sp_base_address)
	   or a
	   sbc hl,de
	   ex de,hl
	   ld hl,(ix)
	   ld a,e
	   call mem_write_any
	   inc hl
	   ld a,d
	   call mem_write_any
	  pop hl
	 pop de
	pop af
	ret
	
ophandler31:
	pop ix
	exx
	ld hl,(ix)
	lea ix,ix+2
	di
	jp.lil set_gb_stack
	
ophandler34:
	ex af,af'
	call mem_read_any
	ld ixl,a
	ex af,af'
	inc ixl
	jr _
	
ophandler35:
	ex af,af'
	call mem_read_any
	ld ixl,a
	ex af,af'
	dec ixl
_
	push af
	 call mem_write_any_ixl
	pop af
	ret
	
ophandler36:
	pop ix
	pea ix+1
	push af
	 ld a,(ix)
	 call mem_write_any
	pop af
	ret
	
ophandler39:
	ex af,af'
	push de
	 exx
	 push hl
	  exx
	  ex (sp),hl
	  ld de,(sp_base_address)
	  or a
	  sbc hl,de
	  ex de,hl
	 pop hl
	 add hl,de
	pop de
	ex af,af'
	ret
	
handle_waitloop_stat:
	jr handle_waitloop_stat
	
handle_waitloop_variable:
handle_waitloop_ly:
	ex af,af'
	pop ix
	ld ix,(ix)
_
	ld iy,-1
	jp cycle_overflow
	
ophandler76:
	ex af,af'
	push hl
	 ld hl,IF
	 ld a,(hl)
	 ld l,h
	 and (hl)
	pop hl
	jr nz,haltdone
	pop ix
	lea ix,ix-3
	jr -_
haltdone:
	ex af,af'
	ret
	
ophandlerE2:
	ld ixh,$FF
	ld ixl,c
	jp mem_write_ports_always
	
ophandlerE8:
	ex af,af'
	exx
	ex.l de,hl
	pop hl
	ld a,(hl)
	inc hl
	push hl
	rlca
	sbc.l hl,hl
	rrca
	ld l,a
	add.l hl,de
	exx
	ex af,af'
	ret
	
ophandlerE9:
	ex af,af'
	ex de,hl
	push bc
	 push de
	  di
	  call.il lookup_code_cached
	 pop de
	pop bc
	ex de,hl
	jp dispatch_cycles
	
ophandlerF2:
	ld ixh,$FF
	ld ixl,c
	ex af,af'
	call mem_read_ports_always
	ld a,ixl
	ret
	
ophandlerF3:
	ex af,af'
	xor a
	ld (intstate),a
	ex af,af'
	ret
	
ophandlerF8:
	ex af,af'
	pop hl
	ld a,(hl)
	inc hl
	push hl
	exx
	push hl
	 exx
	pop hl
	push de
	 ld de,(sp_base_address)
	 or a
	 sbc hl,de
	 ld e,a
	 rla
	 sbc a,a
	 ld d,a
	 add hl,de
	pop de
	ex af,af'
	ret
	
ophandlerF9:
	pop ix
	push hl
	 exx
	pop hl
	di
	jp.lil set_gb_stack
	
ophandlerEI:
	ex af,af'
	ld a,$1F
	ld (intstate),a
	pop ix
	jp checkIntPostEnable
	
ophandlerRETI:
	ex af,af'
	ld a,$1F
	ld (intstate),a
	di
	jr ophandlerRETfinish
	
ophandlerRET:
	di
	dec sp
	dec sp
	ex af,af'
ophandlerRETfinish:
	exx
	push bc
	 call.il pop_and_lookup_code_cached
	pop bc
	jp dispatch_cycles_exx
	
ophandlerINVALID:
	ei
	jr ophandlerINVALID
	
write_vram_handler:
	pop ix
	pea ix+2
	ld ix,(ix)
	jp mem_write_vram_always
	
write_cart_handler:
	pop ix
	pea ix+2
	ld ix,(ix)
	jp mem_write_cart_always
	
write_cram_bank_handler:
	pop ix
	pea ix+2
	exx
	ld de,(ix)
	ld.lil ix,(cram_bank_base)
	ex af,af'
	add.l ix,de
	ex af,af'
	ld.l (ix),a
	exx
	ret
	
read_rom_bank_handler:
	pop ix
	pea ix+2
	exx
	ld de,(ix)
	ld.lil ix,(rom_bank_base)
	ex af,af'
	add.l ix,de
	ex af,af'
	ld.l a,(ix)
	exx
	ret
	
read_cram_bank_handler:
	pop ix
	pea ix+2
	exx
	ld de,(ix)
	ld.lil ix,(cram_bank_base)
	ex af,af'
	add.l ix,de
	ex af,af'
	ld.l a,(ix)
	exx
	ret
	
readP1handler:
	ex af,af'
	call readP1
	ld a,ixl
	ret
	
readDIVhandler:
	ex af,af'
	call readDIV
	ld a,ixl
	ret
	
readTIMAhandler:
	ex af,af'
	call readTIMA
	ld a,ixl
	ret
	
readLYhandler:
	ex af,af'
	call readLY
	ld a,ixl
	ret
	
readSTAThandler:
	ex af,af'
	call readSTAT
	ld a,ixl
	ret
	
writeTIMAhandler:
	ex af,af'
	xor a
	jp write_timer
	
writeTMAhandler:
	ex af,af'
	ld a,1
	jp write_timer
	
writeTAChandler:
	ex af,af'
	jp writeTAC
	
writeLCDChandler:
	ex af,af'
	jp writeLCDC
	
writeSCYhandler:
	ld ix,SCY
	jp write_scroll_swap
	
writeSCXhandler:
	ld ix,SCX
	jp write_scroll_swap
	
writeLYChandler:
	jp writeLYCswap
	
writeWYhandler:
	ld ix,WY
	jp write_scroll_swap
	
writeWXhandler:
	ld ix,WX
	jp write_scroll_swap
	
writeDMAhandler:
	ex af,af'
	jp writeDMA
	
writeIFhandler:
	ld (IF),a
	jp writeINTswap
	
writeIEhandler:
	ld (IE),a
	jp writeINTswap
	
readP1:
	ld a,(P1)
	or $0F
	ld ix,(keys)
	bit 4,a
	jr nz,_
	and ixl 
_
	bit 5,a
	jr nz,_
	and ixh
_
	ld ixl,a
	ex af,af'
	ret
	
readDIV:
	ld a,r
	sub iyl
	ld ixl,a
	ex af,af'
	ret
	
readSTAT:
	exx
	call get_read_cycle_offset
	ld a,(STAT)
	and $F8
	ld c,a
	ld a,(LCDC)
	add a,a
	jr nc,++_
	ld ix,(LY)
	ld a,ixl
	cp ixh
	jr nz,_
	set 2,c
_
	cp 144
	jr nc,_
	ld a,e
	cp 51
	jr c,++_
	set 1,c
	cp 51 + 43
	jr nc,++_
_
	inc c
_
	ld ixl,c
	exx
	ex af,af'
	ret
	
readLY:
	ld a,(LCDC)
	add a,a
	sbc a,a
	jr z,_
	ld a,(LY)
_
	ld ixl,a
	ex af,af'
	ret
	
	;IX=GB address, reads into IXL
mem_read_ports:
	ex af,af'
	ld a,ixh
	cp $FE
	jr c,mem_read_bail
	jr z,mem_read_oam
	;IX=GB address, reads into IXL, AF'=GB AF
mem_read_ports_always:
	ld a,ixl
	add a,a
	jr c,mem_read_oam
	jr z,readP1
	cp TIMA*2 & $FF
	jr z,readTIMA
	cp LY*2 & $FF
	jr z,readLY
	cp DIV*2 & $FF
	jr z,readDIV
	cp STAT*2 & $FF
	jr z,readSTAT
mem_read_oam:
	ld ix,(ix)
	ex af,af'
	ret
	
readTIMA:
	ld a,(TAC)
	bit 2,a
	di
	call.il nz,updateTIMA
	ei
	ld ix,(TIMA)
	ex af,af'
	ret
	
	;HL=GB address, reads into A, AF'=GB AF
mem_read_any:
	ld a,h
	cp $FE
	jr nc,mem_read_any_ports
	ex de,hl
	add a,a
	jr c,++_
	add a,a
	jr c,_
	ld.lil ix,(rom_start)
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
_
	ld.lil ix,(rom_bank_base)
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
_
	add a,$40
	jp pe,_
	ld.lil ix,vram_base
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
_
	ld.lil ix,(cram_bank_base)
	add.l ix,de
	ld.l a,(ix)
	ex de,hl
	ret
	
mem_read_bail:
	pop ix
mem_write_bail_a:
	lea ix,ix-8
	jp (ix)
	
mem_read_any_ports:
	jr z,_
	ld a,l
	add a,a
	jr c,_
	push hl
	pop ix
	call mem_read_ports_always
	ex af,af'
	ld a,ixl
	ret
_
	ld a,(hl)
	ret
	
	;HL=GB address, IXL=data, destroys AF,AF'
mem_write_any_ixl:
	ld a,ixl
	;HL=GB address, A=data, preserves AF, destroys AF'
mem_write_any:
	ex af,af'
	ld a,h
	cp $FE
	jr nc,mem_write_any_ports
	add a,a
	jr nc,mem_write_any_cart
	sub $40
	jr c,mem_write_any_vram
	ld.lil ix,wram_base
	sub $40
	jr nc,_
	ld.lil ix,(cram_bank_base)
_
	ex de,hl
	add.l ix,de
	ex de,hl
	ex af,af'
	ld.l (ix),a
	ret
	
mem_write_bail:
	pop ix
	ld a,(ix-8)
	cp RST_MEM
	jr z,mem_write_bail_a
	lea ix,ix-10
	pop af
	ex af,af'
	jp (ix)
	
writeLCDC:
	di
	jp.lil lcdc_write_helper
	
writeTAC:
	di
	jp.lil tac_write_helper
	
write_timer:
	di
	jp.lil timer_write_helper
	
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_vram:
	ex af,af'
	ld a,ixh
	sub $20
	jp po,mem_write_bail
mem_write_vram_swap:
	ex af,af'
	;IX=GB address, A=data
mem_write_vram_always:
	di
	jp.lil write_vram_and_expand
	
mem_write_any_ports:
	push hl
	pop ix
	jr nz,mem_write_ports_swap
	jr mem_write_oam_swap
mem_write_any_vram:
	push hl
	pop ix
	jr mem_write_vram_swap
mem_write_any_cart:
	push hl
	pop ix
	jr mem_write_cart_swap
	
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_ports:
	ex af,af'
	ld a,ixh
	inc a
	jr z,mem_write_ports_swap
	inc a
	jr nz,mem_write_bail
mem_write_oam_swap:
	ex af,af'
	ld (ix),a
	ret
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_ports_always:
	ex af,af'
mem_write_ports_swap:
	ld a,ixl
	inc a
	jp m,mem_write_oam_swap
	jr z,writeINT
	sub (TAC & $FF) + 1
	jr z,writeTAC
	add a,TAC - TIMA
	jr c,write_timer
	sub IF - TIMA
	jr z,writeINT
	sub LCDC - IF
	jr z,writeLCDC
	sub SCY - LCDC
	cp 2
	jr c,write_scroll
	sub WY - SCY
	cp 2
	jr c,write_scroll
	sub LYC - WY
	jr z,writeLYC
	dec a
	jr nz,mem_write_oam_swap
writeDMA:
	di
	jp.lil oam_transfer_helper
	
write_scroll_swap:
	ex af,af'
write_scroll:
	di
	jp.lil scroll_write_helper
	
writeLYC:
	ex af,af'
writeLYCswap:
	di
	ld (LYC),a
	jp.lil lyc_write_helper
	
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_cart:
	ex af,af'
	ld a,ixh
	rla
	jp c,mem_write_bail
mem_write_cart_swap:
	ex af,af'
	;IX=GB address, A=data, preserves AF, destroys AF'
mem_write_cart_always:
	ex af,af'
	ld a,ixh
	sub $20
	jr c,mbc_0000
	sub $20
	jr c,mbc_2000
	sub $20
	jr c,mbc_4000
	
mbc_6000:
	ld a,(mbc_z80)
	dec a
	jr nz,mbc_6000_denied
	ex af,af'
	push af
	 ex af,af'
	pop af
	rra
	ld a,$28
	jr nc,_
	ld a,$20
_
	ld (mbc1_ram_smc),a
mbc_6000_denied:
mbc_0000:
	ex af,af'
	ret
	
writeINT:
	ex af,af'
	ld (ix),a
writeINTswap:
	ex af,af'
	pop ix
	ld a,(intstate)
	or a
	jr z,_
checkIntPostEnable:
	push hl
	 ld hl,IF
	 ld a,(hl)
	 ld l,h
	 and (hl)
	pop hl
	jr z,_
	; TODO
_
	ex af,af'
	jp (ix)
	
mbc_4000:
	push bc
	 ld b,$60
	 ex af,af'
	 ld c,a
	 ex af,af'
	 ld a,(mbc_z80)
	 dec a
	 jr nz,_
mbc1_ram_smc:
	 jr z,mbc1_ram
	 ld a,c
	 rrca
	 rrca
	 rrca
	 ld c,a
	 jr mbc_2000_finish
_
	 dec a
	 dec a
	 jr nz,_
mbc1_ram:
	 ld a,c
	 rrca
	 rrca
	 rrca
	 and b
	 ld b,a
	 ld c,0
cram_base_0 = $+3
	 ld.lil ix,0
	 add.l ix,bc
	 ld.lil (cram_bank_base),ix
	 ; See if SP is pointing into the swapped bank
	 exx
	 ld de,(sp_base_address)
	 ld a,l
	 sub e
	 ld e,a
	 ld a,h
	 sbc a,d
	 ld d,a
	 sub $A0
	 cp $20
	 jr c,mbc_fix_sp
	 exx
_
	pop bc
	ex af,af'
	ret
	
mbc_2000:
	push bc
	 ex af,af'
	 ld c,a
	 ex af,af'
mbc_z80 = $+1
	 ld b,1
	 djnz _
	 ld b,$1F
	 jr mbc_2000_finish
_
	 djnz _
	 ld b,$0F
	 jr mbc_2000_finish
_
	 djnz mbc_2000_denied
	 ld b,$7F
mbc_2000_finish:
	 ld a,c
curr_rom_bank = $+1
	 ld c,1
	 xor c
	 and b
	 xor c
	 ld c,a
	 ld (curr_rom_bank),a
	 ld b,3
	 mlt bc
	 ld.lil ix,rombankLUT
	 add.l ix,bc
	 ld.l ix,(ix)
	 ld.lil (rom_bank_base),ix
	 ; See if SP is pointing into the swapped bank
	 exx
	 ld de,(sp_base_address)
	 ld a,l
	 sub e
	 ld e,a
	 ld a,h
	 sbc a,d
	 cp $C0
	 jp po,mbc_no_fix_sp
	 ; If so, update it
	 ld d,a
mbc_fix_sp:
	 lea.l hl,ix
	 ld.lil (z80codebase+sp_base_address),hl
	 add.l hl,de
mbc_no_fix_sp:
	 exx
mbc_2000_denied:
	pop bc
	ex af,af'
	ret
	
; Inputs: IY = current block cycle base
;         (SP+4) or (SP+6) = JIT return address
;         AFBCDEHL' have been swapped
; Outputs: E = adjusted current cycle count
; Destroys DE,IX
get_read_cycle_offset:
	push.l hl
	 push bc
	  ld hl,6
	  add hl,sp
	  ld bc,(hl)
	  ld a,b
	  cp z80codesize >> 8
	  jr nc,_
	  inc hl
	  inc hl
	  ld bc,(hl)
	  or a
_
	  ld hl,read_cycle_LUT+(READ_CYCLE_LUT_SIZE*3)-2
	  ld a,-(READ_CYCLE_LUT_SIZE*3)
read_cycle_lookup_loop:
	  ld de,(hl)
	  dec hl
	  ex de,hl
	  sbc hl,bc
	  jr z,read_cycle_lookup_found
	  ex de,hl
	  dec hl
	  dec hl
	  add a,3
	  jr nc,read_cycle_lookup_loop
	  ld d,b
	  ld e,c
	  di
	  call.il lookup_gb_code_address
	  ei
	  push hl
	   ld hl,last_read_cycle
	   ld de,read_cycle_LUT
	   ld c,(READ_CYCLE_LUT_SIZE-1)*3
	   jr read_cycle_lookup_shift
read_cycle_lookup_found:
	  ld l,last_read_cycle
	  add a,READ_CYCLE_LUT_SIZE*3
	  jr z,read_cycle_lookup_found_fast
	  cp (hl)
	  jr nz,read_cycle_lookup_found_fast
	  push bc
	   ld c,a
	   ld a,(de)
read_cycle_lookup_shift:
	   ld (hl),h
	   ld b,h
	   ld l,3
	   add hl,de
	   ldir
	   ld (de),a
	  pop bc
	  dec hl
	  ld (hl),b
	  dec hl
	  ld a,c
read_cycle_lookup_found_fast:
	  ld (hl),a
	  ex de,hl
	  ld a,iyl
	  sub (hl)
	  ld e,a
	 pop bc
	pop.l hl
	ret nc
	add a,112
	ld e,a
	ret
	
keys:
	.dw $FFFF
	
sp_base_address:
	.dl 0
memroutine_next:
	.dl 0
render_save_sps:
	.dw 0
	
	.assume adl=1
z80codesize = $-0
	.org z80code+z80codesize
	
	.echo "Z80 mode code size: ", z80codesize