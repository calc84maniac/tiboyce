#define CALL_STACK_DEPTH 32

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
	 jp do_mem
	
	.block $10-$
r_branch:
	di
	exx
	pop hl
	ld de,(hl)
	jp do_branch
	
	.block $18-$
r_call:
	exx
	pop hl
	ld ix,(hl)
	djnz do_call
	jr do_call_reset_callstack
	
	.block $20-$
r_push:
	pop ix
	exx
	pop de
	jr do_push_no_write
r_push_jr_end:
	
	.block $28-$
r_pop:
	ld.l ix,(iy)
	ex (sp),ix
	jr do_pop
	
	.block $30-$
r_interrupt:
	di
	ex af,af'
	exx
	pop hl
	dec hl
	jp do_interrupt
	
	.block $38-$
rst38h:
	push af
	 ld.lil a,(mpIntMaskedStatus)
	 rra
	 jr c,on_interrupt
	 rra
	 jp c,scanline_interrupt
	 rra
	 jp c,timer_interrupt
	 bit 1,a
	 jr nz,soft_interrupt
	pop af
	ei
	ret
	
on_interrupt:
	ld a,1
	ld.lil (mpIntAcknowledge),a
	jp.lil exit
	
do_call_reset_callstack:
	ld b,CALL_STACK_DEPTH
	ld sp,myz80stack-2
do_call:
	inc hl
	inc hl
	ld de,(hl)
	inc hl
	inc hl
	push hl
	push de
do_call_write_smc = $+1
	call do_push_no_write
	; Call stack return here!
	ex af,af'
	exx
	inc b
	pop hl
	ld.l de,(iy)
	or a
	sbc hl,de
	jr nz,ophandlerRETnomatch
	exx
	ex af,af'
	lea.l iy,iy+2
	ei
	ret
	
do_push:
	ld.l (iy-2),e
	ld.l (iy-1),d
do_push_no_write:
	lea.l iy,iy-2
do_call_finish:
	exx
	ei
	jp (ix)
	
do_pop:
	lea.l iy,iy+2
	ei
	jp (ix)
	
ophandlerRETnomatch:
	exx
	ex af,af'
	ld sp,myz80stack
ophandlerRET:
	di
	ex af,af'
	xor a
	ld.lil (mpTimerCtrl),a
	dec sp
	dec sp
	exx
	call.il pop_and_lookup_code_cached
	ld bc,(CALL_STACK_DEPTH+1)*256 + TMR_ENABLE
	ld a,c
	ld.lil (mpTimerCtrl),a
	exx
	ex af,af'
	ei
	jp (ix)
	
soft_interrupt:
	 push.l hl
	  xor a
	  ld.lil (mpTimerCtrl),a
	  ld a,(TAC)
	  and $04
	  or $03
	  ld.lil (mpIntEnable),a
	  jr test_interrupts
	  
scanline_interrupt:
	 push.l hl
	  ld.lil hl,mpTimerCtrl
	  ld.l (hl),0
	  ld l,mpTimerIntStatus & $FF
	  ld.l a,(hl)
	  bit 1,a
	  jr z,_
	  call.il vblank_stuff
_
	  rra
	  ld a,3
	  ld.l (hl),a
	  dec a
	  ld.lil (mpIntAcknowledge),a
	  jr nc,test_interrupts
	  
	  ld hl,LCDC
	  bit 7,(hl)
	  jr z,test_interrupts
	  inc hl
	  bit 6,(hl)
	  jr z,test_interrupts
	  ld l,IF & $FF
	  set 1,(hl)
test_interrupts:
	  ld a,(intstate)
	  or a
	  jr z,noints
	  ld hl,IF
	  ld a,(hl)
	  ld l,h
	  and (hl)
	  jr nz,interrupt_get
noints:
	 pop.l hl
	 ld a,TMR_ENABLE
	 ld.lil (mpTimerCtrl),a
interrupt_fast_exit:
	pop af
	ei
	ret
	
timer_interrupt:
	 xor a
	 ld.lil (mpTimerCtrl),a
	 ld a,4
	 ld.lil (mpIntAcknowledge),a
	 push.l hl
	  ld hl,IF
	  set 2,(hl)
	  jr test_interrupts
	
interrupt_get:
	  ld (intmask),a
	 pop af
	pop hl
	push hl
	 push af
	  ;call.il yourkidding
	  ld a,l
	  sub z80codesize & $FF
	  ld a,h
	  sbc a,z80codesize >> 8
	  jr c,_
	  ld a,(memroutine_next)
	  sub l
	  ld a,(memroutine_next+1)
	  sbc a,h
	  jr nc,+++_
_
	  ; Handle interrupt triggering after EI
	  dec hl
	  ld a,(hl)
	  cp $FB	;EI
	  jr z,_
	  inc hl
_
	  ; Do a software interrupt!
	  ld a,$11
	  ld.lil (mpIntEnable),a
	 pop af
	pop.l hl
	ret
_
	  push de
	   push bc
	    push.l ix
	     ex de,hl
	     call.il lookup_gb_code_address
	     ld.lil (int_cached_return),ix
	     ld.lil (int_cached_code),hl
	     ld (intretaddr),de
	     xor a
	     ld (intstate),a
	     ld a,(hl)
	     ld (intsavecode),a
	     ld (hl),RST_INTERRUPT
	    pop.l ix
	   pop bc
	  pop de
	  ld a,TMR_ENABLE
	  ld.lil (mpTimerCtrl),a
	 pop af
	pop.l hl
	ei
	ret
	
do_interrupt:
	xor a
	ld.lil (mpTimerCtrl),a
intsavecode = $+1
	ld (hl),0
intmask = $+1
	ld a,0
	ld hl,IF
	rrca
	jr nc,_
	res 0,(hl)
	ld de,$0040
	jr dispatch_int
_
	rrca
	jr nc,_
	res 1,(hl)
	ld de,$0048
	jr dispatch_int
_
	rrca
	jr nc,_
	res 2,(hl)
	ld de,$0050
	jr dispatch_int
_
	res 3,(hl)
	ld de,$0058
dispatch_int:
	push bc
	 call.il lookup_code_cached
	pop bc
	ld a,TMR_ENABLE
	ld.lil (mpTimerCtrl),a
	ex af,af'
intretaddr = $+1
	ld de,0
do_interrupt_write_smc = $+1
	jp do_push_no_write
	
do_branch_slow:
	di
	exx
	pop hl
	push bc
	 call.il decode_branch_slow
	pop bc
	ld (hl),de
	dec hl
	ld (hl),RST_BRANCH
	push hl
	exx
	ei
	ret
	
do_mem:
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
	   call.il decode_mem
	   ld (ix+1),de
	   ld (ix),$CD
	  pop de
	 pop hl
	pop af
	ei
	jp (ix)
	
do_branch:
	push bc
	 push hl
	  call.il decode_branch
	 pop hl
	pop bc
	ld (hl),ix
	dec hl
	ld (hl),a
	push hl
	exx
	ex af,af'
	ei
	ret
	
updateLY:
	xor a
	ld.lil (mpTimerCtrl),a
	ld.lil hl,(mpTimer1Count+1)
	ld de,-SCANDELAY*128
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	add hl,de \ jr c,$+4 \ sbc hl,de \ adc hl,hl
	ld a,153
	sub l
	ret
	
flush_handler:
	di
	exx
flush_address = $+1
	ld de,0
	jp.lil flush_normal
	
flush_mem_handler:
	di
	exx
	pop de
	jp.lil flush_mem
	
do_swap:
	inc a
	jr nz,do_swap_generic
	ex af,af'
	rrca
	rrca
	rrca
	rrca
	ei
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
	ei
	ret
do_swap_hl:
	ex af,af'
	push af
	 call mem_read_any
	 rrca
	 rrca
	 rrca
	 rrca
	 call mem_write_any
	pop af
	ei
	ret
	
do_bits:
	sub $30
	sub 8
	jr c,do_swap
	add a,$38-1	;Use L instead of (HL)
	cp $C0
	jp pe,do_bits_readonly
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
	ei
	ret
do_bits_readonly:
	ld (do_bits_readonly_smc),a
	call mem_read_any
	; Use L because we have to affect flags, bleh
	push hl
	 ld l,a
	 ex af,af'
do_bits_readonly_smc = $+1
	 bit 0,l
	pop hl
	ei
	ret
	
ophandler08:
	pop ix
	pea ix+2
	exx
	push af
	 lea hl,iy
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
	pop af
	exx
	ei
	ret
	
ophandler31:
	pop ix
	ld iy,(ix)
	lea ix,ix+2
	di
	jp.lil set_gb_stack
	
ophandler34:
	ex af,af'
	call mem_read_any
	ld ixl,a
	ex af,af'
	inc ixl
	push af
	 ld a,ixl
	 call mem_write_any
	pop af
	ei
	ret
	
ophandler35:
	ex af,af'
	call mem_read_any
	ld ixl,a
	ex af,af'
	dec ixl
	push af
	 ld a,ixl
	 call mem_write_any
	pop af
	ei
	ret
	
ophandler36:
	pop ix
	pea ix+1
	push af
	 ld a,(ix)
	 call mem_write_any
	pop af
	ei
	ret
	
ophandler39:
	push de
	 ex af,af'
	 scf
	 ld a,(sp_base_address)
	 cpl
	 adc a,iyl
	 ld e,a
	 ld a,(sp_base_address+1)
	 cpl
	 adc a,iyh
	 ld d,a
	 ex af,af'
	 add hl,de
	pop de
	ei
	ret
	
handle_waitloop_stat:
	di
	ex af,af'
	exx
	call updateLY
	cp 144
	jr nc,_
	ld a,h
	sub SCANDELAY * 204 / 456 - 1
	jr c,_
	ld h,a
	sub (SCANDELAY * (204 + 172) / 456) - (SCANDELAY * 204 / 456)
	jr c,_
	ld h,a
	jr _
	
handle_waitloop_variable:
handle_waitloop_ly:
	di
	ex af,af'
	exx
	call updateLY
_
	call.il skip_cycles
	pop hl
	ld ix,(hl)
	exx
	ex af,af'
	ei
	jp (ix)
	
ophandler76:
	ex af,af'
	exx
haltloop:
	di
	ld hl,IF
	ld a,(hl)
	ld l,h
	and (hl)
	jr nz,haltdone
	call updateLY
	call.il skip_cycles
	ei
	jr haltloop
haltdone:
	exx
	ex af,af'
	ei
	ret
	
ophandlerE2:
	ld ixh,$FF
	ld ixl,c
	call mem_write_ports_always
	ei
	ret
	
ophandlerE8:
	ex af,af'
	ex (sp),hl
	ld a,(hl)
	inc hl
	ex (sp),hl
	push de
	 ld e,a
	 rla
	 sbc a,a
	 ld d,a
	 add iy,de
	pop de
	ex af,af'
	ei
	ret
	
ophandlerE9:
	di
	ex af,af'
	xor a
	ld.lil (mpTimerCtrl),a
	push hl
	 push bc
	  push de
	   ex de,hl
	   call.il lookup_code_cached
	  pop de
	 pop bc
	pop hl
	ld a,TMR_ENABLE
	ld.lil (mpTimerCtrl),a
	ex af,af'
	ei
	jp (ix)
	
ophandlerF2:
	ld ixh,$FF
	ld ixl,c
	ex af,af'
	call mem_read_ports_always
	ld a,ixl
	ei
	ret
	
ophandlerF3:
	ex af,af'
	xor a
	ld (intstate),a
	ex af,af'
	ei
	ret
	
ophandlerF8:
	ex af,af'
	pop hl
	ld a,(hl)
	inc hl
	push hl
	push de
	 ld e,a
	 rla
	 sbc a,a
	 ld d,a
	 scf
	 ld a,(sp_base_address)
	 cpl
	 adc a,iyl
	 ld l,a
	 ld a,(sp_base_address+1)
	 cpl
	 adc a,iyh
	 ld h,a
	 add hl,de
	pop de
	ex af,af'
	ei
	ret
	
ophandlerF9:
	pop ix
	push hl
	pop iy
	di
	jp.lil set_gb_stack
	
ophandlerFB:
	ex af,af'
	ld a,1
	ld (intstate),a
	call checkIntPostEnable
	ei
	ret
	
ophandlerRETI:
	di
	ex af,af'
	xor a
	ld.lil (mpTimerCtrl),a
	inc a
	ld (intstate),a
	exx
	push bc
	 call.il pop_and_lookup_code_cached
	pop bc
	exx
	ld a,TMR_ENABLE
	ld.lil (mpTimerCtrl),a
	call checkIntPostEnable
	ei
	jp (ix)
	
ophandlerINVALID:
	ei
	jr ophandlerINVALID
	
write_vram_handler:
	pop ix
	pea ix+2
	ld ix,(ix)
	call mem_write_vram_always
	ei
	ret
	
write_cart_handler:
	pop ix
	pea ix+2
	ld ix,(ix)
	call mem_write_cart_always
	ei
	ret
	
write_cram_bank_handler:
	exx
	pop hl
	ld de,(hl)
	inc hl
	inc hl
	push hl
	ld.lil hl,(cram_bank_base)
	ex af,af'
	add.l hl,de
	ex af,af'
	ld.l (hl),a
	exx
	ei
	ret
	
read_rom_bank_handler:
	exx
	pop hl
	ld de,(hl)
	inc hl
	inc hl
	push hl
	ld.lil hl,(rom_bank_base)
	ex af,af'
	add.l hl,de
	ex af,af'
	ld.l a,(hl)
	exx
	ei
	ret
	
read_cram_bank_handler:
	exx
	pop hl
	ld de,(hl)
	inc hl
	inc hl
	push hl
	ld.lil hl,(cram_bank_base)
	ex af,af'
	add.l hl,de
	ex af,af'
	ld.l a,(hl)
	exx
	ei
	ret
	
readP1handler:
	ex af,af'
	call readP1
	ld a,ixl
	ei
	ret
	
readDIVhandler:
	ex af,af'
	call readDIV
	ld a,ixl
	ei
	ret
	
readTIMAhandler:
	ex af,af'
	call readTIMA
	ld a,ixl
	ei
	ret
	
readLYhandler:
	ex af,af'
	call readLY
	ld a,ixl
	ei
	ret
	
readSTAThandler:
	ex af,af'
	call readSTAT
	ld a,ixl
	ei
	ret
	
writeTIMAhandler:
	ex af,af'
	call writeTIMA
	ei
	ret
	
writeTMAhandler:
	ex af,af'
	call writeTMA
	ei
	ret
	
writeTAChandler:
	ex af,af'
	call writeTAC
	ei
	ret
	
writeLCDChandler:
	ex af,af'
	call writeLCDC
	ei
	ret
	
writeSCYhandler:
	ld ix,SCY
	call write_scroll_swap
	ei
	ret
	
writeSCXhandler:
	ld ix,SCX
	call write_scroll_swap
	ei
	ret
	
writeLYChandler:
	call writeLYCswap
	ei
	ret
	
writeWYhandler:
	ld ix,WY
	call write_scroll_swap
	ei
	ret
	
writeWXhandler:
	ld ix,WX
	call write_scroll_swap
	ei
	ret
	
writeDMAhandler:
	ex af,af'
	call writeDMA
	ei
	ret
	
writeIFhandler:
	ld (IF),a
	call writeINTswap
	ei
	ret
	
writeIEhandler:
	ld (IE),a
	call writeINTswap
	ei
	ret
	
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
	ld.lil a,(mpTimer1Count)
	cpl
	ld ixl,a
	ex af,af'
	ret
	
readSTAT:
	exx
	ld a,(STAT)
	and $F8
	ld c,a
	ld a,(LCDC)
	add a,a
	jr nc,_
	ld ix,(LYC)
	call updateLY
	cp ixl
	jr nz,_
	set 2,c
_
	cp 144
	jr nc,_
	ld a,h
	cp SCANDELAY * 204 / 456 - 1
	jr c,++_
	set 1,c
	cp SCANDELAY * (204 + 172) / 456 - 1
	jr nc,++_
_
	inc c
_
	ld ixl,c
	exx
	ld a,TMR_ENABLE
	ld.lil (mpTimerCtrl),a
	ex af,af'
	ret
	
readLY:
	ld a,(LCDC)
	add a,a
	sbc a,a
	jr z,_
	exx
	call updateLY
	exx
_
	ld ixl,a
	ld a,TMR_ENABLE
	ld.lil (mpTimerCtrl),a
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
	jp.lil lcdc_write
	
writeTAC:
	di
	jp.lil tac_write
	
writeTMA:
	di
	call.il tma_write
	ret
	
writeTIMA:
	di
	call.il tima_write
	ret
	
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
	call.il write_vram_and_expand
	ret
	
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
	sub (TIMA & $FF) + 1
	jr z,writeTIMA
	dec a
	jr z,writeTMA
	dec a
	jr z,writeTAC
	sub IF - TAC
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
	call.il oam_transfer
	ret
	
write_scroll_swap:
	ex af,af'
write_scroll:
	ld a,(frame_skip)
	dec a
	jr nz,mem_write_oam_swap
	di
	call.il render_catchup
	jr mem_write_oam_swap
	
writeLYC:
	ex af,af'
writeLYCswap:
	ld l,a
	ex af,af'
	ld a,154
	sub l
	ld l,a
	ld h,SCANDELAY
	mlt hl
	dec hl
	ld.lil (mpTimer1Match1+1),hl
	ex af,af'
	ld (LYC),a
	ret
	
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
	; Do a software interrupt!
	di
	ld a,$11
	ld.lil (mpIntEnable),a
_
	ex af,af'
	ret
	
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
_
	pop bc
	ex af,af'
	ret
	
mbc_2000:
	push bc
	 ld a,(mbc_z80)
	 ld b,a
	 ex af,af'
	 ld c,a
	 ex af,af'
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
mbc_2000_denied:
	pop bc
	ex af,af'
	ret
	
intstate:
	.db 0
frame_skip:
	.db 1
	
keys:
	.dw $FFFF
	
sp_base_address:
	.dl 0
memroutine_next:
	.dl 0
render_save_sps:
	.dw 0
mbc_z80:
	.db 0
	
	.assume adl=1
z80codesize = $-0
	.org z80code+z80codesize
	
	.echo "Z80 mode code size: ", z80codesize