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
	ex (sp),hl
	push de
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
	pop hl
	jr do_push
	
	.block $28-$
r_pop:
	ld.l ix,(iy)
	ex (sp),ix
	jr do_pop
	
	.block $30-$
r_interrupt:
	di
	ex af,af'
	pop ix
	push hl
	jp do_interrupt
	
	.block $38-$
rst38h:
	push af
	 ld.lil a,(mpIntMaskedStatus)
	 rra
	 jr c,on_interrupt
	 rra
	 jr c,timer_interrupt
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
	lea.l iy,iy-2
	ld.l (iy),e
	ld.l (iy+1),d
	push hl
	push de
	call do_call_finish
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
	lea.l iy,iy-2
	ld.l (iy),l
	ld.l (iy+1),h
do_call_finish:
	exx
	ei
	jp (ix)
	
do_pop:
	lea.l iy,iy+2
	ei
	jp (ix)
	
ophandlerRETnomatch:
	ld b,CALL_STACK_DEPTH+1
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
	push hl
	 push de
	  push bc
	   call.il pop_and_lookup_code_cached
	  pop bc
	 pop de
	pop hl
	ld a,TMR_ENABLE
	ld.lil (mpTimerCtrl),a
	ex af,af'
	ei
	jp (ix)
	
timer_interrupt:
	 push hl
	  ld.lil hl,mpTimerCtrl
	  xor a
	  cp.l (hl)
	  jr z,test_interrupts
	  ld.l (hl),a
	  
	  ld a,(LCDC)
	  add a,a
	  ld hl,LY
	  jr nc,screen_off
	  ld a,(hl)
	  cp 144
	  call c,do_render
	  
	  ld a,(hl)
	  inc a
	  cp 154
	  jr c,_
	  ld hl,SCANDELAY*2*256
	  ld.lil (mpTimer1Count),hl
	  ld hl,LY
screen_off:
	  xor a
_
	  ld (hl),a
	  cp 144
	  jr nz,_
	  call.il vblank_stuff
_
	  inc hl
	  cp (hl)
	  jr nz,test_interrupts
	  ld l,STAT & $FF
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
	 pop hl
	 ld a,TMR_ENABLE
	 ld.lil (mpTimerCtrl),a
	 ld a,2
	 ld.lil (mpIntAcknowledge),a
	pop af
	ei
	ret
	
interrupt_get:
	  ld (intmask),a
	 pop hl
	pop af
	ex (sp),hl
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
	pop af
	ex (sp),hl
	ret
_
	 push hl
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
	     ld a,(intmask)
waitloop_request = $+1
	     and 1
	     jr nz,check_waitloop
waitloop_done:
	     ld a,(hl)
	     ld (intsavecode),a
	     ld (hl),RST_INTERRUPT
	    pop.l ix
	   pop bc
	  pop de
	 pop hl
	 ld a,TMR_ENABLE
	 ld.lil (mpTimerCtrl),a
	 ld a,2
	 ld.lil (mpIntAcknowledge),a
	pop af
	ex (sp),hl
	ei
	ret
	
check_waitloop:
	push hl
	 call.il identify_waitloop
	pop hl
	jr waitloop_done
	
do_render:
	ld a,(frame_skip)
	dec a
	ret nz
	push bc
	 push de
	  call.il render_scanline
	 pop de
	pop bc
	ld hl,LY
	ret
	
do_interrupt:
	 xor a
	 ld.lil (mpTimerCtrl),a
intsavecode = $+3
	 ld (ix-1),0
	 push de
intretaddr = $+1
	  ld de,0
	  lea.l iy,iy-2
	  ld.l (iy),e
	  ld.l (iy+1),d
	  ld hl,IF
intmask = $+1
	  ld a,0
	  rrca
	  jr nc,_
	  ld a,1
	  ld (waitloop_request),a
	  res 0,(hl)
	  ld e,$40
	  jr dispatch_int
_
	  rrca
	  jr nc,_
	  res 1,(hl)
	  ld e,$48
	  jr dispatch_int
_
	  rrca
	  jr nc,_
	  res 2,(hl)
	  ld e,$50
	  jr dispatch_int
_
	  res 3,(hl)
	  ld e,$58
dispatch_int:
	  ld d,$00
	  push bc
	   call.il lookup_code_cached
	  pop bc
	 pop de
	pop hl
	ld a,TMR_ENABLE
	ld.lil (mpTimerCtrl),a
	ex af,af'
	ei
	jp (ix)
	
do_branch_slow:
	di
	ex (sp),hl
	push de
	 push bc
	  call.il decode_branch_slow
	  ld (hl),de
	  dec hl
	  ld (hl),RST_BRANCH
	 pop bc
	pop de
	ex (sp),hl
	ex af,af'
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
	  ld (hl),ix
	 pop bc
	pop de
	dec hl
	ld (hl),a
	ex (sp),hl
	ex af,af'
	ei
	ret
	
flush_handler:
	di
flush_address = $+2
	ld ix,0
	jp.lil flush_normal
	
flush_mem_handler:
	di
	pop ix
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
	push hl
	 ld hl,(ix)
	 push af
	  scf
	  ld a,(sp_base_address)
	  cpl
	  adc a,iyl
	  call mem_write_any
	  ld a,(sp_base_address+1)
	  cpl
	  adc a,iyh
	  inc hl
	  call mem_write_any
	 pop af
	pop hl
	ei
	ret
	
ophandler31:
	pop ix
	ld iy,(ix)
	lea ix,ix+2
	di
	jp.lil set_gb_stack
	
ophandler33:
	dec iy
	ei
	ret
	
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
	
ophandler3B:
	inc iy
	ei
	ret
	
current_waitloop_1:
	.dw waitloop_target
current_waitloop_2:
	.dw waitloop_target
	
handle_waitloop:
waitloop_target = $+2
	ld ix,0
	push ix
	ex af,af'
	ld a,(waitloop_request)
	dec a
	jr nz,_
	ld (waitloop_request),a
	ex af,af'
	ei
	ret
	
haltloop:
	di
	xor a
	ld.lil (mpTimerCtrl),a
	ld ix,1
	ld.lil (mpTimer1Count),ix
	ld a,TMR_ENABLE
	ld.lil (mpTimerCtrl),a
	ex af,af'
	ei
	halt
ophandler76:
	ex af,af'
_
	xor a
	ld (waitloop_request),a
	ld a,i
	jp pe,haltloop
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
	ex af,af'
	ei
	ret
	
ophandlerRETI:
	di
	ex af,af'
	xor a
	ld.lil (mpTimerCtrl),a
	inc a
	ld (intstate),a
	push hl
	 push de
	  push bc
	   call.il pop_and_lookup_code_cached
	  pop bc
	 pop de
	pop hl
	ld a,TMR_ENABLE
	ld.lil (mpTimerCtrl),a
	ex af,af'
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
	pop ix
	pea ix+2
	push de
	 ld de,(ix)
	 ld.lil ix,(cram_bank_base)
	 ex af,af'
	 add.l ix,de
	 ex af,af'
	 ld.l (ix),a
	pop de
	ret
	
read_rom_bank_handler:
	pop ix
	pea ix+2
	push de
	 ld de,(ix)
	 ld.lil ix,(rom_bank_base)
	 ex af,af'
	 add.l ix,de
	 ex af,af'
	 ld.l a,(ix)
	pop de
	ret
	
read_cram_bank_handler:
	pop ix
	pea ix+2
	push de
	 ld de,(ix)
	 ld.lil ix,(cram_bank_base)
	 ex af,af'
	 add.l ix,de
	 ex af,af'
	 ld.l a,(ix)
	pop de
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
	
readSTAThandler:
	ex af,af'
	call readSTAT
	ld a,ixl
	ei
	ret
	
writeDMAhandler:
	ex af,af'
	call writeDMA
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
	ld a,(STAT)
	and $FC
	ld ixl,a
	inc ixl
	ld a,(LCDC)
	add a,a
	jr nc,_
	ld a,(LY)
	cp 144
	jr nc,++_
	or a
	ld.lil a,(mpTimer1Count+1)
	jr nz,_
	cp SCANDELAY
	jr nc,++_
_
	dec ixl
	cp SCANDELAY * 204 / 456 - 1
	jr c,_
	inc ixl
	inc ixl
	cp SCANDELAY * (204 + 172) / 456 - 1
	jr nc,_
	inc ixl
_
	ex af,af'
	ret
	
mem_read_bail:
	pop ix
	lea ix,ix-8
	jp (ix)
	
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
	cp DIV*2 & $FF
	jr z,readDIV
	cp STAT*2 & $FF
	jr z,readSTAT
mem_read_oam:
	ld a,(ix)
	ld ixl,a
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
	
mem_write_any_cart:
	push hl
	pop ix
	jr mem_write_cart_swap
mem_write_any_ports:
	push hl
	pop ix
	jr nz,mem_write_ports_swap
	jr mem_write_oam_swap
mem_write_any_vram:
	push hl
	pop ix
	jr mem_write_vram_swap
	
mem_write_bail:
	pop ix
	pop af
	lea ix,ix-10
	jp (ix)
	
	;IX=GB address, A=data
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
	call.lil write_vram_and_expand
	ret
	
	;IX=GB address, A=data
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
	;IX=GB address, A=data
mem_write_ports_always:
	ex af,af'
mem_write_ports_swap:
	ld a,ixl
	add a,a
	jr c,mem_write_oam_swap
	cp DMA*2 & $FF
	jr nz,mem_write_oam_swap
writeDMA:
	di
	call.il oam_transfer
	ret
	
	;IX=GB address, A=data
mem_write_cart:
	ex af,af'
	ld a,ixh
	rla
	jr c,mem_write_bail
mem_write_cart_swap:
	ex af,af'
	;IX=GB address, A=data
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
	 and b
	 jr nz,_
	 inc a
_
	 ld c,a
	 ld a,b
	 cpl
curr_rom_bank = $+1
	 and 1
	 or c
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