	.assume adl=0
	
	; LUT to index I/O port and HRAM write routines
	.block (mem_write_any_routines+256)-$
mem_write_port_lut:
;00
	.db writeP1 - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db writeSC - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db writeDIV - mem_write_port_routines
	.db writeTIMA - mem_write_port_routines
	.db writeTMA - mem_write_port_routines
	.db writeTAC - mem_write_port_routines
;08
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db writeIF - mem_write_port_routines
;10
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
;18
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
;20
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db write_audio - mem_write_port_routines
	.db writeNR52 - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
;28
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
;30
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
;38
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
;40
	.db writeLCDC - mem_write_port_routines
	.db writeSTAT - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db writeLYC - mem_write_port_routines
	.db writeDMA - mem_write_port_routines
	.db writeBGP - mem_write_port_routines
;48
	.db write_scroll - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	.db write_scroll - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db writeKEY1 - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db writeVBK - mem_write_port_routines
;50
	.db write_port_ignore - mem_write_port_routines
	.db write_hdma - mem_write_port_routines
	.db write_hdma - mem_write_port_routines
	.db write_hdma - mem_write_port_routines
	.db write_hdma - mem_write_port_routines
	.db writeHDMA5 - mem_write_port_routines
	.db writeRP - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
;58
;60
	.safe_fill $FF68 - $FF58, write_port_ignore - mem_write_port_routines
;68
	.db write_palette_index - mem_write_port_routines
	.db write_palette_data - mem_write_port_routines
	.db write_palette_index - mem_write_port_routines
	.db write_palette_data - mem_write_port_routines
	.db writeOPRI - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
;70
	.db writeSVBK - mem_write_port_routines
	.db write_port_ignore - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db write_port_direct - mem_write_port_routines
	.db writeFF75 - mem_write_port_routines
	
	.safe_fill $FF80 - ($FF75+1), write_port_ignore - mem_write_port_routines
;80
	.safe_fill IE - $FF80, write_hram_direct - mem_write_port_routines
;FF
	.db writeIE - mem_write_port_routines
	
mem_write_port_routines:
	; This set of routines is for direct writes to I/O ports or HRAM.
	
	; Inputs: L=write value, B'=addr LSB, C'=cycle offset, DE'=cycle counter
	;         AFBCDEHL' are swapped
	; Outputs: AFBCDEHL' are unswapped, A' is restored from E'
	; Destroys: HL, BC', HL', F'
	
writeIEhandler:
	ld e,a
writeIE:
	exx
	ld a,l
	ld (IE),a
	jr checkInt
	
writeIFhandler:
	ld e,a
writeIF:
	ld a,d
	or a
	call z,handle_events_for_mem_access
	exx
	ld a,l
	and $1F
	ld (active_ints),a
checkInt:
	exx
	; Check the pre-delay interrupt state, since if the interrupt enable
	; delay is active then an interrupt check is already scheduled
	ld a,(intstate_smc_2)
	or a
	jr z,checkIntDisabled
	ld hl,(IE)
	ld a,h
	and l
	jp nz,trigger_event
checkIntDisabled:
	ld a,e
	ex af,af'
	exx
	ret

writeTAChandler:
	ld e,a
writeTAC:
	call updateTIMA
	jp.lil tac_write_helper

writeTIMAhandler:
	ld e,a
writeTIMA:
	call updateTIMA
	jp nz,tima_write_helper
	; Ignore writes directly on the reload cycle
	jr checkIntDisabled

writeLCDChandler:
	ld e,a
writeLCDC:
	push bc
	 call updateSTAT
	 jp.lil lcdc_write_helper

writeSTAThandler:
	ld e,a
writeSTAT:
	push de
	 push bc
	  call updateSTAT
	  jp.lil stat_write_helper
	
writeLYChandler:
	ld e,a
writeLYC:
	call updateSTAT_if_changed_lyc
	  jp.lil lyc_write_helper
	
writeDIVhandler:
	ld e,a
writeDIV:
	call updateTIMA
	jp.lil div_write_helper
	
writeSC:
	jp _writeSC
	
writeKEY1:
	jp _writeKEY1
	
;==============================================================================
; Everything above this point may cause a reschedule on write
;==============================================================================
	
write_hram_direct:
write_port_direct:
	exx
	ld a,l
	exx
	ld c,b
write_audio_finish:
	ld b,$FF
	ld (bc),a
write_port_ignore:
	ld a,e
	ex af,af'
	exx
	ret
	
write_audio_handler:
	ex af,af'
	ld e,a
write_audio:
write_audio_disable_smc = $
	exx
	ld a,l
	exx
	ld c,b
	ld b,a
	ld l,c
	ld h,audio_port_value_base >> 8
	ld (hl),b
	set 7,l ;audio_port_masks
	ld a,(hl)
	; Handle writes to enable bits specially
	or a
	jp po,write_audio_finish_fast
	; For NR30, leave the high bit the same
	; For NRx4, invert the high bit
	xor b
	jp p,write_audio_enable_disable
	or b
	jr write_audio_finish
	
write_scroll:
	jp _write_scroll
	
writeTMA:
	jp _writeTMA
	
writeDMAhandler:
	ld e,a
writeDMA:
	call updateSTAT
	push de
	 jp.lil dma_write_helper
	
writeBGPhandler:
	ld e,a
writeBGP:
	ld a,(BGP)
	call updateSTAT_if_changed_any
	 pop bc
	pop de
	jp.lil BGP_write_helper
	
writeNR52:
	jp _writeNR52
	
write_hdma:
	jp _write_hdma
	
writeHDMA5:
	jp _writeHDMA5
	
writeVBKhandler:
	ld e,a
writeVBK:
	exx
	ld a,l
	or $FE
	ld (VBK),a
	jp.lil writeVBK_helper
	
writeSVBK_stack:
	; Adjust the direct stack pointer when it points to banked WRAM
	ld c,a
	dec h \ dec h
	sub (hl)
	add a,iyh
	ld iyh,a
	ld a,c
	jr writeVBK_finish

;writeP1:
;	jr _writeP1
	
writeRP:
	jr _writeRP
	
writeOPRI:
	jr _writeOPRI
	
writeFF75:
	jr _writeFF75
	
write_palette_index:
	jr _write_palette_index
	
write_palette_data:
	djnz _write_palette_data
	
writeSVBKhandler:
	ld e,a
writeSVBK:
	exx
	.echo mem_write_port_routines+256-$, " bytes remaining for port writes"
	ld a,l
	exx
	or $F8
	ld (SVBK),a
	ld l,a
	ld h,wram_bank_base_lut >> 8
	ld a,(hl)
	ld (wram_mirror_bank_base+1-z80codebase),a
	add a,$20
	ld l,(wram_bank_base_for_write+1) & $FF
	ld (hl),a
writeSVBK_stack_smc = $
	dec h \ dec h
writeVBK_finish:
	ld (hl),a
	dec h \ dec h
	ld (hl),a
	ld a,e
	ex af,af'
	exx
	ret
	
writeP1handler:
	ld e,a
writeP1:
	exx
	ld a,l
	exx
	or $CF
	bit 4,a
	jr nz,_
keys_low = $+1
	and $FF
_
	bit 5,a
	jr nz,_
keys_high = $+1
	and $FF
_
	ld (P1),a
	ld a,e
	ex af,af'
	exx
	ret
	
writeRPhandler:
	ld e,a
_writeRP:
	exx
	ld a,l
	exx
	ld hl,RP
	or $3E
	cp h
	jr nz,writeRP_finish
	ld a,$FD ; Read own IR signal
	jr writeRP_finish
	
writeOPRIhandler:
	ld e,a
_writeOPRI:
	exx
	ld a,l
	exx
	ld hl,OPRI
	or $FE
	jr writeOPRI_finish
	
writeFF75handler:
	ld e,a
_writeFF75:
	exx
	ld a,l
	exx
	ld hl,$FF75
	or $8F
	jr writeFF75_finish
	
write_palette_index_handler:
	ex af,af'
	ld e,a
_write_palette_index:
	ld l,b
	ld h,$FF
	ld a,b
	add a,(gbc_bg_palette_data >> 8) - (BGPI & $FF)
	ld b,a
	exx
	ld a,l
	exx
	ld c,a
	set 6,c
	ld (hl),c
	set 7,c
write_palette_data_inc:
	ld a,(bc)
write_palette_data_no_inc:
	inc hl
writeRP_finish:
writeOPRI_finish:
writeFF75_finish:
	ld (hl),a
	ld a,e
	ex af,af'
	exx
	ret
	
write_palette_data_handler:
	ex af,af'
	ld e,a
_write_palette_data:
	ld l,b
	ld h,$FF
	ld a,b
	add a,(gbc_bg_palette_data >> 8) - (BGPI & $FF)
	ld b,a
	exx
	ld a,l
	exx
	ld c,(hl)
	bit 7,c
	set 7,c
	ld (bc),a
	jr z,write_palette_data_no_inc
	inc c
	ld (hl),c
	jr nz,write_palette_data_inc
	ld c,$C0
	ld (hl),c
	jr write_palette_data_inc
	
writeSChandler:
	ld e,a
_writeSC:
	exx
	ld a,l
	exx
	or $7E
	ld (SC),a
	inc a
	jr nz,writeSC_disable
	ld hl,serial_counter_checker
	ld (event_counter_checker_slot_serial),hl
	push de
	 ; Get the current cycle offset in BC
	 ex de,hl
	 ld b,$FF
	 add hl,bc
	 ld b,h
	 ld c,l
	 ; Get the DIV counter for the current cycle
	 ld hl,i
	 add hl,bc
	 ; To make things simpler, since bits tick every 128 cycles,
	 ; shift left once before overriding the low byte
	 add hl,hl
	 ld a,(serial_counter)
	 ; Preserve the top bit of DIV in bit 0 while shifting left
	 rla
	 ; Check whether the tick has already occurred
	 cp l
	 ld l,a
	 ; Adjust the upper byte by 8 if the tick already occurred,
	 ; otherwise by 7
	 ld a,h
	 adc a,7
	 ld h,a
	 ; Rotate the counter back and save it
	 rra
	 rr l
	 rr h
	 ld (serial_counter),hl
	 ; Get the relative time of the event from the currently scheduled event
	 ex de,hl
	 ld hl,i ; Resets carry
	 sbc hl,de
	pop de
	jp reschedule_event_any
	
writeSC_disable:
	ld hl,disabled_counter_checker
	ld (event_counter_checker_slot_serial),hl
writeTMA_finish:
	ld a,e
	ex af,af'
	exx
	ret

writeTMAhandler:
	ld e,a
_writeTMA:
	call updateTIMA
	exx
	ld a,l
	exx
	ld hl,TMA
	ld (hl),a
	; Subtract TMA from 256, without destroying Z
	cpl
	ld l,a
	inc hl
	; Check if the result was 256, without destroying Z
	ld a,h
	rlca
	; Multiply by the timer factor
timer_cycles_reset_factor_smc = $+1
	ld h,0
	jr nc,_
	mlt hl
_
	ld (timer_period),hl
	jr nz,writeTMA_finish
	; Make sure writes on the reload cycle go through
	ld hl,i ; Resets Z flag
	add hl,bc
	ld (timer_counter),hl
	jr writeTMA_finish
	
writeNR52handler:
	ld e,a
_writeNR52:
	ld hl,NR52
	ld a,(hl)
	exx
	xor l
	ld a,l
	exx
	jp p,writeNR52_finish
	; Whether enabling or disabling audio, all channels are off
	and $80
	or $70
	ld (hl),a
	; If enabling, un-ignore all writes
	.db $21 ;LD HL,
	 exx
	 ld a,l
	jp m,writeNR52_enable
	push de
	 ; Zero all audio registers
	 ld hl,audio_port_values
	 ld de,audio_port_values+1
	 ld bc,NR52 - NR10 - 1
	 ld (hl),b
	 ldir
	 ; Copy masks to CPU-visible registers
	 ld l,audio_port_masks & $FF
	 ld de,NR10
	 ld c,NR52 - NR10
	 ldir
	pop de
	; If disabling, ignore all writes
	ld hl,$18 | ((write_port_ignore - (write_audio_disable_smc+2)) << 8)
writeNR52_enable:
	ld (write_audio_disable_smc),hl
writeNR52_finish:
	ld a,e
	ex af,af'
	exx
	ret
	
writeKEY1handler:
	ld e,a
_writeKEY1:
	ld a,e
	exx
	srl l
	ld hl,KEY1
	rr (hl)
	rlc (hl)
	ex af,af'
	ret
	
write_hdma_handler:
	ex af,af'
	ld e,a
_write_hdma:
	FIXME
	
writeHDMA5handler:
	ld e,a
_writeHDMA5:
	FIXME
	
write_audio_enable_disable:
	xor b
	cp $80
	jr z,write_audio_finish_fast
	ld hl,NR52
	jr c,writeNR30_disable
	or b
	ld b,h
	ld (bc),a
	; Set the appropriate bit in NR52
	ld a,c
	and 3
	add a,-2
	adc a,3
	add a,a
	daa
	rra
	or (hl)
	ld (hl),a
	ld a,e
	ex af,af'
	exx
	ret
	
writeNR30_disable:
	res 2,(hl)
write_audio_finish_fast:
	or b
	ld b,$FF
	ld (bc),a
	ld a,e
	ex af,af'
	exx
	ret
	
;==============================================================================
; Port read handlers
;==============================================================================
	
unpatch_op_read_hl_normal:
	inc a ; Switch from L access back to (HL)
	pop hl
	dec hl
	ld (hl),a
	dec hl
	ld (hl),$49 ;.LIS
	jr unpatch_op_read_hl_finish
	
unpatch_op_read_hl_port_l:
	pop hl
	ld a,(hl)
	cp $CB ;bitwise prefix
	jr c,unpatch_op_read_hl_normal
	ld a,$6E ;LD L,(HL)
	jr nz,unpatch_op_read_hl_tail_call ; Opcode was $EB == EX DE,HL
	; BIT instruction, get the second opcode byte
	inc hl
	ld a,(hl)
	pop hl
	inc a ; Switch from L access back to (HL)
	; Write the bitwise opcode
	ld (hl),a
	ld a,$CB
unpatch_op_read_hl_tail_call:
	dec hl
	ld (hl),a
	dec hl
	ld (hl),$5B ;.LIL
unpatch_op_read_hl_finish:
	dec hl
	ld (hl),RST_GET_HL_READ_PTR
	; Restore the cycle counter
	exx
	ld a,e
	ex af,af'
	exx
	jp (hl)
	
	; Input: DE=Game Boy HL, C'=block cycle offset, BCDEHL' are swapped
	; Output: L=read value
	; Destroys: BC', E', HL', F'
op_read_hl_port_l:
	ex af,af'
	ld e,a
	exx
	ld a,d
	inc a
	jr nz,unpatch_op_read_hl_port_l
	ld a,e
	exx
op_read_hl_port_l_unchecked:
	cp STAT & $FF
	jr z,op_read_hl_STAT
	cp LY & $FF
	jr z,op_read_hl_LY
	sub IF & $FF
	jr z,op_read_hl_IF
	add a,IF-TIMA
	sbc a,$FF
	jr z,op_read_hl_DIV_TIMA
	ld a,e
	exx
	ex af,af'
	ex de,hl
	ld e,(hl)
	ex de,hl
	ret
	
op_read_hl_STAT:
	call updateSTAT
	ld a,e
	exx
	ex af,af'
	ex de,hl
	ld e,(hl)
	ex de,hl
	ret
	
op_read_hl_LY:
	call updateLY
	ld a,e
	exx
	ex af,af'
	ex de,hl
	ld e,(hl)
	ex de,hl
	ret
	
op_read_hl_DIV_TIMA:
	jr nc,op_read_hl_DIV
	call updateTIMA
	ld a,e
	exx
	ex af,af'
	ex de,hl
	ld e,(hl)
	ex de,hl
	ret
	
op_read_hl_IF:
	or d
	call z,handle_events_for_mem_access
	ld a,(active_ints)
	or $E0
	exx
	ld l,a
	exx
	ld a,e
	ex af,af'
	exx
	ret
	
op_read_hl_DIV:
	ld hl,i
	add hl,de
	ld b,$FF
	add hl,bc
	add hl,hl
	add hl,hl
	ld a,h
	exx
	ld l,a
	exx
	ld a,e
	ex af,af'
	exx
	ret
	
port_read_any:
	push de
	 exx
	pop hl
	push bc
	 push de
	  ld c,l
	  ld d,h
	  ld e,a
	  exx
	  call op_read_de_port_unchecked
	  exx
	 pop de
	pop bc
	exx
	ret
	
unpatch_op_read_de_port:
	exx
	ld a,e
	ex af,af'
	exx
	pop hl
	dec hl
	ld (hl),op_read_de_normal >> 8
	dec hl
	ld (hl),op_read_de_normal & $FF
	dec hl
	ld (hl),$CD
	exx
	ld a,e
	ex af,af'
	exx
	jp (hl)
	
unpatch_op_read_bc_port:
	ld a,e
	ex af,af'
	exx
	pop hl
	ld (hl),$D9 ;EXX
	dec hl
	ld (hl),op_read_bc_normal >> 8
	dec hl
	ld (hl),op_read_bc_normal & $FF
	dec hl
	ld (hl),$CD
	jp (hl)
	
	; Input: BC=Game Boy DE, C'=cycle offset, BCDEHL' are swapped
	; Output: A=read value
	; Destroys: HL, BC', E', HL', F'
op_read_de_port:
	ex af,af'
	ld e,a
	exx
	ld a,b
	inc a
	jr nz,unpatch_op_read_de_port
op_read_de_port_unchecked:
	ld a,c
	exx
	cp STAT & $FF
	jr z,op_read_any_STAT
	cp LY & $FF
	jr z,op_read_any_LY
	sub IF & $FF
	jr z,op_read_any_IF
	add a,IF-TIMA
	sbc a,$FF
	jr z,op_read_any_DIV_TIMA
	ld a,e
	exx
	ex af,af'
	ld a,(bc)
	ret
	
	; Input: IX=Game Boy BC, C'=cycle offset, BCDEHL' are swapped
	; Output: A=read value
	; Destroys: HL, DE', HL', F'
op_read_bc_port:
	ex af,af'
	ld e,a
	ld a,ixh
	inc a
	jr nz,unpatch_op_read_bc_port
	ld a,ixl
	cp STAT & $FF
	jr z,op_read_any_STAT
	cp LY & $FF
	jr z,op_read_any_LY
	sub IF & $FF
	jr z,op_read_any_IF
	add a,IF-TIMA
	sbc a,$FF
	jr z,op_read_any_DIV_TIMA
	ld a,e
	exx
	ex af,af'
	ld a,(ix)
	ret
	
	; Inputs: AFBCDEHL' are swapped, C=block cycle offset, DE=cycle counter
	; Outputs: AFBCDEHL' are unswapped, STAT read into A
	; Destroys: BC', E', HL'
readSTAThandler:
op_read_any_STAT:
	call updateSTAT
	ld a,e
	ex af,af'
	exx
	ld a,(STAT)
	ret
	
	; Inputs: AFBCDEHL' are swapped, C=block cycle offset, DE=cycle counter
	; Outputs: AFBCDEHL' are unswapped, LY read into A
	; Destroys: BC', E', HL'
readLYhandler:
op_read_any_LY:
	call updateLY
	ld a,e
	ex af,af'
	exx
	ld a,(LY)
	ret
	
op_read_any_DIV_TIMA:
	jr nc,op_read_any_DIV
	; Inputs: AFBCDEHL' are swapped, C=block cycle offset, DE=cycle counter
	; Outputs: AFBCDEHL' are unswapped, TIMA read into A
	; Destroys: BC', E', HL'
readTIMAhandler:
	call updateTIMA
	ld a,e
	ex af,af'
	exx
	ld a,(TIMA)
	ret
	
	; Inputs: AFBCDEHL' are swapped, C=block cycle offset, DE=cycle counter
	; Outputs: AFBCDEHL' are unswapped, DIV read into A
	; Destroys: BC', E', HL'
readIFhandler:
op_read_any_IF:
	ld a,d
	or a
	call z,handle_events_for_mem_access
	ld a,(active_ints)
	or $E0
	ld l,a
	ld a,e
	ex af,af'
	ld a,l
	exx
	ret
	
	; Inputs: AFBCDEHL' are swapped, C=block cycle offset, DE=cycle counter
	; Outputs: AFBCDEHL' are unswapped, DIV read into A
	; Destroys: BC', E', HL'
readDIVhandler:
op_read_any_DIV:
	ld hl,i
	add hl,de
	ld b,$FF
	add hl,bc
	add hl,hl
	add hl,hl
	ld a,e
	ex af,af'
	ld a,h
	exx
	ret
	
	; Input: IX=Game Boy BC, C'=cycle offset, BCDEHL' are swapped
	; Output: A=read value
	; Destroys: HL, DE', HL', F'
op_read_hl_port:
	ex af,af'
	ld e,a
	exx
	ld a,d
	inc a
	ld a,e
	exx
	jr nz,unpatch_op_read_hl_port
	cp STAT & $FF
	jr z,op_read_any_STAT
	cp LY & $FF
	jr z,op_read_any_LY
	sub IF & $FF
	jr z,op_read_any_IF
	add a,IF-TIMA
	sbc a,$FF
	jr z,op_read_any_DIV_TIMA
	ld a,e
	exx
	ex af,af'
	ld a,(de)
	ret
	
	; Input: IXL=Game Boy C, C'=cycle offset, BCDEHL' are swapped, 
	; Output: A read from ($FF00+C)
	; Destroys: F', BC', E', HL'
op_read_c_hmem:
	ex af,af'
	ld b,ixl
	bit 7,b
	jr nz,op_read_c_hram
	ld e,a
	ld a,b
	cp STAT & $FF
	jr z,op_read_any_STAT
	cp LY & $FF
	jr z,op_read_any_LY
	sub IF & $FF
	jr z,op_read_any_IF
	add a,IF-TIMA
	sbc a,$FF
	jr z,op_read_any_DIV_TIMA
	ld a,e
op_read_c_hram:
	ld c,b
	ld b,$FF
	ex af,af'
	ld a,(bc)
	exx
	ret
	
unpatch_op_read_hl_port:
	pop hl
	; Get the trampoline address in BC
	dec hl
	ld b,(hl)
	ld (hl),$7E ;LD A,(HL)
	dec hl
	ld c,(hl)
	; Check which pool the trampoline is in
	; Carry set if high pool
	push hl
	 ld hl,(trampoline_next)
	 scf
	 sbc hl,bc
	 ld a,e
	 exx
	pop hl
	ld (hl),$49 ;.LIS
	jr c,_
	ld (hl),$5B ;.LIL
_
	dec hl
	ld (hl),RST_GET_HL_READ_PTR
	ex af,af'
	jp (hl)
	
	
;==============================================================================
; Lazy register update routines (STAT, LY, TIMA)
;==============================================================================
	
write_scroll_handler:
	ex af,af'
	ld e,a
_write_scroll:
	exx
	ld a,l
	exx
	ld h,$FF
	ld l,b
	cp (hl)
	jr z,updateSTAT_no_change_scroll
	push hl
	 call updateSTAT
	 jp.lil scroll_write_helper
	
updateSTAT_if_changed_lyc:
	ld a,(LYC)
updateSTAT_if_changed_any:
	exx
	cp l
	ld a,l
	exx
	pop hl   ; Pop return address
	jr nz,updateSTAT_push
updateSTAT_no_change_scroll:
	ld a,e
	exx
	ex af,af'
	ret
	
	; Handle transition from fake mode 0 on LCD startup
lcd_on_STAT_handler:
	call lcd_on_STAT_restore
	inc b
	inc b
	ld a,l
	jr updateSTAT_mode2
	
updateSTAT_maybe_mode1:
	; Special-case line 0 to see if vblank was exited
	inc a
	jr nz,updateSTAT_mode1
	; Check if LY update kept STAT in mode 1 or changed it to mode 0
	ld a,(STAT)
	rra
	jr nc,updateSTAT_mode1_exit
updateSTAT_mode1:
	; Disable catch-up rendering in case of vblank overflow
	xor a
	ld r,a
	; Save LYC coincidence bit and ensure mode 1 is set
	inc a
	or b
	ld (STAT),a
	; Set STAT update time to LY update time
	ld hl,(nextupdatecycle_LY)
	ld (nextupdatecycle_STAT),hl
	ret
	
updateSTAT_push:
	push de
	ld b,a
	push bc
	push hl ; Push return address
	
	; Input: BCDEHL' is swapped, DE=cycle count, C=cycle offset
	; Output: (STAT) is updated if needed
	;         Bit 7 of R set if render catchup is available
	; Destroys: AF, BC, HL
updateSTAT:
	; Quickly test to see if STAT is valid for this memory access
	; Get the value of DIV at the end of the JIT block
updateSTAT_disable_smc = $
	ld hl,i ; Replaced with RET when LCD is disabled
	add hl,de
	ld b,$FF
	add hl,bc
nextupdatecycle_STAT = $+1
	ld bc,0
	add hl,bc
	inc h
	ret z
	; Now check to see if we are within one scanline after the update time
	; This limitation is needed to ensure the STAT update time is still valid
	dec h
	jr nz,updateSTAT_full
	ld a,l
	CPU_SPEED_IMM8($+1)
	cp CYCLES_PER_SCANLINE
	jr nc,updateSTAT_full
	ld h,l
	ld a,(STAT)
	ld b,a
	and 3
	srl a
lcd_on_updateSTAT_smc = $+1
	jr z,updateSTAT_mode0_mode1 ; Note: A already equals L-H (which is 0)
	ld a,l
	jr c,updateSTAT_mode3
updateSTAT_mode2:
	; Check if we're currently in mode 3
	inc b
	CPU_SPEED_IMM8($+1)
	add a,-MODE_3_CYCLES
	jr nc,updateSTAT_finish
updateSTAT_mode3:
	; Check if we're currently in mode 0
	dec b
	dec b
	dec b
	CPU_SPEED_IMM8($+1)
	add a,-MODE_0_CYCLES
	ld l,a
	; Allow rendering catch-up after leaving mode 3, unless this frame is skipped
	ld a,b
updateSTAT_enable_catchup_smc = $+1
	ld r,a
	ld a,l
	jr nc,updateSTAT_finish
	sub h
updateSTAT_mode0_mode1:
	; Update LY if it hasn't already been by an external LY read
	push hl
	 push bc
	  add a,c
	  ld bc,(nextupdatecycle_LY)
	  sub c
	  ld h,a
	  call z,updateLY_from_STAT
	  ; Check LYC coincidence
	  ld hl,(LY)
	  ld a,l
	  cp h
	 pop bc
	pop hl
	res 2,b
	jr nz,_
	set 2,b
_
	dec a
	cp 143
	jr nc,updateSTAT_maybe_mode1
	; Check if we're currently in mode 2
	inc b
updateSTAT_mode1_exit:
	inc b
	ld a,l
	CPU_SPEED_IMM8($+1)
	add a,-MODE_2_CYCLES
	jr c,updateSTAT_mode2
updateSTAT_finish:
	sub h
	add a,c
	ld (nextupdatecycle_STAT),a
	ld a,b
	ld (STAT),a
	ret c
	ld hl,nextupdatecycle_STAT+1
	dec (hl)
	ret
	
get_scanline_past_vblank:
	 xor a
	 ld r,a ; Disable catchup rendering when overflowing to vblank
	 ld bc,((SCANLINES_PER_FRAME-2)<<8) | (CYCLES_PER_SCANLINE<<1)
	 add hl,bc ; Sets carry flag
	 jr get_scanline_from_cycle_count_finish
	
; Inputs: HL-BC = current value of DIV
; Outputs: (LY) = current value of LY
;          (STAT) = current value of STAT
;          (nextupdatecycle_LY) = negated cycle count of next LY update
;          (nextupdatecycle_STAT) = negated cycle count of next STAT update
; Destroys: AF, BC, HL
updateSTAT_full:
	push de
	 ; Get negative DIV, the starting point for update times
	 ; DE=BC-HL
	 ld a,c
	 sub l
	 ld e,a
	 ld a,b
	 sbc a,h
	 ld d,a
	 ; Subtract from the vblank time, to get cycles until vblank
vblank_counter = $+1
	 ld hl,0
	 add hl,de
	 ; Decrement by 1, for cycles until the cycle before vblank
	 dec hl
	 ; Normalize the divisor and also check if the current cycle is past vblank
updateSTAT_full_speed_smc = $
	 add.s hl,hl ; Replace with LD A,H \ INC A
	 jr c,get_scanline_past_vblank ; Replace with JR Z
	
get_scanline_from_cycle_count:
	 ; Algorithm adapted from Improved division by invariant integers
	 ; To make things simpler, a pre-normalized divisor is used, and the dividend
	 ; and remainder are scaled and descaled according to the normalization factor
	 ; This should also make it trivial to support GBC double-speed mode in the
	 ; future where the normalized divisor will be the actual divisor
	 ld b,h
	 ld c,l
	 ld l,65535 / (CYCLES_PER_SCANLINE<<1) - 256
	 mlt hl
	 add hl,bc
	 ld b,h
	 inc h
	 ld a,l
	 ld l,256-(CYCLES_PER_SCANLINE<<1)
	 mlt hl
	 add hl,bc
	 cp l
	 ld a,l
	 jr c,_
	 sub CYCLES_PER_SCANLINE<<1
	 jr c,get_scanline_from_cycle_count_finish ; Carry set to add 1 to quotient
	 ; Unlikely condition, add 2 to quotient
	 inc b \ inc b
	 .db $DA ;JP C
_
	 sub 256-(CYCLES_PER_SCANLINE<<1)
	 jr c,get_scanline_from_cycle_count_finish ; Unlikely condition, set carry
_
	 ld l,a
get_scanline_from_cycle_count_finish:
	 ; Scanline number (backwards from last before vblank) is in B+carry,
	 ; and cycle offset (backwards from last scanline cycle) is in L
	 ld a,143
	 sbc a,b
	 jr c,updateSTAT_full_vblank
	 ; Scanline is during active video
	 ld b,a
	 xor a
	 ld c,a
	 ; Get the (negative) cycles until the next scanline
	 dec a
	 ; Allow rendering catch-up outside of vblank, if this frame isn't skipped
updateSTAT_full_enable_catchup_smc = $+1
	 ld r,a
	 ld h,a
	 xor l
cpu_speed_factor_smc_1 = $
	 rrca ; NOP this out in double-speed mode
	 ld l,a
	 ; Add to the negative DIV count
	 add hl,de
	 ld (nextupdatecycle_LY),hl
	 ; Determine the STAT mode and the cycles until next update
	 ; Check if during mode 0
	 ld l,a
	 CPU_SPEED_IMM8($+1)
	 add a,MODE_0_CYCLES
	 jr c,_
	 ; Check if during mode 3
	 ld l,a
	 inc c
	 inc c
	 inc c
	 CPU_SPEED_IMM8($+1)
	 add a,MODE_3_CYCLES
	 jr c,_
	 ; During mode 2
	 ld l,a
	 dec c
_
	 ld h,$FF
	 add hl,de
	 ld (nextupdatecycle_STAT),hl
updateSTAT_full_finish:
	 ; Write value of LY
	 ld a,b
	 ld hl,LY
	 ld (hl),a
	 ; Check for LYC coincidence
	 inc hl
	 cp (hl)
	 jr nz,_
	 set 2,c
_
	 ; Write low bits of STAT
	 ld l,STAT & $FF
	 ld a,(hl)
	 and $F8
	 or c
	 ld (hl),a
	pop de
lcd_on_STAT_restore:
	ret ; Replaced with .LIL prefix
	.db $C3
	.dl lcd_on_STAT_restore_helper
	
updateSTAT_full_for_LY_restore:
	sub l
	ld l,a
updateSTAT_full_for_LY_trampoline:
	jr updateSTAT_full
	
updateSTAT_full_vblank:
	 ; Set mode 1 unconditionally
	 ld c,1
	 ; Get the actual scanline, and check whether it's the final line
	 inc a
	 add a,SCANLINES_PER_FRAME - 1
	 ld b,a
	 jr nc,updateSTAT_full_last_scanline
	 ; Get the (negative) cycles until the next scanline
	 sbc a,a
	 ld h,a
	 xor l
cpu_speed_factor_smc_2 = $
	 rrca ; NOP this out in double-speed mode
_
	 ld l,a
_
	 ; Add to the negative DIV count
	 add hl,de
	 ; This will be used for both the next LY and STAT update times
	 ; Since during vblank, STAT must still be updated for LY=LYC
	 ld (nextupdatecycle_LY),hl
	 ld (nextupdatecycle_STAT),hl
	 jr updateSTAT_full_finish
	
updateSTAT_full_last_scanline:
	 ; On the final line, set LY to 0 after the first cycle
	 ld h,$FF
	 ld a,l
	 cpl
cpu_speed_factor_smc_3 = $
	 rrca ; NOP this out in double-speed mode
	 ld l,a
	 CPU_SPEED_IMM8($+1)
	 add a,CYCLES_PER_SCANLINE - 1
	 jr nc,--_
	 ld b,0
	 jr -_
	
updateSTAT_full_for_setup:
	call updateSTAT_full
	ret.l
	
updateLY:
	; Quickly test to see if LY is valid for this memory access,
	; or during the entire block if no cycle info is available
	; Get the value of DIV at the end of the JIT block
updateLY_disable_smc = $
	ld hl,i ; Replaced with RET when LCD is disabled
	add hl,de
	ld b,$FF
	add hl,bc
nextupdatecycle_LY = $+1
	ld bc,0
	add hl,bc
	inc h
	ret z
	; Now check to see if we are within one scanline after the update time
	dec h
	jr nz,updateSTAT_full_for_LY_trampoline
updateLY_from_STAT:
	ld a,l
	CPU_SPEED_IMM8($+1)
	ld l,-CYCLES_PER_SCANLINE
	add a,l
	jr c,updateSTAT_full_for_LY_restore
	; If so, advance to the next scanline directly
	dec h
	add hl,bc
	ld c,a
	ld (nextupdatecycle_LY),hl
	ld hl,LY
	ld a,(hl)
	inc (hl)
	dec a
	cp SCANLINES_PER_FRAME-3
	ret c
	; Special cases for advancing from lines 0, 152, and 153
	; Note that the STAT mode is always 1 during vblank, and always not 1
	; outside of vblank; this allows differentiating between the two halves
	; of line 0 (vblank and active video) without tracking additional state.
	; However, the mode value is not used outside of updateLY unless the STAT
	; cache is also valid.
	jr z,updateLY_from_line_152
	inc a
	jr z,updateLY_from_line_0
	; If LY was 153, check whether to exit vblank
	ld a,c
	ld bc,1
	; Always advance to line 0
	ld (hl),b
	add a,c
	jr nc,updateLY_to_line_0_vblank
	; Exit vblank (setting mode 0) and schedule forward to line 1 change
	ld l,STAT & $FF
	dec (hl)
	CPU_SPEED_IMM8($+1)
	ld bc,1-CYCLES_PER_SCANLINE
updateLY_to_line_0_vblank:
	; Line 0 node 1 duration is one cycle less than originally scheduled
updateLY_to_line_153:
	; Keep line 153 and schedule backward to line 0 change
	ld hl,(nextupdatecycle_LY)
	add hl,bc
	ld (nextupdatecycle_LY),hl
	ret
	
updateLY_from_line_152:
	; If LY was 152, check whether to proceed to line 0
	ld a,c
	CPU_SPEED_IMM8($+1)
	ld bc,CYCLES_PER_SCANLINE-1
	add a,c
	jr nc,updateLY_to_line_153
	; Advance to line 0 in mode 1, keeping original schedule
	ld (hl),b
	ret

updateLY_from_line_0:
	; If the PPU is no longer in vblank, keep line 1 advancement
	ld a,(STAT)
	dec a
	ld c,a
	and 3
	ret nz
	; Return to line 0 and exit vblank (arbitrarily setting mode 0)
	ld (hl),a
	ld l,STAT & $FF
	ld (hl),c
	ret
	
; Input: BCDEHL' are swapped, DE=block cycle count, C=block cycle offset
; Output: BC = current cycle offset
;         Z flag set if TIMA reload occurs this cycle
;         (TIMA) updated to current value
updateTIMA:
	ld hl,i ; Resets carry
	ld a,c
	ld bc,(timer_counter)
	sbc hl,bc
	add a,e
	ld c,a
	ld a,d
	adc a,$FF
	ld b,a
enableTIMA_smc = $ ; Replaced with XOR A \ ADD HL,BC when enabled
	dec a ; Resets Z flag
	ret
	; Handle special case if cycle offset is non-negative
	cp h
	jr z,updateTIMAoverflow
updateTIMAcontinue:
	inc hl
updateTIMA_smc = $+1
	jr $+8
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld a,h
	ld (TIMA),a
	ret
	
updateTIMAoverflow:
	; Check if the cycle offset was non-negative, which is the only case
	; that a mid-instruction overflow is possible
	cp b
	jr nz,updateTIMAcontinue
	; Check if the cycle offset caused the overflow
	ld a,c
	cp l
	jr c,updateTIMAcontinue
	push bc
	 push de
updateTIMAoverflow_loop:
	  ; If so, handle timer event(s) immediately
	  push hl
	   ld l,h ;active_ints
	   set 2,(hl)
	   ex de,hl
	   ld bc,(timer_counter)
	   call timer_expired_handler_already_set
	  pop hl
	  ; Set Z flag if the reload happened on this cycle
	  ld a,h
	  or l
	  add hl,de
	  jr c,updateTIMAoverflow_loop
	 pop de
	pop bc
	jr updateTIMAcontinue
	