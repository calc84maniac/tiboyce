mbc_change_rom_bank_helper:
	; Propagate the bank change to the topmost rom callstack entry
	ld c,a
	ld (mbc_change_rom_bank_smc),sp
	inc sp
	ld a,$BF
_
	pop hl
	cp l
	jp po,-_
	ld a,c
	xor h
	ld h,a
	push hl
	ld c,4
	mlt bc
	ld hl,rombankLUT
	add hl,bc
	ld a,(hl)
	inc hl
	ld hl,(hl)
	ld (rom_bank_base),hl
	ld (rom_bank_base_for_read),hl
curr_rom_bank_trim_msb = $+1
	ld c,0
	cp c
	jr nz,mbc_change_rom_bank_trim_msb
mbc_change_rom_bank_smc = $+1
	ld sp,0
	ld a,(z80codebase+curr_gb_stack_region)
	cp rom_bank_base & $FF
	jp.sis nz,mbc_no_fix_sp
	jp.sis mbc_fix_sp_full
	
mbc_change_rom_bank_trim_msb:
	ld (curr_rom_bank_trim_msb),a
	ld hl,z80codebase+mem_read_lut
	jr nc,mbc_change_rom_bank_trim_less
	ld l,c
	sub c
	ld bc,(rom_trimmed_get_ptr & $FF) * $010101
	ld (hl),c
	ld sp,hl
	ld hl,rom_bank_fill_routines
	ld l,a
	ld l,(hl)
	ld a,(z80codebase+curr_gb_stack_region)
	cp rom_bank_base & $FF
CallHL:
	jp (hl)
	
mbc_change_rom_bank_trim_less:
	ld l,a
	sub c
	ld bc,(rom_banked_get_ptr & $FF) * $010101
	ld (hl),c
	ld sp,hl
	ld hl,rom_bank_fill_routines
	ld l,a
	ld l,(hl)
	ld a,(z80codebase+curr_gb_stack_region)
	cp rom_bank_base & $FF
	jp (hl)


	; Propagate the bank mismatch value to the next callstack entry
	; in the same region as the return address
callstack_ret_bank_mismatch_helper:
	ld b,h
	; Ensure the return address is in the $4000-$7FFF bank
	ld a,$BF
	cp l
#ifdef DEBUG
	jp po,$
#else
	jp po,++_
#endif
	ld (callstack_ret_bank_mismatch_smc),sp
_
	pop hl
	cp l
	jp po,-_
	; Combine the callstack entry high bytes
	ld a,b
	xor h
	ld h,a
	push hl
callstack_ret_bank_mismatch_smc = $+1
	ld sp,0
_
	pop.s bc
	pop.s hl
	jp.s (hl)


update_rtc_helper:
	ld ix,mpRtcSecondCount
_
	ld b,(ix)
	ld e,(ix-mpRtcSecondCount+mpRtcMinuteCount)
	ld d,(ix-mpRtcSecondCount+mpRtcHourCount)
	ld hl,(ix-mpRtcSecondCount+mpRtcDayCount)
	; Clear the second interrupt status before reloading the second counter
	ld (ix-mpRtcSecondCount+mpRtcIntStatus),1
	ld a,(ix)
	cp b
	jr nz,-_
	 
	; If the RTC is halted, set the last time to now
	ld ix,z80codebase+rtc_last
	bit 6,(ix-4)
	jp nz,update_rtc_halted
	
	; Get the time since last update
	sub (ix)
	ld (ix),b
	jr nc,_
	add a,60
_
	ld b,a

	ld a,e
	sbc a,(ix+1)
	ld (ix+1),e
	jr nc,_
	add a,60
_
	ld e,a
	 
	ld a,d
	sbc a,(ix+2)
	ld (ix+2),d
	jr nc,_
	add a,24
_
	ld d,a
	 
	ld a,l
	sbc a,(ix+3)
	ld (ix+3),l
	ld l,a
	 
	ld a,h
	sbc a,(ix+4)
	ld (ix+4),h
	ld h,a
	 
	lea ix,ix-8
	ld a,(ix)
	; Adjust carry range
	add a,64-60
	or $C0
	; Add elapsed seconds
	add a,b
	jr c,++_
_
	; Adjust back to normal range
	add a,60
	jr c,++_
	; Value was out of range, but if any minutes elapsed
	; then consume one of them to go back into range
	call update_rtc_dec_minutes
	jr nc,-_
	; No minutes available, so remain out of range
	ld b,a
	jr update_rtc_seconds_only
_
	; Increment elapsed minutes due to carry
	inc e
_
	ld b,a
	 
	ld a,(ix+1)
	; Adjust carry range
	add a,64-60
	or $C0
	; Add elapsed minutes
	add a,e
	jr c,++_
_
	; Adjust back to normal range
	add a,60
	jr c,++_
	; Value was out of range, but if any hours elapsed
	; then consume one of them to go back into range
	call update_rtc_dec_hours
	jr nc,-_
	; No hours available, so remain out of range
	ld (ix+1),a
	jr update_rtc_seconds_only
_
	; Increment elapsed hours due to carry
	inc d
_
	ld e,a
	 
	ld a,(ix+2)
	; Adjust carry range
	add a,32-24
	or $E0
	; Add elapsed hours
	add a,d
	jr c,++_
_
	; Adjust back to normal range and reset carry
	sub -24
	jr nc,_
	; Value was out of range, but if any days elapsed
	; then consume one of them to go back into range
	call update_rtc_dec_days
	jr nc,-_
	; No days available, so remain out of range
	ld d,a
	jr update_rtc_hours_only
_
	ld d,a
	 
	; Add low byte of days, along with carry
	ld a,(ix+3)
	adc a,l
	ld l,a
	
	; Add carry to days, carry out if at least 2 days passed
	ld a,h
	adc a,$FE
	; Put carry in the high bit and bit 8 of the day in the low bit
	rla
	rrca
	and $81
	ld h,a
	; Add to the existing value, ensuring carry from low bit
	; propagates to the high bit
	ld a,(ix+4)
	or $7E
	add a,h
	jr nc,_
	; Make sure overflow bit sticks
	or $80
_
	; Mask result (ensures halt bit is reset)
	and $81
	ld h,a
	
update_rtc_halted:
	ld.s (ix+3),hl
update_rtc_hours_only:
	ld.s (ix+1),de
update_rtc_seconds_only:
	ld (ix),b
	ret.l


update_rtc_dec_minutes:
	dec e
	ret p
	ld e,59
update_rtc_dec_hours:
	dec d
	ret p
	ld d,23
update_rtc_dec_days:
	dec hl
	push hl
	 add hl,hl
	pop hl
	ret
