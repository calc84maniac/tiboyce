	.assume adl=0

ppu_mode2_line_0_lyc_match:
	; The LYC match bit was already set by the scheduled LYC event
	; Just transition from mode 1 to mode 2
	inc a
	ld (hl),a
	; Check for mode 1 or LYC blocking
	tst a,$50
	jr nz,ppu_mode2_continue
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	dec h
	jr ppu_mode2_continue

ppu_expired_mode2_line_0:
	ld hl,ppu_expired_mode2
	push hl
	inc sp
	inc sp
	; Check if LYC is 0
	ld hl,LYC
	ld a,h ;$FF
	ld (ppu_mode2_LY),a
	and (hl)
	ld l,STAT & $FF
	ld a,(hl)
	jr z,ppu_mode2_line_0_lyc_match
	; Check for mode 1 blocking
	bit 4,a
	jr nz,ppu_mode2_blocked_fast
ppu_mode2_not_blocked:
	sbc hl,hl ;ld hl,active_ints
ppu_expired_mode2:
	; Request STAT interrupt
	set 1,(hl) ;active_ints
	; Set mode 2
	ld hl,STAT
ppu_mode2_blocked:
	ld a,(hl)
ppu_mode2_blocked_fast:
	and $F8
	or 2
	ld (hl),a
ppu_mode2_continue:
	; Allow catch-up rendering if this frame is not skipped
ppu_mode2_enable_catchup_smc = $+1
	ld r,a
	CPU_SPEED_IMM8($+1)
	ld l,-MODE_2_CYCLES
	add hl,de
	ld (nextupdatecycle_STAT),hl
	CPU_SPEED_IMM8($+1)
	ld hl,-CYCLES_PER_SCANLINE
	ex de,hl
	add hl,de
	ld (nextupdatecycle_LY),hl
	ld (ppu_counter),hl
ppu_mode2_LY = $+1
	ld a,0
	inc a
	ld (ppu_mode2_LY),a
	ld (LY),a
ppu_mode2_event_line = $+1
	cp 0
	jp nz,audio_counter_checker
	; Check whether vblank should be handled immediately
	cp VBLANK_SCANLINE
	jr z,ppu_mode2_handle_vblank
	; Check whether LYC actually matched this line
	ld hl,LYC
	cp (hl)
	jr nz,ppu_mode2_lyc_mismatch
	; Set LYC coincidence bit
	ld l,STAT & $FF
	set 2,(hl)
	; Record the successful LYC match
	ld (last_lyc_match),a
	; Get the next prediction and set the event line
	ld.lil hl,lyc_prediction_list
	ld l,a
	ld.l a,(hl)
	ld (ppu_mode2_event_line),a
	ld (writeLYC_event_line_smc),a
	call ppu_scheduled

ppu_expired_mode2_maybe_lyc_block:
	ld hl,ppu_expired_mode2
	push hl
	inc sp
	inc sp
	; Check if LYC is still blocking
	ld a,(last_lyc_match)
	ld hl,LYC
	xor (hl)
	jr nz,ppu_mode2_not_blocked
	ld l,STAT & $FF
	bit 6,(hl)
	jr z,ppu_mode2_not_blocked
	jr ppu_mode2_blocked

ppu_mode2_handle_vblank:
	; Trigger vblank interrupt, STAT interrupt is blocked
	sbc hl,hl ;ld hl,active_ints
	set 0,(hl)
	sbc hl,bc
	ex de,hl
	jp ppu_expired_vblank_continue

ppu_mode2_lyc_mismatch:
	; Set the new event line to either LYC or vblank, whichever is sooner
	ld a,VBLANK_SCANLINE
	jr nc,_
	cp (hl)
	jr c,_
	ld a,(hl)
_
	ld (ppu_mode2_event_line),a
	ld (writeLYC_event_line_smc),a
	; Reset the prediction for the last successful LYC match
	ld.lil hl,(z80codebase+last_lyc_match)
	ld.l (hl),a
	; Schedule non-blocked mode 2 event
	ld hl,ppu_expired_mode2
	push hl
	jp ppu_scheduled

ppu_expired_mode0_line_0:
	xor a
	ld (ppu_mode0_LY),a
	ld hl,LYC
	or (hl)
	jr z,ppu_expired_mode0_maybe_lyc_block
	ld hl,ppu_expired_mode0
	push hl
	inc sp
	inc sp
	sbc hl,hl ;ld hl,active_ints
ppu_expired_mode0:
	; Request STAT interrupt
	set 1,(hl) ;active_ints
	; Set mode 0
	ld hl,STAT
	ld a,(hl)
	and $F8
	ld (hl),a
ppu_mode0_blocked:
	; Allow catch-up rendering if this frame is not skipped
ppu_mode0_enable_catchup_smc = $+1
	ld r,a
	CPU_SPEED_IMM8($+1)
	ld l,-MODE_0_CYCLES
	add hl,de
	ld (nextupdatecycle_STAT),hl
	ld (nextupdatecycle_LY),hl
	CPU_SPEED_IMM8($+1)
	ld hl,-CYCLES_PER_SCANLINE
	ex de,hl
	add hl,de
	ld (ppu_counter),hl
ppu_mode0_LY = $+1
	ld a,0
	ld (LY),a
	inc a
	ld (ppu_mode0_LY),a
ppu_mode0_event_line = $+1
	cp 0
	jp nz,audio_counter_checker

	; Schedule at the start of the next line instead
	sbc hl,de
	CPU_SPEED_IMM8($+1)
	ld e,-MODE_0_CYCLES
	add hl,de
	ld (ppu_counter),hl
	; Check whether vblank should be scheduled
	cp VBLANK_SCANLINE
	jr z,ppu_mode0_schedule_vblank
	call ppu_scheduled

ppu_expired_mode0_maybe_lyc_match:
	; Schedule the mode 0 event
	ex de,hl
	CPU_SPEED_IMM8($+1)
	ld de,-(MODE_2_CYCLES + MODE_3_CYCLES)
	add hl,de
	ld (ppu_counter),hl
	; Check if LYC matches the prediction
	ld hl,LYC
	ld a,(ppu_mode0_event_line)
	cp (hl)
	jr nz,ppu_mode0_lyc_mismatch
	; Record the successful LYC match and get the next prediction
	ld (last_lyc_match),a
	ld.lil hl,lyc_prediction_list
	ld l,a
	ld.l a,(hl)
	ld (ppu_mode0_event_line),a
	ld (writeLYC_event_line_smc),a
	; Set the LY=LYC bit (all we care about for STAT blocking)
	ld hl,STAT
	ld a,(hl)
	set 2,a
	ld (hl),a
	; Check if LYC interrupt is enabled and mode 2 doesn't block
	add a,a
	cp $40
	jp po,_
	; Request STAT interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
_
	call ppu_scheduled

ppu_expired_mode0_maybe_lyc_block:
	ld hl,ppu_expired_mode0
	push hl
	inc sp
	inc sp
	; Set STAT mode 0, preserving LY=LYC bit
	ld hl,STAT
	ld a,(hl)
	and $FC
	ld (hl),a
	; Check if LYC interrupt is blocking hblank interrupt
	cpl
	and $44
	cpl
	jr z,ppu_mode0_blocked
	; Request STAT interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	dec h
	jr ppu_mode0_blocked

ppu_mode0_schedule_vblank:
	ld hl,ppu_expired_vblank
	push hl
	jp ppu_scheduled

ppu_mode0_lyc_mismatch:
	; Set the new event line to either LYC or vblank, whichever is sooner
	ld a,VBLANK_SCANLINE
	jr nc,_
	cp (hl)
	jr c,_
	ld a,(hl)
_
	ld (ppu_mode0_event_line),a
	ld (writeLYC_event_line_smc),a
	; Reset the prediction for the last successful LYC match
	ld.lil hl,(z80codebase+last_lyc_match)
	ld.l (hl),a
	; Schedule non-blocked mode 0 event
	ld hl,ppu_expired_mode0
	push hl
	jp ppu_scheduled

_
	inc sp
	inc sp
ppu_post_vblank_initial_lyc_prediction = $+1
	ld a,0
	jr _

ppu_expired_active_lyc_post_vblank:
	call -_

ppu_expired_active_lyc:
	; Check if the current prediction equals LYC
	ld a,(writeLYC_event_line_smc)
_
	ld hl,LYC
	cp (hl)
	jr nz,ppu_active_lyc_mismatch
	; Set LY to the current value of LYC since it matched
	dec hl
	ld (hl),a
	; Update the last recorded match as well
	ld (last_lyc_match),a
	; Set STAT to mode 2 with LY=LYC set
	ld l,STAT & $FF
	ld a,(hl)
	or $07
	dec a
	ld (hl),a
	; Allow catch-up rendering if this frame is not skipped
ppu_lyc_enable_catchup_smc = $+1
	ld r,a
	; Check for LYC interrupt enable
	bit 6,a
	jr z,_
	; Request STAT interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
_
	; Get the next prediction
last_lyc_match = $+2
	ld.lil hl,lyc_prediction_list + 0
	ld.l a,(hl)
	ld (writeLYC_event_line_smc),a
	; Get the number of scanlines until the next prediction
	sub l
	; Set LY/STAT caches
	CPU_SPEED_IMM8($+1)
	ld hl,-MODE_2_CYCLES
	add hl,de
	ld (nextupdatecycle_STAT),hl
	ex de,hl
	CPU_SPEED_IMM8($+1)
	ld de,-CYCLES_PER_SCANLINE
	add hl,de
	ld (nextupdatecycle_LY),hl
	; Fast path for difference of 1
	ld (ppu_counter),hl
	add a,d ;$FF
	jp z,audio_counter_checker
	; Check for difference of 0, which means vblank has been reached
	jr nc,ppu_active_lyc_handle_vblank
ppu_active_lyc_mismatch_finish:
	; Adjust time for the remaining scanlines
	ld d,a
	xor a
	sub d
	mlt de
	add a,d
	ld d,a
	add hl,de
	ld (ppu_counter),hl
	add hl,bc
	ex de,hl
	jp audio_counter_checker

ppu_active_lyc_mismatch:
	; Check if vblank was the prediction
	cp VBLANK_SCANLINE
	jr z,ppu_active_lyc_handle_vblank_fast
	; Set the new event line to either LYC or vblank, whichever is sooner
	cp (hl)
	ld a,VBLANK_SCANLINE
	jr nc,_
	cp (hl)
	jr c,_
	ld a,(hl)
_
	ld (writeLYC_event_line_smc),a
	; Update the prediction for the last successful LYC match
	ld.lil hl,(z80codebase+last_lyc_match)
	ld.l (hl),a
	; Calculate the offset from vblank
	add a,SCANLINES_PER_VBLANK
	; Schedule using the LYC event offset from vblank
	ld hl,(vblank_counter)
	CPU_SPEED_IMM8($+1)
	ld e,-CYCLES_PER_SCANLINE
	jr ppu_active_lyc_mismatch_finish

ppu_active_lyc_handle_vblank:
	sbc hl,de
	ex de,hl
	or a
ppu_active_lyc_handle_vblank_fast:
	sbc hl,hl ;ld hl,active_ints

ppu_expired_vblank:
	; Always trigger vblank interrupt
	set 0,(hl) ;active_ints
ppu_expired_vblank_continue:
	; Reset last LYC match
	xor a
	ld (last_lyc_match),a
	; Only allow LYC writes outside of valid lines
	ld a,SCANLINES_PER_FRAME
	ld (writeLYC_event_line_smc),a
	; Re-enable rescheduling if it was disabled
	ld a,$40 ;.SIS
	ld.lil (stat_setup_impending_vblank_smc),a
	; Set LY to 144
	ld hl,LY
	ld a,VBLANK_SCANLINE
	ld (hl),a
	; Check for either a LYC match or an LYC block
	inc hl
	sub (hl)
	sub 2
	inc a
	ld l,STAT & $FF
	ld a,(hl)
	jr c,ppu_vblank_lyc_close_match
	; Set mode 1, LYC not matching
	and $F8
	inc a
	ld (hl),a
	; Check for mode 1 interrupt enable
	bit 4,a
	jr nz,ppu_vblank_mode1_int
ppu_vblank_stat_int_continue:
	; Set next LY/STAT update to scanline 145
	CPU_SPEED_IMM8($+1)
	ld l,-CYCLES_PER_SCANLINE
	add hl,de
	ld (nextupdatecycle_LY),hl
	ld (nextupdatecycle_STAT),hl
	; Set the current vblank start time
	ex de,hl
	ld (vblank_counter),hl
	; Save a persistent time relative to which the next vblank must occur,
	; in case the LCD is toggled on and off
	ld (persistent_vblank_counter),hl
	; Check for LYC value during vblank
	ld a,(LYC)
	or a
	jr z,ppu_vblank_schedule_lyc_0
	ld e,a
	sub VBLANK_SCANLINE+1
	cp SCANLINES_PER_VBLANK-1
	jr c,ppu_vblank_schedule_lyc_mode1
	ld.lil a,(lyc_prediction_list+0)
	cp e
	jr z,_
	jr c,_
	ld a,e
	ld.lil (lyc_prediction_list+0),a
_
	; Set the next event time and handler
	call ppu_schedule_post_vblank_event
ppu_vblank_finish:
	jp.lil vblank_helper

ppu_vblank_lyc_close_match:
	jr nz,ppu_vblank_lyc_match
	; LYC=143 case
	; Set mode 1
	and $F8
	inc a
	ld (hl),a
	; Check for mode 1 or mode 2 interrupt enable
	tst a,$30
	jr z,ppu_vblank_stat_int_continue
	; Check for STAT block
	bit 6,a
	jr nz,ppu_vblank_stat_int_continue
ppu_vblank_mode1_int:
	; Check for mode 0 block
	bit 3,a
	jr nz,ppu_vblank_stat_int_continue
	; Trigger STAT interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	dec h
	jr ppu_vblank_stat_int_continue

ppu_vblank_lyc_match:
	; LYC=144 case
	; Set mode 1 with LY=LYC bit
	and $F8
	or 5
	ld (hl),a
	; Check for mode 0 block
	bit 3,a
	jr nz,ppu_vblank_stat_int_continue
	; Check for either mode 1, mode 2, or LY=LYC interrupt enable
	and $70
	jr z,ppu_vblank_stat_int_continue
	; Trigger STAT interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
	dec h
	jr ppu_vblank_stat_int_continue

ppu_vblank_schedule_lyc_0:
	CPU_SPEED_IMM16($+1)
	ld de,-(CYCLES_PER_SCANLINE * 9 + 1)
	jr ppu_vblank_schedule_lyc_finish

ppu_vblank_schedule_lyc_mode1:
	ld d,a
	inc d
	CPU_SPEED_IMM8($+1)
	ld e,-CYCLES_PER_SCANLINE
	mlt de
	cpl
	add a,d
	ld d,a
ppu_vblank_schedule_lyc_finish:
	add hl,de
	ld (ppu_counter),hl
	call ppu_vblank_finish

ppu_expired_lyc_mode1:
	; Make sure LYC wasn't written since this event was scheduled
	ld hl,LYC
	ld a,(hl)
	cp SCANLINES_PER_FRAME
	jr nc,ppu_lyc_mode1_mismatch
	; Set LY to LYC
	dec hl
	ld (hl),a
	; Calculate scanline length depending on LY value
	or a
	jr z,_
	cp 153
	CPU_SPEED_IMM8($+1)
	ld a,-1
	jr z,++_
_
	CPU_SPEED_IMM8($+1)
	add a,1-CYCLES_PER_SCANLINE
_
	; Set LY/STAT caches
	ld l,a
	add hl,de
	ld (nextupdatecycle_LY),hl
	ld (nextupdatecycle_STAT),hl
	; Mode is already 1 from the vblank event, so simply set LY=LYC bit
	ld hl,STAT
	ld a,(hl)
	set 2,a
	ld (hl),a
	; Check if LY=LYC interrupt is enabled and not blocked by mode 1 interrupt
	xor $40
	and $50
	jr nz,_
	; If so, trigger LYC interrupt
	sbc hl,hl ;ld hl,active_ints
	set 1,(hl)
_
ppu_lyc_mode1_mismatch:
	ld hl,(vblank_counter)
	ld.lil a,(lyc_prediction_list+0)
	call ppu_schedule_post_vblank_event
	jp ppu_scheduled

ppu_schedule_post_vblank_event:
	ld d,a
	ld a,(STAT)
	and $28
	jr nz,ppu_schedule_post_vblank_mode0_mode2
	ld a,d
	ld (ppu_post_vblank_initial_lyc_prediction),a
	add a,SCANLINES_PER_VBLANK
	ld d,a
	CPU_SPEED_IMM8($+1)
	ld e,CYCLES_PER_SCANLINE
	mlt de
	sbc hl,de
	ld (ppu_counter),hl
	add hl,bc
	ex de,hl
	ld hl,ppu_expired_active_lyc_post_vblank
	ex (sp),hl
	jp (hl)

ppu_schedule_post_vblank_mode0_mode2:
	bit 3,a
	ld a,d
	jr z,ppu_schedule_post_vblank_mode2
	ld (ppu_mode0_event_line),a
	CPU_SPEED_IMM16($+1)
	ld de,-((CYCLES_PER_SCANLINE * SCANLINES_PER_VBLANK) + MODE_2_CYCLES + MODE_3_CYCLES)
	add hl,de
	ld (ppu_counter),hl
	add hl,bc
	ex de,hl
	ld hl,ppu_expired_mode0_line_0
	ex (sp),hl
	jp (hl)

ppu_schedule_post_vblank_mode2:
	ld (ppu_mode2_event_line),a
	CPU_SPEED_IMM16($+1)
	ld de,-(CYCLES_PER_SCANLINE * SCANLINES_PER_VBLANK)
	add hl,de
	ld (ppu_counter),hl
	add hl,bc
	ex de,hl
	ld hl,ppu_expired_mode2_line_0
	ex (sp),hl
	jp (hl)

ppu_expired_lcd_off:
	; Set the current vblank start time
	ex de,hl
	ld (vblank_counter),hl
	; Save a persistent time relative to which the next vblank must occur,
	; in case the LCD is toggled on and off
	ld (persistent_vblank_counter),hl
	; Set the next event time
	CPU_SPEED_IMM16($+1)
	ld de,-CYCLES_PER_FRAME
	add hl,de
	ld (ppu_counter),hl
	dec sp
	dec sp
	jp.lil vblank_helper

timer_counter_checker:
timer_counter = $+1
	ld hl,0
	or a
	sbc hl,bc
	jr z,timer_expired_handler
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret

timer_expired_handler:
	set 2,(hl) ;active_ints
timer_expired_handler_already_set:
timer_period = $+1
	ld hl,0
	; If scheduled for 65536 cycles in the future, no need to reschedule
	; Returning here prevents the delay from being interpreted as 0
	add hl,hl
	ret c
	add hl,bc
	ld (timer_counter),hl
	or a
	sbc hl,bc
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret

audio_expired_handler:
	ld.lil a,(mpIntMaskedStatus+1)
	or a
	jr nz,do_polled_interrupt
polled_interrupt_return:
	ld a,(NR52)
	tst a,$0F
	jr z,audio_expired_disabled
	push.l bc
	 ld h,audio_port_value_base >> 8
	 ld b,$3F
	 ld c,a
	 rra
	 jr nc,++_
	 ld l,NR14-ioregs
	 bit 6,(hl)
	 jr z,++_
	 ld l,NR11-ioregs
	 ld a,(hl)
	 inc a
	 tst a,b
	 jr nz,_
	 dec c
	 sub $40
_
	 ld (hl),a
_
	 bit 1,c
	 jr z,++_
	 ld l,NR24-ioregs
	 bit 6,(hl)
	 jr z,++_
	 ld l,NR21-ioregs
	 ld a,(hl)
	 inc a
	 tst a,b
	 jr nz,_
	 res 1,c
	 sub $40
_
	 ld (hl),a
_
	 bit 2,c
	 jr z,_
	 ld l,NR34-ioregs
	 bit 6,(hl)
	 jr z,_
	 ld l,NR31-ioregs
	 inc (hl)
	 jr nz,_
	 res 2,c
_
	 bit 3,c
	 jr z,_
	 ld l,NR44-ioregs
	 bit 6,(hl)
	 jr z,_
	 ld l,NR41-ioregs
	 ld a,(hl)
	 inc a
	 and b
	 ld (hl),a
	 jr nz,_
	 res 3,c
_
	 ld a,c
	 ld (NR52),a
	pop.l bc
audio_expired_disabled:
	ld a,b
audio_counter_offset = $+1
	CPU_SPEED_IMM8($+1)
	add a,4096 >> 8	; Double this in double-speed mode
	ld (audio_counter+1),a
	sub b
	add a,d
	ret c
	CPU_SPEED_IMM8($+2)
	ld de,-4096	; Double this in double-speed mode
	ret

do_polled_interrupt:
	jp.lil polled_interrupt

serial_counter_checker:
serial_counter = $+1
	ld hl,0
	or a
	sbc hl,bc
	jr z,serial_expired_handler
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret

serial_expired_handler:
	set 3,(hl) ;active_ints
	dec h
	inc hl ;SB
	ld (hl),h ;$FF
	inc hl ;SC
	res 7,(hl)
	call disabled_counter_checker
disabled_counter_checker:
	ret

hdma_immediate_handler:
	; Do an immediate transfer, then check the time until the next transfer
	ld a,(LCDC)
	rla
	jr nc,hdma_immediate_lcd_off
	call hdma_do_transfer

hdma_counter_checker:
hdma_counter = $+1
	ld hl,0
	or a
	sbc hl,bc
	jr z,hdma_expired_handler
	add hl,de
	ret c
	ex de,hl
	sbc hl,de
	ex de,hl
	ret

hdma_immediate_lcd_off:
	call hdma_do_transfer

hdma_lcd_off_handler:
	ret

hdma_expired_handler:
	inc hl ;hdma_line_counter
	inc (hl)
	jr z,hdma_schedule_next_vblank
	CPU_SPEED_IMM8($+1)
	ld l,CYCLES_PER_SCANLINE
hdma_schedule_next:
	add hl,bc
	ld (hdma_counter),hl
	or a
	sbc hl,bc
	add hl,de
	jr c,hdma_do_transfer
	ex de,hl
	sbc hl,de
	ex de,hl
hdma_do_transfer:
	jp.lil hdma_transfer_helper

hdma_schedule_next_vblank:
	ld (hl),256-144
	CPU_SPEED_IMM16($+1)
	ld hl,CYCLES_PER_SCANLINE * 11
	jr hdma_schedule_next

hdma_transfer_finish:
	ret p
hdma_transfer_overflow:
	; Set SP directly in case this was an immediate HDMA
	ld sp,event_counter_checkers_ei_delay_2
	ex (sp),hl
	push hl
	ld (event_counter_checkers_ei_delay_smc_1),sp
	ld (event_counter_checkers_ei_delay_smc_2),sp
	ret
